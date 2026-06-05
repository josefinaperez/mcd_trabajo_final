# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project context

Master's thesis on Species Distribution Modeling (SDM) of fungi in Argentina. All active work lives in **`r/`**: GBIF download → cleaning → background generation → environmental-layer harmonization (2.5 arc-min grid: bioclim + vegetation + topography + soil) → multi-algorithm training (`maxnet`, `ranger`/Random Forest, `xgboost`) with spatial-block cross-validation → prediction maps + XAI.

Documentation, methodology (`docs/metodologia/`), and the README are in **Spanish**. Code, identifiers, and column names are in English. Match this convention when editing.

The `python/` directory is **deprecated** (early EDA only) and can be deleted. Do not add new work there; do not refactor it. If asked to clean up, removing it wholesale is fine.

## Commands

Run from the **repo root** — paths in `r/src/*.R` are repo-relative.

```r
# 0) Harmonize raw environmental rasters to the 2.5 arc-min Argentina grid
#    (reference grid + vegetation + topography). Run once after placing raw inputs.
source("r/src/prepare_env_layers.R")

# 1) Full dataset build pipeline (download → preprocess → bias correction → BG → env extraction)
source("r/src/dataset_pipeline.R")

# 2) (Optional) Collinearity reduction per env_set → selected_vars*.csv, which
#    dataset_pipeline.R then registers as *_reduced env_sets on the next run.
source("r/src/env_selection_pipeline.R")

# 3) Train all algorithms (maxnet, ranger, xgboost) across every dataset config
#    under spatial-block CV; persist per-run artifacts + summary.
source("r/src/train_pipeline.R")

# 4) Prediction maps, XAI (SHAP/PDP/LIME) and residual spatial-autocorrelation
#    diagnostics. NOTE: these must be run with `Rscript r/src/<file>.R` directly
#    — they branch on sys.nframe() and skip the main block when source()'d.
#    residual_autocorr_pipeline.R reads predict's winner_summary.csv, so run it
#    after predict_pipeline.R.
#    Rscript r/src/predict_pipeline.R
#    Rscript r/src/xai_pipeline.R
#    Rscript r/src/residual_autocorr_pipeline.R

# 5) (Optional) Render the display-only notebooks from the persisted outputs
rmarkdown::render("r/notebooks/stage1_maxent.Rmd")        # metrics + curves
rmarkdown::render("r/notebooks/stage3_distribution_map.Rmd")
rmarkdown::render("r/notebooks/stage4_xai.Rmd")
rmarkdown::render("r/notebooks/stage5_residual_diagnostics.Rmd")
```

**Convention**: scripts under `r/src/` do all the computation and write artifacts to disk. Notebooks under `r/notebooks/` are **display-only** — they `read_csv` / `include_graphics` from `data/outputs/...` and never train, fit, or transform. If a notebook needs new data, add it to the corresponding `*_pipeline.R` first.

**Idempotency**: `dataset_pipeline.R` and `train_pipeline.R` are **skip-if-exists** by default — already-downloaded occurrences, already-built datasets, and already-trained `(run × cv × algo)` models are detected on disk and reused (their manifest/metrics rows are reconstructed from the persisted CSVs, so the summary stays complete). This makes adding a new `env_set` cheap: only the new configs train. Set `SDM_FORCE=1` (or `true`/`yes`) in the environment to force a full recompute: `SDM_FORCE=1 Rscript r/src/train_pipeline.R`. Use `force = TRUE` when calling the builder functions directly. Note this is a **content-blind** skip (presence of the output files), so after changing the code that *produces* an artifact, force a rebuild of the affected runs.

The active RStudio project is `r/mcd_trabajo_final.Rproj`. The top-level `mcd_trabajo_final.Rproj` is stale.

## Architecture

### Dataset pipeline

`r/src/dataset_pipeline.R` is the single orchestration entry point and runs three stages in order:

1. **Download** (`download_gbif.R`) — paginated `rgbif::occ_search` per species, written to `data/ocurrences/raw/df_<species_slug>.csv`.
2. **Preprocess** (`preprocessing.R` + `utils.R`) — date filter, scientificName match, clip to Argentina shapefile (`data/shp/argentina/argentina.shp`), uncertainty filter, `basisOfRecord` whitelist, then `CoordinateCleaner::clean_coordinates` flag tests. Cleaned rows go to `data/ocurrences/processed/`.
3. **Build SDM datasets** (`build_parallel_sdm_datasets.R`) — fans out across a Cartesian config table (species × bias_method × bp_method × bp_n_strategy × env_set × grid_size). Each config produces a `run_id`-named subfolder under `data/outputs/sdm_parallel/` containing `occ_processed.csv`, `background_points.csv`, `sdm_dataset.csv`, and `sdm_dataset_model_ready.csv`. A top-level `manifest.csv` indexes every run.

`grid_thinning.R` implements grid-based spatial rarefaction in an Argentina-centered equal-area projection (`+proj=aea`) — required because thinning distances are in km.

### Environmental layers (2.5 arc-min grid)

All predictors are harmonized to a single reference grid — **2.5 arc-min (~4.6 km), EPSG:4326, clipped to Argentina** — so models across `env_set`s are comparable. The `env_set` (which variables) is the hyperparameter; the resolution is fixed.

- `r/src/env_layers.R` — **pure functions** (no I/O): `crop_mask_to_region`, `align_to_template`, `cover_fraction`, `temporal_mean`, `temporal_amplitude`, `fill_na`, `interior_na_fraction` (QA: fraction of NA cells inside the region). Unit-tested on synthetic rasters by `r/checks/check_env_layers.R`.
- `r/src/prepare_env_layers.R` — **orchestrator** (the only place with raster I/O for layer prep). Builds the reference grid from WorldClim 2.5m bioclim, then aligns vegetation, topography, soil and anthropic layers to it, writing to `data/features/env_2.5m_ar/{bioclim,vegetation,topography,soil,anthropic}/*.tif`. It is **resilient to partial downloads**: the grid is always built; the vegetation, topography, soil and anthropic blocks each run only if all their raw inputs are present (`veg_ready` / `topo_ready` / `soil_ready` / `anthro_ready` guards), otherwise they skip with a message. Heavy downsampling (e.g. ESA WorldCover 10m, INTA canopy 30m, Geomorpho90m 90m, SoilGrids 250m) is done via GDAL warp (`sf::gdal_utils`) with `-r average`, then `align_to_template`.
  - **Topography, soil & anthropic** share a generic helper `build_aggregated_layers(raw_dir, vars, out_dir, template, ar)` (VRT → warp average → align → crop, **no** `fill_na` — NA = no data, unlike canopy where NA→0). `build_topography_layers` (Geomorpho90m `cti`/`slope`/`tpi`/`tri`, raw under `data/features/topography_raw/`), `build_soil_layers` (SoilGrids 0-5cm `phh2o`/`soc`/`sand`/`silt`/`clay`/`cec`/`bdod`, raw under `data/features/soil_raw/`, exported from GEE already rescaled to conventional units) and `build_anthropic_layers` (issue #47: `travel_time` = accessibility to cities (Weiss 2018) + `human_modification` = gHM index (Kennedy 2019), raw under `data/features/anthropic_raw/`, exported from GEE) are thin wrappers. Verified by the guarded `r/checks/check_topography_layers.R`, `check_soil_layers.R` and `check_anthropic_layers.R` (alignment + interior-NA coverage).
- **`env_sets`** are assembled in `dataset_pipeline.R` by `list.files()` over those output dirs: `bioclim`, `bioclim_veg`, and (if prepared) `bioclim_topo`, `bioclim_veg_topo`, `bioclim_soil`, `bioclim_veg_soil`, `bioclim_anthro`, `bioclim_veg_anthro`. Each also gets a non-collinear `*_reduced` variant **iff** the matching `data/outputs/env_selection/selected_vars*.csv` exists (`register_reduced_env_set`). The **anthropic** env_sets (#47) are a **bias diagnostic**, not ecological predictors: if `travel_time`/`human_modification` dominate the winner's XAI, the GBIF sample is accessibility-driven rather than ecology-driven (expected for wood-decay fungi) — a reportable negative result.
- `r/src/env_selection_pipeline.R` + `select_env_vars.R` — collinearity reduction (`|r| > cutoff`) per env_set over a reference dataset; writes `selected_vars{,_veg,_topo,_veg_topo,_soil,_veg_soil}.csv` consumed by the reduced env_sets. **Multi-pass:** build datasets for the base env_sets → run selection → re-run `dataset_pipeline.R` to materialize the `*_reduced` datasets.

### Model training

Two-layer split, **algorithm-agnostic**:

- `r/src/train_models.R` — pure functions. `fit_model(algo, X, y, hp)` dispatches over `SUPPORTED_ALGOS = c("maxnet", "ranger", "xgboost")` (defaults in `ALGO_DEFAULTS`). `run_model_for_dataset()` consumes one `sdm_dataset_model_ready.csv` and evaluates under **spatial-block CV** (`run_model_spatial_block`): K-fold spatial cross-validation, then a final refit on all data. Writes per-run artifacts into `data/outputs/sdm_models/<run_id>/<cv_scheme>/<algo>/` (`model.rds`, `predictions.csv`, `metrics.csv`, response/PDP curves, etc.).
  - Required dataset columns: `class`, `decimalLongitude`, `decimalLatitude`. Everything else is treated as a predictor.
- `r/src/evaluate_model.R` — metric computation, algorithm-agnostic: dual threshold-based metrics (threshold at max TSS → `tss`, `fnr`, sens/spec) plus the continuous **Boyce index** (Hirzel et al. 2006). No fixed 70/30 holdout — evaluation is the spatial-block folds.
- `r/src/spatial_cv.R` — builds the spatial blocks. `train_pipeline.R` calibrates the block size **once per run** from the spatial autocorrelation range of the BIO layers (capped at `BLOCK_SIZE_CAP_KM = 300`), persisting `autocor_range_table.csv` and `spatial_cv_config.csv`.
- `r/src/train_pipeline.R` — orchestrator. Reads the dataset manifest and loops over `cv_scheme × algo × run` (`CV_SCHEMES = c("spatial_block")`, `ALGOS = c("maxnet", "ranger", "xgboost")`, `K_FOLDS = 5`). Spatial folds are cached per `run_id` (they depend only on the dataset, not the algorithm). Writes a joined `manifest.csv` (dataset manifest + metrics) and summary/comparison outputs into `data/outputs/sdm_models/`.

Prediction maps and explainability are separate stages: `predict_pipeline.R` (+ `predict_distribution_map.R`) and `xai_pipeline.R` (+ `xai_shap.R` / `xai_pdp.R` / `xai_lime.R`, dispatched via `xai_predict.R`). Both **must be run with `Rscript` directly** (they guard their main block on `sys.nframe()`).

**Residual spatial-autocorrelation diagnostics** (issue #34): `residual_autocorr_pipeline.R` (+ pure functions in `residual_autocorrelation.R`, tested by `r/checks/check_residual_autocorrelation.R`). For each winner in `predict_pipeline`'s `winner_summary.csv`, it reads the pooled spatial-block `predictions_test.csv`, computes raw residuals (`class − score`) over all evaluation points, and runs **Moran's I** (k=8 nearest-neighbour weights, AEA projection) with a Monte-Carlo permutation p-value (`spdep::moran.mc`) plus a distance-band correlogram and a residual map. Writes `morans_i.csv`, `correlogram.csv`, `residual_map.png`, `correlogram.png` per winner and a top-level `summary.csv` under `data/outputs/sdm_residuals/`. Same `Rscript`-only / `sys.nframe()` / skip-if-exists+`SDM_FORCE` conventions; **depends on `predict_pipeline.R` having produced `winner_summary.csv`**. New CRAN dependency: `spdep`.

The display-only notebooks (`stage1_maxent.Rmd` metrics/curves, `stage3_distribution_map.Rmd`, `stage4_xai.Rmd`, `stage5_residual_diagnostics.Rmd`) `read_csv` / `include_graphics` from `data/outputs/...`. **Notebook paths are relative to `r/notebooks/` (e.g. `../../data/...`)** — different from the source scripts, which assume the working directory is the repo root.

### Working-directory contract

This is the single biggest footgun: everything in `r/src/*.R` `source()`s siblings as `r/src/foo.R` and reads data as `data/...`, so it only works when the current directory is the **repo root**. Notebooks under `r/notebooks/` use `../../data/...`. When adding new scripts, follow whichever convention matches the file's location — do not mix.

### Manifest-driven workflow

The `manifest.csv` files are the integration contract between stages. The dataset pipeline writes one; `train_pipeline.R` reads it, trains every `algo × cv_scheme` for each row, and writes a joined `manifest.csv` for the models. Downstream comparison/evaluation/prediction/XAI work should consume these manifests rather than walking directories.

## Data

`data/` is gitignored. Only `data/ocurrences/raw/*.csv` (a few small species CSVs) are tracked. Raw inputs must be obtained separately and placed manually:

- **WorldClim 2.5m bioclim** at `data/features/worldclim/wc2.1_2.5m_bio/` (the reference grid).
- **Vegetation** raw at `data/features/vegetation_raw/` (ESA WorldCover tiles, MODIS NDVI, INTA canopy height).
- **Topography** raw at `data/features/topography_raw/{cti,slope,tpi,tri}/` (Geomorpho90m; e.g. clipped GeoTIFFs exported from Google Earth Engine `projects/sat-io/open-datasets/Geomorpho90m/*`).
- **Anthropic** (#47) raw at `data/features/anthropic_raw/{travel_time,human_modification}/` (GEE: `Oxford/MAP/accessibility_to_cities_2015_v1_0` + `CSP/HM/GlobalHumanModification`, clipped to Argentina at ~1km; see `data/features/anthropic_raw/GEE_export.js`).
- **Argentina shapefile** at `data/shp/argentina/argentina.shp`.

`prepare_env_layers.R` turns these into the harmonized `data/features/env_2.5m_ar/` set that the dataset pipeline consumes.

## Caveats when editing

- **`r/src/borrador/`** is a scratch/draft folder ("borrador" = draft). Don't treat its contents as authoritative when refactoring.
- Background generation in `build_parallel_sdm_datasets.R` uses `generate_background_points()`, which dispatches on `bp_method`:
  - `"random"` → `sample_random_background()`, sampling over a per-`env_set` validity mask (`valid_mask <- sum(env_rast)`: NA wherever any layer is NA). Since the `env_2.5m_ar/` layers are already clipped to Argentina, this is the intersection of raster validity ∩ the country polygon, so no background falls in NA cells (issue #37). The old shapefile sampler (`sample_random_background_from_shp()`) was removed.
  - `"bias_weighted"` (issue #48, target-group / Phillips et al. 2009) → `sample_weighted_background()`, which samples cells with probability ∝ an **accessibility** surface `w = exp(-travel_time / τ)` (τ = median `travel_time` at the presences). This makes the background share the presences' sampling bias so accessibility **cancels** in the presence/background contrast — bias correction *without* putting the anthropic variable in the model. Registered in `make_config_table` via `bias_weighted_env_sets` (ecological env_sets only, `bias_method="none"`), so `bp-bias_weighted` runs sit alongside the `bp-random` ones for comparison. Needs the #47 `travel_time` layer (`TRAVEL_TIME_PATH`); `dataset_pipeline.R` only enables it if that layer exists. Verified by `r/checks/check_weighted_background.R`.
  - `"spatially_constrained"` (issue #52, Miyaji strategy (ii) / Lobo et al. 2010) → `sample_spatially_constrained_background()`, which imposes a geographic exclusion buffer (`buffer_km`, default 20) around the presences (computed in AEA) and delegates to `sample_random_background()` over the buffered-out mask. Registered via `spatially_constrained_env_sets` (ecological env_sets, `bias_method="none"`); doesn't depend on anthropic layers, so always enabled. Verified by `r/checks/check_background_strategies.R`.
  - `"environmentally_dissimilar"` (issue #53, Miyaji strategy (iii) / Chefaoui & Lobo 2008 + One-Class SVM, Schölkopf et al. 2001) → `sample_environmentally_dissimilar_background()`, which fits an OCSVM (`e1071::svm(type="one-classification")`, RBF) on the **presence env** to define the niche envelope (via the shared helper `ocsvm_dissimilar_cells`), predicts over all valid cells, and samples background uniformly among the **outliers** (cells outside the envelope = environmentally dissimilar). `nu` (default 0.1, exposed via `bp_params$nu`) is the upper bound on the fraction of presences left outside the envelope — **small `nu` ⇒ permissive envelope ⇒ fewer dissimilar cells** (the opposite of the naive reading); 0.1 is conservative to avoid putting suitable habitat in the background. Registered via `environmentally_dissimilar_env_sets` (ecological env_sets, `bias_method="none"`); always enabled. This is the only `bp_method` that needs the **`env_rast` stack** (not just `valid_mask`), so `generate_background_points()` takes an `env_rast` argument (ignored by the other methods) fed from `build_one_sdm_dataset`. **New CRAN dependency: `e1071`** (loaded via `requireNamespace`). Verified by `r/checks/check_background_strategies.R`.
  - `"three_step"` (issue #54, Miyaji strategy (iv) / Senay et al. 2013) → `sample_three_step_background()`, which **composes** the buffer (#52, `build_exclusion_mask`) and the OCSVM outliers (#53, `ocsvm_dissimilar_cells`): the candidate pool is (outside the `buffer_km`=20 buffer) ∩ (OCSVM outliers), then **K-means** (`k`=10, exposed via `bp_params$k`) stratifies that pool in env space and background is allocated **uniformly ~n/k per cluster** (`allocate_uniform_per_cluster`) to cover the dissimilar space evenly. Registered via `three_step_env_sets` (ecological env_sets, `bias_method="none"`); always enabled; also needs `env_rast`. Verified by `r/checks/check_background_strategies.R`. With #54 the four Miyaji strategies (#51-#54) are all implemented.
- `bias_method = "grid_thin"` paths in `build_one_sdm_dataset` require `grid_thin_gbif()` to be in scope; `dataset_pipeline.R` sources it transitively, so re-source the pipeline rather than calling builder helpers directly.
