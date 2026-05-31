# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project context

Master's thesis on Species Distribution Modeling (SDM) of fungi in Argentina. All active work lives in **`r/`**: GBIF download → cleaning → background generation → environmental-layer harmonization (2.5 arc-min grid: bioclim + vegetation + topography) → multi-algorithm training (`maxnet`, `ranger`/Random Forest, `xgboost`) with spatial-block cross-validation → prediction maps + XAI.

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

# 4) Prediction maps and XAI (SHAP/PDP/LIME). NOTE: these two must be run with
#    `Rscript r/src/<file>.R` directly — they branch on sys.nframe() and skip the
#    main block when source()'d interactively.
#    Rscript r/src/predict_pipeline.R
#    Rscript r/src/xai_pipeline.R

# 5) (Optional) Render the display-only notebooks from the persisted outputs
rmarkdown::render("r/notebooks/stage1_maxent.Rmd")        # metrics + curves
rmarkdown::render("r/notebooks/stage3_distribution_map.Rmd")
rmarkdown::render("r/notebooks/stage4_xai.Rmd")
```

**Convention**: scripts under `r/src/` do all the computation and write artifacts to disk. Notebooks under `r/notebooks/` are **display-only** — they `read_csv` / `include_graphics` from `data/outputs/...` and never train, fit, or transform. If a notebook needs new data, add it to the corresponding `*_pipeline.R` first.

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
- `r/src/prepare_env_layers.R` — **orchestrator** (the only place with raster I/O for layer prep). Builds the reference grid from WorldClim 2.5m bioclim, then aligns vegetation and topography to it, writing to `data/features/env_2.5m_ar/{bioclim,vegetation,topography}/*.tif`. It is **resilient to partial downloads**: the grid is always built; vegetation and topography blocks each run only if all their raw inputs are present (`veg_ready` / `topo_ready` guards), otherwise they skip with a message. Heavy downsampling (e.g. ESA WorldCover 10m, INTA canopy 30m, Geomorpho90m 90m) is done via GDAL warp (`sf::gdal_utils`) with `-r average`, then `align_to_template`.
  - **Topography** (`build_topography_layers`): Geomorpho90m `cti` (TWI), `slope`, `tpi`, `tri` (ruggedness). Raw tiles expected under `data/features/topography_raw/{cti,slope,tpi,tri}/`. NA is **not** filled (NA = no data, unlike canopy where NA→0). Verified by the guarded `r/checks/check_topography_layers.R` (alignment + interior-NA coverage).
- **`env_sets`** are assembled in `dataset_pipeline.R` by `list.files()` over those output dirs: `bioclim`, `bioclim_veg`, and (if topography is prepared) `bioclim_topo`, `bioclim_veg_topo`. Each also gets a non-collinear `*_reduced` variant **iff** the matching `data/outputs/env_selection/selected_vars*.csv` exists (`register_reduced_env_set`).
- `r/src/env_selection_pipeline.R` + `select_env_vars.R` — collinearity reduction (`|r| > cutoff`) per env_set over a reference dataset; writes `selected_vars{,_veg,_topo,_veg_topo}.csv` consumed by the reduced env_sets. **Multi-pass:** build datasets for the base env_sets → run selection → re-run `dataset_pipeline.R` to materialize the `*_reduced` datasets.

### Model training

Two-layer split, **algorithm-agnostic**:

- `r/src/train_models.R` — pure functions. `fit_model(algo, X, y, hp)` dispatches over `SUPPORTED_ALGOS = c("maxnet", "ranger", "xgboost")` (defaults in `ALGO_DEFAULTS`). `run_model_for_dataset()` consumes one `sdm_dataset_model_ready.csv` and evaluates under **spatial-block CV** (`run_model_spatial_block`): K-fold spatial cross-validation, then a final refit on all data. Writes per-run artifacts into `data/outputs/sdm_models/<run_id>/<cv_scheme>/<algo>/` (`model.rds`, `predictions.csv`, `metrics.csv`, response/PDP curves, etc.).
  - Required dataset columns: `class`, `decimalLongitude`, `decimalLatitude`. Everything else is treated as a predictor.
- `r/src/evaluate_model.R` — metric computation, algorithm-agnostic: dual threshold-based metrics (threshold at max TSS → `tss`, `fnr`, sens/spec) plus the continuous **Boyce index** (Hirzel et al. 2006). No fixed 70/30 holdout — evaluation is the spatial-block folds.
- `r/src/spatial_cv.R` — builds the spatial blocks. `train_pipeline.R` calibrates the block size **once per run** from the spatial autocorrelation range of the BIO layers (capped at `BLOCK_SIZE_CAP_KM = 300`), persisting `autocor_range_table.csv` and `spatial_cv_config.csv`.
- `r/src/train_pipeline.R` — orchestrator. Reads the dataset manifest and loops over `cv_scheme × algo × run` (`CV_SCHEMES = c("spatial_block")`, `ALGOS = c("maxnet", "ranger", "xgboost")`, `K_FOLDS = 5`). Spatial folds are cached per `run_id` (they depend only on the dataset, not the algorithm). Writes a joined `manifest.csv` (dataset manifest + metrics) and summary/comparison outputs into `data/outputs/sdm_models/`.

Prediction maps and explainability are separate stages: `predict_pipeline.R` (+ `predict_distribution_map.R`) and `xai_pipeline.R` (+ `xai_shap.R` / `xai_pdp.R` / `xai_lime.R`, dispatched via `xai_predict.R`). Both **must be run with `Rscript` directly** (they guard their main block on `sys.nframe()`).

The display-only notebooks (`stage1_maxent.Rmd` metrics/curves, `stage3_distribution_map.Rmd`, `stage4_xai.Rmd`) `read_csv` / `include_graphics` from `data/outputs/...`. **Notebook paths are relative to `r/notebooks/` (e.g. `../../data/...`)** — different from the source scripts, which assume the working directory is the repo root.

### Working-directory contract

This is the single biggest footgun: everything in `r/src/*.R` `source()`s siblings as `r/src/foo.R` and reads data as `data/...`, so it only works when the current directory is the **repo root**. Notebooks under `r/notebooks/` use `../../data/...`. When adding new scripts, follow whichever convention matches the file's location — do not mix.

### Manifest-driven workflow

The `manifest.csv` files are the integration contract between stages. The dataset pipeline writes one; `train_pipeline.R` reads it, trains every `algo × cv_scheme` for each row, and writes a joined `manifest.csv` for the models. Downstream comparison/evaluation/prediction/XAI work should consume these manifests rather than walking directories.

## Data

`data/` is gitignored. Only `data/ocurrences/raw/*.csv` (a few small species CSVs) are tracked. Raw inputs must be obtained separately and placed manually:

- **WorldClim 2.5m bioclim** at `data/features/worldclim/wc2.1_2.5m_bio/` (the reference grid).
- **Vegetation** raw at `data/features/vegetation_raw/` (ESA WorldCover tiles, MODIS NDVI, INTA canopy height).
- **Topography** raw at `data/features/topography_raw/{cti,slope,tpi,tri}/` (Geomorpho90m; e.g. clipped GeoTIFFs exported from Google Earth Engine `projects/sat-io/open-datasets/Geomorpho90m/*`).
- **Argentina shapefile** at `data/shp/argentina/argentina.shp`.

`prepare_env_layers.R` turns these into the harmonized `data/features/env_2.5m_ar/` set that the dataset pipeline consumes.

## Caveats when editing

- **`r/src/borrador/`** is a scratch/draft folder ("borrador" = draft). Don't treat its contents as authoritative when refactoring.
- Two background-generation paths exist in `build_parallel_sdm_datasets.R`: a raster-mask `generate_background_points()` (currently commented out) and `sample_random_background_from_shp()` (currently active). The shapefile-based one is marked "provisorio" — the intent per the in-file comment is to eventually intersect raster validity with the Argentina polygon.
- `bias_method = "grid_thin"` paths in `build_one_sdm_dataset` require `grid_thin_gbif()` to be in scope; `dataset_pipeline.R` sources it transitively, so re-source the pipeline rather than calling builder helpers directly.
