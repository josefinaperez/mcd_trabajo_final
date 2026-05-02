# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project context

POC for a Master's thesis on Species Distribution Modeling (SDM) of fungi in Argentina. All active work lives in **`r/`**: GBIF download → cleaning → background generation → MaxEnt (`maxnet`).

Documentation, methodology (`docs/metodologia/`), and the README are in **Spanish**. Code, identifiers, and column names are in English. Match this convention when editing.

The `python/` directory is **deprecated** (early EDA only) and can be deleted. Do not add new work there; do not refactor it. If asked to clean up, removing it wholesale is fine.

## Commands

Run from the **repo root** — paths in `r/src/*.R` are repo-relative.

```r
# 1) Full dataset build pipeline (download → preprocess → bias correction → BG → env extraction)
source("r/src/dataset_pipeline.R")

# 2) Train MaxEnt across all dataset configs and persist artifacts + summary
source("r/src/train_pipeline.R")

# 3) (Optional) Render the visualization-only notebook from the persisted outputs
rmarkdown::render("r/notebooks/stage1_maxent.Rmd")
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

### MaxEnt training

Two-layer split:

- `r/src/train_maxent.R` — pure functions. `run_maxent_for_dataset()` consumes one `sdm_dataset_model_ready.csv`, does stratified 70/30 split (`seed = 42`), trains `maxnet::maxnet` with `regmult = 1`, predicts with `type = "cloglog"`, computes test AUC, extracts response curves, and writes 5 artifacts per run into `data/outputs/sdm_models/<run_id>/`: `model.rds`, `predictions_test.csv`, `metrics.csv`, `response_curves.csv`, `response_curves.png`.
  - Required dataset columns: `class`, `decimalLongitude`, `decimalLatitude`. Everything else is treated as a predictor.
- `r/src/train_pipeline.R` — orchestrator. Reads the dataset manifest, loops `run_maxent_for_dataset` over every row, then writes `manifest.csv` (join of dataset manifest + metrics), `summary_table.csv` (display-ready), and `auc_comparison.png` into `data/outputs/sdm_models/`.

`r/notebooks/stage1_maxent.Rmd` is **display-only**: `read_csv(summary_table.csv)` + `include_graphics(auc_comparison.png)` + per-run response-curve PNGs. **Notebook paths are relative to `r/notebooks/` (e.g. `../../data/...`)** — different from the source scripts, which assume the working directory is the repo root.

### Working-directory contract

This is the single biggest footgun: everything in `r/src/*.R` `source()`s siblings as `r/src/foo.R` and reads data as `data/...`, so it only works when the current directory is the **repo root**. Notebooks under `r/notebooks/` use `../../data/...`. When adding new scripts, follow whichever convention matches the file's location — do not mix.

### Manifest-driven workflow

The `manifest.csv` files are the integration contract between stages. The dataset pipeline writes one; the training notebook reads it, trains one model per row, and writes a joined `manifest.csv` for the models. Downstream comparison/evaluation work should consume these manifests rather than walking directories.

## Data

`data/` is gitignored. Only `data/ocurrences/raw/*.csv` (a few small species CSVs) are tracked. WorldClim rasters (expected at `data/features/worldclim/wc2.1_30s_bio/`) and the Argentina shapefile (`data/shp/argentina/argentina.shp`) must be obtained separately and placed manually.

## Caveats when editing

- **`r/src/borrador/`** is a scratch/draft folder ("borrador" = draft). Don't treat its contents as authoritative when refactoring.
- Two background-generation paths exist in `build_parallel_sdm_datasets.R`: a raster-mask `generate_background_points()` (currently commented out) and `sample_random_background_from_shp()` (currently active). The shapefile-based one is marked "provisorio" — the intent per the in-file comment is to eventually intersect raster validity with the Argentina polygon.
- `bias_method = "grid_thin"` paths in `build_one_sdm_dataset` require `grid_thin_gbif()` to be in scope; `dataset_pipeline.R` sources it transitively, so re-source the pipeline rather than calling builder helpers directly.
