library(readr)
library(CoordinateCleaner)
library(dplyr)
source("r/src/utils.R")


# ------------------------------------------------------------
# Función pura: limpieza secuencial de ocurrencias.
#
# Aplica los filtros uno a uno sobre `df` y devuelve, además del
# conjunto limpio, la trazabilidad necesaria para los resultados
# del informe (issue #68):
#   - `clean`  : data.frame de presencias que sobreviven todos los filtros.
#   - `removed`: una fila por registro descartado, con el `stage` (etapa
#                del embudo) y `reason` (motivo granular) que lo descartó.
#                Atribución SECUENCIAL: el motivo es el primer filtro que
#                lo eliminó, de modo que los conteos cuadran con el embudo.
#   - `funnel` : una fila por etapa con n_in / n_removed / n_out.
#
# No hace I/O: recibe el polígono de área (`shp_area`, objeto sf) ya
# cargado. El wrapper `preprocess_dataset` se encarga de leer/escribir.
# ------------------------------------------------------------

BASIS_OF_RECORD_WHITELIST <- c("HUMAN_OBSERVATION",
                               "PRESERVED_SPECIMEN",
                               "MATERIAL_SAMPLE",
                               "LIVING_SPECIMEN")

clean_occurrences <- function(df,
                              min_year,
                              scientific_name,
                              max_uncertainty_km,
                              shp_area,
                              tests)
{
  df <- as.data.frame(df)
  df$.row_id <- seq_len(nrow(df))

  removed_acc <- list()
  funnel_acc  <- list()

  # Aplica una etapa: registra removidos (df_in[!keep]) con stage/reason,
  # acumula la fila del embudo y devuelve los que continúan (df_in[keep]).
  apply_stage <- function(df_in, keep, stage, reason = stage) {
    keep[is.na(keep)] <- FALSE
    rem <- df_in[!keep, , drop = FALSE]
    if (nrow(rem) > 0) {
      rem$stage  <- stage
      rem$reason <- reason
      removed_acc[[length(removed_acc) + 1L]] <<- rem
    }
    funnel_acc[[length(funnel_acc) + 1L]] <<- data.frame(
      stage     = stage,
      n_in      = nrow(df_in),
      n_removed = sum(!keep),
      n_out     = sum(keep),
      stringsAsFactors = FALSE
    )
    df_in[keep, , drop = FALSE]
  }

  # 1) Coordenadas faltantes
  df <- apply_stage(df, !is.na(df$decimalLongitude) & !is.na(df$decimalLatitude),
                    "no_coords")

  # 2) Fecha. `year >= min_year` descarta también year = NA (sin año no se
  #    puede verificar coherencia temporal con los predictores; issue #22).
  df <- apply_stage(df, !is.na(df$year) & df$year >= min_year, "year")

  # 3) scientificName objetivo
  df <- apply_stage(df, df$scientificName %in% scientific_name, "species")

  # 4) Recorte al área de estudio (Argentina). filter_points devuelve los
  #    registros dentro del polígono; identificamos los removidos por .row_id.
  kept_ids <- filter_points(shp_area, df,
                            lon_col = "decimalLongitude",
                            lat_col = "decimalLatitude")$.row_id
  df <- apply_stage(df, df$.row_id %in% kept_ids, "outside_argentina")

  # 5) Precisión de coordenadas (NA se conserva: incertidumbre desconocida)
  df <- apply_stage(
    df,
    is.na(df$coordinateUncertaintyInMeters) |
      df$coordinateUncertaintyInMeters / 1000 <= max_uncertainty_km,
    "coord_uncertainty"
  )

  # 6) Whitelist de basisOfRecord
  df <- apply_stage(df, df$basisOfRecord %in% BASIS_OF_RECORD_WHITELIST,
                    "basis_of_record")

  # 7) CoordinateCleaner (una sola etapa del embudo; el motivo por registro
  #    es el primer test que lo marca). Se llama cada cc_* por separado para
  #    evitar el bug de cbind del wrapper clean_coordinates() con datasets
  #    chicos / 1 sola especie.
  if (length(tests) > 0 && nrow(df) > 0) {
    cc_input <- as.data.frame(df)
    test_fns <- list(
      equal        = function(d) cc_equ(d, lon = "decimalLongitude", lat = "decimalLatitude", value = "flagged"),
      zeros        = function(d) cc_zero(d, lon = "decimalLongitude", lat = "decimalLatitude", value = "flagged"),
      duplicates   = function(d) cc_dupl(d, lon = "decimalLongitude", lat = "decimalLatitude", species = "species", value = "flagged"),
      outliers     = function(d) cc_outl(d, lon = "decimalLongitude", lat = "decimalLatitude", species = "species", value = "flagged"),
      institutions = function(d) cc_inst(d, lon = "decimalLongitude", lat = "decimalLatitude", species = "species", value = "flagged"),
      centroids    = function(d) cc_cen(d,  lon = "decimalLongitude", lat = "decimalLatitude", species = "species", value = "flagged"),
      capitals     = function(d) cc_cap(d,  lon = "decimalLongitude", lat = "decimalLatitude", species = "species", value = "flagged")
    )

    flag_mat <- vapply(
      tests,
      function(t) {
        fn <- test_fns[[t]]
        if (is.null(fn)) {
          warning("Unknown CoordinateCleaner test: ", t, " (skipped)")
          return(rep(TRUE, nrow(cc_input)))
        }
        tryCatch(
          as.logical(fn(cc_input)),
          error = function(e) {
            warning("cc_", t, " failed (", conditionMessage(e), "); skipping")
            rep(TRUE, nrow(cc_input))
          }
        )
      },
      logical(nrow(cc_input))
    )
    if (is.null(dim(flag_mat))) flag_mat <- matrix(flag_mat, ncol = length(tests))
    colnames(flag_mat) <- tests

    ok_rows <- apply(flag_mat, 1, all)
    # motivo = primer test que marca el registro (atribución secuencial)
    first_fail <- apply(flag_mat, 1, function(x) {
      if (all(x)) NA_character_ else paste0("cc_", tests[which(!x)[1]])
    })

    rem <- df[!ok_rows, , drop = FALSE]
    if (nrow(rem) > 0) {
      rem$stage  <- "coordinate_cleaner"
      rem$reason <- first_fail[!ok_rows]
      removed_acc[[length(removed_acc) + 1L]] <- rem
    }
    funnel_acc[[length(funnel_acc) + 1L]] <- data.frame(
      stage     = "coordinate_cleaner",
      n_in      = nrow(df),
      n_removed = sum(!ok_rows),
      n_out     = sum(ok_rows),
      stringsAsFactors = FALSE
    )
    df <- df[ok_rows, , drop = FALSE]
  }

  removed <- if (length(removed_acc) > 0) dplyr::bind_rows(removed_acc) else df[0, ]
  funnel  <- dplyr::bind_rows(funnel_acc)

  df$.row_id      <- NULL
  removed$.row_id <- NULL

  list(clean = df, removed = removed, funnel = funnel)
}


# ------------------------------------------------------------
# Descriptivos del conjunto limpio (issue #68). Persiste agregados
# por año, basisOfRecord, fuente (datasetName) e incertidumbre de
# coordenadas. La agregación vive acá (script), no en el notebook.
# ------------------------------------------------------------

write_occ_descriptive <- function(df_clean, eda_dir) {
  dir.create(eda_dir, recursive = TRUE, showWarnings = FALSE)

  by_year <- df_clean %>%
    count(year, name = "n") %>%
    arrange(year)
  readr::write_csv(by_year, file.path(eda_dir, "desc_by_year.csv"))

  by_basis <- df_clean %>%
    count(basisOfRecord, name = "n") %>%
    arrange(desc(n))
  readr::write_csv(by_basis, file.path(eda_dir, "desc_by_basis.csv"))

  source_col <- if ("datasetName" %in% names(df_clean)) "datasetName" else "datasetKey"
  by_source <- df_clean %>%
    count(.data[[source_col]], name = "n") %>%
    rename(source = 1) %>%
    arrange(desc(n))
  readr::write_csv(by_source, file.path(eda_dir, "desc_by_source.csv"))

  uncertainty <- data.frame(
    coordinateUncertaintyInMeters = df_clean$coordinateUncertaintyInMeters
  )
  readr::write_csv(uncertainty, file.path(eda_dir, "desc_uncertainty.csv"))

  invisible(eda_dir)
}


# ------------------------------------------------------------
# Wrapper de I/O: lee el CSV crudo y el shapefile, limpia, y persiste
# el CSV procesado. Si `eda_dir` no es NULL, además escribe los
# artefactos para Resultados (embudo, removidos, descriptivos).
# ------------------------------------------------------------

preprocess_dataset <- function(df_path,
                               preproc_path,
                               min_year,
                               scientific_name,
                               max_uncertainty_km,
                               shp_path,
                               tests,
                               eda_dir = NULL)
{
  df_raw   <- read_csv(df_path)
  shp_area <- create_shp_from(shp_path)

  res <- clean_occurrences(
    df                 = df_raw,
    min_year           = min_year,
    scientific_name    = scientific_name,
    max_uncertainty_km = max_uncertainty_km,
    shp_area           = shp_area,
    tests              = tests
  )

  dir.create(dirname(preproc_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(res$clean, preproc_path)

  if (!is.null(eda_dir)) {
    dir.create(eda_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(res$funnel, file.path(eda_dir, "cleaning_funnel.csv"))
    readr::write_csv(res$removed, file.path(eda_dir, "removed_points.csv"))
    write_occ_descriptive(res$clean, eda_dir)
  }

  return(list(df_clean   = res$clean,
              df_flagged = res$removed,
              funnel     = res$funnel))
}
