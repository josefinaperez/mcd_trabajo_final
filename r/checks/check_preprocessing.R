# ============================================================
# File: r/checks/check_preprocessing.R
# Purpose: Chequeo de la función pura clean_occurrences()
#          (preprocessing.R) sobre datos sintéticos. Verifica el
#          cuadre del embudo y la atribución secuencial de motivos.
#          No descarga datos ni lee shapefiles de disco.
# Ejecutar desde repo root:
#   Rscript r/checks/check_preprocessing.R
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})
source("r/src/preprocessing.R")

ok <- function(msg) cat("OK:", msg, "\n")

# Polígono de "área de estudio": cuadrado [-65,-60] x [-40,-35].
shp_area <- st_sf(
  geometry = st_sfc(
    st_polygon(list(rbind(
      c(-65, -40), c(-60, -40), c(-60, -35), c(-65, -35), c(-65, -40)
    ))),
    crs = 4326
  )
)

# Dataset sintético: 1 fila por motivo de descarte + filas que sobreviven.
# Coordenadas dentro del área salvo donde se indica.
df <- data.frame(
  scientificName = "Polyporaceae",
  species        = "Polyporaceae sp",
  year           = 2015,
  basisOfRecord  = "PRESERVED_SPECIMEN",
  coordinateUncertaintyInMeters = 1000,
  decimalLongitude = -62,
  decimalLatitude  = -37,
  datasetName    = "ds_a",
  stringsAsFactors = FALSE
)
df <- df[rep(1, 8), ]
rownames(df) <- NULL

# fila 1: sin coordenadas
df$decimalLongitude[1] <- NA
# fila 2: año viejo
df$year[2] <- 1990
# fila 3: otra especie
df$scientificName[3] <- "Otra especie"
# fila 4: fuera de Argentina (Brasil)
df$decimalLongitude[4] <- -45; df$decimalLatitude[4] <- -10
# fila 5: incertidumbre alta (10 km > 5 km)
df$coordinateUncertaintyInMeters[5] <- 10000
# fila 6: basisOfRecord no permitido
df$basisOfRecord[6] <- "FOSSIL_SPECIMEN"
# filas 7,8: válidas (sobreviven; sin tests CC)

res <- clean_occurrences(
  df = df, min_year = 2010, scientific_name = "Polyporaceae",
  max_uncertainty_km = 5, shp_area = shp_area, tests = character(0)
)

# ---- el embudo cuadra: n_out de cada etapa == n_in de la siguiente ----
fn <- res$funnel
for (i in seq_len(nrow(fn) - 1)) {
  stopifnot(fn$n_out[i] == fn$n_in[i + 1])
}
# n_in primera etapa == filas crudas; n_out final == filas limpias
stopifnot(fn$n_in[1] == nrow(df))
stopifnot(fn$n_out[nrow(fn)] == nrow(res$clean))
ok("embudo cuadra (n_out[i] == n_in[i+1], extremos correctos)")

# ---- 6 descartes, 2 sobreviven ----
stopifnot(nrow(res$clean) == 2)
stopifnot(nrow(res$removed) == 6)
ok("conteos limpio/removido correctos")

# ---- atribución secuencial: cada motivo aparece exactamente una vez ----
expected <- c("no_coords", "year", "species", "outside_argentina",
              "coord_uncertainty", "basis_of_record")
stopifnot(setequal(res$removed$stage, expected))
stopifnot(all(table(res$removed$stage) == 1))
ok("atribución secuencial por etapa (1 registro por motivo)")

# ---- total removido == suma de n_removed del embudo ----
stopifnot(nrow(res$removed) == sum(fn$n_removed))
ok("removidos totales == suma del embudo")

# ---- tests CC: una etapa coordinate_cleaner aparece cuando hay tests ----
res_cc <- clean_occurrences(
  df = df, min_year = 2010, scientific_name = "Polyporaceae",
  max_uncertainty_km = 5, shp_area = shp_area,
  tests = c("zeros", "equal")
)
stopifnot("coordinate_cleaner" %in% res_cc$funnel$stage)
ok("etapa coordinate_cleaner presente con tests")

# ---- match_field: matchear por `species` ignora la autoría de scientificName ----
df_sp <- data.frame(
  scientificName = c("Trametes versicolor (L.) Lloyd",
                     "Trametes versicolor (L.) Lloyd",
                     "Trametes villosa (Sw.) Kreisel"),
  species        = c("Trametes versicolor", "Trametes versicolor",
                     "Trametes villosa"),
  year = 2015, basisOfRecord = "PRESERVED_SPECIMEN",
  coordinateUncertaintyInMeters = 1000,
  decimalLongitude = -62, decimalLatitude = -37,
  stringsAsFactors = FALSE
)
# match por scientificName exacto: 0 sobreviven (la autoría no matchea)
r_sci <- clean_occurrences(df_sp, 2010, "Trametes versicolor", 5, shp_area,
                           character(0), match_field = "scientificName")
stopifnot(nrow(r_sci$clean) == 0)
# match por species: sobreviven los 2 de versicolor, se descarta villosa
r_spc <- clean_occurrences(df_sp, 2010, "Trametes versicolor", 5, shp_area,
                           character(0), match_field = "species")
stopifnot(nrow(r_spc$clean) == 2)
stopifnot(all(r_spc$clean$species == "Trametes versicolor"))
ok("match_field='species' matchea binomio ignorando autoría")

cat("\nTodos los checks de preprocessing OK\n")
