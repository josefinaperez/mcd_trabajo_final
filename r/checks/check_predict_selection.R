# ============================================================
# File: r/checks/check_predict_selection.R
# Purpose: Chequea la selección de ganadores de Etapa 3 (#59) sobre
#          un summary sintético: un ganador por (cv_scheme x bp_group)
#          —uniform vs target_group— vía argmax TSS tras el filtro FNR,
#          exclusión de los env_sets antrópicos (#47), winner_role
#          etiquetado por bp_group, robustez τ_FNR por bp_group, y los
#          casos de un solo grupo.
#
# Ejecutar desde repo root:
#   Rscript r/checks/check_predict_selection.R
# ============================================================

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("r/src/predict_pipeline.R")

ok <- function(msg) cat("OK:", msg, "\n")

# Summary sintético: un cv_scheme, ambos grupos.
#   uniform (varios bp_method compiten por el grupo entero):
#     A random       tss .80 fnr .10 pasa
#     B random       tss .90 fnr .50 FNR alto -> filtrado
#     C anthro       tss .95 fnr .05 excluido por regex
#     D three_step   tss .85 fnr .15 pasa  -> GANA uniform (mayor TSS no-anthro que pasa)
#     E env_dissim   tss .82 fnr .10 pasa, pierde
#   target_group:
#     F bias_weighted tss .55 fnr .12 pasa -> GANA target_group
summary_df <- tribble(
  ~run_id,           ~cv_scheme,      ~bp_method,                   ~bp_group,      ~algorithm, ~tss, ~fnr,
  "A_random",        "spatial_block", "random",                     "uniform",      "ranger",   0.80, 0.10,
  "B_random",        "spatial_block", "random",                     "uniform",      "maxnet",   0.90, 0.50,
  "C_anthro_random", "spatial_block", "random",                     "uniform",      "xgboost",  0.95, 0.05,
  "D_three_step",    "spatial_block", "three_step",                 "uniform",      "ranger",   0.85, 0.15,
  "E_env_dissim",    "spatial_block", "environmentally_dissimilar", "uniform",      "maxnet",   0.82, 0.10,
  "F_bias_weighted", "spatial_block", "bias_weighted",              "target_group", "maxnet",   0.55, 0.12
)

sel <- select_runs(summary_df, tau_fnr = 0.20, winner_exclude_regex = "_anthro")

# --- Un ganador por (cv_scheme x bp_group): dos grupos -> 2 ganadores ---
winners <- sel |> filter(is_winner)
stopifnot(nrow(winners) == 2L)
stopifnot(setequal(winners$run_id, c("D_three_step", "F_bias_weighted")))
ok("dos bp_group -> dos ganadores, uno por grupo")

# --- argmax TSS sobre el GRUPO ENTERO, respetando filtro FNR ---
# uniform: B .90 filtrado por FNR; C .95 anthro excluido; gana D .85 (> A .80, E .82).
stopifnot(sel$is_winner[sel$run_id == "B_random"]   == FALSE)
stopifnot(sel$is_winner[sel$run_id == "A_random"]   == FALSE)
stopifnot(sel$is_winner[sel$run_id == "E_env_dissim"] == FALSE)
stopifnot(sel$is_winner[sel$run_id == "D_three_step"] == TRUE)
ok("argmax TSS por grupo (cruza bp_method dentro de uniform) respeta FNR")

# --- exclusión anthro (#47): pasa el filtro pero nunca gana ---
c_row <- sel |> filter(run_id == "C_anthro_random")
stopifnot(c_row$passes_filter == TRUE, c_row$is_winner == FALSE)
ok("env_set anthro excluido de la candidatura pero survivor")

# --- winner_role etiquetado por bp_group ---
stopifnot(winners$winner_role[winners$run_id == "D_three_step"]   == "winner_spatial_block_uniform")
stopifnot(winners$winner_role[winners$run_id == "F_bias_weighted"] == "winner_spatial_block_target_group")
ok("winner_role incluye bp_group")

# --- caso solo uniform -> 1 ganador ---
sel_u <- select_runs(summary_df |> filter(bp_group == "uniform"), 0.20, "_anthro")
stopifnot(sum(sel_u$is_winner) == 1L)
stopifnot(sel_u$run_id[sel_u$is_winner] == "D_three_step")
ok("solo uniform -> 1 ganador")

# --- caso solo target_group -> 1 ganador ---
sel_t <- select_runs(summary_df |> filter(bp_group == "target_group"), 0.20, "_anthro")
stopifnot(sum(sel_t$is_winner) == 1L)
stopifnot(sel_t$run_id[sel_t$is_winner] == "F_bias_weighted")
ok("solo target_group -> 1 ganador")

# --- robustez τ_FNR reportada por bp_group ---
rob <- compute_tau_fnr_robustness(summary_df, c(0.20, 0.30), "_anthro")
stopifnot(all(c("bp_group", "n_survivors") %in% names(rob)))
rob_20 <- rob |> filter(tau_fnr == 0.20)
stopifnot(nrow(rob_20) == 2L)
stopifnot(setequal(rob_20$bp_group, c("uniform", "target_group")))
ok("compute_tau_fnr_robustness reporta por bp_group")

cat("\nTodos los chequeos de selección de ganadores pasaron.\n")
