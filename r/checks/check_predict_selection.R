# ============================================================
# File: r/checks/check_predict_selection.R
# Purpose: Chequea la selección de ganadores de Etapa 3 (#59 + #56) sobre
#          un summary sintético: un ganador por (cv_scheme x bp_group)
#          —uniform vs target_group— vía argmax BOYCE (no TSS), con el
#          filtro FNR degradado a DIAGNÓSTICO (no decide el ganador),
#          exclusión de los env_sets antrópicos (#47), winner_role
#          etiquetado por bp_group y robustez τ_FNR por bp_group.
#
# Replica el caso real (Coprinus): el ganador del grupo corregido tiene
# Boyce alto pero FNR alto (no pasa el piso) y AÚN ASÍ debe ganar — el
# bug previo (argmax TSS + piso FNR) coronaba un sobreajuste de Boyce bajo.
#
# Ejecutar desde repo root:
#   Rscript r/checks/check_predict_selection.R
# ============================================================

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("r/src/predict_pipeline.R")

ok <- function(msg) cat("OK:", msg, "\n")

# Summary sintético: un cv_scheme, ambos grupos.
#   uniform:
#     A random      tss .80 fnr .10 boyce .99 -> GANA (mayor Boyce no-anthro)
#     B three_step  tss .90 fnr .15 boyce .85    mayor TSS pero pierde por Boyce
#     C anthro      tss .95 fnr .05 boyce .999   mayor Boyce pero excluido (regex)
#   target_group:
#     F bias_weighted tss .41 fnr .40 boyce .98 -> GANA pese a NO pasar el FNR
#     G bias_weighted tss .27 fnr .12 boyce .27    pasa FNR pero Boyce bajo, pierde
summary_df <- tribble(
  ~run_id,            ~cv_scheme,      ~bp_method,        ~bp_group,      ~algorithm, ~tss, ~fnr, ~boyce,
  "A_random",         "spatial_block", "random",          "uniform",      "maxnet",   0.80, 0.10, 0.99,
  "B_three_step",     "spatial_block", "three_step",      "uniform",      "ranger",   0.90, 0.15, 0.85,
  "C_anthro_random",  "spatial_block", "random",          "uniform",      "xgboost",  0.95, 0.05, 0.999,
  "F_bias_weighted",  "spatial_block", "bias_weighted",   "target_group", "ranger",   0.41, 0.40, 0.98,
  "G_bias_weighted",  "spatial_block", "bias_weighted",   "target_group", "xgboost",  0.27, 0.12, 0.27
)

sel <- select_runs(summary_df, tau_fnr = 0.20, winner_exclude_regex = "_anthro")

# --- Un ganador por (cv_scheme x bp_group): dos grupos -> 2 ganadores ---
winners <- sel |> filter(is_winner)
stopifnot(nrow(winners) == 2L)
stopifnot(setequal(winners$run_id, c("A_random", "F_bias_weighted")))
ok("dos bp_group -> dos ganadores, uno por grupo")

# --- criterio = argmax BOYCE, no TSS ---
# uniform: B tiene mayor TSS (.90) pero gana A por Boyce (.99 > .85).
stopifnot(sel$is_winner[sel$run_id == "A_random"]     == TRUE)
stopifnot(sel$is_winner[sel$run_id == "B_three_step"] == FALSE)
ok("ganador por argmax Boyce (no por TSS)")

# --- el FNR NO decide al ganador (clave del fix) ---
# F target_group tiene FNR .40 (no pasa el piso .20) y AÚN ASÍ gana por Boyce;
# G pasa el piso pero pierde por Boyce bajo.
f_row <- sel |> filter(run_id == "F_bias_weighted")
g_row <- sel |> filter(run_id == "G_bias_weighted")
stopifnot(f_row$passes_filter == FALSE, f_row$is_winner == TRUE)
stopifnot(g_row$passes_filter == TRUE,  g_row$is_winner == FALSE)
ok("FNR es diagnóstico: el ganador puede no pasar el piso de sensibilidad")

# --- exclusión anthro (#47): mayor Boyce pero nunca gana ---
c_row <- sel |> filter(run_id == "C_anthro_random")
stopifnot(c_row$passes_filter == TRUE, c_row$is_winner == FALSE)
ok("env_set anthro excluido de la candidatura aunque tenga el mayor Boyce")

# --- winner_role etiquetado por bp_group ---
stopifnot(winners$winner_role[winners$run_id == "A_random"]        == "winner_spatial_block_uniform")
stopifnot(winners$winner_role[winners$run_id == "F_bias_weighted"] == "winner_spatial_block_target_group")
ok("winner_role incluye bp_group")

# --- caso solo uniform -> 1 ganador ---
sel_u <- select_runs(summary_df |> filter(bp_group == "uniform"), 0.20, "_anthro")
stopifnot(sum(sel_u$is_winner) == 1L)
stopifnot(sel_u$run_id[sel_u$is_winner] == "A_random")
ok("solo uniform -> 1 ganador")

# --- caso solo target_group -> 1 ganador ---
sel_t <- select_runs(summary_df |> filter(bp_group == "target_group"), 0.20, "_anthro")
stopifnot(sum(sel_t$is_winner) == 1L)
stopifnot(sel_t$run_id[sel_t$is_winner] == "F_bias_weighted")
ok("solo target_group -> 1 ganador")

# --- robustez τ_FNR reportada por bp_group (diagnóstico de supervivencia) ---
rob <- compute_tau_fnr_robustness(summary_df, c(0.20, 0.30), "_anthro")
stopifnot(all(c("bp_group", "n_survivors") %in% names(rob)))
rob_20 <- rob |> filter(tau_fnr == 0.20)
stopifnot(nrow(rob_20) == 2L)
stopifnot(setequal(rob_20$bp_group, c("uniform", "target_group")))
ok("compute_tau_fnr_robustness reporta por bp_group")

cat("\nTodos los chequeos de selección de ganadores pasaron.\n")
