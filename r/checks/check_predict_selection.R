# ============================================================
# File: r/checks/check_predict_selection.R
# Purpose: Chequea la selección de ganadores de Etapa 3 (#57) sobre
#          un summary sintético: un ganador por (cv_scheme x bp_method)
#          vía argmax TSS tras el filtro FNR, exclusión de los env_sets
#          antrópicos (#47), winner_role etiquetado por bp_method, y la
#          robustez τ_FNR reportada por bp_method.
#
# Ejecutar desde repo root:
#   Rscript r/checks/check_predict_selection.R
# ============================================================

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("r/src/predict_pipeline.R")

ok <- function(msg) cat("OK:", msg, "\n")

# Summary sintético: un solo cv_scheme, dos bp_method.
#   random:    A pasa FNR (tss .80) | B mejor tss pero FNR alto (filtrado)
#              | C anthro mejor tss pero excluido por regex
#   three_step: D pasa (tss .85, gana) | E pasa (tss .70, pierde)
summary_df <- tribble(
  ~run_id,            ~cv_scheme,      ~bp_method,    ~algorithm, ~tss,  ~fnr,
  "A_random",         "spatial_block", "random",      "ranger",   0.80,  0.10,
  "B_random",         "spatial_block", "random",      "maxnet",   0.90,  0.50,
  "C_anthro_random",  "spatial_block", "random",      "xgboost",  0.95,  0.05,
  "D_three_step",     "spatial_block", "three_step",  "ranger",   0.85,  0.15,
  "E_three_step",     "spatial_block", "three_step",  "maxnet",   0.70,  0.10
)

sel <- select_runs(summary_df, tau_fnr = 0.20, winner_exclude_regex = "_anthro")

# --- Un ganador por (cv_scheme x bp_method) ---
winners <- sel |> filter(is_winner)
stopifnot(nrow(winners) == 2L)
stopifnot(setequal(winners$run_id, c("A_random", "D_three_step")))
ok("dos bp_method -> dos ganadores, uno por grupo")

# --- argmax TSS dentro del grupo, respetando filtro FNR ---
# random: B tiene mayor TSS pero fnr=0.50 > 0.20 -> queda A.
stopifnot(sel$is_winner[sel$run_id == "B_random"] == FALSE)
stopifnot(sel$is_winner[sel$run_id == "A_random"] == TRUE)
# three_step: D (0.85) > E (0.70).
stopifnot(sel$is_winner[sel$run_id == "E_three_step"] == FALSE)
ok("argmax TSS por grupo respeta el filtro FNR")

# --- exclusión anthro (#47): pasa el filtro pero nunca gana ---
c_row <- sel |> filter(run_id == "C_anthro_random")
stopifnot(c_row$passes_filter == TRUE)
stopifnot(c_row$is_winner == FALSE)
ok("env_set anthro excluido de la candidatura pero survivor")

# --- winner_role etiquetado por bp_method ---
stopifnot(winners$winner_role[winners$run_id == "A_random"] ==
            "winner_spatial_block_random")
stopifnot(winners$winner_role[winners$run_id == "D_three_step"] ==
            "winner_spatial_block_three_step")
ok("winner_role incluye bp_method")

# --- grupo sin survivors -> sin ganador para ese bp_method ---
# Bajando τ a 0.12, three_step se queda sin nadie (D fnr .15, E fnr .10 -> solo E).
sel_low <- select_runs(summary_df, tau_fnr = 0.05, winner_exclude_regex = "_anthro")
stopifnot(sum(sel_low$is_winner) == 0L)  # nadie pasa fnr <= 0.05
ok("grupo sin survivors no produce ganador")

# --- robustez τ_FNR reportada por bp_method ---
rob <- compute_tau_fnr_robustness(summary_df, c(0.20, 0.30), "_anthro")
stopifnot(all(c("bp_method", "n_survivors") %in% names(rob)))
# τ=0.20: ganadores random=A, three_step=D -> 2 filas.
rob_20 <- rob |> filter(tau_fnr == 0.20)
stopifnot(nrow(rob_20) == 2L)
stopifnot(setequal(rob_20$bp_method, c("random", "three_step")))
ok("compute_tau_fnr_robustness reporta por bp_method")

cat("\nTodos los chequeos de selección de ganadores pasaron.\n")
