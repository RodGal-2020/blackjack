# =============================================================================
# scripts/compare_strategies.R
# Compare all built-in blackjack strategies using Monte Carlo simulation and
# produce ranked summary tables and ggplot2 visualisations.
#
# Usage:
#   Rscript scripts/compare_strategies.R
#
# Optional environment variables:
#   N_GAMES    – number of games per strategy (default 10000)
#   N_WORKERS  – parallel workers (default 1)
#   SEED       – random seed (default 2024)
# =============================================================================

# ---- 0. Bootstrap -----------------------------------------------------------
.get_proj_root <- function() {
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0L) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[[1L]]),
                                 mustWork = FALSE)
    return(normalizePath(file.path(dirname(script_path), ".."),
                         mustWork = FALSE))
  }
  normalizePath(".", mustWork = FALSE)
}

proj_root <- .get_proj_root()

source(file.path(proj_root, "R", "blackjack_engine.R"))
source(file.path(proj_root, "R", "game_state.R"))
source(file.path(proj_root, "R", "strategy_parser.R"))
source(file.path(proj_root, "R", "simulation.R"))
source(file.path(proj_root, "R", "analysis.R"))
source(file.path(proj_root, "R", "visualization.R"))

# ---- 1. Configuration -------------------------------------------------------
N_GAMES    <- as.integer(Sys.getenv("N_GAMES",   "10000"))
N_WORKERS  <- as.integer(Sys.getenv("N_WORKERS", "1"))
SEED       <- as.integer(Sys.getenv("SEED",      "2024"))

STRATEGIES_DIR <- file.path(proj_root, "strategies")
rules          <- default_rules()

# ---- 2. Define strategies ---------------------------------------------------
cat("Loading strategies...\n")
strategies <- list(
  random       = random_strategy,
  naive        = naive_strategy,
  conservative = conservative_strategy,
  aggressive   = aggressive_strategy,
  basic        = load_strategy(file.path(STRATEGIES_DIR, "basic_strategy.csv"))
)
cat(sprintf("  Loaded %d strategies: %s\n",
            length(strategies), paste(names(strategies), collapse = ", ")))

# ---- 3. Run simulations -----------------------------------------------------
cat(sprintf("\nRunning %d games per strategy (seed = %d)...\n", N_GAMES, SEED))
all_results <- simulate_all_strategies(
  strategies = strategies,
  n_games    = N_GAMES,
  rules      = rules,
  n_workers  = N_WORKERS,
  seed       = SEED
)

# ---- 4. Summarise and rank --------------------------------------------------
cat("\nSummarising results...\n")
summaries <- do.call(rbind, lapply(names(strategies), function(s) {
  summarise_results(all_results[all_results$strategy == s, ],
                    strategy_name = s)
}))

ranked <- rank_strategies(summaries)

cat("\n--- Strategy Rankings ---\n")
print(ranked[, c("rank", "strategy", "expected_return", "win_rate",
                 "bust_rate", "blackjack_rate")])

# ---- 5. Visualise -----------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  cat("\nGenerating plots...\n")

  # Profit distribution (faceted by strategy)
  p_dist <- plot_profit_distribution(all_results)
  ggplot2::ggsave(file.path(proj_root, "compare_profit_dist.png"),
                  plot = p_dist, width = 12, height = 7)

  # Cumulative winnings
  p_cum <- plot_cumulative_winnings(all_results)
  ggplot2::ggsave(file.path(proj_root, "compare_cumulative.png"),
                  plot = p_cum, width = 10, height = 5)

  # Expected return bar chart
  p_bar <- plot_strategy_comparison(ranked)
  ggplot2::ggsave(file.path(proj_root, "compare_expected_return.png"),
                  plot = p_bar, width = 7, height = 5)

  # Basic strategy heatmaps
  basic_strat <- strategies$basic
  p_hard <- plot_strategy_heatmap(basic_strat, soft = FALSE)
  ggplot2::ggsave(file.path(proj_root, "basic_hard_heatmap.png"),
                  plot = p_hard, width = 9, height = 7)

  p_soft <- plot_strategy_heatmap(basic_strat, soft = TRUE)
  ggplot2::ggsave(file.path(proj_root, "basic_soft_heatmap.png"),
                  plot = p_soft, width = 9, height = 7)

  cat("  Plots saved to project root.\n")
} else {
  cat("Install ggplot2 to enable plots: install.packages('ggplot2')\n")
}

# ---- 6. Export results ------------------------------------------------------
out_file <- file.path(proj_root, "simulation_results.csv")
write.csv(all_results, out_file, row.names = FALSE)
cat(sprintf("\nFull results written to: %s\n", out_file))

summary_file <- file.path(proj_root, "strategy_summary.csv")
write.csv(ranked, summary_file, row.names = FALSE)
cat(sprintf("Strategy summary written to: %s\n", summary_file))

cat("\nDone.\n")
