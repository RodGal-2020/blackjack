# =============================================================================
# scripts/run_simulation.R
# Example script: run a Monte Carlo simulation for a single strategy and
# visualise the results.
#
# Usage:
#   Rscript scripts/run_simulation.R
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
  # Fallback: assume working directory is the project root
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
N_GAMES      <- 10000L   # number of games to simulate
N_WORKERS    <- 1L       # set > 1 to use parallel processing
SEED         <- 2024L    # fixed seed for reproducibility
STRATEGIES_DIR <- file.path(proj_root, "strategies")

rules <- default_rules()   # single deck, dealer stands soft 17

# ---- 2. Load strategy -------------------------------------------------------
cat("Loading basic strategy...\n")
strategy <- load_strategy(file.path(STRATEGIES_DIR, "basic_strategy.csv"))

# ---- 3. Run simulation ------------------------------------------------------
cat(sprintf("Simulating %d games with basic strategy (seed = %d)...\n",
            N_GAMES, SEED))
results <- simulate_games(
  strategy  = strategy,
  n_games   = N_GAMES,
  rules     = rules,
  n_workers = N_WORKERS,
  seed      = SEED
)

# ---- 4. Print summary -------------------------------------------------------
summary_df <- summarise_results(results, strategy_name = "basic")
cat("\n--- Simulation Summary ---\n")
print(summary_df)

cat(sprintf(
  "\nWin rate:       %.1f%%\n",
  summary_df$win_rate * 100
))
cat(sprintf("Loss rate:      %.1f%%\n", summary_df$loss_rate * 100))
cat(sprintf("Push rate:      %.1f%%\n", summary_df$push_rate * 100))
cat(sprintf("Blackjack rate: %.1f%%\n", summary_df$blackjack_rate * 100))
cat(sprintf("Bust rate:      %.1f%%\n", summary_df$bust_rate * 100))
cat(sprintf("Expected return per game: %.4f units\n", summary_df$expected_return))

# ---- 5. Visualise (requires ggplot2) ----------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  cat("\nGenerating plots...\n")

  results <- add_cumulative_profit(results)

  p1 <- plot_profit_distribution(results)
  p2 <- plot_cumulative_winnings(results)
  p3 <- plot_strategy_heatmap(strategy, soft = FALSE)
  p4 <- plot_strategy_heatmap(strategy, soft = TRUE)

  # Save to PNG if grDevices is available
  for (i in seq_along(list(p1, p2, p3, p4))) {
    fname <- file.path(proj_root, sprintf("plot_%d.png", i))
    ggplot2::ggsave(fname, plot = list(p1, p2, p3, p4)[[i]],
                    width = 8, height = 5)
    cat(sprintf("  Saved %s\n", fname))
  }
} else {
  cat("Install ggplot2 to enable plots: install.packages('ggplot2')\n")
}

cat("\nDone.\n")
