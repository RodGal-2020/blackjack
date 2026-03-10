# =============================================================================
# tests/test_strategies.R
# Unit tests for strategy loading, validation, and decision-making.
# Run with: source("tests/test_strategies.R")
# =============================================================================

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

STRATEGIES_DIR <- file.path(proj_root, "strategies")

# ---- Minimal testing framework ----------------------------------------------
.tests_run    <- 0L
.tests_passed <- 0L
.tests_failed <- 0L

expect_equal <- function(actual, expected, label = "") {
  .tests_run <<- .tests_run + 1L
  if (isTRUE(all.equal(actual, expected))) {
    .tests_passed <<- .tests_passed + 1L
    cat(sprintf("  PASS: %s\n", label))
  } else {
    .tests_failed <<- .tests_failed + 1L
    cat(sprintf("  FAIL: %s\n    expected: %s\n    got:      %s\n",
                label, deparse(expected), deparse(actual)))
  }
}

expect_true  <- function(actual, label = "") expect_equal(isTRUE(actual), TRUE,  label)
expect_false <- function(actual, label = "") expect_equal(isTRUE(actual), FALSE, label)

expect_error_thrown <- function(expr, label = "") {
  .tests_run <<- .tests_run + 1L
  result <- tryCatch(expr, error = function(e) e)
  if (inherits(result, "error")) {
    .tests_passed <<- .tests_passed + 1L
    cat(sprintf("  PASS (error): %s\n", label))
  } else {
    .tests_failed <<- .tests_failed + 1L
    cat(sprintf("  FAIL (no error): %s\n", label))
  }
}

# ---- Tests: validate_strategy -----------------------------------------------
cat("\n=== validate_strategy ===\n")

good_df <- data.frame(
  player_total = c(16L, 12L, 18L),
  dealer_card  = c(10L,  4L,  9L),
  soft         = c(FALSE, FALSE, TRUE),
  action       = c("hit", "stand", "hit"),
  stringsAsFactors = FALSE
)
validated <- validate_strategy(good_df)
expect_true(is.data.frame(validated),        "returns data.frame")
expect_true(is.logical(validated$soft),      "soft column is logical")
expect_true(is.integer(validated$player_total), "player_total is integer")

bad_df <- data.frame(total = 1:3, action = c("hit", "stand", "hit"),
                     stringsAsFactors = FALSE)
expect_error_thrown(validate_strategy(bad_df),
                    "error on missing required columns")

bad_action_df <- good_df
bad_action_df$action <- c("hit", "stand", "surrender")
expect_error_thrown(validate_strategy(bad_action_df),
                    "error on unknown action")

# ---- Tests: load_strategy (CSV) ---------------------------------------------
cat("\n=== load_strategy (CSV) ===\n")
basic <- load_strategy(file.path(STRATEGIES_DIR, "basic_strategy.csv"))
expect_true(is.data.frame(basic),            "basic strategy loaded as data.frame")
expect_true(nrow(basic) > 0,                 "basic strategy has rows")
expected_cols <- c("player_total", "dealer_card", "soft", "action")
expect_true(all(expected_cols %in% names(basic)), "basic strategy has required columns")

naive_df <- load_strategy(file.path(STRATEGIES_DIR, "naive_strategy.csv"))
expect_true(is.data.frame(naive_df),         "naive strategy CSV loaded")

# ---- Tests: load_strategy (data.frame) --------------------------------------
cat("\n=== load_strategy (data.frame) ===\n")
loaded <- load_strategy(good_df)
expect_equal(nrow(loaded), 3L,               "data.frame strategy loaded")

# ---- Tests: load_strategy (function) ----------------------------------------
cat("\n=== load_strategy (function) ===\n")
f <- load_strategy(naive_strategy)
expect_true(is.function(f),                  "function strategy returned as-is")

# ---- Tests: decide_action (data.frame strategy) -----------------------------
cat("\n=== decide_action (data.frame) ===\n")

state_hit <- list(
  player_hand   = c(10L, 6L),
  dealer_upcard = 10L,
  hand_value    = 16L,
  soft_hand     = FALSE,
  can_double    = FALSE,
  can_split     = FALSE,
  turn_number   = 1L
)

# Basic strategy: hard 16 vs dealer 10 -> hit
action <- decide_action(basic, state_hit)
expect_equal(action, "hit", "basic: hard 16 vs 10 -> hit")

state_stand <- list(
  player_hand   = c(8L, 4L),
  dealer_upcard = 4L,
  hand_value    = 12L,
  soft_hand     = FALSE,
  can_double    = FALSE,
  can_split     = FALSE,
  turn_number   = 1L
)
action_stand <- decide_action(basic, state_stand)
expect_equal(action_stand, "stand", "basic: hard 12 vs 4 -> stand")

# Double constraint: can_double = FALSE forces hit instead
state_double <- list(
  player_hand   = c(5L, 4L),
  dealer_upcard = 6L,
  hand_value    = 9L,
  soft_hand     = FALSE,
  can_double    = FALSE,  # cannot double
  can_split     = FALSE,
  turn_number   = 2L
)
action_no_dbl <- decide_action(basic, state_double)
expect_equal(action_no_dbl, "hit",
             "basic: hard 9 vs 6, no double allowed -> hit (downgrade)")

# ---- Tests: decide_action (function strategy) --------------------------------
cat("\n=== decide_action (function) ===\n")

state_16 <- list(
  hand_value = 16L, soft_hand = FALSE,
  can_double = FALSE, can_split = FALSE, turn_number = 1L
)
expect_equal(decide_action(naive_strategy, state_16), "hit",
             "naive: 16 -> hit")

state_17 <- list(
  hand_value = 17L, soft_hand = FALSE,
  can_double = FALSE, can_split = FALSE, turn_number = 1L
)
expect_equal(decide_action(naive_strategy, state_17), "stand",
             "naive: 17 -> stand")

state_12 <- list(
  hand_value = 12L, soft_hand = FALSE,
  can_double = FALSE, can_split = FALSE, turn_number = 1L
)
expect_equal(decide_action(conservative_strategy, state_12), "hit",
             "conservative: 12 -> hit (stands only on >= 13)")
# Conservative stands on >= 13, so 13 -> stand:
expect_equal(decide_action(conservative_strategy,
               modifyList(state_12, list(hand_value = 13L))),
             "stand", "conservative: 13 -> stand")

expect_equal(decide_action(aggressive_strategy,
               modifyList(state_17, list(hand_value = 19L))),
             "hit",  "aggressive: 19 -> hit")
expect_equal(decide_action(aggressive_strategy,
               modifyList(state_17, list(hand_value = 20L))),
             "stand", "aggressive: 20 -> stand")

# ---- Tests: random_strategy -------------------------------------------------
cat("\n=== random_strategy ===\n")
state_rand <- list(
  hand_value = 14L, soft_hand = FALSE,
  can_double = TRUE, can_split = FALSE, turn_number = 1L
)
set.seed(1)
actions_seen <- replicate(50, random_strategy(state_rand))
expect_true(all(actions_seen %in% c("hit", "stand", "double")),
            "random strategy produces valid actions")
# With can_double=TRUE, double should appear eventually
expect_true("double" %in% actions_seen,
            "random strategy includes double when allowed")

# ---- Tests: simulate_games --------------------------------------------------
cat("\n=== simulate_games ===\n")
res <- simulate_games(naive_strategy, n_games = 100L, seed = 42L)
expect_equal(nrow(res), 100L,                "simulate_games returns correct row count")
expect_true(all(c("game_id", "profit", "outcome", "blackjack", "bust", "turns")
                %in% names(res)),            "simulate_games has required columns")
expect_true(all(res$game_id == seq_len(100L)), "game_id is sequential")
expect_true(all(res$outcome %in% c("win", "loss", "push", "blackjack")),
            "all outcomes valid in simulate_games")

# ---- Tests: summarise_results -----------------------------------------------
cat("\n=== summarise_results ===\n")
res100 <- simulate_games(naive_strategy, n_games = 500L, seed = 7L)
summ   <- summarise_results(res100, "naive")
expect_equal(summ$n_games, 500L,            "n_games recorded")
expect_true(summ$win_rate  >= 0 && summ$win_rate  <= 1, "win rate in [0,1]")
expect_true(summ$bust_rate >= 0 && summ$bust_rate <= 1, "bust rate in [0,1]")
expect_equal(
  round(summ$win_rate + summ$loss_rate + summ$push_rate, 8),
  1,
  "win + loss + push rates sum to 1"
)

# ---- Tests: compare_strategies / rank_strategies ----------------------------
cat("\n=== compare_strategies / rank_strategies ===\n")
strats <- list(naive = naive_strategy, conservative = conservative_strategy)
comp   <- compare_strategies(strats, n_games = 200L, seed = 11L)
expect_equal(nrow(comp), 2L,               "compare_strategies: one row per strategy")
expect_true("strategy" %in% names(comp),   "compare_strategies: strategy column present")

ranked <- rank_strategies(comp)
expect_equal(nrow(ranked), 2L,             "rank_strategies: same number of rows")
expect_equal(ranked$rank, 1:2,             "rank column is 1:2")
expect_true(ranked$expected_return[1] >= ranked$expected_return[2],
            "ranked in descending order")

# ---- Summary ----------------------------------------------------------------
cat(sprintf("\n=== Results: %d/%d passed, %d failed ===\n",
            .tests_passed, .tests_run, .tests_failed))
if (.tests_failed > 0L) {
  stop(sprintf("%d test(s) failed.", .tests_failed))
}
