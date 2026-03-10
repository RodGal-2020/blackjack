# =============================================================================
# tests/test_engine.R
# Unit tests for the core blackjack engine (R/blackjack_engine.R).
# Run with: source("tests/test_engine.R")
# Alternatively, use testthat: testthat::test_file("tests/test_engine.R")
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

# ---- Minimal testing framework (no external dependencies) -------------------
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

expect_true <- function(actual, label = "") {
  expect_equal(isTRUE(actual), TRUE, label)
}

expect_false <- function(actual, label = "") {
  expect_equal(isTRUE(actual), FALSE, label)
}

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

# ---- Tests: create_shoe -----------------------------------------------------
cat("\n=== create_shoe ===\n")
shoe <- create_shoe(n_decks = 1, seed = 42)
expect_equal(length(shoe$cards), 52L,   "single deck has 52 cards")
expect_equal(shoe$position, 1L,          "initial position is 1")
expect_equal(shoe$n_decks, 1L,           "n_decks recorded correctly")

shoe2 <- create_shoe(n_decks = 2, seed = 1)
expect_equal(length(shoe2$cards), 104L,  "double deck has 104 cards")

# Card values are in {2,3,...,10,11}
expect_true(all(shoe$cards %in% 2:11),   "all card values valid")

# Reproducibility
shoe3 <- create_shoe(n_decks = 1, seed = 42)
expect_equal(shoe3$cards, shoe$cards,    "same seed gives same shoe")

# ---- Tests: draw_card -------------------------------------------------------
cat("\n=== draw_card ===\n")
shoe <- create_shoe(n_decks = 1, seed = 7)
r    <- draw_card(shoe)
expect_true(r$card %in% 2:11,            "drawn card is valid")
expect_equal(r$shoe$position, 2L,        "position advances after draw")

# Draw all cards, then error on next draw
shoe_full <- create_shoe(n_decks = 1, seed = 1)
for (i in seq_len(52L)) { r <- draw_card(shoe_full); shoe_full <- r$shoe }
expect_error_thrown(draw_card(shoe_full), "error when shoe is empty")

# ---- Tests: hand_value ------------------------------------------------------
cat("\n=== hand_value ===\n")
expect_equal(hand_value(c(10L, 7L)),         17L, "hard 17")
expect_equal(hand_value(c(11L, 7L)),         18L, "soft 18 (A+7)")
expect_equal(hand_value(c(11L, 11L)),        12L, "two aces = 12")
expect_equal(hand_value(c(11L, 11L, 9L)),    21L, "A+A+9 = 21")
expect_equal(hand_value(c(10L, 10L, 5L)),    25L, "bust hand value (no aces)")
expect_equal(hand_value(c(11L, 6L, 5L)),     12L, "A+6+5: ace reduced to 1 -> 12")
expect_equal(hand_value(c(11L, 5L, 5L)),     21L, "A+5+5 = 21")
expect_equal(hand_value(c(2L)),               2L, "single card")

# ---- Tests: is_soft ---------------------------------------------------------
cat("\n=== is_soft ===\n")
expect_true(is_soft(c(11L, 7L)),         "A+7 is soft")
expect_false(is_soft(c(10L, 7L)),        "10+7 is hard")
expect_true(is_soft(c(11L, 11L, 9L)),    "A+A+9: one ace still counts as 11 -> soft 21")
expect_true(is_soft(c(11L, 5L)),         "A+5 is soft")
expect_false(is_soft(c(11L, 7L, 8L)),    "A+7+8=26 -> ace becomes 1 -> 16, hard")

# ---- Tests: is_bust ---------------------------------------------------------
cat("\n=== is_bust ===\n")
expect_true(is_bust(c(10L, 10L, 5L)),    "10+10+5 = 25, bust")
expect_false(is_bust(c(10L, 11L)),       "10+A = 21, not bust")
expect_false(is_bust(c(10L, 7L)),        "17, not bust")

# ---- Tests: is_blackjack ----------------------------------------------------
cat("\n=== is_blackjack ===\n")
expect_true(is_blackjack(c(11L, 10L)),   "A+10 = blackjack")
expect_true(is_blackjack(c(10L, 11L)),   "10+A = blackjack")
expect_false(is_blackjack(c(10L, 10L)),  "10+10 = 20, not blackjack")
expect_false(is_blackjack(c(10L, 7L, 4L)), "3-card 21 is not blackjack")

# ---- Tests: can_split_hand --------------------------------------------------
cat("\n=== can_split_hand ===\n")
expect_true(can_split_hand(c(10L, 10L)),  "pair of 10s can split")
expect_true(can_split_hand(c(11L, 11L)),  "pair of aces can split")
expect_false(can_split_hand(c(10L, 7L)),  "non-pair cannot split")
expect_false(can_split_hand(c(5L, 5L, 5L)), "3-card hand cannot split")

# ---- Tests: run_blackjack_game outcome types --------------------------------
cat("\n=== run_blackjack_game ===\n")
set.seed(123)
results_outcomes <- replicate(100, {
  r <- run_blackjack_game(naive_strategy, default_rules())
  r$outcome
})
valid_outcomes <- c("win", "loss", "push", "blackjack")
expect_true(all(results_outcomes %in% valid_outcomes),
            "all outcomes are valid strings")

set.seed(99)
results_profits <- replicate(200, {
  run_blackjack_game(naive_strategy, default_rules())$profit
})
# Profit must be in {-2, -1, 0, 1, 1.5, 2} (unit bet, possible double)
expect_true(all(results_profits >= -2 & results_profits <= 2),
            "profits in expected range")

# Test that shoe_state is returned and updated
shoe <- create_shoe(n_decks = 1, seed = 5)
r    <- run_blackjack_game(naive_strategy, default_rules(), shoe_state = shoe)
expect_true(r$shoe_state$position > 4L, "shoe position advances during game")

# ---- Tests: default_rules ---------------------------------------------------
cat("\n=== default_rules ===\n")
rules <- default_rules()
expect_equal(rules$n_decks, 1L,             "default 1 deck")
expect_true(rules$dealer_stands_soft17,      "dealer stands soft 17 by default")
expect_equal(rules$blackjack_payout, 1.5,   "blackjack pays 3:2 by default")

# ---- Tests: get_game_state --------------------------------------------------
cat("\n=== get_game_state ===\n")
state <- make_game_state(c(10L, 7L), dealer_upcard = 6L)
df    <- get_game_state(state)
expect_equal(df$hand_value, 17L,  "hand_value correct in state df")
expect_equal(df$dealer_upcard, 6L, "dealer_upcard correct in state df")
expect_false(df$soft_hand,         "hard hand detected")

state_soft <- make_game_state(c(11L, 7L), dealer_upcard = 4L)
df_soft    <- get_game_state(state_soft)
expect_true(df_soft$soft_hand,     "soft hand detected")

# ---- Summary ----------------------------------------------------------------
cat(sprintf("\n=== Results: %d/%d passed, %d failed ===\n",
            .tests_passed, .tests_run, .tests_failed))
if (.tests_failed > 0L) {
  stop(sprintf("%d test(s) failed.", .tests_failed))
}
