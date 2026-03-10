# =============================================================================
# random_strategy.R
# A strategy that chooses actions uniformly at random from valid options.
#
# This file is a standalone copy of the random_strategy function defined in
# R/strategy_parser.R. It exists so users can source it independently
# (e.g., via load_strategy("strategies/random_strategy.R")) without needing
# to source the full strategy_parser module.
# =============================================================================

#' Random blackjack strategy
#'
#' Selects uniformly at random from the set of valid actions given the
#' current game state.  Useful as a baseline comparison.
#'
#' @param game_state A named list as produced by \code{make_game_state()}.
#' @return A character string: one of \code{"hit"}, \code{"stand"},
#'   \code{"double"}, or \code{"split"}.
#' @export
random_strategy <- function(game_state) {
  actions <- c("hit", "stand")
  if (isTRUE(game_state$can_double)) actions <- c(actions, "double")
  if (isTRUE(game_state$can_split))  actions <- c(actions, "split")
  sample(actions, size = 1L)
}
