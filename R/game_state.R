# =============================================================================
# game_state.R
# Structured representation of a blackjack game state.
# =============================================================================

#' Create a structured game state from components
#'
#' @param player_hand   Integer vector of the player's card values.
#' @param dealer_upcard Integer value of the dealer's visible card.
#' @param turn_number   Integer current turn number (default 1).
#' @param rules         A rules list as returned by \code{default_rules()}.
#' @return A named list representing the game state.
#' @export
make_game_state <- function(player_hand, dealer_upcard,
                            turn_number = 1L, rules = default_rules()) {
  list(
    player_hand   = player_hand,
    dealer_upcard = dealer_upcard,
    hand_value    = hand_value(player_hand),
    soft_hand     = is_soft(player_hand),
    deck_state    = NULL,
    can_double    = (turn_number == 1L && rules$allow_double),
    can_split     = (turn_number == 1L && rules$allow_split &&
                       can_split_hand(player_hand)),
    turn_number   = turn_number
  )
}

#' Extract a tidy data.frame snapshot of a game state or finished game
#'
#' Works with both active state lists (from \code{make_game_state}) and
#' completed game result lists (from \code{run_blackjack_game}).
#'
#' @param game A game state list or a finished game result list.
#' @return A one-row \code{data.frame} with columns:
#'   \code{player_hand}, \code{dealer_upcard}, \code{hand_value},
#'   \code{soft_hand}, \code{can_double}, \code{can_split},
#'   \code{turn_number}.
#' @export
get_game_state <- function(game) {
  if (is.null(game)) return(NULL)

  ph <- game$player_hand
  du <- if (!is.null(game$dealer_upcard)) {
    game$dealer_upcard
  } else if (!is.null(game$dealer_hand)) {
    game$dealer_hand[[1L]]
  } else {
    NA_integer_
  }

  data.frame(
    player_hand  = paste(ph, collapse = ","),
    dealer_upcard = du,
    hand_value   = hand_value(ph),
    soft_hand    = is_soft(ph),
    deck_state   = NA_character_,
    can_double   = if (!is.null(game$can_double))  game$can_double  else NA,
    can_split    = if (!is.null(game$can_split))   game$can_split   else NA,
    turn_number  = if (!is.null(game$turn_number)) game$turn_number else NA_integer_,
    stringsAsFactors = FALSE
  )
}

#' Expand a game's log into a tidy data.frame of state transitions
#'
#' @param game A finished game result list from \code{run_blackjack_game}.
#' @return A \code{data.frame} with one row per logged event, columns:
#'   \code{turn}, \code{event}, \code{player_value}, \code{action},
#'   \code{outcome}.
#' @export
game_log_to_df <- function(game) {
  if (is.null(game$log)) return(data.frame())

  rows <- lapply(game$log, function(entry) {
    data.frame(
      turn         = entry$turn %||% NA_integer_,
      event        = entry$event %||% NA_character_,
      player_value = entry$player_value %||% NA_integer_,
      action       = entry$action %||% NA_character_,
      outcome      = entry$outcome %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Null-coalescing helper (not exported)
`%||%` <- function(x, y) if (!is.null(x)) x else y
