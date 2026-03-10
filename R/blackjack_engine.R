# =============================================================================
# blackjack_engine.R
# Core blackjack game engine following standard casino rules.
# =============================================================================

#' Create a new shoe (one or more decks of cards)
#'
#' Card values: 2-9 = face value, 10/J/Q/K = 10, A = 11 (reduced to 1 when needed).
#'
#' @param n_decks Number of decks to use (default 1).
#' @param seed    Optional integer random seed for reproducibility.
#' @return A named list with elements:
#'   \item{cards}{Integer vector of card values (shuffled).}
#'   \item{position}{Current draw position (starts at 1).}
#'   \item{n_decks}{Number of decks.}
#'   \item{total_cards}{Total number of cards in the shoe.}
#' @examples
#' shoe <- create_shoe(n_decks = 2, seed = 42)
#' @export
create_shoe <- function(n_decks = 1, seed = NULL) {
  stopifnot(is.numeric(n_decks), n_decks >= 1)
  if (!is.null(seed)) set.seed(as.integer(seed))

  # One suit: 2-9 (face value), 10/J/Q/K = 10, A = 11
  one_suit <- c(2:9, 10, 10, 10, 10, 11)  # 13 cards
  one_deck <- rep(one_suit, 4)             # 4 suits = 52 cards
  shoe_cards <- rep(one_deck, n_decks)
  shoe_cards <- sample(shoe_cards)

  list(
    cards      = shoe_cards,
    position   = 1L,
    n_decks    = n_decks,
    total_cards = length(shoe_cards)
  )
}

#' Draw one card from a shoe
#'
#' @param shoe A shoe list as returned by \code{create_shoe}.
#' @return A named list with elements:
#'   \item{card}{Integer value of the drawn card.}
#'   \item{shoe}{Updated shoe with position incremented.}
#' @export
draw_card <- function(shoe) {
  if (shoe$position > length(shoe$cards)) {
    stop("Shoe is exhausted. Create a new shoe.")
  }
  card <- shoe$cards[[shoe$position]]
  shoe$position <- shoe$position + 1L
  list(card = card, shoe = shoe)
}

#' Compute the total value of a blackjack hand
#'
#' Aces (stored as 11) are reduced to 1 as needed to avoid busting.
#'
#' @param hand Integer vector of card values.
#' @return Integer total value of the hand (between 2 and 21 unless bust).
#' @examples
#' hand_value(c(11, 7))   # Soft 18 -> 18
#' hand_value(c(11, 11))  # Two aces -> 12
#' hand_value(c(11, 11, 9)) # Bust with aces -> 21? No -> 11+1+9 = 21
#' @export
hand_value <- function(hand) {
  total  <- sum(hand)
  n_aces <- sum(hand == 11L)
  while (total > 21L && n_aces > 0L) {
    total  <- total - 10L
    n_aces <- n_aces - 1L
  }
  total
}

#' Determine whether a hand is soft (at least one ace counted as 11)
#'
#' @param hand Integer vector of card values.
#' @return Logical \code{TRUE} if the hand is soft.
#' @export
is_soft <- function(hand) {
  if (!any(hand == 11L)) return(FALSE)
  total  <- sum(hand)
  n_aces <- sum(hand == 11L)
  while (total > 21L && n_aces > 0L) {
    total  <- total - 10L
    n_aces <- n_aces - 1L
  }
  n_aces > 0L
}

#' Check if a hand is busted (value exceeds 21)
#'
#' @param hand Integer vector of card values.
#' @return Logical.
#' @export
is_bust <- function(hand) {
  hand_value(hand) > 21L
}

#' Check if a hand is a natural blackjack (two cards summing to 21)
#'
#' @param hand Integer vector of card values.
#' @return Logical.
#' @export
is_blackjack <- function(hand) {
  length(hand) == 2L && hand_value(hand) == 21L
}

#' Check if a hand can be split (two cards of equal value)
#'
#' @param hand Integer vector of card values.
#' @return Logical.
#' @export
can_split_hand <- function(hand) {
  length(hand) == 2L && hand[[1L]] == hand[[2L]]
}

#' Default game rules
#'
#' @return A named list of rule settings.
#' @export
default_rules <- function() {
  list(
    n_decks              = 1L,
    dealer_stands_soft17 = TRUE,
    blackjack_payout     = 1.5,
    allow_double         = TRUE,
    allow_split          = TRUE,
    seed                 = NULL
  )
}

#' Run a single complete blackjack game
#'
#' Follows the standard casino sequence:
#' \enumerate{
#'   \item Deal two cards to player and dealer.
#'   \item Check for natural blackjacks.
#'   \item Player acts according to \code{strategy} until standing or busting.
#'   \item Dealer acts according to house rules.
#'   \item Determine outcome and profit.
#' }
#'
#' @param strategy  A strategy data.frame (with columns \code{player_total},
#'   \code{dealer_card}, \code{soft}, \code{action}) or a function of the form
#'   \code{function(game_state) -> action_string}.
#' @param rules     A named list of rules as returned by \code{default_rules()}.
#' @param shoe_state An existing shoe list. If \code{NULL} a fresh shoe is created.
#' @return A named list with:
#'   \item{profit}{Numeric net profit/loss in units of the initial bet.}
#'   \item{outcome}{Character: "win", "loss", "push", or "blackjack".}
#'   \item{player_hand}{Final player hand.}
#'   \item{dealer_hand}{Final dealer hand.}
#'   \item{blackjack}{Logical: did the player get a natural blackjack?}
#'   \item{bust}{Logical: did the player bust?}
#'   \item{turns}{Integer number of player turns taken.}
#'   \item{shoe_state}{Updated shoe after the game.}
#'   \item{log}{List of game event records.}
#' @export
run_blackjack_game <- function(strategy, rules = default_rules(), shoe_state = NULL) {
  if (is.null(shoe_state)) {
    shoe_state <- create_shoe(n_decks = rules$n_decks, seed = rules$seed)
  }

  game_log <- list()

  # --- Deal initial hands ---
  r1 <- draw_card(shoe_state); shoe_state <- r1$shoe; player_hand <- r1$card
  r2 <- draw_card(shoe_state); shoe_state <- r2$shoe; dealer_hand <- r2$card
  r3 <- draw_card(shoe_state); shoe_state <- r3$shoe; player_hand <- c(player_hand, r3$card)
  r4 <- draw_card(shoe_state); shoe_state <- r4$shoe; dealer_hand <- c(dealer_hand, r4$card)

  bet <- 1  # unit bet

  game_log[[1L]] <- list(
    turn         = 0L,
    event        = "deal",
    player_hand  = player_hand,
    dealer_upcard = dealer_hand[[1L]],
    player_value = hand_value(player_hand),
    dealer_value = hand_value(dealer_hand)
  )

  # --- Check for natural blackjacks ---
  player_bj <- is_blackjack(player_hand)
  dealer_bj <- is_blackjack(dealer_hand)

  if (player_bj || dealer_bj) {
    if (player_bj && dealer_bj) {
      outcome <- "push"
      profit  <- 0
    } else if (player_bj) {
      outcome <- "blackjack"
      profit  <- bet * rules$blackjack_payout
    } else {
      outcome <- "loss"
      profit  <- -bet
    }

    game_log[[2L]] <- list(
      turn        = 1L,
      event       = "blackjack_check",
      outcome     = outcome,
      player_hand = player_hand,
      dealer_hand = dealer_hand
    )

    return(list(
      profit      = profit,
      outcome     = outcome,
      player_hand = player_hand,
      dealer_hand = dealer_hand,
      blackjack   = player_bj,
      bust        = FALSE,
      turns       = 0L,
      shoe_state  = shoe_state,
      log         = game_log
    ))
  }

  # --- Player's turn ---
  turn          <- 0L
  player_busted <- FALSE

  repeat {
    turn <- turn + 1L

    state <- list(
      player_hand  = player_hand,
      dealer_upcard = dealer_hand[[1L]],
      hand_value   = hand_value(player_hand),
      soft_hand    = is_soft(player_hand),
      deck_state   = NULL,
      can_double   = (turn == 1L && rules$allow_double),
      can_split    = (turn == 1L && rules$allow_split && can_split_hand(player_hand)),
      turn_number  = turn
    )

    action <- decide_action(strategy, state)

    game_log[[length(game_log) + 1L]] <- list(
      turn         = turn,
      event        = "player_action",
      action       = action,
      player_hand  = player_hand,
      player_value = hand_value(player_hand),
      soft         = is_soft(player_hand)
    )

    if (action == "stand") {
      break

    } else if (action == "hit") {
      r <- draw_card(shoe_state); shoe_state <- r$shoe
      player_hand <- c(player_hand, r$card)
      if (is_bust(player_hand)) { player_busted <- TRUE; break }

    } else if (action == "double") {
      bet <- bet * 2
      r <- draw_card(shoe_state); shoe_state <- r$shoe
      player_hand <- c(player_hand, r$card)
      if (is_bust(player_hand)) { player_busted <- TRUE }
      break  # only one card after doubling

    } else if (action == "split") {
      # Simplified split: draw one additional card to the first card of the pair
      # and play as a single hand (full split support is out of scope for v1)
      r <- draw_card(shoe_state); shoe_state <- r$shoe
      player_hand <- c(player_hand[[1L]], r$card)
      if (is_bust(player_hand)) { player_busted <- TRUE; break }
    }

    if (hand_value(player_hand) == 21L) break
  }

  game_log[[length(game_log) + 1L]] <- list(
    turn         = turn + 1L,
    event        = "player_done",
    player_hand  = player_hand,
    player_value = hand_value(player_hand),
    busted       = player_busted
  )

  # --- Dealer's turn ---
  if (!player_busted) {
    repeat {
      dv <- hand_value(dealer_hand)
      ds <- is_soft(dealer_hand)

      # Dealer stands on hard 17+ and (if rules say so) soft 17
      dealer_stands <- if (rules$dealer_stands_soft17) {
        dv >= 17L
      } else {
        dv > 17L || (dv == 17L && !ds)
      }

      if (dealer_stands) break

      r <- draw_card(shoe_state); shoe_state <- r$shoe
      dealer_hand <- c(dealer_hand, r$card)
      if (is_bust(dealer_hand)) break
    }
  }

  game_log[[length(game_log) + 1L]] <- list(
    turn         = turn + 2L,
    event        = "dealer_done",
    dealer_hand  = dealer_hand,
    dealer_value = hand_value(dealer_hand)
  )

  # --- Determine outcome ---
  pv            <- hand_value(player_hand)
  dv            <- hand_value(dealer_hand)
  dealer_busted <- is_bust(dealer_hand)

  if (player_busted) {
    outcome <- "loss"; profit <- -bet
  } else if (dealer_busted) {
    outcome <- "win";  profit <-  bet
  } else if (pv > dv) {
    outcome <- "win";  profit <-  bet
  } else if (pv < dv) {
    outcome <- "loss"; profit <- -bet
  } else {
    outcome <- "push"; profit <-  0
  }

  game_log[[length(game_log) + 1L]] <- list(
    turn    = turn + 3L,
    event   = "result",
    outcome = outcome,
    profit  = profit
  )

  list(
    profit      = profit,
    outcome     = outcome,
    player_hand = player_hand,
    dealer_hand = dealer_hand,
    blackjack   = FALSE,
    bust        = player_busted,
    turns       = turn,
    shoe_state  = shoe_state,
    log         = game_log
  )
}
