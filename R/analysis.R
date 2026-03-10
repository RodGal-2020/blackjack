# =============================================================================
# analysis.R
# Strategy comparison, ranking, and statistical summaries.
# =============================================================================

#' Summarise simulation results for a single strategy
#'
#' @param results  A \code{data.frame} from \code{\link{simulate_games}}.
#' @param strategy_name Optional character label to include in the summary.
#' @return A one-row \code{data.frame} with columns:
#'   \code{strategy}, \code{n_games}, \code{expected_return},
#'   \code{win_rate}, \code{loss_rate}, \code{push_rate},
#'   \code{blackjack_rate}, \code{bust_rate},
#'   \code{mean_profit}, \code{sd_profit},
#'   \code{total_profit}.
#' @export
summarise_results <- function(results, strategy_name = NA_character_) {
  n       <- nrow(results)
  wins    <- sum(results$outcome %in% c("win", "blackjack"))
  losses  <- sum(results$outcome == "loss")
  pushes  <- sum(results$outcome == "push")
  bjs     <- sum(results$blackjack)
  busts   <- sum(results$bust)

  data.frame(
    strategy        = strategy_name,
    n_games         = n,
    expected_return = mean(results$profit),
    win_rate        = wins   / n,
    loss_rate       = losses / n,
    push_rate       = pushes / n,
    blackjack_rate  = bjs    / n,
    bust_rate       = busts  / n,
    mean_profit     = mean(results$profit),
    sd_profit       = sd(results$profit),
    total_profit    = sum(results$profit),
    stringsAsFactors = FALSE
  )
}

#' Compare multiple strategies over a set of simulated games
#'
#' Runs simulations for each strategy and returns a combined summary table.
#'
#' @param strategies A named list of strategies (see \code{\link{builtin_strategies}}).
#' @param n_games    Number of games per strategy (default 10 000).
#' @param rules      A rules list from \code{\link{default_rules}}.
#' @param n_workers  Number of parallel workers per strategy.
#' @param seed       Master random seed.
#' @return A \code{data.frame} with one row per strategy and summary columns.
#' @examples
#' strats <- list(naive = naive_strategy, conservative = conservative_strategy)
#' compare_strategies(strats, n_games = 1000, seed = 42)
#' @export
compare_strategies <- function(strategies,
                               n_games   = 10000L,
                               rules     = default_rules(),
                               n_workers = 1L,
                               seed      = NULL) {
  if (!is.list(strategies) || is.null(names(strategies))) {
    stop("'strategies' must be a named list.")
  }

  summaries <- vector("list", length(strategies))
  for (i in seq_along(strategies)) {
    strat_seed <- if (!is.null(seed)) seed + i else NULL
    res <- simulate_games(
      strategy   = strategies[[i]],
      n_games    = n_games,
      rules      = rules,
      n_workers  = n_workers,
      seed       = strat_seed
    )
    summaries[[i]] <- summarise_results(res, strategy_name = names(strategies)[[i]])
  }

  do.call(rbind, summaries)
}

#' Rank strategies by expected return (best first)
#'
#' @param results A summary \code{data.frame} from \code{\link{compare_strategies}}.
#' @return The same \code{data.frame} sorted by \code{expected_return} (descending)
#'   with an added integer \code{rank} column.
#' @export
rank_strategies <- function(results) {
  if (!"expected_return" %in% names(results)) {
    stop("'results' must have an 'expected_return' column. ",
         "Run compare_strategies() or summarise_results() first.")
  }
  results <- results[order(-results$expected_return), ]
  results$rank <- seq_len(nrow(results))
  rownames(results) <- NULL
  results
}

#' Compute action frequency by hand total and dealer upcard
#'
#' Useful for verifying that a strategy is being applied correctly.
#'
#' @param strategy   A strategy data.frame.
#' @param player_totals Integer vector of totals to analyse (default 5:21).
#' @param dealer_cards  Integer vector of dealer upcards (default 2:11).
#' @param soft          Logical: analyse soft hands?  Default \code{FALSE}.
#' @return A \code{data.frame} with columns \code{player_total},
#'   \code{dealer_card}, \code{soft}, \code{action}.
#' @export
action_frequency <- function(strategy,
                             player_totals = 5:21,
                             dealer_cards  = 2:11,
                             soft          = FALSE) {
  if (is.function(strategy)) {
    # Generate action table from a function-based strategy
    rows <- vector("list", length(player_totals) * length(dealer_cards))
    k <- 1L
    for (pt in player_totals) {
      for (dc in dealer_cards) {
        state <- list(
          player_hand   = if (soft) c(11L, pt - 11L) else c(pt %/% 2L, pt - pt %/% 2L),
          dealer_upcard = dc,
          hand_value    = pt,
          soft_hand     = soft,
          can_double    = TRUE,
          can_split     = FALSE,
          turn_number   = 1L
        )
        rows[[k]] <- data.frame(
          player_total = pt,
          dealer_card  = dc,
          soft         = soft,
          action       = strategy(state),
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }
    return(do.call(rbind, rows))
  }

  # Data.frame-based strategy: filter and return
  idx <- strategy$soft == soft &
         strategy$player_total %in% player_totals &
         strategy$dealer_card  %in% dealer_cards
  strategy[idx, c("player_total", "dealer_card", "soft", "action")]
}

#' Compute profit percentiles for a set of simulation results
#'
#' @param results A \code{data.frame} from \code{\link{simulate_games}}.
#' @param probs   Numeric vector of probabilities (default quartiles + extremes).
#' @return A named numeric vector of profit quantiles.
#' @export
profit_quantiles <- function(results,
                             probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)) {
  stats::quantile(results$profit, probs = probs)
}

#' Compute the bust probability from simulation results
#'
#' @param results A \code{data.frame} from \code{\link{simulate_games}}.
#' @return A numeric value between 0 and 1.
#' @export
bust_probability <- function(results) {
  mean(results$bust)
}

#' Compute the expected value (EV) grid by player total and dealer upcard
#'
#' Runs simulations fixing the initial player total and dealer upcard, then
#' records mean profit.  This is an approximation because the starting hand
#' is determined by the shoe draw, not forced.
#'
#' @param strategy     A strategy data.frame or function.
#' @param rules        A rules list.
#' @param n_per_cell   Simulations per (total, upcard) cell.
#' @param player_totals Integer vector of player totals to evaluate.
#' @param dealer_cards  Integer vector of dealer upcards to evaluate.
#' @param seed         Random seed.
#' @return A \code{data.frame} with columns \code{player_total},
#'   \code{dealer_card}, \code{expected_value}.
#' @export
ev_grid <- function(strategy,
                    rules         = default_rules(),
                    n_per_cell    = 200L,
                    player_totals = 5:21,
                    dealer_cards  = 2:11,
                    seed          = NULL) {
  if (!is.null(seed)) set.seed(as.integer(seed))

  rows <- vector("list", length(player_totals) * length(dealer_cards))
  k <- 1L

  for (pt in player_totals) {
    for (dc in dealer_cards) {
      profits <- numeric(n_per_cell)
      for (j in seq_len(n_per_cell)) {
        shoe  <- create_shoe(n_decks = rules$n_decks)
        # Override player hand: find two cards that sum to pt (hard total)
        # We approximate by using a fixed state and simulating from it
        card1 <- min(pt - 2L, 10L)
        card2 <- pt - card1
        # Ensure both are valid card values (stored as 2-11)
        card2 <- max(2L, min(10L, card2))
        card1 <- pt - card2
        card1 <- max(2L, min(11L, card1))

        # Simulate from the fixed starting state
        result <- .simulate_from_state(
          player_hand   = c(card1, card2),
          dealer_upcard = dc,
          strategy      = strategy,
          rules         = rules,
          shoe          = shoe
        )
        profits[[j]] <- result$profit
      }
      rows[[k]] <- data.frame(
        player_total   = pt,
        dealer_card    = dc,
        expected_value = mean(profits),
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  do.call(rbind, rows)
}

# Internal: simulate a game given a fixed starting state
.simulate_from_state <- function(player_hand, dealer_upcard,
                                 strategy, rules, shoe) {
  # Build a fake dealer hand: upcard + a hidden card from shoe
  r <- draw_card(shoe); shoe <- r$shoe
  dealer_hand <- c(dealer_upcard, r$card)

  turn          <- 0L
  player_busted <- FALSE
  bet           <- 1

  repeat {
    turn <- turn + 1L
    state <- list(
      player_hand   = player_hand,
      dealer_upcard = dealer_upcard,
      hand_value    = hand_value(player_hand),
      soft_hand     = is_soft(player_hand),
      can_double    = (turn == 1L && rules$allow_double),
      can_split     = FALSE,
      turn_number   = turn
    )
    action <- decide_action(strategy, state)

    if (action == "stand") break

    if (action %in% c("hit", "split")) {
      r <- draw_card(shoe); shoe <- r$shoe
      player_hand <- c(player_hand, r$card)
      if (is_bust(player_hand)) { player_busted <- TRUE; break }
    } else if (action == "double") {
      bet <- bet * 2
      r <- draw_card(shoe); shoe <- r$shoe
      player_hand <- c(player_hand, r$card)
      if (is_bust(player_hand)) player_busted <- TRUE
      break
    }
    if (hand_value(player_hand) == 21L) break
  }

  if (!player_busted) {
    repeat {
      dv <- hand_value(dealer_hand)
      ds <- is_soft(dealer_hand)
      dealer_stands <- if (rules$dealer_stands_soft17) dv >= 17L else
        dv > 17L || (dv == 17L && !ds)
      if (dealer_stands) break
      r <- draw_card(shoe); shoe <- r$shoe
      dealer_hand <- c(dealer_hand, r$card)
      if (is_bust(dealer_hand)) break
    }
  }

  pv            <- hand_value(player_hand)
  dv            <- hand_value(dealer_hand)
  dealer_busted <- is_bust(dealer_hand)

  profit <- if (player_busted) {
    -bet
  } else if (dealer_busted || pv > dv) {
    bet
  } else if (pv < dv) {
    -bet
  } else {
    0
  }

  list(profit = profit)
}
