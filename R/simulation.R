# =============================================================================
# simulation.R
# Large-scale Monte Carlo blackjack simulation with optional parallelism.
# =============================================================================

#' Simulate multiple blackjack games with a single strategy
#'
#' Each game uses a fresh shoe (or a persistent shoe if \code{fresh_shoe =
#' FALSE}).  Results are returned as a tidy \code{data.frame}.
#'
#' @param strategy   A strategy \code{data.frame} or function (see
#'   \code{\link{load_strategy}}).
#' @param n_games    Number of games to simulate (default 1 000).
#' @param rules      A rules list from \code{\link{default_rules}}.
#' @param n_workers  Number of parallel workers.  Values > 1 require the
#'   \pkg{parallel} package.  Default \code{1} (sequential).
#' @param seed       Master random seed for reproducibility.
#' @param fresh_shoe Logical.  If \code{TRUE} (default), create a new shoe for
#'   every game.  If \code{FALSE}, draw from one continuous shoe, reshuffling
#'   when fewer than 15 cards remain.
#' @return A \code{data.frame} with columns:
#'   \code{game_id}, \code{profit}, \code{outcome}, \code{blackjack},
#'   \code{bust}, \code{turns}.
#' @examples
#' results <- simulate_games(naive_strategy, n_games = 500, seed = 1)
#' @export
simulate_games <- function(strategy,
                           n_games    = 1000L,
                           rules      = default_rules(),
                           n_workers  = 1L,
                           seed       = NULL,
                           fresh_shoe = TRUE) {
  if (!is.null(seed)) set.seed(as.integer(seed))

  game_seeds <- if (!is.null(seed)) {
    sample.int(.Machine$integer.max, n_games)
  } else {
    vector("integer", n_games)  # 0L = no seed per game
  }

  run_one <- function(i) {
    s <- if (game_seeds[[i]] != 0L) game_seeds[[i]] else NULL
    local_rules <- rules
    local_rules$seed <- s

    shoe <- if (fresh_shoe) {
      create_shoe(n_decks = rules$n_decks, seed = s)
    } else {
      NULL
    }

    result <- run_blackjack_game(strategy, local_rules, shoe_state = shoe)

    data.frame(
      game_id   = i,
      profit    = result$profit,
      outcome   = result$outcome,
      blackjack = result$blackjack,
      bust      = result$bust,
      turns     = result$turns,
      stringsAsFactors = FALSE
    )
  }

  if (n_workers > 1L) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      warning("Package 'parallel' not available; falling back to sequential mode.")
      n_workers <- 1L
    }
  }

  if (n_workers > 1L) {
    cl <- parallel::makeCluster(n_workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Export local variables to workers
    local_env <- environment()
    parallel::clusterExport(cl,
      varlist = c("strategy", "rules", "game_seeds", "fresh_shoe", "run_one"),
      envir   = local_env
    )
    # Export global engine and strategy functions to workers
    parallel::clusterExport(cl,
      varlist = c("create_shoe", "draw_card", "hand_value", "is_soft",
                  "is_bust", "is_blackjack", "can_split_hand",
                  "run_blackjack_game", "decide_action", "default_rules"),
      envir   = globalenv()
    )

    rows <- parallel::parLapply(cl, seq_len(n_games), run_one)
  } else {
    rows <- lapply(seq_len(n_games), run_one)
  }

  do.call(rbind, rows)
}

#' Simulate multiple games for multiple strategies simultaneously
#'
#' Wraps \code{\link{simulate_games}} for each strategy and binds results.
#'
#' @param strategies A named list of strategies.
#' @param n_games    Number of games per strategy.
#' @param rules      A rules list from \code{\link{default_rules}}.
#' @param n_workers  Number of parallel workers per strategy.
#' @param seed       Master random seed.
#' @return A \code{data.frame} like \code{simulate_games} but with an
#'   additional \code{strategy} column.
#' @export
simulate_all_strategies <- function(strategies,
                                    n_games   = 1000L,
                                    rules     = default_rules(),
                                    n_workers = 1L,
                                    seed      = NULL) {
  if (!is.list(strategies) || is.null(names(strategies))) {
    stop("'strategies' must be a named list.")
  }

  strategy_names <- names(strategies)
  results_list   <- vector("list", length(strategies))

  for (i in seq_along(strategies)) {
    strat_seed <- if (!is.null(seed)) seed + i else NULL
    res <- simulate_games(
      strategy   = strategies[[i]],
      n_games    = n_games,
      rules      = rules,
      n_workers  = n_workers,
      seed       = strat_seed,
      fresh_shoe = TRUE
    )
    res$strategy    <- strategy_names[[i]]
    results_list[[i]] <- res
  }

  do.call(rbind, results_list)
}

#' Compute per-game cumulative profit for one strategy's results
#'
#' @param results A \code{data.frame} from \code{\link{simulate_games}}.
#' @return The same \code{data.frame} with an additional
#'   \code{cumulative_profit} column.
#' @export
add_cumulative_profit <- function(results) {
  results$cumulative_profit <- cumsum(results$profit)
  results
}
