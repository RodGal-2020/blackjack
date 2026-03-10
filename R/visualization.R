# =============================================================================
# visualization.R
# ggplot2-based visual summaries for blackjack simulation results.
# =============================================================================

#' Plot profit distribution for one or more strategies
#'
#' @param results A \code{data.frame} from \code{\link{simulate_games}} or
#'   \code{\link{simulate_all_strategies}}.  Must contain a \code{profit}
#'   column and, optionally, a \code{strategy} column.
#' @param binwidth Histogram bin width (default 0.5).
#' @return A \code{ggplot2} object.
#' @examples
#' res <- simulate_games(naive_strategy, n_games = 500, seed = 1)
#' plot_profit_distribution(res)
#' @export
plot_profit_distribution <- function(results, binwidth = 0.5) {
  .require_ggplot2()
  has_strategy <- "strategy" %in% names(results)

  p <- ggplot2::ggplot(results, ggplot2::aes(x = profit)) +
    ggplot2::geom_histogram(
      binwidth = binwidth,
      fill = "steelblue", colour = "white", alpha = 0.8
    ) +
    ggplot2::geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
    ggplot2::labs(
      title = "Profit Distribution",
      x     = "Profit (units)",
      y     = "Count"
    ) +
    ggplot2::theme_minimal()

  if (has_strategy) {
    p <- p + ggplot2::facet_wrap(~ strategy, scales = "free_y")
  }
  p
}

#' Plot cumulative winnings over simulated games
#'
#' @param results A \code{data.frame} from \code{\link{simulate_games}} (single
#'   strategy) or \code{\link{simulate_all_strategies}} (multiple strategies).
#'   Must contain \code{game_id} and \code{profit} columns.
#' @return A \code{ggplot2} object.
#' @export
plot_cumulative_winnings <- function(results) {
  .require_ggplot2()
  has_strategy <- "strategy" %in% names(results)

  if (has_strategy) {
    strat_names <- unique(results$strategy)
    cum_list    <- lapply(strat_names, function(s) {
      sub <- results[results$strategy == s, ]
      sub <- sub[order(sub$game_id), ]
      sub$cumulative_profit <- cumsum(sub$profit)
      sub
    })
    results <- do.call(rbind, cum_list)

    p <- ggplot2::ggplot(
      results,
      ggplot2::aes(x = game_id, y = cumulative_profit, colour = strategy)
    )
  } else {
    results <- results[order(results$game_id), ]
    results$cumulative_profit <- cumsum(results$profit)

    p <- ggplot2::ggplot(
      results,
      ggplot2::aes(x = game_id, y = cumulative_profit)
    )
  }

  p <- p +
    ggplot2::geom_line(alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
    ggplot2::labs(
      title  = "Cumulative Winnings",
      x      = "Game Number",
      y      = "Cumulative Profit (units)",
      colour = "Strategy"
    ) +
    ggplot2::theme_minimal()

  p
}

#' Plot a heatmap of recommended actions from a strategy
#'
#' @param strategy A strategy data.frame or function.
#' @param soft     Logical.  If \code{TRUE} plot soft-hand actions; if
#'   \code{FALSE} (default) hard-hand actions.
#' @param player_totals Integer vector of player totals (default 4:21).
#' @param dealer_cards  Integer vector of dealer upcards (default 2:11).
#' @return A \code{ggplot2} object.
#' @export
plot_strategy_heatmap <- function(strategy,
                                  soft          = FALSE,
                                  player_totals = 4:21,
                                  dealer_cards  = 2:11) {
  .require_ggplot2()

  grid <- action_frequency(strategy,
                           player_totals = player_totals,
                           dealer_cards  = dealer_cards,
                           soft          = soft)

  action_colours <- c(
    hit    = "#E74C3C",
    stand  = "#2ECC71",
    double = "#F39C12",
    split  = "#9B59B6"
  )

  grid$player_total <- factor(grid$player_total,
                               levels = sort(unique(grid$player_total)))
  grid$dealer_label <- ifelse(grid$dealer_card == 11L, "A",
                               as.character(grid$dealer_card))
  grid$dealer_label <- factor(grid$dealer_label,
                               levels = c(as.character(2:10), "A"))

  ggplot2::ggplot(
    grid,
    ggplot2::aes(x = dealer_label, y = player_total, fill = action)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = toupper(substr(action, 1, 1))),
                       size = 3.5, fontface = "bold") +
    ggplot2::scale_fill_manual(values = action_colours) +
    ggplot2::labs(
      title    = paste0(if (soft) "Soft" else "Hard", " Hand Strategy"),
      x        = "Dealer Upcard",
      y        = "Player Total",
      fill     = "Action"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 9))
}

#' Plot expected value by player total and dealer upcard (heatmap)
#'
#' @param ev_data A \code{data.frame} from \code{\link{ev_grid}}.
#' @return A \code{ggplot2} object.
#' @export
plot_ev_heatmap <- function(ev_data) {
  .require_ggplot2()

  ev_data$dealer_label <- ifelse(ev_data$dealer_card == 11L, "A",
                                  as.character(ev_data$dealer_card))
  ev_data$dealer_label <- factor(ev_data$dealer_label,
                                  levels = c(as.character(2:10), "A"))
  ev_data$player_total <- factor(ev_data$player_total,
                                  levels = sort(unique(ev_data$player_total)))

  ggplot2::ggplot(
    ev_data,
    ggplot2::aes(x = dealer_label, y = player_total, fill = expected_value)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::scale_fill_gradient2(
      low  = "#E74C3C",
      mid  = "#FFFFFF",
      high = "#2ECC71",
      midpoint = 0
    ) +
    ggplot2::labs(
      title = "Expected Value by Player Total vs Dealer Upcard",
      x     = "Dealer Upcard",
      y     = "Player Total",
      fill  = "EV"
    ) +
    ggplot2::theme_minimal()
}

#' Bar chart comparing expected returns across strategies
#'
#' @param summary_df A \code{data.frame} from \code{\link{compare_strategies}}.
#' @return A \code{ggplot2} object.
#' @export
plot_strategy_comparison <- function(summary_df) {
  .require_ggplot2()

  summary_df <- summary_df[order(summary_df$expected_return), ]
  summary_df$strategy <- factor(summary_df$strategy,
                                 levels = summary_df$strategy)

  ggplot2::ggplot(
    summary_df,
    ggplot2::aes(x = strategy, y = expected_return, fill = expected_return > 0)
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C")) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Expected Return by Strategy",
      x     = "Strategy",
      y     = "Expected Return per Game (units)"
    ) +
    ggplot2::theme_minimal()
}

# Internal helper: stop with a helpful message if ggplot2 is absent
.require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualisation. ",
         "Install it with: install.packages('ggplot2')")
  }
}
