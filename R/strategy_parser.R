# =============================================================================
# strategy_parser.R
# Strategy loading, validation, and action decision.
# =============================================================================

#' Load a strategy from various input formats
#'
#' Supported formats:
#' \itemize{
#'   \item \code{data.frame} – passed through after validation.
#'   \item CSV file (\code{.csv}) – read with \code{read.csv()}.
#'   \item YAML file (\code{.yaml} / \code{.yml}) – requires the \pkg{yaml} package.
#'   \item JSON file (\code{.json}) – requires the \pkg{jsonlite} package.
#'   \item \code{function} – returned as-is; must accept a game-state list and
#'         return an action string.
#' }
#'
#' The data.frame representation must have columns:
#' \code{player_total} (integer), \code{dealer_card} (integer),
#' \code{soft} (logical), \code{action} (character).
#'
#' @param source A file path (character), \code{data.frame}, or function.
#' @return A validated strategy \code{data.frame} or a strategy \code{function}.
#' @examples
#' strat <- load_strategy("strategies/basic_strategy.csv")
#' @export
load_strategy <- function(source) {
  if (is.data.frame(source)) {
    return(validate_strategy(source))
  }

  if (is.function(source)) {
    return(source)
  }

  if (is.character(source)) {
    ext <- tolower(tools::file_ext(source))

    if (ext == "csv") {
      strat <- read.csv(source, stringsAsFactors = FALSE)
      return(validate_strategy(strat))
    }

    if (ext %in% c("yaml", "yml")) {
      if (!requireNamespace("yaml", quietly = TRUE)) {
        stop("Package 'yaml' is required to load YAML strategies. ",
             "Install it with: install.packages('yaml')")
      }
      raw   <- yaml::read_yaml(source)
      strat <- as.data.frame(do.call(rbind, lapply(raw, as.data.frame)),
                             stringsAsFactors = FALSE)
      return(validate_strategy(strat))
    }

    if (ext == "json") {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("Package 'jsonlite' is required to load JSON strategies. ",
             "Install it with: install.packages('jsonlite')")
      }
      strat <- jsonlite::fromJSON(source)
      if (!is.data.frame(strat)) strat <- as.data.frame(strat, stringsAsFactors = FALSE)
      return(validate_strategy(strat))
    }

    # Try to source an R file and retrieve a "strategy" object or function
    if (ext == "r") {
      env <- new.env(parent = emptyenv())
      sys.source(source, envir = env)
      nms <- ls(env)
      if (length(nms) == 1L) return(get(nms[[1L]], envir = env))
      if ("strategy" %in% nms) return(env$strategy)
      stop("R strategy file must define exactly one object or an object named 'strategy'.")
    }

    stop("Unsupported file extension '", ext, "'. ",
         "Use .csv, .yaml/.yml, .json, or .R.")
  }

  stop("'source' must be a data.frame, a file path, or a function.")
}

#' Validate a strategy data.frame
#'
#' Ensures required columns exist and coerces types.
#'
#' @param strategy A \code{data.frame}.
#' @return The (possibly coerced) strategy \code{data.frame}.
#' @export
validate_strategy <- function(strategy) {
  required <- c("player_total", "dealer_card", "soft", "action")
  missing_cols <- setdiff(required, names(strategy))
  if (length(missing_cols) > 0L) {
    stop("Strategy data.frame is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  strategy$player_total <- as.integer(strategy$player_total)
  strategy$dealer_card  <- as.integer(strategy$dealer_card)
  strategy$soft         <- as.logical(strategy$soft)
  strategy$action       <- trimws(tolower(as.character(strategy$action)))

  valid_actions <- c("hit", "stand", "double", "split")
  bad <- strategy$action[!strategy$action %in% valid_actions]
  if (length(bad) > 0L) {
    stop("Unknown action(s) in strategy: ", paste(unique(bad), collapse = ", "),
         ". Valid actions are: ", paste(valid_actions, collapse = ", "))
  }
  strategy
}

#' Decide the next action given a strategy and the current game state
#'
#' Looks up the exact \code{(player_total, dealer_card, soft)} combination in
#' the strategy table.  If no row matches, falls back to a sensible default:
#' stand on 17+, hit otherwise.
#'
#' @param strategy A strategy \code{data.frame} or a strategy \code{function}.
#' @param game_state A named list as produced by \code{make_game_state()}.
#' @return A character string: \code{"hit"}, \code{"stand"}, \code{"double"},
#'   or \code{"split"}.
#' @export
decide_action <- function(strategy, game_state) {
  # Function-based strategy
  if (is.function(strategy)) {
    action <- strategy(game_state)
    return(.enforce_constraints(action, game_state))
  }

  pv   <- game_state$hand_value
  du   <- game_state$dealer_upcard
  soft <- isTRUE(game_state$soft_hand)

  # Exact lookup
  idx <- which(
    strategy$player_total == pv &
    strategy$dealer_card  == du &
    strategy$soft         == soft
  )

  if (length(idx) > 0L) {
    action <- strategy$action[[idx[[1L]]]]
    return(.enforce_constraints(action, game_state))
  }

  # Fallback: basic hard-total heuristic
  action <- if (pv >= 17L) "stand" else "hit"
  .enforce_constraints(action, game_state)
}

# Downgrade actions that are not allowed in the current state
.enforce_constraints <- function(action, game_state) {
  if (action == "double" && !isTRUE(game_state$can_double)) action <- "hit"
  if (action == "split"  && !isTRUE(game_state$can_split))  action <- "hit"
  action
}

# ---- Built-in strategy functions ------------------------------------------ #

#' Naive strategy: hit until hand value >= 17, then stand
#'
#' @param game_state A named list as produced by \code{make_game_state()}.
#' @return \code{"hit"} or \code{"stand"}.
#' @export
naive_strategy <- function(game_state) {
  if (game_state$hand_value < 17L) "hit" else "stand"
}

#' Aggressive strategy: always hit unless hand value >= 20
#'
#' @param game_state A named list as produced by \code{make_game_state()}.
#' @return \code{"hit"} or \code{"stand"}.
#' @export
aggressive_strategy <- function(game_state) {
  if (game_state$hand_value < 20L) "hit" else "stand"
}

#' Conservative strategy: stand on 13 or more
#'
#' @param game_state A named list as produced by \code{make_game_state()}.
#' @return \code{"hit"} or \code{"stand"}.
#' @export
conservative_strategy <- function(game_state) {
  if (game_state$hand_value < 13L) "hit" else "stand"
}

#' Random strategy: choose uniformly from valid actions
#'
#' @param game_state A named list as produced by \code{make_game_state()}.
#' @return A random valid action string.
#' @export
random_strategy <- function(game_state) {
  actions <- c("hit", "stand")
  if (isTRUE(game_state$can_double)) actions <- c(actions, "double")
  if (isTRUE(game_state$can_split))  actions <- c(actions, "split")
  sample(actions, size = 1L)
}

#' Load the built-in basic strategy from the bundled CSV
#'
#' @param strategies_dir Path to the \code{strategies/} directory.
#'   Defaults to the \code{strategies/} folder relative to this file.
#' @return A validated strategy \code{data.frame}.
#' @export
load_basic_strategy <- function(strategies_dir = NULL) {
  if (is.null(strategies_dir)) {
    # Resolve relative to the location of this source file
    this_file <- tryCatch(
      normalizePath(sys.frames()[[1L]]$ofile, mustWork = FALSE),
      error = function(e) NULL
    )
    if (!is.null(this_file)) {
      strategies_dir <- file.path(dirname(dirname(this_file)), "strategies")
    } else {
      strategies_dir <- "strategies"
    }
  }
  load_strategy(file.path(strategies_dir, "basic_strategy.csv"))
}

#' Return a named list of all built-in strategies
#'
#' @param strategies_dir Path to the \code{strategies/} directory.
#' @return A named list containing strategies: \code{random}, \code{naive},
#'   \code{basic}, \code{aggressive}, \code{conservative}.
#' @export
builtin_strategies <- function(strategies_dir = "strategies") {
  list(
    random       = random_strategy,
    naive        = naive_strategy,
    basic        = load_strategy(file.path(strategies_dir, "basic_strategy.csv")),
    aggressive   = aggressive_strategy,
    conservative = conservative_strategy
  )
}
