# Blackjack Strategy Simulator

A modular R framework for simulating blackjack games at scale and comparing
player strategies through Monte Carlo methods.

---

## Project Structure

```
blackjack/
├── R/
│   ├── blackjack_engine.R   # Core game engine (deck, dealing, rules)
│   ├── game_state.R         # Structured game-state representation
│   ├── strategy_parser.R    # Strategy loading, validation, action decisions
│   ├── simulation.R         # Monte Carlo simulation (sequential & parallel)
│   ├── analysis.R           # Summary statistics and strategy comparison
│   └── visualization.R      # ggplot2 plots
├── strategies/
│   ├── basic_strategy.csv       # Standard basic strategy table
│   ├── naive_strategy.csv       # Hit until 17
│   ├── aggressive_strategy.csv  # Hit until 20
│   ├── conservative_strategy.csv # Stand on 13+
│   └── random_strategy.R        # Random action strategy
├── scripts/
│   ├── run_simulation.R         # Single-strategy simulation example
│   └── compare_strategies.R     # Multi-strategy comparison example
├── tests/
│   ├── test_engine.R            # Engine unit tests
│   └── test_strategies.R        # Strategy unit tests
└── README.md
```

---

## Quick Start

```r
# Source the modules (from the project root)
source("R/blackjack_engine.R")
source("R/game_state.R")
source("R/strategy_parser.R")
source("R/simulation.R")
source("R/analysis.R")
source("R/visualization.R")

# Load basic strategy
basic <- load_strategy("strategies/basic_strategy.csv")

# Simulate 10 000 games
results <- simulate_games(basic, n_games = 10000, seed = 42)

# Summarise
summarise_results(results, "basic")

# Plot profit distribution (requires ggplot2)
plot_profit_distribution(results)
```

---

## Game Engine

Implements standard casino blackjack rules:

| Rule | Default |
|------|---------|
| Decks | 1 |
| Dealer stands on soft 17 | Yes |
| Blackjack payout | 3:2 (1.5) |
| Double down | Allowed |
| Split | Simplified (v1) |

### Key functions

```r
# Create a shoe
shoe <- create_shoe(n_decks = 1, seed = 42)

# Draw a card
result <- draw_card(shoe)
result$card   # card value
result$shoe   # updated shoe

# Compute hand value (handles soft aces)
hand_value(c(11, 7))    # => 18 (soft)
hand_value(c(11, 11))   # => 12

# Check hand properties
is_soft(c(11, 7))        # TRUE
is_bust(c(10, 10, 5))    # TRUE
is_blackjack(c(11, 10))  # TRUE

# Run a full game
game <- run_blackjack_game(
  strategy   = naive_strategy,
  rules      = default_rules(),
  shoe_state = shoe
)
game$profit   # net profit in bet units
game$outcome  # "win", "loss", "push", or "blackjack"
game$log      # list of game events
```

### Customising rules

```r
rules <- default_rules()
rules$n_decks              <- 6      # six-deck shoe
rules$dealer_stands_soft17 <- FALSE  # dealer hits soft 17
rules$blackjack_payout     <- 1.2    # 6:5 payout (worse for player)
```

---

## Game State

```r
# Build a state snapshot manually
state <- make_game_state(
  player_hand   = c(10, 6),
  dealer_upcard = 7,
  turn_number   = 1
)

# Or extract from a finished game
get_game_state(game)
```

The state list contains:

| Field | Type | Description |
|-------|------|-------------|
| `player_hand` | integer vector | Card values |
| `dealer_upcard` | integer | Dealer's visible card |
| `hand_value` | integer | Effective hand total |
| `soft_hand` | logical | Is an ace counted as 11? |
| `can_double` | logical | May the player double? |
| `can_split` | logical | May the player split? |
| `turn_number` | integer | Player turn index |

---

## Strategies

### Built-in strategies

| Name | Description | Function / CSV |
|------|-------------|----------------|
| `naive_strategy` | Hit until ≥ 17 | function |
| `aggressive_strategy` | Hit until ≥ 20 | function |
| `conservative_strategy` | Stand on ≥ 13 | function |
| `random_strategy` | Uniform random valid action | function |
| basic strategy | Optimal lookup table | `strategies/basic_strategy.csv` |

### Strategy table format

A strategy CSV (or data.frame) must have four columns:

| Column | Type | Description |
|--------|------|-------------|
| `player_total` | integer | Hand value (4–21) |
| `dealer_card` | integer | Dealer upcard (2–11; 11 = Ace) |
| `soft` | logical | TRUE for soft hands |
| `action` | character | `hit`, `stand`, `double`, or `split` |

### Loading strategies

```r
# From CSV
strat <- load_strategy("strategies/basic_strategy.csv")

# From data.frame
df <- data.frame(
  player_total = c(16, 12),
  dealer_card  = c(10,  4),
  soft         = c(FALSE, FALSE),
  action       = c("hit", "stand")
)
strat <- load_strategy(df)

# From a function
strat <- load_strategy(naive_strategy)
```

### Adding a new strategy

1. Create a CSV with the four required columns covering all
   `(player_total, dealer_card, soft)` combinations you want to specify.
   The engine falls back to "stand on 17+, hit otherwise" for unmatched rows.

2. Or write an R function:

   ```r
   my_strategy <- function(game_state) {
     if (game_state$hand_value < 16) return("hit")
     if (game_state$soft_hand)       return("stand")
     "stand"
   }
   ```

3. Pass it to `simulate_games()` or `compare_strategies()`.

---

## Simulation

### Single strategy

```r
results <- simulate_games(
  strategy   = naive_strategy,
  n_games    = 10000,
  rules      = default_rules(),
  n_workers  = 1,      # set > 1 for parallel (requires 'parallel' package)
  seed       = 42
)
```

Returned data.frame columns:

| Column | Description |
|--------|-------------|
| `game_id` | Sequential game index |
| `profit` | Net profit in bet units |
| `outcome` | "win", "loss", "push", "blackjack" |
| `blackjack` | Player had a natural blackjack |
| `bust` | Player busted |
| `turns` | Number of player action turns |

### Multiple strategies

```r
all_results <- simulate_all_strategies(
  strategies = list(naive = naive_strategy, basic = basic_strat),
  n_games    = 5000,
  seed       = 1
)
# all_results has an extra 'strategy' column
```

### Parallel execution

Set `n_workers > 1` to use `parallel::parLapply()`:

```r
results <- simulate_games(naive_strategy, n_games = 100000, n_workers = 4)
```

---

## Analysis

```r
# Summary statistics for one strategy
summarise_results(results, strategy_name = "naive")

# Compare multiple strategies
comp <- compare_strategies(
  strategies = list(naive = naive_strategy, basic = basic_strat),
  n_games    = 10000,
  seed       = 42
)

# Rank by expected return
rank_strategies(comp)

# Action frequency table
action_frequency(basic_strat, soft = FALSE)

# Profit quantiles
profit_quantiles(results)

# EV grid (player total × dealer upcard)
ev <- ev_grid(basic_strat, n_per_cell = 200, seed = 1)
```

---

## Visualisation

All plots require **ggplot2** (`install.packages("ggplot2")`).

```r
library(ggplot2)

# Profit histogram
plot_profit_distribution(results)

# Cumulative winnings
plot_cumulative_winnings(results)

# Strategy action heatmap (hard hands)
plot_strategy_heatmap(basic_strat, soft = FALSE)

# Strategy action heatmap (soft hands)
plot_strategy_heatmap(basic_strat, soft = TRUE)

# EV heatmap
ev <- ev_grid(basic_strat, seed = 1)
plot_ev_heatmap(ev)

# Strategy comparison bar chart
plot_strategy_comparison(comp)
```

---

## Running the Example Scripts

```bash
# Single-strategy simulation
Rscript scripts/run_simulation.R

# Multi-strategy comparison (all built-in strategies, 10 000 games each)
Rscript scripts/compare_strategies.R

# Override defaults via environment variables
N_GAMES=50000 N_WORKERS=4 SEED=99 Rscript scripts/compare_strategies.R
```

---

## Running Tests

```bash
Rscript tests/test_engine.R
Rscript tests/test_strategies.R
```

Or from an R session:

```r
source("tests/test_engine.R")
source("tests/test_strategies.R")
```

---

## Dependencies

| Package | Purpose | Required? |
|---------|---------|-----------|
| *(none)* | Core engine, strategies, simulation | No external deps |
| `ggplot2` | Visualisation | Optional |
| `parallel` | Parallel simulation (`n_workers > 1`) | Optional |
| `yaml` | Loading YAML strategy files | Optional |
| `jsonlite` | Loading JSON strategy files | Optional |

---

## Reproducibility

- Pass an integer `seed` to `simulate_games()` / `compare_strategies()` to
  get deterministic results.
- The `create_shoe()` function also accepts a `seed` argument.
- All rule parameters are centralised in `default_rules()`.

---

## Optional Extensions

- **Card counting**: extend `game_state.R` to track the running count and
  create a function strategy that adjusts bets and decisions accordingly.
- **Reinforcement learning**: record state-action-reward transitions from
  simulation and train with standard RL algorithms (e.g., Q-learning).
- **Shiny app**: wrap `simulate_games()` and the visualisation functions in
  a Shiny UI for interactive exploration.
- **House rules**: modify `default_rules()` (e.g., 6:5 payout, no doubling
  after split, multiple decks) and re-run any simulation.
- **Tidy export**: results are already tidy data.frames; use `write.csv()` or
  `arrow::write_parquet()` to export.
