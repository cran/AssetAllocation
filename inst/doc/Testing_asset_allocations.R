## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=4) 

## ----message=FALSE, warning=FALSE---------------------------------------------
library(AssetAllocation)
library(PerformanceAnalytics)
names(basic_asset_alloc)

## -----------------------------------------------------------------------------
basic_asset_alloc$all_weather

## -----------------------------------------------------------------------------
## Example 1: backtesting one of the asset allocations in the package
strat_permanent <- basic_asset_alloc$permanent

# test using the data set provided in the package
bt_strat_permanent <- backtest_allocation(strat_permanent, ETFs_daily)

# plot cumulative returns
chart.CumReturns(bt_strat_permanent$returns,
                 main = paste0("Cumulative returns of the ",
                               bt_strat_permanent$strat$name,
                               " portfolio"),
                 ylab = "Cumulative returns"
)

# table with performance metrics
bt_strat_permanent$table_performance

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------


## -----------------------------------------------------------------------------
factor_strat  <- list(name = "EW Factors",
                      tickers = c("MTUM", "VLUE", "USMV", "QUAL"),
                      default_weights = c(0.25, 0.25, 0.25, 0.25),
                      rebalance_frequency = "month",
                      portfolio_rule_fn = "identity")

## -----------------------------------------------------------------------------
returns_ETFs <- get_return_data_from_tickers(factor_strat$tickers,
                                             starting_date = "2013-08-01")

## -----------------------------------------------------------------------------
# backtest the strategy
bt_factor_strat <- backtest_allocation(factor_strat,
                                       returns_ETFs)

# plot returns
library(PerformanceAnalytics)
chart.CumReturns(bt_factor_strat$returns,
                 main = paste0("Cumulative returns of the ",
                               bt_factor_strat$strat$name,
                               " portfolio"),
                 ylab = "Cumulative returns"
)

# table with performance metrics
bt_factor_strat$table_performance

