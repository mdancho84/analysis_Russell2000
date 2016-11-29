
# Prerequisites ----------------------------------------------------------------
library(rvest)
library(quantmod)
library(tidyverse)
library(stringr)
library(lubridate)
library(modelr)
library(plotly)


# Web Scraping: Get the List of Russell 2000 Stocks ----------------------------

# Base path and page rows from www.marketvolume.com
base_path <- "http://www.marketvolume.com/indexes_exchanges/r2000_components.asp?s=RUT&row="
row_num <- seq(from = 0, by = 250, length.out = 9)

# Function to web scrape stocks from individual pages
get_stocklist <- function(base_path, row_num) {
    path <- paste0(base_path, row_num)
    # Get raw table
    stock_table <- read_html(path) %>%
        html_node("table") %>%
        html_table() 
    # Format table
    stock_table <- stock_table[-1, 1:2] %>%
        as_tibble() %>%
        rename(symbol = X1, company = X2)
    stock_table
}

# Map get_stocklist() function, unnest and tidy
stocklist <- tibble(row_num) %>%
    mutate(
        stock_table = map(row_num, 
                          function(.x) get_stocklist(base_path = base_path,
                                                     row_num = .x)
                          )
           ) %>%
    unnest() %>%
    select(-row_num) %>%
    mutate_all(function(x) str_trim(x, side = 'both') %>% str_to_upper()) %>%
    distinct()


# Function Mapping with quantmod and purrr -------------------------------------

# Create functions to get stock prices and log returns
get_stock_prices <- function(ticker, return_format = "tibble", ...) {
    # Get stock prices
    stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
    # Rename
    names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    # Return in xts format if tibble is not specified
    if (return_format == "tibble") {
        stock_prices <- stock_prices_xts %>%
            as_tibble() %>%
            rownames_to_column(var = "Date") %>%
            mutate(Date = ymd(Date))
    } else {
        stock_prices <- stock_prices_xts
    }
    
    stock_prices
}

get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
    # Convert tibble to xts
    if (!is.xts(x)) {
        x <- xts(x[,-1], order.by = x$Date)
    }
    # Get stock prices
    log_returns_xts <- periodReturn(x = x$Adjusted, type = 'log', period = period, ...)
    # Rename
    names(log_returns_xts) <- "Log.Returns"
    # Return in xts format if tibble is not specified
    if (return_format == "tibble") {
        log_returns <- log_returns_xts %>%
            as_tibble() %>%
            rownames_to_column(var = "Date") %>%
            mutate(Date = ymd(Date))
    } else {
        log_returns <- log_returns_xts
    }
    log_returns
}

# Use purrr to map the functions across the entire list of Russell 2000 stocks
# Warning: This takes about 15 minutes 
# Start the clock!
ptm <- proc.time()
from <- "2007-01-01"
to   <- today()
stocklist <- stocklist %>%
    mutate(
        stock.prices = map(symbol,
                           function(.x) tryCatch({
                               get_stock_prices(.x,
                                                return_format = "tibble",
                                                from = from,
                                                to   = to)
                           }, error = function(e) {
                               NA
                           })
        ),
        len = map_int(stock.prices, length)
    ) %>%
    filter(len > 1) %>%
    select(-len) %>%
    mutate(
        log.returns  = map(stock.prices,
                           function(.x) get_log_returns(.x, return_format = "tibble")),
        mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
        sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
        n.trade.days = map_dbl(stock.prices, nrow)
    )
# Stop the clock
proc.time() - ptm

# user  system elapsed 
# 524.18   12.36  790.16



# Visualize the Relationship between Std Dev and Mean --------------------------
trade_day_thresh <- 1500
sd_limit <- 0.075
stocklist %>%
    filter(n.trade.days >= trade_day_thresh,
           sd.log.returns <= sd_limit) %>%
    ggplot(aes(x = sd.log.returns, y = mean.log.returns)) +
    geom_point(alpha = 0.1) +
    geom_smooth() +
    labs(title = "Market Tends to Penalize Stocks with Large Std Dev",
         subtitle = "Best to Focus on Stocks with Highest Reward to Risk Ratio",
         x = "Standard Deviation of Log Returns",
         y = "Mean of Log Returns")


# Develop Reward-to-Risk Ratio -------------------------------------------------
stocklist <- stocklist %>%
    mutate(reward.ratio = (mean.log.returns * n.trade.days) / sd.log.returns)


# Visualize the Results with Plotly --------------------------------------------
trade_day_thresh <- 1500
lab <- "Russell 2000"
back_col <- '#2C3E50'
font_col <- '#FFFFFF'
line_col <- "#FFFFFF"
grid_col <- 'rgb(255, 255, 255)'
col_brew_pal <- 'BrBG'
plot_ly(data   = stocklist %>% filter(n.trade.days >= trade_day_thresh),
        type   = "scatter",
        mode   = "markers",
        x      = ~ sd.log.returns,
        y      = ~ mean.log.returns,
        color  = ~ reward.ratio,
        colors = col_brew_pal,
        size   = ~ reward.ratio,
        text   = ~ str_c("<em>", company, "</em><br>",
                         "Ticker: ", symbol, "<br>",
                         "No. of Trading Days: ", n.trade.days, "<br>",
                         "Reward to Risk: ", round(reward.ratio, 1)),
        marker = list(opacity = 0.8,
                      symbol = 'circle',
                      sizemode = 'diameter',
                      sizeref = 4.0,
                      line = list(width = 2, color = line_col))
) %>%
    layout(title   = str_c(lab, 'Analysis: Stock Risk vs Reward', sep = " "),
           xaxis   = list(title = 'Risk: StDev Log Returns',
                          gridcolor = grid_col,
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
           yaxis   = list(title = 'Reward: Mean Log Returns',
                          gridcolor = grid_col,
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 2),
           margin = list(l = 100,
                         t = 100,
                         b = 100),
           font   = list(color = font_col),
           paper_bgcolor = back_col,
           plot_bgcolor = back_col)



# Trim List to Top 6 High Performing -------------------------------------------

# Filter high performing stocks
top_n_limit <- 15
hp <- stocklist %>%
    mutate(rank = reward.ratio %>% desc() %>% min_rank()) %>%
    filter(rank <= top_n_limit) %>%
    arrange(rank) %>%
    select(symbol, rank, reward.ratio, mean.log.returns, sd.log.returns, n.trade.days, log.returns)
hp

# Unnest high performing stocks
hp_unnest <- hp %>%
    select(symbol, log.returns) %>%
    unnest()
hp_unnest

# Get mean log returns by year
hp_means_by_year <- hp_unnest %>%
    mutate(year = year(Date)) %>%
    group_by(symbol, year) %>%
    summarize(mean.log.returns = mean(Log.Returns))
hp_means_by_year

# Visualize using ggplot
hp_means_by_year %>%
    ggplot(aes(x = year, y = mean.log.returns)) +
    geom_ref_line(h = 0) +
    geom_line(aes(col = symbol)) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ symbol, nrow = 3) +
    theme(legend.position = "None", axis.text.x = element_text(angle=90)) +
    labs(title = "Best Prospects Have Consistent, Above-Zero MDLR",
         subtitle = "Stocks to Watch: CMN, EXPO, HIFS, LANC, NATH, PLUS",
         x = "Year",
         y = "Mean Daily Log Returns")


# Review Stocks to Watch -------------------------------------------------------

top_stocks <- c("CMN", "EXPO", "HIFS", "LANC", "NATH", "PLUS")

stocklist %>%
    filter(symbol %in% top_stocks) %>%
    select(symbol, stock.prices) %>%
    unnest() %>%
    ggplot(aes(x = Date, y = Adjusted, col = symbol)) +
    geom_line() +
    facet_wrap(~ symbol, nrow = 3, scales = "free_y") +
    theme(legend.position = "None") +
    labs(title = "Prospecting Best Russell 2000 Stocks",
         subtitle = "Six Small Caps with Amazing Growth, Lowest Risk",
         x = "Year",
         y = "Price per Share")




