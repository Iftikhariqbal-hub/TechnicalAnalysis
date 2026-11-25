# ==============================================================
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary)
# Author : Iftikhar Iqbal
# Files  : portfolio.txt (one stock symbol per line) in the same folder
# ==============================================================

# ---- 1. Packages ---------------------------------------------
required_packages <- c("quantmod", "TTR")
for (p in required_packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(quantmod)
library(TTR)

# ---- 2. Import: read portfolio.txt and load each stock --------
# Returns a named list of data frames (one per symbol).
# Also creates a separate data frame per symbol in the global environment.
load_stock_data <- function(file = "portfolio.txt",
                            from = "2023-01-01",
                            to = Sys.Date()) {
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[symbols != ""]

  stocks <- list()
  for (sym in symbols) {
    xt <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE),
      error = function(e) {
        message("Could not load ", sym, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(xt)) next

    df <- data.frame(Date = index(xt), coredata(xt))
    colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    stocks[[sym]] <- df
    assign(make.names(sym), df, envir = .GlobalEnv)
  }
  stocks
}

# ---- 3. Statistics -------------------------------------------
# Base R has no statistical mode, so define one.
get_mode <- function(x) {
  x <- round(x, 2)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Takes one stock data frame; returns summary stats + moving averages.
calculate_statistics <- function(df, ma_window = 20) {
  close <- df$Close
  list(
    summary = data.frame(
      Mean    = mean(close, na.rm = TRUE),
      Median  = median(close, na.rm = TRUE),
      Mode    = get_mode(close),
      Std_Dev = sd(close, na.rm = TRUE),
      Min     = min(close, na.rm = TRUE),
      Max     = max(close, na.rm = TRUE)
    ),
    moving_average = data.frame(
      Date  = df$Date,
      Close = close,
      SMA20 = as.numeric(SMA(close, n = ma_window)),
      SMA50 = as.numeric(SMA(close, n = 50))
    )
  )
}

# Runs calculate_statistics() on every stock.
calculate_all_statistics <- function(stocks) {
  lapply(stocks, calculate_statistics)
}

# ---- 4. Display utilities ------------------------------------
display_stock_data <- function(stocks, n = 6) {
  for (sym in names(stocks)) {
    cat("\n====", sym, "- first", n, "and last", n, "rows ====\n")
    print(head(stocks[[sym]], n))
    print(tail(stocks[[sym]], n))
    cat("Rows:", nrow(stocks[[sym]]), "\n")
  }
}

display_statistics <- function(stats) {
  table <- do.call(rbind, lapply(names(stats), function(sym) {
    cbind(Symbol = sym, round(stats[[sym]]$summary, 2))
  }))
  rownames(table) <- NULL
  cat("\n==== Summary statistics (Close price) ====\n")
  print(table)
  invisible(table)
}

plot_stock <- function(sym, stats) {
  ma <- stats[[sym]]$moving_average
  mu <- stats[[sym]]$summary$Mean
  plot(ma$Date, ma$Close, type = "l", col = "grey40",
       main = paste(sym, "- Close Price with Moving Averages"),
       xlab = "Date", ylab = "Price (USD)")
  lines(ma$Date, ma$SMA20, col = "blue", lwd = 2)
  lines(ma$Date, ma$SMA50, col = "red", lwd = 2)
  abline(h = mu, col = "darkgreen", lty = 2)
  legend("topleft", bty = "n",
         legend = c("Close", "SMA 20", "SMA 50", "Mean"),
         col = c("grey40", "blue", "red", "darkgreen"),
         lty = c(1, 1, 1, 2), lwd = c(1, 2, 2, 1))
}

plot_all_stocks <- function(stats) {
  for (sym in names(stats)) plot_stock(sym, stats)
}

# Yahoo-style candlestick chart using quantmod
plot_candlestick <- function(sym, from = "2023-01-01") {
  xt <- getSymbols(sym, src = "yahoo", from = from, auto.assign = FALSE)
  chartSeries(xt, name = sym, theme = chartTheme("white"),
              TA = "addSMA(20);addSMA(50);addVo()")
}

# ---- 5. Run --------------------------------------------------
stocks <- load_stock_data("portfolio.txt")
stats  <- calculate_all_statistics(stocks)

display_stock_data(stocks)
display_statistics(stats)
plot_all_stocks(stats)
# plot_candlestick(names(stocks)[1])   # optional extra
