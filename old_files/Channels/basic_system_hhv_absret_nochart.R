
basicSysAbsRetHHV <- function(ticker, x, window, hold.period, absret.lag1, absret.lag2, verbose = TRUE) {
  # Basic trading system
  # Goes long when the closing price exceeds the highest high of
  # determined weeks. Includes lag variables for rolling absolute 
  # return moving average.
  #
  # Hard coded 5 period moving average for absolute returns. 
  # 
  # Args:
  #   ticker: ticker of security
  #   x: data in xts format. variable must be 'x'.
  #   window: Number of periods to determine the highest high.
  #   hold.period: Holding period.
  #   lag1: lag of absolute return. lag = 0 is current record
  #   lag2: lag to compare lag1 to
  # 
  # Returns:
  #   pnl 
  
  x$ret <- Delt(Cl(x))            # Create return vector
  
  # Create highest high vector
  x.hhv <- rollapply(Cl(x), window , max, align = "right")
  x <- merge(x, x.hhv)
  colnames(x)[7] <- "hhv"
  
  x$absret <- abs(x$ret)
  absret.ma <- rollapply(na.omit(x$absret), 5, mean, align = "right")
  x <- merge(x, absret.ma)

  ##////////////////////////////////////
  ##  Back test begins here
  ##
  head(x)
  # Create long signal
  x$signal <- NA    # initialize signals with NA 
  ind <- which(Cl(x) > Lag(x$hhv) & Lag(x$absret.1, absret.lag1) < Lag(x$absret.1, absret.lag2))
  x$signal[ind] <- 1
  
  # Isolate signals
  signal.lag <- Lag(x$signal)
  signal.lag <- x$signal - Lag(x$signal)
  ind <- which(signal.lag == 0)
  x$signal[ind] <- NA
  
  # Lagged vector to calculate PNL 
  # Returns are for the weeks following the week of the signal
  x$pnl <- Lag(x$signal)
  
  # Determine signals for hold periods
  x$sell <- Lag(x$pnl, hold.period)
  ind <- which(x$sell == 1)
  x$pnl[ind] <- 0
  x$pnl <- na.locf(x$pnl)

  ##
  ##  Back test ends here
  ##//////////////////////////////////////
  
  return(x)
}


