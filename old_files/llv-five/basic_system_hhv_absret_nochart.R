
basicSysAbsRetHHV <- function(ticker, x, window, hold.period, verbose = TRUE) {
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
  ind <- which(Cl(x) > Lag(x$hhv))
  x$signal[ind] <- 1
  
  x$position <- 0
  
	# Trading algorithm
	for (i in 1:(nrow(x)-hold.period)) {
		if (x$signal[i] == 1) {      # This is week is highest high
			for (j in 1:hold.period) { # Create factor for next hold.period periods
	  		x$factor[i+j] = -1
	  	}  
		}
	}
	
	x$pnl <- x$position * x$ret
  
  ##
  ##  Back test ends here
  ##//////////////////////////////////////
  
  return(x)
}


