

HHV <- function(ticker, x, window, hold.period, verbose = TRUE) {
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
  #   pnl con <- dbConnect(PostgreSQL(), host = '192.168.1.111', user = 'wchoi', password = 'medulla624', dbname ='wchoi')
  
  x$ret <- Delt(x$Close)            # Create return vector
  
  # Create highest high vector
  x.hhv <- rollapply(x$Close, window , max, align = "right")
  x <- merge(x, x.hhv)
  colnames(x)[7] <- "hhv"
 

  ##////////////////////////////////////
  ##  Back test begins here
  ##
  head(x)
  # Create long signal
  x$signal <- 0    # initialize signals with NA 
  ind <- which(x$Close > Lag(x$hhv))
  x$signal[ind] <- 1
  
  x$position <- 0
  
	# Trading algorithm
	for (i in 1:(nrow(x)-hold.period)) {
		if (x$signal[i] == 1) {      # This is week is highest high
			for (j in 1:hold.period) { # Create factor for next hold.period periods
	  		x$position[i+j] = 1
	  	}  
		}
	}
	
	x$pnl <- x$position * x$ret
  
  ##
  ##  Back test ends here
  ##//////////////////////////////////////
  
  return(x)
}

  ticker <- 'EC'
  trade.window <- 25
  hold.period <- 1
  
  con <- dbConnect(PostgreSQL(), host = '192.168.1.111', user = 'wchoi', password = 'medulla624', dbname ='wchoi')
  
  SQL <- "SELECT * FROM norgate_data WHERE symbol = 'EC'"
  x <- dbGetQuery(con, SQL)
  x <- xts(x[,3:7], x$dt)
  colnames(x) <- c('Open', 'High', 'Low', 'Close', 'Volume')
  x <- x['1999::2010']  
png('ec_25_1.png')
z <- HHV(ticker, x, trade.window, hold.period)
plot(cumsum(na.omit(z$pnl)))
dev.off()
