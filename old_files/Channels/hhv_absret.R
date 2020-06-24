symbol <- getNorgate('ES', 'xts')
x <- to.weekly(symbol)
window = 5
hold.period = 2
absret.lag1 = 1
absret.lag2 = 2

x$ret <- Delt(Cl(x))            # Create return vector
  
# Create highest high vector
x.hhv <- rollapply(Cl(x), window , max, align = "right")
x <- merge(x, x.hhv)
colnames(x)[7] <- "hhv"

x$absret <- abs(x$ret)
absret.ma <- rollapply(na.omit(x$absret), 5, mean, align = "right")
x <- merge(x, absret.ma)
colnames(x)[ncol(x)] <- 'absret.ma'
  
  ##////////////////////////////////////
  ##  Back test begins here
  ##
  head(x)
  # Create long signal
  x$signal <- 0    # initialize signals with NA 
  ind <- which(Cl(x) > Lag(x$hhv)& Lag(x$absret.ma, absret.lag1) > Lag(x$absret.ma, absret.lag2))
  x$signal[ind] <- 1
  
	x$factor = 0
    
  # Trading algorithm
	for (i in 1:(nrow(x)-hold.period)) {
	  if (x$signal[i] == 1) {      # This is week is highest high
	  	for (j in 1:hold.period) {
	    	x$factor[i+j] = 1
	    }  
	  }
	}   
  
  x$pnl <- x$factor * x$ret

  ##
  ##  Back test ends here
  ##//////////////////////////////////////
  
  plot(cumsum(na.omit(x$pnl)))



#Tally
ind <- which(x$pnl != 0)
ret <- x$pnl[ind]
m <- mean(ret)
s <- sd(ret)
kelly <- m/s^2

print(m)
print(s)
print(kelly)



