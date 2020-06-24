symbol <- getNorgate('EC', 'xts')
x <- to.weekly(symbol)
window = 5
hold.period = 2

x$ret <- Delt(Cl(x))            # Create return vector
  
# Create highest high vector
x.hhv <- rollapply(Cl(x), window , max, align = "right")
x <- merge(x, x.hhv)
colnames(x)[7] <- "hhv"
  
# Create long signal
x$signal <- 0    # initialize signals with NA 
ind <- which(Cl(x) > Lag(x$hhv))
x$signal[ind] <- 1

x$factor = 0

# Trading algorithm
for (i in 1:(nrow(x)-hold.period)) {
	if (x$signal[i] == 1) {      # This is week is highest high
		for (j in 1:hold.period) { # Create factor for next hold.period periods
  		x$factor[i+j] = 1
  	}  
	}
}   

x$pnl <- x$factor * x$ret

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






