# Initialize database
con <- dbConnect(PostgreSQL(), host = '192.168.1.111', user = 'wchoi', password = 'medulla624', dbname ='wchoi')

SQL = "SELECT * FROM norgate_data WHERE symbol = 'ES'"
x <- dbGetQuery(con, SQL)

# Convert to xts
x <- xts(x[,3:7], as.Date(x[,2]))
colnames(x) <- c('Open', 'High', 'Low', 'Close', 'Volume')

# Initialize variables
window = 10
hold.period = 2
absret.lag1 = 1
absret.lag2 = 2

x$ret <- Delt(Cl(x))            # Create return vector
  
# Create highest high vector
x.llv <- rollapply(Cl(x), window , min, align = "right")
x <- merge(x, x.llv)
colnames(x)[7] <- "llv"

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
  ind <- which(Cl(x) < Lag(x$llv) & Lag(x$absret.ma, absret.lag1) < Lag(x$absret.ma, absret.lag2))
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



