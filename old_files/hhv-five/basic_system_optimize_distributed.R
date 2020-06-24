############################################
#
# Optimization to run HHV combinations
# and return PDF files for every file in 
# the directory
#
############################################


# Modify variables here

trade.window <- 10
hold.period <- 10

############################################

library(quantmod)
library(RPostgreSQL)

# Delete existing files
if(file.exists('sharpe_ratios.csv') == TRUE) {
print('Removing sharpe_ratios.csv')
file.remove('sharpe_ratios.csv')}

if(file.exists('total_pnl.csv') == TRUE) {
  print('Removing total_pnl.csv')
  file.remove('total_pnl.csv')
}

source('hhv.R')
#source('basic_system_llv_absret_nochart.R')

# Initialize database
con <- dbConnect(PostgreSQL(), host = '192.168.1.111', user = 'wchoi', password = 'medulla624', dbname ='wchoi')

# Get symbols from database
s <-  c('EC', 'ES', 'GC', 'SF', 'TY2')

# Analyze all files in directory
for (symbol in s) { 
  # Initial
  ticker = symbol
  # Message
  print (paste('Retrieving data for:', ticker, sep = ""))

  # Get symbol data
  SQL <- paste('SELECT * FROM norgate_data WHERE symbol = \'', ticker,'\';', sep = "")
  x <- dbGetQuery(con, SQL)
  x <- xts(x[,3:7], x$dt)
  colnames(x) <- c('Open', 'High', 'Low', 'Close', 'Volume')
  x <- x['1999::2010']  
    
  # Run basic system HHV
    for (i in 1:trade.window) {
      for (j in 1:hold.period) {
        # Run basic system Long
        print(paste('Running HHV Analysis for:',i, j))
        strategy.pnl<- HHV(ticker, x, (i * 5), j)
        
        # Write pnl of strategy 
        strategy.pnl.df <- data.frame(na.omit(strategy.pnl))
        model_id <- paste(ticker, 'hh', i, 'h', j, sep = '')
        strategy.pnl.df$mid <- model_id              
        strategy.pnl.df$ticker <- ticker
        strategy.pnl.df$trade.window <- i*5
        strategy.pnl.df$hold.period <- j
        strategy.pnl.df$strategy <- 'HHV'
        print(paste('Writing total HHV pnl for:',ticker, i, j))
        write.table(strategy.pnl.df, 'total_pnl.csv', sep = ',', 
                    quote = FALSE, col.names = FALSE, append = TRUE)
        
        # Calculate sharpe ratio & write to file
        ind <- which(strategy.pnl.df$pnl != 0)
        ret <- strategy.pnl.df$pnl[ind]
        ind <- which(ret > 0) 
        win <- length(ret[ind])     
        loss <- length(ret) - win
        mu <- mean(ret)
        sigma <- sd(ret)
        sharpe <- (mu - (.05/50)) / sigma
        kelly <- mu / (sigma^2)
        risk.ret <- kelly + (10 * sum(ret))
        trade.count <- length(ret) / hold.period
        print(sharpe)
        sharpe.record <- paste(model_id, mu, sigma, sharpe, kelly, risk.ret, sum(ret), ticker, i, j, trade.count, win, loss, win/loss, sep = ',')
        print(paste('Writing sharpe ratio:', sharpe.record))
        write.table(sharpe.record, 'sharpe_ratios.csv', append = TRUE, 
                    quote = FALSE, col.names = FALSE, row.names=FALSE)         
        }
   }
  
  # Complete message
  print(paste('Final symbol completed at', Sys.time()))
}

