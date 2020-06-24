wkdr = "/Users/briandolan/Discovix/svn/VGER/branches/gaps/analysis"
wksp = "gaps_v2_analysis.Rdata"

setwd(wkdr)
library(RPostgreSQL)

load(wksp)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(PostgreSQL(), host = '127.0.0.1', user = 'vger', password = 'v3ntr1s', dbname ='vger')

SQL = "
--SELECT * FROM ats.gap_returns WHERE symbol = '$DJ' ORDER BY symbol, dt;
SELECT * FROM ats.gap_returns ORDER BY symbol, dt;
"

d <- dbGetQuery(con, SQL)
d$symbol = as.factor(d$symbol)

ret_cols =  c(
  "cat_a_up_return"
, "cat_b_up_return"
, "cat_c_up_return"
, "cat_d_up_return"
, "cat_a_dwn_return"
, "cat_b_dwn_return"
, "cat_c_dwn_return"
, "cat_d_dwn_return"
)

ret_todate_cols = c(
  "cat_a_up_return_todate"
, "cat_b_up_return_todate"
, "cat_c_up_return_todate"
, "cat_d_up_return_todate"
, "cat_a_dwn_return_todate"
, "cat_b_dwn_return_todate"
, "cat_c_dwn_return_todate"
, "cat_d_dwn_return_todate"
)

sharpe_cols = c(
  "cat_a_up_sharpe"
, "cat_b_up_sharpe"
, "cat_c_up_sharpe"
, "cat_d_up_sharpe"
, "cat_a_dwn_sharpe"
, "cat_b_dwn_sharpe"
, "cat_c_dwn_sharpe"
, "cat_d_dwn_sharpe"
)



# Some plots
clrs = terrain.colors(length(ret_todate_cols))
sharpe_sdate = as.Date("2000-01-01", "%Y-%m-%d")

pdf("book.pdf", onefile=TRUE, width=17, height=11)
for (sy in levels(d$symbol)){
  tmp = d[(d$symbol == sy) & (d$dt >= sharpe_sdate),]
  par(new=FALSE)
  par(bty="n", col="grey"
   ,mfrow = c(2,2))
  plot(tmp$close ~ tmp$dt, type="l", col="black"
  , main = paste(sy, " Strategy Returns")
  , ylab="close"
  , xlab = "")
  par(new = TRUE)
  yl = range(tmp[,ret_todate_cols], na.rm=TRUE) 
  plot(tmp[,"symbol_return_todate"] ~ tmp[,"dt"], type="n"
  , ylab = ""
  , xlab = ""
  , yaxt="n"
  , xaxt="n"
  , ylim = ifelse(yl != c(Inf, -Inf), yl, c(-1,1))
  )  
  axis(side=4, col="grey", labels = TRUE)
  for (i in 1:length(ret_todate_cols)) {
    cl = ret_todate_cols[i]
    points(tmp[,"dt"], tmp[,cl], type="l", col=clrs[i])
  }
 
  par(new = FALSE)
  plot(tmp$close ~ tmp$dt, type="l", col="black"
  , main = paste(sy, " Strategy Sharpe")
  , ylab="close"
  , xlab = "")
  par(new = TRUE)
  yl =  range(tmp[,sharpe_cols], na.rm=TRUE)
  plot(tmp[,"symbol_sharpe_todate"] ~ tmp[,"dt"], type="n"
  , ylab = ""
  , xlab = ""
  , yaxt="n"
  , xaxt="n"
  , ylim = ifelse(yl != c(Inf, -Inf), yl, c(-1,1))
  )  
  axis(side=4, col="grey", labels = TRUE)
  for (i in 1:length(sharpe_cols)) {
    cl = sharpe_cols[i]
    points(tmp[,"dt"], tmp[,cl], type="l", col=clrs[i])
  }

  par(new = FALSE) 
  plot(tmp$close ~ tmp$dt, type="l", col="black"
  , main = paste(sy, "Total Returns", sep=" ")
  , ylab="close"
  , xlab = "")
  par(new=TRUE)
  yl =  range(tmp$symbol_return_todate, na.rm=TRUE)
  plot(tmp$symbol_return_todate ~ tmp$dt, type="l"
  , yaxt="n"
  , ylab = ""
  , xlab = ""
  , xaxt = "n"
  , yaxt = "n"
  , ylim = ifelse(yl != c(Inf, -Inf), yl, c(-1,1))
  , col="grey")
  axis(side=4, col="grey", labels=TRUE)

  par(new=FALSE)
  plot(tmp$close ~ tmp$dt, type="l", col="black"
  , main = paste(sy, "Total Sharpe", sep=" ")
  , ylab="close"
  , xlab = "")
  par(new=TRUE)
  yl =  range(tmp$symbol_sharpe_todate, na.rm=TRUE)
  plot(tmp$symbol_sharpe_todate ~ tmp$dt, type="l"
  , yaxt="n"
  , ylab = ""
  , xlab = ""
  , xaxt = "n"
  , yaxt = "n"
  , ylim = ifelse(yl != c(Inf, -Inf), yl, c(-1,1))
  , col="grey")
  axis(side=4, col="grey", labels=TRUE)
}
dev.off()


tmp = d[d$dt >= as.Date('2001-01-01', '%Y-%m-%d'),]
tmpA = tmp[(tmp$gap_category == "A"), ]

par(bty="n", col="grey")
plot(tmp$close ~ tmp$dt, type="l", col="black"
  , main = '$NK'
  , ylab="close"
  , xlab = ""
  , yaxt="n")
par(new = TRUE)
plot(tmp[,"symbol_return_todate"] ~ tmp[,"dt"], type="n"
  , ylab = ""
  , xlab = ""
  , xaxt="n"
  , ylim = range(tmp[,ret_todate_cols])
)  
axis(side=4, col="grey", labels = "Strategy Returns To Date")
for (cl in ret_todate_cols) {
  points(tmp[,"dt"], tmp[,cl], type="l", col="grey")
}


par(mfrow = c(2,1)
  , bty="n", col="grey", fg="grey"
)
plot(tmp$gap ~ tmp$dt, type="l", col="grey")
points(tmpA$dt, tmpA$gap, col="red")

plot(tmp$ret ~ tmp$dt, type="l", col="grey")
points(tmpA$dt, tmpA$ret, col="red")


#######################
  WHOLE PORTFOLIO
#######################

SQL = quote("
SELECT
  dt
, SUM(portfolio_return) OVER todate_wdw AS return_todate
, AVG(portfolio_return) OVER todate_wdw AS return_avg_todate
, VARIANCE(portfolio_return) OVER todate_wdw AS return_var_todate
, CASE WHEN VARIANCE(portfolio_return) OVER todate_wdw > 0
    THEN (AVG(portfolio_return) OVER todate_wdw - 0.002) /  VARIANCE(portfolio_return) OVER todate_wdw
    ELSE NULL
    END AS sharpe 
FROM
  (
  SELECT
    dt
  , SUM(symbol_return) AS portfolio_return
  FROM
    ats.gap_returns
  GROUP BY dt
  ) AS a
WINDOW todate_wdw AS (ORDER BY dt ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
ORDER BY dt
;
")

pf <- dbGetQuery(con, SQL)

tmp = pf[pf$dt >= sharpe_sdate,] 
pdf("portfolio.pdf", onefile=TRUE, width=17, height=11)
par(bty="n", col="grey")
plot(tmp$sharpe ~ tmp$dt, type="l"
  , xlab=""
  , ylab = "Sharpe Ratio"
  , main = "Portfolio Performance")
abline (h=2, lty=2, col="grey")
par(new=TRUE)
plot(tmp$return_todate ~ tmp$dt
  , type="l"
  , xlab=""
  , xaxt = "n"
  , ylab=""
  , yaxt="n"
  , col = "darkgreen"
)
axis(side=4, col="darkgreen", labels=TRUE )
dev.off()

########################################
 Buy & Hold Portfolio v TYSON Portfolio
#######################################
SQL = quote("
SELECT
  a.dt AS dt
, SUM(portfolio_return) OVER todate_wdw AS tyson_portfolio_return
, SUM(portfolio_ret) OVER todate_wdw AS buyhold_portfolio_return
FROM
  (
  SELECT
    dt
  , SUM(symbol_return) / COUNT(symbol_return) AS portfolio_return
  FROM
    ats.gap_returns
  GROUP BY dt
  ) AS a,
  (
  SELECT
    dt
  , portfolio_ret
  FROM
    norgate_sum_returns
  ) AS b
  WHERE a.dt = b.dt
WINDOW todate_wdw AS (ORDER BY a.dt ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
ORDER BY a.dt
;
")

pf <- dbGetQuery(con, SQL)

tmp = pf[pf$dt >= sharpe_sdate,] 
pdf("tyson_portfolio.pdf", onefile=TRUE, width=17, height=11)
par(bty="n", col="grey")
plot(tmp$buyhold_portfolio_return ~ tmp$dt, type="l"
  , xlab=""
  , ylab = "Cumulative Buy & Hold Returns"
  , main = "Portfolio Performance")
abline (h=2, lty=2, col="grey")
par(new=TRUE)
plot(tmp$tyson_portfolio_return ~ tmp$dt
  , type="l"
  , xlab=""
  , xaxt = "n"
  , ylab=""
  , yaxt="n"
  , col = "darkgreen"
)
axis(side=4, col="darkgreen", labels=TRUE )
dev.off()



