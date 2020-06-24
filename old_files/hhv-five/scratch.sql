DROP TABLE IF EXISTS  hhv_kelly;
CREATE TABLE hhv_kelly AS
SELECT 
    mid
  , symbol 
  , lag_wdw
  , hold_period
  , dt
  , close
  , hhv
  , ret
  , signal
  , pnl
  , CASE
    WHEN variance(pnl) OVER (PARTITION BY mid ORDER BY dt) > 0
      THEN (AVG(pnl) OVER (PARTITION BY mid ORDER BY dt)) / (variance(pnl) OVER (PARTITION BY mid ORDER BY dt)) 
    ELSE NULL
    END AS kelly_todate
  , WHEN pnl > 0 
      THEN AVG(pnl) / VARIANCE(PNL) OVER (PARTITION BY symbol ORDER BY dt ROWS BETWEEN 100 PRECEDING AND CURRENT ROW)
    ELSE NULL
    END AS kelly_roll  
FROM hhv_pnl
;

DROP TABLE IF EXISTS hhv_metrics;
CREATE TABLE hhv_metrics AS 
SELECT
    mid
  , AVG(pnl) AS mu
  , STDDEV(pnl) AS sigma
  , AVG(pnl) / VARIANCE(pnl) AS kelly
  , SUM(pnl) AS total_ret
  , AVG(kelly_todate) AS avg_kellyroll
  , STDDEV(kelly_todate) AS std_kellyroll
  , MIN(kelly_todate) AS min_kellyroll
  , MAX(kelly_todate) AS max_kellyroll
FROM
  (SELECT 
      mid
    , symbol
    , dt
    , pnl
    , kelly_todate
   FROM 
    hhv_kelly
   ) AS a
WHERE pnl <> 0 
GROUP BY mid;


DROP TABLE IF EXISTS hhv_kelly_summary;
CREATE TABLE hhv_kelly_summary AS
SELECT
    A.mid
  , AVG(A.kelly_todate) AS avg_kelly
  , STDDEV(A.kelly_todate) AS std_kelly
  , MIN(A.kelly_todate) AS min_kelly
  , MAX(A.kelly_todate) AS max_kelly
  , B.kelly
FROM hhv_kelly AS A, hhv_metrics AS B
WHERE A.mid = B.model_id
GROUP BY A.mid, B.kelly
;


library(RPostgreSQL)
library(quantmod)
library(ggplot2)
# Initialize database
con <- dbConnect(PostgreSQL(), host = 'localhost', user = 'postgres', password = 'medulla624', dbname ='wchoi')

# Plot rolling kelly
SQL <- "SELECT * FROM hhv_kelly WHERE symbol = 'EC' AND dt > '1-1-2001' ORDER BY mid, dt"
kellyRoll <- dbGetQuery(con, SQL)
max_dt <- max(kellyRoll$dt)
p <- ggplot(kellyRoll, aes(x=dt, y=kelly_todate, group = mid, colour = mid, label = mid))
p1 = p + geom_line() + geom_text(data = subset(kellyRoll, dt == max_dt), aes( x = dt + 0.5), size = 3, hjust = .25)
p1 + theme_bw() + opts(legend.position = "none",
       panel.border = theme_blank(), axis.ticks = theme_blank(), title = 'EC Kelly Roll')


# Plot kelly to average Kelly
SQL <- "SELECT * FROM hhv_kelly_summary WHERE mid LIKE 'EC%'"
kelly_summary <- dbGetQuery(con, SQL)  
#png('kelly_avgkelly.png')
p <- ggplot(kelly_summary, aes(kelly, avg_kelly, label = mid, colour = mid))
p + geom_text(size = 3) + theme_bw() + opts(legend.position = "none",
     panel.border = theme_blank(), axis.ticks = theme_blank(), title = 'EC Kelly to Avg Kelly')

       
# Plot kelly to total return
SQL <- "SELECT * FROM hhv_metrics WHERE mid LIKE 'EC%'"
metrics <- dbGetQuery(con, SQL)  
#png('kelly_total_return.png')
p <- ggplot(metrics, aes(kelly, total_ret, label = mid, colour = mid))
p + geom_text(size = 3) + theme_bw() + opts(legend.position = "none",
   panel.border = theme_blank(), axis.ticks = theme_blank(), title = 'EC Kelly to Total Return')
#dev.off()
     
     
# Plot pnls
SQL <- "SELECT * FROM hhv_kelly WHERE mid = 'ECw25h1'  ORDER BY mid, dt"
mid_pnl <- dbGetQuery(con, SQL)
