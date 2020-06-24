library(RPostgreSQL)
library(quantmod)

query =
"INSERT INTO hhv_pnl(mid, symbol, lag_wdw, hold_period, dt, close, hhv, ret, signal, pnl)
SELECT
    symbol || 'w' || lag_wdw || 'h' || hold_period as mid
  , symbol
  , lag_wdw
  , hold_period
  , dt
  , close
  , hhv
  , ret
  , signal
  , CASE
    WHEN lag(signal, 1) OVER hld_wdw = 1
      OR lag(signal, :hld) OVER hld_wdw =1
    THEN ret
    ELSE NULL
    END AS pnl
FROM
  -- (b) BEGIN
  (SELECT 
      symbol
    , lag_wdw
    , hold_period  
    , dt
    , close
    , hhv
    , ret
    , CASE
      WHEN close > (lag(hhv, 1) OVER sig_wdw)
        THEN 1 
      ELSE NULL
      END AS signal
  FROM   
      -- (a) BEGIN
      (SELECT
          :lwdw AS lag_wdw
        , :hld AS hold_period
        , symbol
        , dt
        , close
        , MAX(close) OVER lag_wdw AS hhv
        , (close - lag(close, 1) OVER lag_wdw) / lag(close, 1) OVER lag_wdw AS ret 
      FROM stage.norgate_data
      WHERE symbol = ':symbol' AND dt > '1-1-1999'
      WINDOW lag_wdw AS (PARTITION BY symbol ORDER BY dt ROWS BETWEEN :lwdw - 1 PRECEDING AND CURRENT ROW)
      ) AS a
      -- (a) END  
  WINDOW sig_wdw AS (PARTITION BY symbol ORDER BY dt ROWS BETWEEN :lwdw PRECEDING AND CURRENT ROW)
  ) AS b
  -- (b) END
WINDOW hld_wdw AS (PARTITION BY symbol ORDER BY dt ROWS BETWEEN :hld PRECEDING AND CURRENT ROW)"

con <- dbConnect(PostgreSQL(), host = 'localhost', user = 'postgres', password = 'medulla624', dbname ='wchoi')

wdw = 10
hld = 10
s <-  c('EC', 'ES', 'GC', 'SF', 'TY2')

for (symbol in s) {
  ticker = symbol
  for (i in 1:wdw) {
    for (j in 1:hld) {
      print(paste('Running analysis for',ticker,i * 5,j, sep = '-'))
      subquery <- gsub(':lwdw', i * 5, query)
      subquery <- gsub(':hld', j, subquery)
      subquery <- gsub(':symbol', ticker, subquery)
      x <- dbGetQuery(con, subquery)
      }
  }
}
