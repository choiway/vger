DROP TABLE IF EXISTS ats.norgate_sum_returns;
CREATE TABLE ats.norgate_sum_returns AS
SELECT
    dt
  , portfolio_ret
  , SUM(portfolio_ret) OVER (ORDER BY dt) AS sum_portfolio_ret
  , AVG(portfolio_ret) OVER (ORDER BY dt) AS avg_portfolio_ret
  , CASE WHEN STDDEV(portfolio_ret) OVER (ORDER BY dt) > 0
      THEN AVG(portfolio_ret) OVER (ORDER BY dt) - .0002/ STDDEV(portfolio_ret) OVER (ORDER BY dt)
      ELSE NULL END AS sharpe_todate
  , CASE WHEN VARIANCE(portfolio_ret) OVER (ORDER BY dt) > 0
      THEN AVG(portfolio_ret) OVER (ORDER BY dt) / VARIANCE(portfolio_ret) OVER (ORDER BY dt)
      ELSE NULL END AS kelly_todate
FROM
  (
  SELECT
    dt,
    SUM(ret) / COUNT(ret) AS portfolio_ret
  FROM
    (  
      SELECT 
          a.symbol
        , dt
        , close
        , (close - LAG(close, 1) OVER (PARTITION BY a.symbol ORDER BY dt))/ LAG(close, 1) OVER (PARTITION BY a.symbol ORDER BY dt) AS ret
      FROM stage.norgate_data AS a, base.liquid_symbols AS b
      WHERE a.symbol = b.symbol AND a.dt > '1-1-1995'
    ) AS c
  GROUP BY c.dt
  ) AS d
;


