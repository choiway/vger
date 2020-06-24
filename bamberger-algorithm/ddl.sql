COPY
(
SELECT *
, CASE
    WHEN pct_rank > .9 THEN -1
    WHEN pct_rank < .1 THEN 1
    ELSE 0
    END AS pos
FROM (
  SELECT *
  , PERCENT_RANK() OVER (PARTITION BY dt ORDER BY ret) AS pct_rank
  FROM (
    SELECT
      *,
      (LEAD(adj_close, 1) OVER w  - adj_close) / adj_close AS next_ret,
      (adj_close - LAG(adj_close, 1) OVER w) / LAG(adj_close, 1) OVER w AS ret
    FROM (
      SELECT
        dt,
        EXTRACT(DOW FROM dt) AS day_of_week,
        ticker,
        adj_close
      FROM 
        av.stock_quotes
      WHERE EXTRACT(DOW FROM dt) = 4
    ) AS a
    WINDOW w AS (PARTITION BY ticker ORDER BY dt)
  ) AS b
) AS c
) 
TO '/tmp/qqq_weekly_thursday_ranked.csv'
CSV HEADER;