DROP TABLE IF EXISTS hhv_pnl;
CREATE TABLE hhv_pnl (
    mid text
  , symbol text
  , lag_wdw numeric
  , hold_period numeric
  , dt date
  , close numeric
  , hhv numeric
  , ret numeric
  , signal numeric
  , pnl numeric
);

DROP TABLE IF EXISTS hhv_metrics;
CREATE TABLE hhv_metrics (
    model_id text
  , mu numeric
  , sigma numeric
  , sharpe numeric
  , kelly numeric
  , risk_ret numeric
  , total_ret numeric
  , ticker text
  , wdw numeric
  , hold numeric
  , trade_count numeric
  , win numeric
  , loss numeric
  , win_loss numeric
);





