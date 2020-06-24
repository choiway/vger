# Bamberger Algorithm

 The Bamberger algorithm is a long/short strategy that assumes returns, on average, mean revert. I named it after Gerry Bamberger who worked at Morgan Stanley's original statistical arbitrage group back in the 80s. After he left Morgan Stanley he worked with Ed Thorpe and implemented this strategy at Thorpe's hedge fund, Princeton-Newport.  

 The algorithm takes a group of stocks, ranks them by return and assumes that the higher percentile of returns will decrease over the next period while the lower percentile of returns will increase. I don't remember what period Bamberger originally worked off of but I think it was monthly. This algorithm can be applied to any period but extracting meaningful returns and risk management is harder in shorter time frames.

 In this repo, I use Thursday to Thursday weekly returns resulting in a weekly rebalance. It assumes you short the highest decile of returns and go long the lowest decile. This analysis doesn't take trading costs into account. 


