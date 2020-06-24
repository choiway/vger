library(quantmod)
library(tseries)
library(ggplot2)

# Get Data
x <- getNorgate('ES', 'xts')
x <- x['1999::2010']
y <- getNorgate('EC', 'xts')
y <- y['1999::2010']

# Convert to xts
d.xts <- na.omit(merge(Cl(x), Cl(y)))
colnames(d.xts) <- c('xx', 'yy')

# Measure cointegartion & calculate BETA
m <- lm(d$xx ~ d$yy + 0)
beta <- coef(m)[1]
sprd <- d$xx - beta*d$yy

ht <- adf.test(sprd, alternative="stationary", k = 0)
print(ht)

# Create spread vector
sprd.xts <- xts(sprd, as.Date(index(d.xts)))
d.xts <- merge(d.xts, sprd.xts)

# Calculate mean and standard deviation of spread
# 20 trading days is a month
sprd_mean <- rollapply(d.xts$sprd.xts, 20, mean, align = "right")
d.xts <- merge(d.xts, sprd_mean)
colnames(d.xts)[ncol(d.xts)] <- 'sprd_mean'
sprd_sd <- rollapply(d.xts$sprd.xts, 20, sd, align = "right")
d.xts <- merge(d.xts, sprd_sd)
colnames(d.xts)[ncol(d.xts)] <- 'sprd_sd'

# Calculate z-score
d.xts$zscore <- (d.xts$sprd.xts - d.xts$sprd_mean) / d.xts$sprd_sd

# Calculate Returns
d.xts$ret_xx <- Delt(d.xts$xx)
d.xts$ret_yy <- Delt(d.xts$yy)
d.xts$ret_pair <- -d.xts$ret_xx + d.xts$ret_yy
d.xts$absret <- abs(d.xts$ret_pair)

# Long signals
d.xts$signal_long <- NA
ind <- which(d.xts$zscore < -2 )
d.xts$signal_long[ind] <- 1
d.xts$signal_long_factor <- lag(d.xts$signal_long)

# Short signals
d.xts$signal_short <- NA
ind <- which(d.xts$zscore > 2 & lag(d.xts$absret) > lag(d.xts$absret, 2))
d.xts$signal_short[ind] <- -1
d.xts$signal_short_factor <- lag(d.xts$signal_short)

# Exits
d.xts$exit <- NA
ind <- which(abs(d.xts$zscore) < 1)
d.xts$exit[ind] <- 1
d.xts$exit_lag <- lag(d.xts$exit)

# Exit Longs
ind <- which(d.xts$exit_lag == 1)
d.xts$signal_long_factor[ind] <- 0
d.xts$signal_long_factor <- na.locf(d.xts$signal_long_factor)

# Exit Shorts
ind <- which(d.xts$exit_lag == 1)
d.xts$signal_short_factor[ind] <- 0
d.xts$signal_short_factor <- na.locf(d.xts$signal_short_factor)

# PNL
d.xts$pnl_long <- d.xts$ret_pair * d.xts$signal_long_factor
plot(cumsum(na.omit(d.xts$pnl_long)))

d.xts$pnl_short <- d.xts$ret_pair * d.xts$signal_short_factor
plot(cumsum(na.omit(d.xts$pnl_short)))

# Total PNL
d.xts$total_factor <- NA
ind <- which(d.xts$signal_long_factor == 1)
d.xts$total_factor[ind] <- 1

ind <- which(d.xts$signal_short_factor == -1)
d.xts$total_factor[ind] <- -1

ind <- which(d.xts$exit_lag == 1)
d.xts$total_factor[ind] <- 0

d.xts$total_factor <- na.locf(d.xts$total_factor)
d.xts$total_pnl <- d.xts$total_factor * d.xts$ret_pair
plot(cumsum(na.omit(d.xts$total_pnl)))

# Tally ho
ind <- which(d.xts$total_pnl != 0)
ret <- na.omit(d.xts$total_pnl[ind])
mean(ret)
sd(ret)
mean(ret) / sd(ret) ^ 2



