total.pnl <- merge(es_llv_pnl, es_hhv_pnl)
total.pnl <- merge(total.pnl, es_gap_pnl)

total.pnl[is.na(total.pnl)] <- 0
total.pnl.df <- data.frame(total.pnl)

t <- rowSums(total.pnl.df)
t.xts <- xts(t, as.Date(index(total.pnl)))

#Tally
ind <- which(t != 0)
ret <- t[ind]
m <- mean(ret)
s <- sd(ret)
kelly <- m/s^2
rfr <- .05/250
sharpe <- (m- rfr) / s

print(m)
print(s)
print(kelly)
print(sharpe)

png('es_total_pnl.png')
plot(na.omit(cumsum(t.xts)))
dev.off()
