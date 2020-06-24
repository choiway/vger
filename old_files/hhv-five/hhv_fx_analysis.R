library(RPostgreSQL)
library(quantmod)
library(ggplot2)
# Initialize database
con <- dbConnect(PostgreSQL(), host = '10.31.33.1', user = 'vger', password = 'v3ntr1s', dbname ='vger')


# PLOT non-filtered kelly_todate
SQL <- "
        SELECT * 
        FROM hhv_fx_kelly
        WHERE symbol = 'GBPUSD' 
        AND dt > '1-1-2002'
        ORDER BY mid, dt
       "
rs <- dbGetQuery(con, SQL)
max_dt <- max(rs$dt)

p <- ggplot( rs, 
             aes( x=dt, 
                  y=pnl_todate, 
                  group = mid, 
                  colour = mid, 
                  label = mid
                )
           )

p1 = p + 
     geom_line() + 
     geom_text (  data = subset(rs, dt == max_dt), 
                  aes( x = dt + 0.5), 
                  size = 3, 
                  hjust = .5
               )
dev.new()

p1 + 
theme_bw() + 
opts( legend.position = "none",
      panel.border = theme_blank(), 
      axis.ticks = theme_blank(), 
      title = 'Kelly to Date'
    )
    

       
# Plot filtered kelly
SQL <- "
        SELECT * 
        FROM hvv_fx_filtered_symbol_pnl 
        WHERE symbol = 'SF' AND dt > '1-1-1984'
        ORDER BY mid, dt
       "
rs <- dbGetQuery(con, SQL)
max_dt <- max(rs$dt)

p <- ggplot(rs, 
            aes(x=dt, 
                y=kelly_pnl_todate, 
                group = mid, 
                colour = mid, 
                label = mid
               )
           )

p1 =  p + 
      geom_line() + 
      geom_text ( data = subset(rs, dt == max_dt), 
                  aes( x = dt + 0.5), 
                  size = 3, 
                  hjust = .5
                )
dev.new()
p1 + theme_bw() + opts(legend.position = "none",
       panel.border = theme_blank(), axis.ticks = theme_blank(), title = 'EC Kelly to Date')

     
# Plot pnls
SQL <- "SELECT * FROM gaps_filtered WHERE mid = 'SFd2xar2'  ORDER BY mid, dt"
mid_pnl <- dbGetQuery(con, SQL)
mid_pnl[is.na(mid_pnl)] <- 0

dev.new()
p <- ggplot(mid_pnl, aes(dt, cumsum(kelly_pnl)))
p + geom_line() + opts(title = 'Kelly PNL')

dev.new()
p <- ggplot(mid_pnl, aes(dt, cumsum(long_pnl)))
p + geom_line() + opts(title = 'PNL')


