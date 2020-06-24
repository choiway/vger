library(RPostgreSQL)
# Initialize database
con <- dbConnect(PostgreSQL(), host = 'localhost', user = 'postgres', password = 'medulla624', dbname ='wchoi')
