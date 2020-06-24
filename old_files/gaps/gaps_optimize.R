SQL = "SELECT * FROM :condition"

x <- c(
"x < -2 * std",
"x < -1 * std AND x > -2 * std",
"x < 0 AND x > -1 * std",
"x > 2 * std",
"x > 1 * std AND x < 2 * std",
"x > 0 AND x < 1 * std"
)

for (i in 1:length(x)){
	v <- gsub(':condition', x[i], SQL)
	print(v)
}

