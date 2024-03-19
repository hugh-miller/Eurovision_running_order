library(xlsx)
library(data.table)

results1 <- read.xlsx(file="indata.xlsx", sheetName="import")

results1 <- data.table(results1)
results1[, diff := Public - Jury ]
results1[, running_fac := factor(Running) ]

results1[, running_1_26 := pmax(0, pmin(26-1, Running-1)  )]
#results1[, running_1_12 := pmax(0, pmin(12-1, Running-1)  )]

# saturated fit - raw impact at each position
a <- lm(diff~running_fac, data=results1)

# Linear fit (simple line of best fit)
b <- lm(diff~running_1_26, data=results1)
summary(b)

# Smoothing spline fit, as a point of comparison
loess1 <- loess(diff ~ Running, data=results1, span=0.4)
smoothed1 <- predict(loess1)[1:26] 

# charting the impact of running order
plot( 1:26, predict(a, newdata=data_temp))
lines(1:26,predict(b, newdata=data_temp))
lines(1:26,smoothed1, col="blue")

chart_out <- data.frame( running = c(1:26), raw = predict(a, newdata=data_temp), lm = predict(b, newdata=data_temp), loess = smoothed1)


# Revised predictions
results1$Public_adj <- results1$Public - predict(b)
results1$Total_adj <- results1$Public_adj + results1$Jury

# output results
write.xlsx(results1, file="outdata.xlsx", sheetName="output")
write.xlsx(chart_out, file="outdata.xlsx", sheetName="chart_out", append=TRUE)





