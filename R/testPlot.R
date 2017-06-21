library(xts)
Data = read.csv("Example1.csv", header = F, sep = "," , stringsAsFactors = FALSE)
x = as.yearmon(2005 + seq(0, 107)/12)
Data = Data[,-1]
Data = Data[-length(Data[,1]),]
colnames(Data) = c("Parvest", "RUS2500")
Data = as.xts(Data, order.by = x)
results = cusumActMgr(portfolioName = "Parvest", benchmarkName = "RUS2500",
                      data = Data)

#Plot of log-excess returns with annually moving average returns
logER_plot = barplot(100*results$Logarithmic_Excess_Returns, main = "Monthly Excess Returns",
                     xlab = "", ylab = "%", col=4, las=1)
lines(x = logER_plot, y = 100*results$Annually_Moving_Average, col=2, lwd=2)

#Plot of tracking error
plot(100*as.zoo(sqrt(12)*results$Tracking_Errors), main="Annualized Tracking Error", type = 'l', las=0,
     xlab = "", ylab = "%", col=4, las=2)

#Plot of IR
barplot(results$Information_Ratios, main="Information Ratios", col=4, las=1)

#cusumIR
plot(as.zoo(results$Annualized_Cusum_IR), auto.grid=FALSE,
     main="CUSUM Plot: Information Ratio", las=2, col=4, ylab = "")
lines(coredata(results$Protractor[,1]), col=2, lty = 1)
lines(coredata(results$Protractor[,2]), col=2, lty = 3)
lines(coredata(results$Protractor[,3]), col=2, lty = 4)
lines(coredata(results$Protractor[,4]), col="purple", lty = 2)
lines(coredata(results$Protractor[,5]), col=3, lty = 4)
lines(coredata(results$Protractor[,6]), col=3, lty = 3)
lines(coredata(results$Protractor[,7]), col=3, lty = 1)
legend(x = 70, y = 39, bty = 'n', legend = format(round(seq(-2.0,2.0,2/3), 2), nsmall = 2),
       col = c(2,2,2,"purple",3,3,3), lty = c(1,3,4,2,4,3,1), cex = 0.5,
       title = "Slopes on Protractor", ncol = 2)

#Loglikeloihood Ratios
plot(as.zoo(-results$`Lindley's_Recursion`), auto.grid=FALSE,
     main="Lindley's Recursion", las=2, col=4, ylab = "")
abline(h = -3.41, col=8, lwd=2)
abline(h = -4.33, col=5, lwd=2)
abline(h = -5.08, col=6, lwd=2)
abline(h = -5.72, col=7, lwd=2)
abline(h = -6.29, col=3, lwd=2)
abline(h = -6.81, col=2, lwd=2)
text(as.yearmon('2005-09', "%Y-%m"), y = -2.0, "IR = .5/0/-.5", cex = 0.7)
text(as.yearmon('2005-06', "%Y-%m"), y = -3.2, "24/16/11", cex = 0.7)
text(as.yearmon('2005-06', "%Y-%m"), y = -4.2, "36/22/15", cex = 0.7)
text(as.yearmon('2005-06', "%Y-%m"), y = -4.9, "48/27/18", cex = 0.7)
text(as.yearmon('2005-06', "%Y-%m"), y = -5.6, "60/32/21", cex = 0.7)
text(as.yearmon('2005-06', "%Y-%m"), y = -6.1, "72/37/23", cex = 0.7)
text(as.yearmon('2005-06', "%Y-%m"), y = -6.7, "84/41/25", cex = 0.7)

#Excess volatility plot
plot(as.zoo(100*results$Excess_Volatility[,3]),
     main = "Excess Volatility Relative to Benchmark", las=2, col=4,
     ylab = "%")
