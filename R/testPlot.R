library(xts)
library(MASS)
Data = read.csv("Example1.csv", header = F, sep = "," , stringsAsFactors = FALSE)
x = as.yearmon(2005 + seq(0, 107)/12)
Data = Data[,-1]
Data = Data[-length(Data[,1]),]
colnames(Data) = c("Parvest", "RUS2500")
Data = as.xts(Data, order.by = x)
results = cusumActMgr(portfolioName = "Parvest", benchmarkName = "RUS2500",
                      data = Data)

pdf("myOutPut.pdf")
#Plot of log-excess returns with annually moving average returns
logER_plot = barplot(100*results$Logarithmic_Excess_Returns, main = "Monthly Excess Returns",
                     xlab = "", ylab = "%", col=4, las=1)
lines(x = logER_plot, y = 100*results$Annual_Moving_Average, col=2, lwd=2)

#Plot of tracking error
plot(100*as.zoo(sqrt(12)*results$Tracking_Error), main="Annualized Tracking Error", type = 'l', las=0,
     xlab = "", ylab = "%", col=4, las=2)

#Plot of IR
barplot(results$Information_Ratios, main="Estimated Information Ratios", col=4, las=1)

#cusumIR
plot(as.zoo(results$Annualized_Cusum_IR),
     main="CUSUM Plot: Estimated Information Ratio", las=2, col=4, ylab = "", yaxt='n', lwd=2)
lines(coredata(results$Protractor_IR[,1]), col=2, lty = 1, lwd=2)
lines(coredata(results$Protractor_IR[,2]), col=2, lty = 3, lwd=2)
lines(coredata(results$Protractor_IR[,3]), col=2, lty = 4, lwd=2)
lines(coredata(results$Protractor_IR[,4]), col="purple", lty = 2, lwd=2)
lines(coredata(results$Protractor_IR[,5]), col=3, lty = 4, lwd=2)
lines(coredata(results$Protractor_IR[,6]), col=3, lty = 3, lwd=2)
lines(coredata(results$Protractor_IR[,7]), col=3, lty = 1, lwd=2)
legend("bottomright", bty = 'n', legend = format(round(seq(-2.0,2.0,2/3), 2), nsmall = 2),
       col = c(2,2,2,"purple",3,3,3), lty = c(1,3,4,2,4,3,1), cex = 1,
       title = "Slopes on Protractor", ncol = 2, lwd=2)

#Lindley's Recursion
plot(as.zoo(-results$`Lindley's_Recursion`),
     main="Lindley's Recursion", las=2, col=4, ylab = "")
abline(h = -3.41, col=3, lwd=2)
abline(h = -4.33, col="green3", lwd=2)
abline(h = -5.08, col=7, lwd=2)
abline(h = -5.72, col="goldenrod1", lwd=2)
abline(h = -6.29, col="orangered", lwd=2)
abline(h = -6.81, col=2, lwd=2)
abline(h = 0, col = 4, lty = 3)
text(as.yearmon('2005-06', "%Y-%m"), y = -2.0, "Avg. Crossing Time", cex = 0.7)
text(as.yearmon('2005-06', "%Y-%m"), y = -2.5, "IR = 0.5 | IR = 0", cex = 0.7)
text(as.yearmon('2005-06', "%Y-%m"), y = -3.3, "24 | 16", cex = 0.89)
text(as.yearmon('2005-06', "%Y-%m"), y = -4.2, "36 | 22", cex = 0.89)
text(as.yearmon('2005-06', "%Y-%m"), y = -4.9, "48 | 27", cex = 0.89)
text(as.yearmon('2005-06', "%Y-%m"), y = -5.6, "60 | 32", cex = 0.89)
text(as.yearmon('2005-06', "%Y-%m"), y = -6.2, "72 | 37", cex = 0.89)
text(as.yearmon('2005-06', "%Y-%m"), y = -6.7, "84 | 41", cex = 0.89)

#Excess volatility plot
plot(as.zoo(100*results$Excess_Volatility[,3]),
     main = "Excess Volatility Relative to Benchmark", las=2, col=4,
     xlab = "", ylab = "%")
abline(h = 0, col = 4, lty = 3)

#Scatter plot with robust regression
portRet = 100 * coredata(results$Means[,1])
benchRet = 100 * coredata(results$Means[,2])
Rob_lm = rlm(benchRet ~ portRet)
Alpha = round(Rob_lm$coefficients[1], 2)
Beta = round(Rob_lm$coefficients[2], 2)
summary(Rob_lm)

plot(portRet, benchRet, pch = 16, col = 4, main = "Scatter Plot
     Portfolio Returns and Benchmark Returns",
     xlab = "Portfolio Returns (%)", ylab = "Benchmark Returns (%)", las=1)
abline(Rob_lm, col=2)
abline(v=0, h=0)
legend("bottomright", legend = c(paste(expression(alpha), " = ", Alpha, "%"), paste(expression(beta), " = ", Beta, "")), cex = 0.7)

#CUSUM for returns
plot(as.zoo(results$Annualized_Cusum_ER),
     main="CUSUM Plot: Annualized Excess Returns", las=2, col=4, ylab = "", lwd=2)
lines(coredata(results$Protractor_ER[,1]), col=2, lty = 1, lwd=2)
lines(coredata(results$Protractor_ER[,2]), col=2, lty = 3, lwd=2)
lines(coredata(results$Protractor_ER[,3]), col=2, lty = 4, lwd=2)
lines(coredata(results$Protractor_ER[,4]), col="purple", lty = 2, lwd=2)
lines(coredata(results$Protractor_ER[,5]), col=3, lty = 4, lwd=2)
lines(coredata(results$Protractor_ER[,6]), col=3, lty = 3, lwd=2)
lines(coredata(results$Protractor_ER[,7]), col=3, lty = 1, lwd=2)
legend("bottomright", bty = 'n', legend = format(round(seq(-2.0,2.0,2/3), 2), nsmall = 2),
       col = c(2,2,2,"purple",3,3,3), lty = c(1,3,4,2,4,3,1), cex = 1,
       title = "Slopes on Protractor", ncol = 2, lwd=2)
dev.off()

# argument for users to pick for the line type**
# remove the y label, legend = -2% per annum, ....., cusum plot annualized excess returns
