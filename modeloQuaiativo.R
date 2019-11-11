library(readxl)
df <- read_excel("Table 15_1.xls", col_names = TRUE, range="b9:D49")

par(mfrow=c(1,1), mar=c(3,3,2,1), mgp=c(1.5,0.5,0), cex=0.8)
plot(df$X, df$Y, pch=16)
abline(lm(df$Y~df$X), lwd=2, col="red")
