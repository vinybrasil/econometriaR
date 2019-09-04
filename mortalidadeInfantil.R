#pag 214, ex 7.1 do gujarati

library(readxl)
file <- "Table 6_4.xls"
df <- read_excel(file, sheet="Table 6_4", col_names=TRUE, skip=2)
#a 3 linha eh o cabeçalho, por isso nao pula ela, mas só se col name = true
#colnames <- c("MI", "TAF", "PNB", "TFT")
plot(df$PGNP, df$CM, pch=16, col="blue")
plot(df$FLR, df$CM, pch=16, col="blue") #plot(x, y)

#criar o x

x <- cbind(rep(1,64), df$PGNP, df$FLR)
#transposta:
txx <- t(x)%*%x
inv <- solve(txx)
b <- inv %*% t(x) %*% df$CM

modLin <- lm(df$CM~df$FLR+df$PGNP)
summary(modLin)
