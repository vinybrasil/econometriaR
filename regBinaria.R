#quebra estrutual em series(dado uma politica publica, por exemplo)
#teste de chow
#variaveis dummies

#testar para ver se houve mudança na poupança quando o Reagan colocou as politicas anti inflacionarias
library(readxl)
df <- read_excel("Table 8_9.xls", col_names = TRUE, skip = 4)
colnames(df) = c("ano", "poup", "renda")
df1 <- df[df$ano<1982,]
df2 <- df[df$ano>1981,]
df1b <- df[1:12,]

par(mfrow=c(2,2), mgp=c(1.5,0.5,0))

plot(df$renda, df$poup,main="1970-1995", xlab="Renda", ylab="Poup", pch=16, col="blue")
abline(lm(df$poup~df$renda), col="red",lwd=2) #mto erro

plot(df1$renda, df1$poup,main="1970-1981", xlab="Renda", ylab="Poup", pch=16, col="blue")
abline(lm(df1$poup~df1$renda), col="red",lwd=2) #primeiro split

plot(df2$renda, df2$poup,main="1982-1995", xlab="Renda", ylab="Poup", pch=16, col="blue")
abline(lm(df2$poup~df2$renda), col="red",lwd=2) #segundo split


mod1 <- lm(df1$poup~df1$renda)
mod2 <- lm(df2$poup~df2$renda)
mod <- lm(df$poup~df$renda)

summary(mod)
summary(mod1)
summary(mod2)
