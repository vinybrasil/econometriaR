library(readxl)
df <- read_excel("Table 15_1.xls", col_names = TRUE, range="b9:D49")

par(mfrow=c(1,1), mar=c(3,3,2,1), mgp=c(1.5,0.5,0), cex=0.8)
plot(df$X, df$Y, pch=16)
abline(lm(df$Y~df$X), lwd=2, col="red")


#probit, probabilidade linear, tobit e logit

#modelo MPL

mod1 <- lm(df$Y~df$X)
summary(mod1)

#p/ renda(x)=10, temos 5,5% de ter a casa propria
#apos uma politica publica, faz a regressao de novo e ve se 
#a prob aumentou


#se só b2 nao explica, muita variancia e mt erro (variavel omitida)
#ou compara com regioes que nao  fizeram
plot(mod1$residuals, type="p", lwd=2)
hist(mod1$residuals, col="yellow")


#aplicar os MQ ponderados

#falta a parte final