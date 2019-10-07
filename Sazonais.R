library(readxl)
library(xts) #variaveis temporais
df <-  read_excel("data/Table 9_3.xls", col_names=TRUE, skip=5)

df$D1 <- 0
df$D2 <- 0
df$D3 <- 0
df$D4 <- 0



df <- ts(df, start=1978,frequency=4)
df[cycle(df)==1,5] <- 1
df[cycle(df)==2,6] <- 1
df[cycle(df)==3,7] <- 1
df[cycle(df)==4,8] <- 1

par(mfrow=c(2,2), mar=c(3,3,2,1), mgp=c(1.5,0.5,0))

plot(df[,1],main="DISH",cex.main=0.8,type="l",col="black")
plot(df[,2],main="FRIG",cex.main=0.8,type="l",col="black")
plot(df[,3],main="WASH",cex.main=0.8,type="l",col="black")
plot(df[,4],main="DUR",cex.main=0.8,type="l",col="black")

df <- as.data.frame(df)

mod1 <- lm(df$FRIG~0+df$D1+df$D2+df$D3+df$D4)

summary(mod1)
mod2 <- lm(df$FRIG~df$D2+df$D3+df$D4)
summary(mod2)
mod3 <- lm(df$FRIG~df$D2+df$D3+df$D4+df$DUR)
summary(mod3)
mod4 <- lm(df$FRIG~df$DUR)                                #pr(>t) alto mas r2 baixo
summary(mod4)


#PARTE QUE NAO TEM
df$res <- mod1$residuals
df$dess <- mean(df$FRIG)+df$res
par(mfrow=c(1,2), mar=c(4,4,2,1), mgp=c(1.5,0.5,0))
barplot(df$res, col="green", horiz="true", main="errors")
plot(df$FRIG, type="l",lty=3, col="black",ylim=c(800,1800))

lines(df$dess,main="Serie dessazonalidade", type="l", lwd=2, col="blue")
legend("bottomleft", legend=c("Original", "Dessazonalizado"),
       col=c("black", "blue", lwd=2,lty=c(2,1),cex=0.8))
#FALTA UMA PARTE AINDA
