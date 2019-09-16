library(readxl)
file <- "Table 6_4.xls"
df <- read_excel(file, sheet="Table 6_4", col_names=TRUE, skip=2)
plot(df$PGNP, df$CM, pch=16, col="blue")
plot(df$FLR, df$CM, pch=16, col="blue") #plot(x, y)
modLin <- lm(df$CM~df$FLR+df$PGNP)
summary(modLin)

anova <- aov(df$CM~df$PGNP+df$FLR)
summary(anova)

#reg padronizaa, onde há soma ds desvis en ekação à média dividio
#pelo desvio padrao

df$CMp <- (df$CM-mean(df$CM))/sd(df$CM)
df$PGNPp <- (df$PGNP-mean(df$PGNP))/sd(df$PGNP)
df$FLRp <- (df$FLR-mean(df$FLR))/sd(df$FLR)
modp <- lm(df$CMp~df$PGNPp+df$FLRp)
summary(modp)

#se fizer o 0,763/0,2026, dá pra ver que a educação é 3x mais importante que a renda