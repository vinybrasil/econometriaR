library(readxl)

df  <- read_excel("Table 7_3.xls",col_names=TRUE,range="A6:D57")

colnames(df) <- c("State","GDP","Labor","Capital")
df$lnGDP     <- log(df$GDP)       # variáveis em log natural
df$lnLabor   <- log(df$Labor)
df$lnCapital <- log(df$Capital)

#modLin     <- lm(df$GDP~df$Labor+df$Capital)        # estima modelos
modLogLog  <- lm(df$lnGDP~df$lnLabor+df$lnCapital)
#summary(modLin)                                     # lista modelos para comparar
summary(modLogLog)

anova <- aov(df$lnGDP~df$lnLabor+df$lnCapital)
summary(anova)

#trabalho faz variar mais o produto