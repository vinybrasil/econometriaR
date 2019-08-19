#gujarati inferencia
#tabela: able 3.2.xls

library(readxl)
File <- "Table 3_2.xls"
df1 <- read_excel(File, sheet="Table 3_2", col_names = TRUE, skip=4)
colnames(df1) <- c("obs", "wage", "educ")
#par(mar=c(3,2.5,2,1), mgp=c(1.5,0.5,0))
plot(df1$educ, df1$wage, pch=16, col="black", main="Teoria do capital humano")
mod <- lm(df1$wage~df1$educ)
#sem o beta1(intercepto) = mod <- lm(df$wage~0=df$educ), nesse caso dá mais certo
summary(mod)
abline(mod, lwd=2, col="red")
