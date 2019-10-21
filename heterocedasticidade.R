#C0D3D BY N13M4ND

#testar heterocedasticidade 

library(readxl)
rm(list = ls()) #remove tudo da memoria
#colunas = tamanho da empresa
#linhas = setor

#------------------COM DP CONHECIDO---------------------------------------------------

df  <- read_excel("Table 11_1.xls",col_names=TRUE,skip=29)
df[11,1] = "Media"
df[12,1] = "dp"
df[13,1] = "Produtividade"
df

#arrumar os dados para a regressao
df2 = as.data.frame(rep(0,9))
df2$Y <- t(df[11,2:10]) #media dos salarios, linha das medias; copia linha 11 e transpoe
df2$X <- 1:9            #cria X(com tamanho de 1 a 9)
df2$dp <- t(df[12,2:10])#copia o dp
df2$dpinv <- 1/df2$dp   #cria coluna inverso do dp
df2$Ydp <- df2$Y/df2$dp 
df2$Xdp <- df2$X/df2$dp
df2 <- df2[,-1]
df2

#fazer o mqo ponderado pelo desvio padrao e nao ponderado

mq01 <- lm(df2$Y~df2$X) #automaticamente poe o intercepto
summary(mq01) #alto t e alto r2, normalmente Pr(t) alto demais

mqp <- lm(df2$Ydp~0+df2$dpinv+df2$Xdp)
summary(mqp)
#y = 3392 + 154.Xdp

#-----------------------------COM DP DESCONHECIDO-------------------------------

#ao inves de dividir por dp, estima a 