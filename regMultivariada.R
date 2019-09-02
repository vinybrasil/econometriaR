y <- matrix(c(10,20,30,13),nrow=4 ,ncol=1)
x <- matrix(c(1,1,1,1, #matriz unitaria pq b1 ta sozinho
              20,25,45,26,
              37,30,28,42), nrow=4, ncol=3, byrow=FALSE)

x
y

#calculando o vetor b por matrizes
                #x*x: produto direto simpls dos elemtos da matriz(x1*x1)
                #x%*%x(inner product): produto matricial
txx <- t(x)%*%x #transposta de x multiplicada por ela mesma
txx
xinv <- solve(txx) #pega a inversa
xinv
b <- xinv %*% t(x) %*% y #inversa vezes a transposta vezes o y
b
mod <- (lm(y~x[,2]+x[,3])) # de outro modo, fazer o modelo com duas colunas 
summary(mod)
