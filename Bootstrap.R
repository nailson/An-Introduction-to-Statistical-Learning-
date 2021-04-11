#install.packages("ISLR")
library(ISLR)
library(boot)


set.seed(1)

Portifolio = ISLR::Portfolio
nrow(Portifolio)

alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  alpha = (var(Y) - cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))
  return (alpha)
}

# estimating the value of alpha
alpha.fn(Portifolio, 1:100)

# Bootstrapping the value of alpha 1x
alpha.fn(Portifolio, sample(100, 100,replace=T))

# Bootstrapping the value of alpha 1000x
boot(Portfolio, alpha.fn, R=1000)

