library(tidyverse)
library(stringr)
library(ggplot2)
library(caret)
#library(pls)

data("iris")

# Cross Validation
# Útil para bases de dados pequenas

trainIndex <-
  createDataPartition(iris$Species, p = .8, list = FALSE, times = 1)

irisTrain <- iris[trainIndex,]
irisTest <- iris[-trainIndex,]

# Cross-validation
controle <- trainControl(method = "cv", number = 10)
modelo <- train(Species ~ Sepal.Length, data = irisTrain, method = "rf", trControl = controle)


# Regressão linear com cálculo de erro

x <- rnorm(200)
y <- 1.2 + 1.75*x + rnorm(200)

funcao_erro_mo <- function(betas) {
  yhat <- betas[1] + betas[2]*x
  erros <- abs((y - yhat))
  soma_erros <- sum(erros)
  return(soma_erros)
}

funcao_erro_qt <- function(betas) {
  yhat <- betas[1] + betas[2]*x
  erros <- (y - yhat)^4
  soma_erros <- sum(erros)
  return(soma_erros)
}

parametros_mo <- optim(c(2,2), funcao_erro_mo)$par
parametros_qt <- optim(c(2,2), funcao_erro_qt)$par

ggplot(NULL, aes(x=x,y=y)) +
  geom_point() +
  geom_abline(intercept=parametros_qt[1], slope = parametros_qt[1], colour="green") +
  geom_abline(intercept=parametros_mo[1], slope = parametros_qt[1], colour="pink")