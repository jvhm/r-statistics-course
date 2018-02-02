# Classificação (Regressão logística)

library(ggplot2)
library(tidyverse)

data(iris)
str(iris)

# Plotar gráfico para detectar possíveis classificações
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, colour=Species)) +
  geom_point()

# Exemplo
x <- seq(-10, 10, .1)
y <- exp(1.3 + 1.54*x)/(1+exp(1.3 + 1.54*x))
qplot(x, y)

# Regressão usando 'logit'
iris <- mutate(iris, Species = as.numeric(ifelse(Species=="setosa",1,0)))
modelo <- glm(Species ~ .,
              family = binomial(link = 'logit'), data=iris)

# Obtendo valores preditos
valores <- predict(modelo, type='response')

summary(modelo) # É possível usar AIC para comparar os modelos.

# Threshold
valores <- ifelse(valores > .5, 1, 0)

# Titanic example
library(titanic)
data(titanic_train)
str(titanic_train)

titanic <- titanic_train %>%
  mutate(Sex = ifelse(Sex=="male",1,0)) %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked)
titanic <- na.omit(titanic)
titanic <- filter(titanic, Embarked!="")
str(titanic)

titanic$id <- 1:nrow(titanic)
titanic <- titanic %>%
  mutate(Pclass=paste0("C", Pclass), valorp=1, valore=1) %>%
  spread(Pclass, valorp, fill=0) %>%
  spread(Embarked, valore, fill=0)
titanic$id <- NULL

modelo <- glm(Survived ~ ., data=titanic,
              family=binomial(link="logit"))

valores <- predict(modelo, type='response')
valores <- ifelse(valores > .5, 1, 0)

# Média de acerto.
mean(valores == titanic$Survived)

# Aplicando modelo na base de teste
data("titanic_test")
str(titanic_test)
titanic_t <- titanic_test %>%
  mutate(Sex = ifelse(Sex=="male",1,0)) %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked)
titanic_t <- na.omit(titanic_t)
titanic_t <- filter(titanic_t, Embarked!="")
str(titanic_t)

titanic_t$id <- 1:nrow(titanic_t)
titanic_t <- titanic_t %>%
  mutate(Pclass=paste0("C", Pclass), valorp=1, valore=1) %>%
  spread(Pclass, valorp, fill=0) %>%
  spread(Embarked, valore, fill=0)
titanic_t$id <- NULL

valores_t <- predict(modelo, type='response', newdata = titanic_t)
valores_t <- ifelse(valores_t > .5, 1, 0)

# Média de acerto.
mean(valores_t == titanic_t$Survived)

# Teste com Spams
library(kernlab)
data("spam")
str(spam)

spam <- spam %>%
  mutate(type=as.numeric(type) - 1)
spam <- na.omit(spam)

# Dividindo treino e teste
ind_spam <- sample(nrow(spam), floor(.7*nrow(spam)))
spam_treino <- spam[ind_spam,]
spam_teste <- spam[-ind_spam,]

modelo_logit <- glm(type ~ ., data=spam_treino,
                    family = binomial(link = "logit"))

predito_teste <- predict(modelo_logit, newdata=spam_teste, type="response")
predito_teste <- ifelse(predito_teste > .5, 1, 0)

library(randomForest)
modelo_rf <- randomForest(type ~ ., data=spam_treino, ntree=500)

predito_rf <- predict(modelo_rf, newdata=spam_teste)
predito_rf <- ifelse(predito_rf > .5, 1, 0)

mean(spam_teste$type==predito_teste)
mean(spam_teste$type==predito_rf)

# Confusion matrix
library(caret)
confusionMatrix(data = predito_teste, reference = spam_teste$type)
confusionMatrix(data = predito_rf, reference = spam_teste$type)

# Curva ROC - Curve que indica se o modelo é bom ou não
# AUC - cálculo da área da curva ROC; quanto maior, melhor
# Com essa curva, podemos estimar um threshold melhor
library(pROC)
rocCurve <- roc(response = spam_teste$type, predictor = predito_rf)
plot(rocCurve)
auc(rocCurve)
