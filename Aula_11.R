library(tidyverse)
library(AppliedPredictiveModeling)
library(caret)
library(party)
library(partykit)
library(rpart)
library(randomForest)

data("solubility")

checkImportance <- function(model){
  tt_vi <- varImp(model)$importance %>%
    mutate(vars = row.names(.)) %>%
    arrange(desc(Overall)) %>%
    top_n(20)
  ggplot(tt_vi, aes(x = Overall, y = reorder(vars, Overall))) +
    geom_segment(aes(yend = vars), xend = 0) +
    geom_point() +
    theme_bw() +
    ylab("Variables")
}

# Árvores de Decisão

# Opção 1
plot(as.party(rpart(solTrainY ~ ., data = solTrainX)))

# Opção 2, para eliminar nós desnecessários
tt <- train(solTrainX, solTrainY, method = "rpart1SE",
      trControl = trainControl(method = "cv"))
tt
print(tt$finalModel)
plot(tt$finalModel)
text(tt$finalModel)

# Detectar a importância das variáveis utilizadas pelo algoritmo
tt_vi <- varImp(tt)$importance %>%
  mutate(vars = row.names(.)) %>%
  arrange(desc(Overall)) %>%
  top_n(20)
tt_vi
ggplot(tt_vi, aes(x = Overall, y = reorder(vars, Overall))) +
  geom_segment(aes(yend = vars), xend = 0) +
  geom_point() +
  theme_bw() +
  ylab("Variables")

# Verificar correlação
cor(solTrainX$SurfaceArea1, solTrainX$SurfaceArea2) # 0.9 = muito alto

#-----------------------------------------------------------------------------

# Aplicação métodos mais avançados de criação de árvores
temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00288/leaf.zip", temp)
leaves <- read_csv(unz(temp, "leaf.csv"),
                   col_names = c("Class",
                                 "Specimen",
                                 "Eccent",
                                 "Aspect",
                                 "Length",
                                 "Solidity",
                                 "Convexity",
                                 "Iso",
                                 "Max_Depth",
                                 "Lobe",
                                 "Intensity",
                                 "Contrast",
                                 "Smooth",
                                 "Moment",
                                 "Uniformity",
                                 "Entropy"))
unlink(temp)

leaves$Class <- as.factor(leaves$Class)
trainIndex <- createDataPartition(leaves$Class,  p = .8, list = FALSE, times = 1)
leavesTrain <- leaves[trainIndex,]

controle <- trainControl(method = "cv", number = 10)
rfModel <- train(Class ~ ., data = leavesTrain, method = "rf", trControl = controle, importance = TRUE)
gbmModel <- train(Class ~ ., data = leavesTrain, method = "gbm", trControl = controle)

# Comparação entre métodos
comp <- resamples(list(samp_RF = rfModel,
                       samp_GBM = gbmModel))
difference <- diff(comp)
summary(difference)

# Importância das variáveis
checkImportance(rfModel)
checkImportance(gbmModel)

leavesTest <- leaves[-trainIndex,]

predito_rf <- predict(rfModel, newdata=leavesTest)
predito_gbm <- predict(gbmModel, newdata=leavesTest)

confusionMatrix(predito_rf, leavesTest$Class)
confusionMatrix(predito_gbm, leavesTest$Class)

postResample(predito_rf, leavesTest$Class)
postResample(predito_gbm, leavesTest$Class)


# Testes com outros dados

library(mlbench)
set.seed(200)
simulated <- mlbench.friedman1(200, sd = 1)
simulated <- as_data_frame(simulated$x, simulated$y)
colnames(simulated)[10] <- "y"
