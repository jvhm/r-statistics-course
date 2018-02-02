# Modelos Preditivos

library(fueleconomy)
library(ggplot2)

data("vehicles")
str(vehicles)

# Gráfico de dispersão para identificar relação entre variáveis
ggplot(vehicles, aes(x=displ, y=cty)) +
  geom_point() +
  geom_smooth(method = "lm")

# R² pode ser usado para verificar se uma correlação é boa
# Outra forma é o cálculo do Erro Quadrático Médio
# EQM = Bias² + Variance
x <- rnorm(200)
y <- 1.3 * x + rnorm(200)
modelo <- lm(y ~ x)
y_hat <- predict(modelo)
eqm <- mean((y - y_hat)^2)

# Cálculo de R² (para predição, quanto maior, melhor) e EQM para modelo de veículos
vehicles <- na.omit(vehicles) # remove missing data
x <- vehicles$displ
y <- vehicles$cty
modelo_2 <- lm(y ~ x)
y_hat <- predict(modelo_2)
eqm_2 <- mean((y - y_hat)^2)

x1 <- vehicles$displ
x2 <- vehicles$cyl
y <- vehicles$cty
modelo_3 <- lm(y ~ x1 + x2 + x1*x2)
y_hat <- predict(modelo_3)
eqm_3 <- mean((y - y_hat)^2)

x1 <- vehicles$displ
y <- vehicles$cty
modelo_4 <- lm(y ~ poly(x1,2) + x1)
y_hat <- predict(modelo_4)
eqm_4 <- mean((y - y_hat)^2)

# Validação do EQM (overfitting)
# Divisão dos dados, aleatoriamente, em parte de Treino e parte de Teste
vehicles <- na.omit(vehicles) # remove missing data
n_treino <- floor(.70*nrow(vehicles))
ind_treino <- sample(nrow(vehicles), n_treino)
treino <- vehicles[ind_treino,]
teste <- vehicles[-ind_treino,]

# Modelo Treino
modelo_treino <- lm(cty ~ displ, data = treino)
y_hat <- predict(modelo_treino)
eqm_treino <- mean((y - y_hat)^2)
treino$cty_predict <- predict(modelo_treino)

ggplot(treino) + 
  geom_point(aes(x=displ, y=cty)) +
  geom_line(aes(x=displ, y=cty_predict))

# Previsão utilizando dados de teste
y_hat_teste <- predict.lm(modelo_treino, newdata = teste)
eqm_teste <- mean((teste$cty - y_hat_teste)^2)
teste$cty_predict <- predict(modelo_treino, newdata=teste)

ggplot(teste) + 
  geom_point(aes(x=displ, y=cty)) +
  geom_line(aes(x=displ, y=cty_predict))

##########################################

# Modelo para base de dados 'vehicles'
ggplot(vehicles, aes(x=cyl, y=cty)) + 
  geom_point() +
  geom_smooth(method = "lm")

unique(vehicles$fuel) # Verificar os valores distintos da coluna 'fuel'

ggplot(vehicles, aes(x=fuel,y=cty)) +
  geom_boxplot()

ggplot(vehicles, aes(x=displ, y=cty)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ fuel)

ggplot(vehicles, aes(x=cty)) + 
  geom_histogram(bins = 40) 

ggplot(vehicles, aes(x=hwy)) + 
  geom_histogram(bins = 40)

ggplot(vehicles, aes(x=hwy)) + 
  geom_histogram(bins = 40) +
  facet_wrap(~ fuel)

vehicles2 <- vehicles %>%
  mutate(valor = 1) %>%
  spread(fuel, valor, fill = 0)

vehicles2 <- vehicles2 %>%
  mutate(marcha_automatica=ifelse(str_detect(trans, "Auto"), 1, 
                                  ifelse(str_detect(trans, "Manual"), 0, NA)))

ggplot(vehicles, aes(x=displ, y=cty, colour=factor(cyl))) + 
  geom_point() +
  geom_smooth(method = "lm") 

colnames(vehicles2) <- gsub(" ", "_", colnames(vehicles2))

# Selecionando colunas para cada modelo
banco_model1 <- vehicles2 %>%
  select(cty, cyl, displ, CNG:Regular_Gas_and_Electricity)
banco_model2 <- vehicles2 %>%
  select(cty, cyl, displ)
banco_model3 <- vehicles2 %>%
  select(cty, cyl, displ, marcha_automatica) %>%
  mutate(displ_quadrado = displ^2)

calcula_erroreg <- function(banco) {
  # Divisão dos dados, aleatoriamente, em parte de Treino e parte de Teste
  n_treino <- floor(.70*nrow(banco))
  ind_treino <- sample(nrow(banco), n_treino)
  treino <- banco[ind_treino,]
  teste <- banco[-ind_treino,]
  
  vetor_erro <- vector(mode = "numeric", length = 60)
  for(i in 1:60) {
    cat(paste0(i,","))
    # Modelo Treino
    modelo_treino <- lm(cty ~ ., data = treino)
    # opcional com a biblioteca 'caret'
    #modelo_treino <- train(cty ~ ., data = treino, method = "lm")
    y_hat <- predict(modelo_treino, newdata = teste)
    eqm_treino <- mean((teste$cty - y_hat)^2)
    vetor_erro[i] <- eqm_treino
    
    
  }
  #treino$cty_predict <- predict(modelo_treino)
  
  # Previsão utilizando dados de teste
  #y_hat_teste <- predict.lm(modelo_treino, newdata = teste)
  #eqm_teste <- mean((teste$cty - y_hat_teste)^2)
  #teste$cty_predict <- predict(modelo_treino, newdata=teste)
  
  return(mean(vetor_erro))
}

calcula_erroreg(banco_model1)
calcula_erroreg(banco_model2)
calcula_erroreg(banco_model3)

# 1º modelo: cty = beta_0 + beta_1 * displ + beta_2 * cyl + ...fuel


# 2º modelo: cty = beta_0 + beta_1 * displ + beta_2 * cyl


# 3º modelo: 

##########################################



