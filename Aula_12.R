library(ggplot2)
library(gridExtra)
library(tidyverse)

# Machine Learning Supervisionado e Não-Supervisionado

# PCA -> redução de dimensões ("prcomp")

data("USArrests")
str(USArrests)

# Quais estados mais se parecem em criminalidade?

# Tentar achar relação com duas colunas
teste1 <- ggplot(data=USArrests, 
       aes(x=UrbanPop, y=Murder)) +
  geom_text(label = rownames(USArrests))
teste2 <- ggplot(data=USArrests, 
                 aes(x=UrbanPop, y=Rape)) +
  geom_text(label = rownames(USArrests))
grid.arrange(teste1, teste2)

ggplot(data=USArrests, aes(x=Rape, y=Murder)) +
  geom_point() + geom_smooth(method="lm")

# Aplicar PCA para reduzir colunas
dados <- prcomp(t(USArrests), scale=T)
ggplot(data=NULL, aes(x=dados$rotation[,1],
                      y=dados$roation[,2])) +
  geom_text(aes(label = rownames(dados$roation)))


##############################################

indice <- readRDS('~/Downloads/Treinamento R/votacao_camara/indice_legislador.rds')
votacoes <- readRDS('~/Downloads/Treinamento R/votacao_camara/votacao.rds')

# Distância Euclidiana (Teorem. Pitágoras)
d_legislador <- dist(votacoes)

#PCA
pca_deputados <- cmdscale(d_legislador)
pca_deputados <- data.frame(pca_deputados)
pca_deputados$id_legislator <- votacoes$id_legislator
str(pca_deputados)

str(indice)
indice <- indice[!duplicated(indice),]
pca_deputados <- inner_join(pca_deputados, indice)
pca_deputados <- pca_deputados[!duplicated(pca_deputados),]

ggplot(pca_deputados, aes(x=X1,y=X2)) + 
  geom_text(aes(label = Partido), size = 2)


##############################################

# Clusters

# No Cluster, é possível agrupar os dados sem necessidade de reduzir dimensões

v <- .3
x <- matrix(c(.3,.5,1.6,.5,1,.8), ncol=2)
x1 <- c(rnorm(150,x[1,1],v), rnorm(150, x[2,1]), rnorm(150, x[3,1], v))
x2 <- c(rnorm(150,x[1,2],v), rnorm(150, x[2,2]), rnorm(150, x[3,2], v))
y <- c(rep("grupo1", 150),rep("grupo2", 150),rep("grupo3", 150))
dados <- data.frame(x1=x1,x2=x2,y=y)

g1 <- ggplot(dados, aes(x=x1,y=x2, colour = factor(y))) + geom_point()

# K-means
resultado <- kmeans(dados[,1:2],3)
dados$yhat <- resultado$cluster

g2 <- ggplot(dados, aes(x=x1,y=x2, colour = factor(yhat))) + geom_point()

# Hierarchical Cluster
cluster <- hclust(dist(dados[,1:2]), method="centroid") # O método pode variar, para fins de testes e verificação de qual se encaixa melhor no cenário
plot(cluster)
dados$hy <- cutree(cluster, 3)

g3 <- ggplot(dados, aes(x=x1,y=x2, colour = factor(hy))) + geom_point()

grid.arrange(g1, g2, g3)


##################################################

# Redes Neurais

# Pacote 'nnet'
