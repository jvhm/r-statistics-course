library(devtools)
#install_github("rmcelreath/rethinking")
library(tidyverse)
#library(rethinking)
library(grid)
library(gridExtra)

# Carrega o dataframe.
preg <- read_csv("2002fempreg.csv")

# Exibe as colunas.
head(preg)

# Seleciona as colunas desejadas.
preg_m <- select(preg, one_of(c("caseid", "prglngth", "outcome", "pregordr", "birthord", "birthwgt_lb", "birthwgt_oz", "agepreg")))

# Exibe valores "estranhos" das colunas.
sort(unique(preg_m$birthwgt_lb))
sort(unique(preg_m$birthwgt_oz))

# Filtrando os dados.
preg_m <- mutate(preg_m, idade=agegrep/100)
preg_m <- filter(preg_m, birthwgt_lb <= 15, birthwgt_oz <= 15)
preg_m <- filter(preg_m, outcome==1) %>% mutate(peso_kg=birthwgt_lb*0.453592)
preg_m <- mutate(preg_m, total_lb=birthwgt_lb+(birthwgt_oz/16))

# Gráficos
library(ggplot2)
library(grid)

library(gridExtra)
bar <- ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
hist <- ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
box <- ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()
grid.arrange(bar, hist, box, ncol=3)

# Barplot
ggplot(preg_m, aes(pregordr)) + geom_bar()

# Hitogramas
ggplot(preg_m) + geom_histogram(mappings = aes(x = peso_kg), bins = 5)
ggplot(preg_m) + geom_histogram(mappings = aes(x = peso_kg), bins = 15)
ggplot(preg_m) + geom_histogram(mappings = aes(x = peso_kg), bins = 50)

# Média e variância
mean(preg_m$agepreg)
var(preg_m$agepreg)
sd(preg_m$agepreg)
summary(preg_m$agepreg)

# Distinguir entre primeiros bebês e outros.
preg_m <- mutate(preg_m, grupo=ifelse(birthord=1, "primeiro", "outro"))

# Hitogramas e boxplots
ggplot(preg_m, aes(x=prglngth, fill=grupo)) +
  geom_histogram(binwidth=0.5, colour="black") +
  facet_grid(grupo ~ .)
ggplot(data = preg_m, mapping = aes(x = grupo, y = prglngth)) +
  geom_boxplot()

# Scatterplot
ggplot(data = faithful) +
  geom_point(mapping = aes(x=eruptions,y=waiting))

# Distribuição binomial
dbinom(6, size=9,prop=0.5)

# Samples
samp <- sample(c(-1,1), 1000, replace=TRUE, prob = c(0.5, 0.5))
plot(cumsum(samp), type="l")

# For-loop examples
results <- list()
X <- seq(from=200, to=250, by=1)
for(i in 1:length(X)) {
  X[i] <- X[i] + 2
}

int_vect <- seq(from=1, to=50, by=1)
for(i in 1:length(int_vect)){
  int_vect[i] <- int_vect[i] + 1
  print(int_vect[i])
}
int_vect

results <- list()
for(i in 1:10000){
  samp <- sample(c(-1,1), 1000, replace=TRUE, prob = c(0.5, 0.5))
  soma <- last(cumsum(samp))
  results[[i]] <- soma
}
#results

# Histogram
m <- as.data.frame(results)
m <- as.data.frame(t(m))
ggplot() + 
  geom_histogram(m, mapping = aes(x=m), bins = 100, color = "purple", fill = "grey") +
  geom_vline(xintercept = mean(m$V1), colour = "orange", linetype = "longdash")
  

# Rethinking
library(rethinking)
data("Howell1")
d <- Howell1
d <- filter(d, age > 18)
# Scatterplot
ggplot(data = d, aes(x=height,y=weight)) +
  geom_point() +
  geom_smooth()

height~dnorm(mu,sigma)
mu <- a + b*weigth
a~dnorm(155,20)
b~dnorm(0,10)
sigma~dunif(0,50)
curve(dnorm(x, 155, 20), from=100, to=250)

model_l <- map(alist(
  height~dnorm(mu,sigma),
  mu <- a + b*weight,
  a~dnorm(178,20),
  b~dnorm(0,10),
  sigma~dunif(0,50)
), data=d)

precis(model_l)
plot(height ~weight, data = d)
abline(a = coef(model_l)["a"], b = coef(model_l)["b"])

ggplot(data = d, aes(x=height,y=weight)) +
  geom_point() +
  geom_smooth(method = "lm")
l <- lm(height ~ weight, d)
summary(l)
plot(l)

#==============================================================================#

library(nycflights13)
d <- flights
d <- mutate(d, horario_pico=ifelse(between(dep_time,600,800)|between(dep_time,1700,1900), 1, 0))
l <- lm(dep_delay ~ horario_pico, d)
summary(l)
plot(l)

#==============================================================================#
# Aula 3
#==============================================================================#

data("Howell1")
df <- Howell1 %>% filter(age > 18)
df_men <- filter(df, male == 1)
df_women <- filter(df, male == 0)

options(scipen=999999)
t.test(df_men$height, df_women$height, alternative = "greater")

library(BEST)
bayes_t <- BESTmcmc(df_men$height, df_women$height, rnd.seed = 1234)
plot(bayes_t, which = "mean")
plotAll(bayes_t)

bayes_t <- BESTmcmc(df_men$weight, df_women$weight, rnd.seed = 1234)
plot(bayes_t, which = "mean")
plotAll(bayes_t)

bayes_t <- BESTmcmc(filter(df_men, age >= 50)$height, filter(df_men, age < 50)$height, rnd.seed = 1234)
plot(bayes_t, which = "mean")
plotAll(bayes_t)

#library(rstanarm)

lm <- lm(height ~ weight + male, df)
summary(lm)
plot(lm)

# Intercept = y quando x é 0
# Preditor = variáveis (x1, x2, etc)
# y = intercept + preditor.x

l <- lm(height ~ weight*male, df)
