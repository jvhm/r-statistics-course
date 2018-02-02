# Limpando o ambiente
rm(list = lm())
options(scipen = 99999)

library(BEST)
library(tidyverse)
library(nycflights13)

flights <- flights

# -------- ESTATÍSTICA TRADICIONAL -------- #

# Criação de vetores
AA <- flights$air_time[flights$carrier == "AA"] # American Airlines
AA <- AA[!is.na(AA)]
DL <- flights$air_time[flights$carrier == "DL"] # Delta Airlines
DL <- DL[!is.na(DL)]

# T-test das médias da variável air_time
t.test(AA, DL)

# BESTmcmc. Versão Bayeseana do T-Test
AA <- sample(AA, 100)
DL <- sample(DL, 100)
teste_t <- BESTmcmc(AA, DL, rnd.seed = 1234)

# Plot
plotAll(teste_t)

# Limpando o ambiente
rm(list = lm())
options(scipen = 99999)

# Regressão
library(rethinking)
library(rstanarm)
data("Howell1")
head(Howell1)

# Correlação
library(corrr)
head(airquality)
df <- airquality %>% correlate() %>% rearrange()
rplot(df)
network_plot(df)

# Transformações
data("Howell1")
Howell1_scale <- as_data_frame(scale(Howell1))
head(Howell1_scale)

# Correlação
Howell1_corr <- Howell1 %>%
  select(-height) %>% # tira o 'y' da relação
  correlate() %>%
  rearrange()
Howell1_corr
Howell1 %>% correlate() %>% fashion()
rplot(Howell1_corr)
#network_plot(Howell1_corr)

# Modelo
Howell1_mod <- lm(height ~ weight + male, data = Howell1)
summary(Howell1_mod)
par(mfrow=c(2,2))
plot(Howell1_mod) # Análise gráfica dos resultados

Howell1_mod_2 <- lm(height ~ weight + male + age, data = Howell1)
summary(Howell1_mod_2)
par(mfrow=c(2,2))
plot(Howell1_mod_2) # Análise gráfica dos resultados

Howell1_mod_3 <- lm(height ~ weight + age, data = Howell1)
summary(Howell1_mod_3)
par(mfrow=c(2,2))
plot(Howell1_mod_3) # Análise gráfica dos resultados

Howell1_mod_4 <- lm(height ~ weight + male + age + weight * age, data = Howell1)
summary(Howell1_mod_4)
par(mfrow=c(2,2))
plot(Howell1_mod_4) # Análise gráfica dos resultados

library(interplot)
interplot(m = Howell1_mod_4, var1 = "weight", var2 = "age")

# Modelo não-linear
sim <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)
ggplot(sim, aes(x, y)) + geom_point()


# Regressão linear polinomial
library(modelr)
mod1 <- lm(y ~ poly(x, 1), data = sim)
mod2 <- lm(y ~ poly(x, 2), data = sim)
mod3 <- lm(y ~ poly(x, 3), data = sim)
mod4 <- lm(y ~ poly(x, 4), data = sim)
mod5 <- lm(y ~ poly(x, 5), data = sim)

grid <- sim %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")
ggplot(sim, aes(x,y)) +
  geom_point() +
  geom_line(data = grid, colour = "red") + 
  facet_wrap(~model)

Howell1_mod_5 <- lm(height ~ polym(weight, male, age, weight * age, degree=2, raw=TRUE), data = Howell1)
summary(Howell1_mod_5)
par(mfrow=c(2,2))
plot(Howell1_mod_5) # Análise gráfica dos resultados


# -------- ESTATÍSTICA BAYESEANA -------- #

at_howell_mod <- stan_lm(height ~ weight + male, data = Howell1,
                         prior = R2(0.5, what = "mean"), seed = 1234)
# prior = serve para indicar se existe alguma idea anterior da previsibilidade. 0.5 indica mais neutralidade.
launch_shinystan(at_howell_mod)

# Posterior check (para validar se o modelo funciona bem com novos dados)
pp_check(at_howell_mod, "scatter", nreps = 3)
pp_check(at_howell_mod, "residuals", nreps = 9)
pp_check(at_howell_mod, nreps = 9)
pp_check(at_howell_mod, "test")

at_howell_mod_2 <- stan_lm(height ~ weight + male + age, data = Howell1,
                         prior = R2(0.5, what = "mean"), seed = 1234)
at_howell_mod_3 <- stan_lm(height ~ weight + male + age + weight*age, data = Howell1,
                           prior = R2(0.5, what = "mean"), seed = 1234)

# Comparação de modelos
loo1 <- loo(at_howell_mod)
loo2 <- loo(at_howell_mod_2)
loo3 <- loo(at_howell_mod_3)
compare(loo1, loo2, loo3)

# Fazer uma análise completa usando os dados kidiq do rstanarm.

library(rstanarm)
data("kidiq")

# Correlação
library(corrr)
head(kidiq)
df <- kidiq %>% correlate() %>% rearrange()
rplot(df)
network_plot(df)
mod_1 <- stan_lm(kid_score ~ mom_iq + mom_hs, data = kidiq,
                           prior = R2(0.5, what = "mean"), seed = 1234)
mod_2 <- stan_lm(kid_score ~ mom_iq + mom_hs + mom_iq*mom_hs, data = kidiq,
                 prior = R2(0.5, what = "mean"), seed = 1234)
mod_3 <- stan_lm(kid_score ~ mom_iq + mom_age + mom_age*mom_hs, data = kidiq,
                 prior = R2(0.5, what = "mean"), seed = 1234)

pp_check(mod_1, "scatter", nreps = 3)
pp_check(mod_1, "residuals", nreps = 9)
pp_check(mod_1, nreps = 9)
pp_check(mod_1, "test")

pp_check(mod_2, "scatter", nreps = 3)
pp_check(mod_2, "residuals", nreps = 9)
pp_check(mod_2, nreps = 9)
pp_check(mod_2, "test")

# Comparação de modelos
loo1 <- loo(mod_1)
loo2 <- loo(mod_2)
loo3 <- loo(mod_3)
compare(loo1, loo2, loo3)

#par(mfrow=c(2,2))
#plot(mod_1) 

#yrep <- posterior_predict(mod_1)
#yrep_t <- table(yrep)
