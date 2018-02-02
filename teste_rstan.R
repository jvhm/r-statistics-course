#=========== TESTE FUNCIONAMENTO RSTAN ===========#

library(rstanarm)
data(kidiq)
kidiq
modelo <- stan_lm(kid_score ~ mom_age, 
                  data = kidiq, 
                  prior = R2(location = 0.5, 
                             what="mean"))

summary(modelo)
launch_shinystan(modelo)