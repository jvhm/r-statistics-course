library(BayesVarSel)
data("mtcars")

# Detectar a quantidade de variáveis que seria melhor utilizar no modelo.
mt_sel <- Bvs(formula = "hp ~ .", data = mtcars,
              prior.betas = "Robust", prior.models = "Constant",
              time.test = TRUE)
plotBvs(mt_sel)


# ----- REGRESSÃO ROBUSTA ----- #

# Utiliza uma distribuição T (como no t-test), ao invés da distribuição normal.
# Com essa distribuição, é possível incluir os outliers.

# ----- REGRESSÃO QUARTIL ----- #

# Cálculo da regressão por quartis. Para funcionar, o erro não deve variar muito entre os quartis.

library(quantreg)
x <- seq(0, 100, length.out = 100) # variável independente
sig <- 0.1 + 0.05*x # variância não-constante
b_0 <- 6 # intercept
b_1 <- 0.1 # slope
set.seed(1) # reproduzir a próxima linha
e <- rnorm(100, mean = 0, sd = sig)
y <- b_0 + b_1*x + e
dat <- data.frame(x,y)
ggplot(dat, aes(x,y)) + geom_point() + geom_smooth(method = "lm")
ggplot(dat, aes(x,y)) + geom_point() + geom_quantile()
ggplot(dat, aes(x,y)) + geom_point() + geom_quantile(quantiles = 0.9)

# Criação da regressão
qs <- 1:9/10
#qr2 <- rq(y ~x, data = dat, tau = qs)
ggplot(dat, aes(x,y)) + geom_point() + geom_quantile(quantiles = qs)

# Testar se os quartis 0.25 e 0.75 são estatisticamente diferentes
q_reg_25 <- rq(y ~ x, data = dat, tau = 0.25)
q_reg_75 <- rq(y ~ x, data = dat, tau = 0.75)
anova(q_reg_25, q_reg_75)
# Pelo resultado do p-value (muitas estrelas), podemos ver que elas são muito diferentes.
# Ou seja, o modelo linear não é uma boa escolha nesse caso, a não ser
# que o que se deseja seja analisar apenas uma parte do gráfico, com um dos quartis.

data(barro)
library(lmtest)
bgtest(y.net ~ ., data = barro)
bgtest(y.net ~ mse2 + fse2 + lexp2, data = barro)


# ----- MODELOS LINEARES GENERALIZADOS  -----

# glm serve para rodar um modelo logístico generalizado
# No exemplo, regressão logística.
model <- glm(y ~ x, family = binomial(link = "logit"),
             data = Data_frame) # REGRESSÃO LOGÍSTICA

library(rstanarm)
data(wells)
wells$dist100 <- wells$dist/100

t_prior <- student_t(df = 7, location = 0, scale = 2.5)

# Stan_glm para estatística bayeseana
fitl <- stan_glm(switch ~ dist100, data = wells,
                 family = binomial(link = "logit"),
                 prior = t_prior, prior_intercept = t_prior,
                 chains = 2, cores = 2, seed = 1234)

fitl_a <- glm(switch ~ dist100, family = binomial(link = "logit"),
             data = wells)

# Predict pode ser usado com todos os GLMs
# Neste caso, passamos um valor e exibimos a sua probabilidade,
# que ficará entre 0 e 1 na regressão logística.
newdata <- data.frame(dist100 = mean(wells$dist100))
predict(fitl_a, newdata, type = "response")
predict(fitl_a, data.frame(dist100 = c(min(wells$dist100), max(wells$dist100))), type = "response")

# Checcagem do modelo com stan
pp_check(fitl)

# Testes com mtcars 

# Correlação
library(corrr)
rplot(correlate(mtcars))
rplot(correlate(select(mtcars, -am)))

head(mtcars)
fitl_a <- glm(vs ~ cyl, family = binomial(link = "logit"),
              data = mtcars)
newdata <- data.frame(cyl = c(min(mtcars$cyl), max(mtcars$cyl)))
predict(fitl_a, newdata, type = "response")

t_prior_b <- student_t(df = 2, location = 0, scale = 2.5)
fitl <- stan_glm(vs ~ cyl, data = mtcars,
                 family = binomial(link = "logit"),
                 prior = t_prior_b, prior_intercept = t_prior,
                 chains = 2, cores = 2, seed = 1234)
pp_check(fitl)

fitl_a_2 <- glm(am ~ cyl, family = binomial(link = "logit"),
              data = mtcars)
newdata <- data.frame(cyl = c(min(mtcars$cyl), max(mtcars$cyl)))
predict(fitl_a_2, newdata, type = "response")

t_prior_b <- student_t(df = 7, location = 0, scale = 2.5)
fitl_2 <- stan_glm(am ~ cyl + hp + wt + disp + drat + hp*cyl + wt*drat, data = mtcars,
                 family = binomial(link = "logit"),
                 prior = t_prior_b, prior_intercept = t_prior,
                 chains = 2, cores = 2, seed = 1234)
pp_check(fitl)


# ----- Regressão logística ordinal ----- #

library(MASS)
print(polr(tobgp ~ agegp + alcgp, data = esoph), digits = 1)

# Versao com stan
stan_polr(tobgp ~ agegp + alcgp, data = esoph,
          prior = R2(0.25), prior_counts = dirichlet(1),
          chains = 2, cores = 2, seed = 1234, iter = 200)