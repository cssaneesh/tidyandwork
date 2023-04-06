# Coding club brms 
# Benjamin Rosenbaum
# benjamin.rosenbaum@idiv.de
# 04.04.2023

# topics covered in this brief tutorial:
# linear (mixed) models, ANOVA, ANCOVA, nonlinear models

# other cool stuff available in brms but not covered in this tutorial:
# generalized linear (mixed) models (GLMM), generalized additive models (GAM), 
# Gaussian processes, autocorrelation (spatial & temporal),
# multivariate models (= multiple responses), phylogenetic models, and many more!

# further reading:
# brms website: https://paul-buerkner.github.io/brms/
# especially vignettes: https://paul-buerkner.github.io/brms/articles/ 
# Solomon Kurz translated Richard McElreath's 'Statistical Rethinking' in brms:
# https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/ 


rm(list=ls())

library("brms")
library("palmerpenguins")
library("ggplot2")
library("emmeans")
library("rstan")
library("BayesianTools")
library("sjPlot")

df.peng = as.data.frame(penguins)
str(df.peng)

# Regression -------------------------------------------------------------------

ggplot( aes(x=body_mass_g, y=flipper_length_mm), data=df.peng) + 
  geom_point()

model.1 = brm( flipper_length_mm ~ body_mass_g, data=df.peng )

summary(model.1) # brm
model.1$fit      # rstan

# check convergence
plot(model.1)            # brm
stan_trace(model.1$fit)  # rstan
samples = As.mcmc.list(model.1$fit) # coda
plot(samples[, 1:3])

# pairwise marginal plots
pairs(model.1) # brm
pairs(model.1$fit, verInd=1:3, horInd=1:3) # rstan
correlationPlot(as.matrix(model.1$fit)[, 1:3], thin=1) # BayesianTools

# stan code
stancode(model.1)

# plots
plot(conditional_effects(model.1),
     points = TRUE)

plot(conditional_effects(model.1,
                         spaghetti = TRUE,
                         ndraws = 20),
     points = TRUE)

plot(conditional_effects(model.1,
                         method = "posterior_predict"),
     points = TRUE)

# hypothesis
hypothesis(model.1, "body_mass_g>0", class="b")

# posterior predictive check (distr. of data and distr. of predicted data)
pp_check(model.1, ndraws=100)
# observed vs predicted
pp_check(model.1, ndraws=100, type="scatter_avg" )

# fitted and predicted values
fit.model.1 = fitted(model.1)
head(fit.model.1)
pred.model.1 = predict(model.1)
head(pred.model.1)

# observed vs fitted plot
plot(fit.model.1[, 1], df.peng$flipper_length_mm, xlab="pred", ylab="obs")
# data contained some NAs: can remove NAs here, but better take care of that before model fitting!
plot(fit.model.1[, 1], na.omit(df.peng$flipper_length_mm), xlab="pred", ylab="obs")
abline(0,1, col="red")

# residual vs fitted plot
plot(fit.model.1[, 1], na.omit(df.peng$flipper_length_mm)-fit.model.1[, 1], xlab="pred", ylab="residual")
abline(0,0, col="red")

# extract posterior
as.matrix(model.1)

# priors ?
model.1$prior

# intercept and sigma priors are chosen automatically from the data
# attention when overwriting Intercept prior: predictors are internally mean-centered
# Hence this definition of intercept would be on the wrong scale
priors = c(prior(normal(0.02,0.01), class=b, coef=body_mass_g),
           prior(normal(0,10), class=Intercept))

# you can prevent that from using "y ~ 0 + Intercept + x"
priors = c(prior(normal(0.02,0.01), class=b, coef=body_mass_g),
           prior(normal(0,20), class=b, coef=Intercept))

model.1.p = brm( flipper_length_mm ~ 0 + Intercept + body_mass_g, data=df.peng,
                 prior = priors)
summary(model.1.p)
model.1.p$prior
stancode(model.1.p)

# ANOVA ------------------------------------------------------------------------

# remove NAs now
df.peng = subset(df.peng, !is.na(sex))

# difference in flipper length is the function of male and females of three different speices.
ggplot(data=df.peng, aes(y=flipper_length_mm, x=species, colour=sex)) +
  geom_boxplot()

# no interaction model
model.2 = brm( flipper_length_mm ~ species + sex, data=df.peng )
summary(model.2)
 
plot(conditional_effects(model.2))

plot(conditional_effects(model.2, effects = "species:sex"))

# extraction of est. group means and contrasts
emmeans(model.2, ~species)
pairs(emmeans(model.2, ~species))
emmeans(model.2, ~sex)
pairs(emmeans(model.2, ~sex))

# these are the actual fitted group means, they are restricted by "species + sex"
emmeans(model.2, ~species:sex)

# interaction model
model.3 = brm(flipper_length_mm ~ species * sex, data = df.peng)
summary(model.3)

plot(conditional_effects(model.3, "species:sex"))

# model comparison to check for the interaction 
LOO(model.3, model.2)

# ANCOVA additive --------------------------------------------------------------

# scale continuous predictor
df.peng$mass_z = as.numeric(scale(df.peng$body_mass_g))

ggplot(aes(x = mass_z, y = flipper_length_mm, color = species), data = df.peng) +
  geom_point()

model.4 = brm( flipper_length_mm ~ mass_z + species, data=df.peng )

summary(model.4) 

plot(model.4)

plot(conditional_effects(model.4), 
     # points=TRUE, 
     ask=FALSE) # only plots categorical predictors

plot(
  conditional_effects(
    model.4,
    effects = "mass_z",
    conditions = data.frame(species = levels(df.peng$species))
  ),
  points = TRUE,
  ask = FALSE
) 

plot_model(model.4, # from sjplot package
           type="pred", 
           terms=c("mass_z", "species"), 
           show.data = TRUE) 

# group-level intercepts and their contrasts
emmeans(model.4, ~species)
pairs( emmeans(model.4, ~species) )
# emtrends(model.2, ~species, var="mass_z")

# model comparison
model.5 = brm( flipper_length_mm ~ mass_z, data=df.peng ) # the old regression model

pp_check(model.4, ndraws=100)
pp_check(model.5, ndraws=100)

LOO(model.4, model.5)

bayes_R2(model.4)
bayes_R2(model.5)

# ANCOVA interaction -----------------------------------------------------------

ggplot( aes(x=mass_z, y=flipper_length_mm, color=species), data=df.peng) + 
  geom_point()

model.6 = brm(flipper_length_mm ~ mass_z * species, data = df.peng)

summary(model.6)
plot(model.6)

plot(conditional_effects(model.6),
     points = TRUE,
     ask = FALSE)

plot(
  conditional_effects(
    model.6,
    effects = "mass_z",
    conditions = data.frame(species = levels(df.peng$species))
  ),
  points = TRUE,
  ask = FALSE
)

plot_model(model.6, # from sjplot package
           type="pred",
           terms=c("mass_z", "species"),
           show.data = TRUE)

# group-level slopes and their contrasts
emtrends(model.6, ~species, var="mass_z")
pairs( emtrends(model.6, ~species, var="mass_z") )

pp_check(model.6, ndraws=100)

LOO(model.4, model.6)

bayes_R2(model.4)
bayes_R2(model.6)

# LMM ------------------------------------------------------------------------

ggplot( aes(x=mass_z, y=flipper_length_mm, color=species), data=df.peng) + 
  geom_point()

model.7 = brm( flipper_length_mm ~ mass_z + (1+mass_z | species), data=df.peng, cores=4) # chains computed in parallel with cores=
model.7 = brm( flipper_length_mm ~ mass_z + (1+mass_z | species), data=df.peng, cores=4, 
               control = list(adapt_delta = 0.9) ) # increase sampler accuracy, but will be slower
summary(model.7)
ranef(model.7)

model.7$prior
print(prior_summary(model.7, all = FALSE), show_df = FALSE)

plot(model.7)

plot( conditional_effects(model.7),
      points=TRUE ) 
plot( conditional_effects(model.7,
                          effects = "mass_z",
                          re_formula = NULL,
                          conditions = data.frame(species=levels(df.peng$species)) ),
      points=TRUE) 
plot_model(model.7, # from sjplot package
           type="pred", 
           show.data = TRUE) 
plot_model(model.7, 
           type="pred", 
           terms=c("mass_z", "species"),
           pred.type="re",
           show.data = TRUE) 

# posterior predictive checks
pp_check(model.7, ndraws=100)
pp_check(model.7, ndraws=100, type="scatter_avg" )

# fitted and predicted values, extract manually
fit.model.7.1 = fitted(model.7) # includes group-level effect
fit.model.7.2 = fitted(model.7, re_formula=NA)  # excludes group-level effect (fixed effects only)

plot(fit.model.7.1[, 1], df.peng$flipper_length_mm, xlab="pred (random & fixed effects)", ylab="obs")
abline(0,1, col="red")
plot(fit.model.7.2[, 1], df.peng$flipper_length_mm, xlab="pred (only fixed effects)", ylab="obs")
abline(0,1, col="red")

# NLM --------------------------------------------------------------------------

# Functional response = density dependent feeding rate of a consumer
# N = resource density (independent variable)
# a = search rate
# h = handling time
# F(N) = a*N/(1+a*h*N) nonlinear function

# simulate feeding experiments with different numbers of resource offered
set.seed(20)
n = 100
a = 0.5
h = 1/20
df.feed = data.frame(Nstart = sample(1:100, n, replace=TRUE))
df.feed$Neaten = a*df.feed$Nstart/(1+a*h*df.feed$Nstart)
df.feed$Neaten = rpois(n, df.feed$Neaten)

ggplot( aes(x=Nstart, y=Neaten), data=df.feed) + 
  geom_point() +
  xlim(0, 100) + ylim(0,25) +
  stat_function(fun = function(x) a*x/(1+a*h*x) )

# custom model formula for brms
formula.1 = bf(Neaten ~ a*Nstart/(1.0+a*h*Nstart),
               nl=TRUE, # nonlinear
               a~1,     # parameters (~1 means: do not depend on additional covariates)
               h~1)     # parameters

# priors for parameters of nonlinear function
priors = c(prior(exponential(1), nlpar="a", lb=0),  # lower bound at 0
           prior(exponential(2), nlpar="h", lb=0) ) # lower bound at 0

# call brm with custom model formula
model.8 = brm(formula.1, data=df.feed, prior=priors, cores=4)

summary(model.8)

plot(model.8)

plot(conditional_effects(model.8), 
     points=TRUE) 
plot(conditional_effects(model.8,
                         spaghetti=TRUE,
                         ndraws=100), 
     points=TRUE
)

plot(conditional_effects(model.8, method="posterior_predict"), 
     points=TRUE) 

pp_check(model.8, ndraws=100)
pp_check(model.8, ndraws=100, type="ecdf_overlay" )

# non-Gaussian residuals: Poisson
# specify "identity", because default Poisson link function is log
model.9 = brm(formula.1, data=df.feed, prior=priors, cores=4,
              family=poisson(link="identity")) 

summary(model.9)

plot(model.9)

plot(conditional_effects(model.9), 
     points=TRUE) 
plot(conditional_effects(model.9, method="posterior_predict"), 
     points=TRUE) 
pp_check(model.9, ndraws=100 )
pp_check(model.9, ndraws=100, type="ecdf_overlay" )

