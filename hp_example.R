################# CODE EXAMPLE: HP UNIVARIATE ESTIMATION VIA BAYESMORTALITYPLUS PACKAGE ################
library(BayesMortalityPlus)

### prior estimatives for the parameters: BREMS parameter estimatives
prior = apply(brems, 2, median)[5:6]; prior[2] = prior[2]

#### Midwest ----
### data
Ex = CO_exposure[19:92] # exposure
Dx = CO_deaths[19:92] # death counts
x = 18:91 # age interval

### fitting the reduced heligman-pollard curve (A = B = C = 0): age interval (x), exposure and death counts (Ex and Dx), mean and variance priors for the parameters (m and v), iterations (M), burn-in (bn), thinning (thin), initial points (inits) and K = BREMS' K parameter of choice 
fit = hp(x = x, Ex = Ex, Dx = Dx,
         m = c(NA, NA, NA, 1e-4, prior[1], prior[2], NA, NA),
         v = c(NA, NA, NA, 1e-8, 1, 0.1, NA, NA),
         M = 200000, bn = 100000, thin = 100, inits = c(rep(0.5, 4), 5, 20, 0.5, 1.1),
         K = -0.8, reduced_model = T)

### model diagnostics
fit$summary
plot_chain(fit)
plot(fit)

### retrieving death probabillities and quantile estimatives:
qx_co <- fitted(fit, prob = 0.95)


#### North ----
### data
Ex = N_exposure[19:92] # exposure
Dx = N_deaths[19:92] # death counts
x = 18:91 # age interval

fit = hp(x = x, Ex = Ex, Dx = Dx,
         m = c(NA, NA, NA, 1e-4, prior[1], prior[2], NA, NA),
         v = c(NA, NA, NA, 1e-9, 1, 0.1, NA, NA),
         M = 200000, bn = 100000, thin = 100, inits = c(rep(0.5, 4), 5, 20, 0.5, 1.1),
         K = -0.8, reduced_model = T)

fit$summary
plot_chain(fit)
plot(fit)

qx_n <- fitted(fit, prob = 0.95)

#### Northeast ----
### data
Ex = NE_exposure[19:92] # exposure
Dx = NE_deaths[19:92] # death counts
x = 18:91 # age interval

fit = hp(x = x, Ex = Ex, Dx = Dx,
         m = c(NA, NA, NA, 1e-4, prior[1], prior[2], NA, NA),
         v = c(NA, NA, NA, 5e-9, 1, 0.1, NA, NA),
         M = 200000, bn = 100000, thin = 100, inits = c(rep(0.5, 4), 5, 20, 0.5, 1.1),
         K = -0.8, reduced_model = T)

fit$summary
plot_chain(fit)
plot(fit)

qx_ne <- fitted(fit, prob = 0.95)


#### South ----
### data
Ex = S_exposure[19:92] # exposure
Dx = S_deaths[19:92] # death counts
x = 18:91 # age interval

fit = hp(x = x, Ex = Ex, Dx = Dx,
         m = c(NA, NA, NA, 5e-4, prior[1], prior[2]+1, NA, NA),
         v = c(NA, NA, NA, 1e-8, 1, 0.1, NA, NA),
         M = 200000, bn = 100000, thin = 100, inits = c(rep(0.5, 4), 5, 20, 0.5, 1.1),
         K = -0.8, reduced_model = T)

fit$summary
plot_chain(fit)
plot(fit)

qx_s <- fitted(fit, prob = 0.95)

#### Southeast ----
### data
Ex = SE_exposure[19:92] # exposure
Dx = SE_deaths[19:92] # death counts
x = 18:91 # age interval

fit = hp(x = x, Ex = Ex, Dx = Dx,
         m = c(NA, NA, NA, 3e-4, prior[1], prior[2], NA, NA),
         v = c(NA, NA, NA, 1e-8, 0.25, 0.1, NA, NA),
         M = 200000, bn = 100000, thin = 100, inits = c(rep(0.5, 4), 5, 20, 0.5, 1.1),
         K = -0.8, reduced_model = T)

fit$summary
plot_chain(fit)
plot(fit)

qx_se <- fitted(fit, prob = 0.95)