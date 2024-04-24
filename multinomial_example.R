################# CODE EXAMPLE: MULTIVARIATE ESTIMATION VIA KDGLM PACKAGE ################
library(kDGLM)

x = 18:91  ## modelled age interval

## total exposure from every population by age
Ex_tot <- CO_exposure[x+1] + N_exposure[x+1] + NE_exposure[x+1] +
  S_exposure[x+1] + SE_exposure[x+1] 

## Defining Y, death count matrix. The last entry will be considered the reference by the model
Y <- cbind(CO_deaths[x+1], N_deaths[x+1],
           NE_deaths[x+1], S_deaths[x+1],
           SE_deaths[x+1])

## Defining exposure matrix, keeping Y's entry order
Ex <- cbind(CO_exposure[x+1],
            N_exposure[x+1],
            NE_exposure[x+1],
            S_exposure[x+1],
            SE_exposure[x+1])

## Defining model's offset argument
ofs <- Ex/Ex_tot

## Building the model
### Specifying the level for the model: four 2nd order polynomial blocks with one mean parameter (a) for each regional and discount factor (D) at ages
level <- polynomial_block(a = 1, order = 2, D = c(rep(0.99,4),rep(0.95,70)))*4 
### Fitting the model: level structure and multinomial structure for estimation: level predict names (p), truncated Y matrix (data) and offset
fitted_data <- fit_model(level,
                         morm = Multinom(p = level$pred.names, data = trunc(Y),
                                         offset = ofs))

## retrieving estimated model coefficients for death probabilities calculation
coef_model = coef(fitted_data, eval.pred = T)
names(coef_model)
param_dir = coef_model$conj.param$morm

## category probabilities (pi = alpha_i/sum(alpha))
pi = param_dir/rowSums(param_dir)

## retrieving alpha and beta parameters 
alphai = param_dir
betai = rowSums(param_dir) - param_dir

## lower quantiles
ql = sapply(1:5, function(i){ qbeta(0.025, alphai[,i], betai[,i]) })

## upper quantiles
qu = sapply(1:5, function(i){ qbeta(0.975, alphai[,i], betai[,i]) })

## death probabilities calculation with the BREMS table 
qx_se = pi[,5] * brems[x+1]/ofs[,5]
qx_co = pi[,1] * brems[x+1]/ofs[,1]
qx_n = pi[,2] * brems[x+1]/ofs[,2]
qx_ne = pi[,3] * brems[x+1]/ofs[,3]
qx_s = pi[,4] * brems[x+1]/ofs[,4]

## quantile calculations with the BREMS table quantiles
ql_se = ql[,5] * ci_brems$ql[x+1]/ofs[,5]
ql_co = ql[,1] * ci_brems$ql[x+1]/ofs[,1]
ql_n = ql[,2] * ci_brems$ql[x+1]/ofs[,2]
ql_ne = ql[,3] * ci_brems$ql[x+1]/ofs[,3]
ql_s = ql[,4] * ci_brems$ql[x+1]/ofs[,4]

qu_se = qu[,5] * ci_brems$qu[x+1]/ofs[,5]
qu_co = qu[,1] * ci_brems$qu[x+1]/ofs[,1]
qu_n = qu[,2] * ci_brems$qu[x+1]/ofs[,2]
qu_ne = qu[,3] * ci_brems$qu[x+1]/ofs[,3]
qu_s = qu[,4] * ci_brems$qu[x+1]/ofs[,4]
