rm(list=ls())

library(tidyverse)
library(parallel)
library(splines)
library(rstan)

geo_structure <- readRDS("temp_data/geo_structure.rds")
mortality <- readRDS("temp_data/mortality.rds")
priors.sex <- readRDS("temp_data/sex_diff.rds")
east_west <- readRDS("temp_data/east_west.rds")

nkreis <- length(unique(mortality$AGS))
ngroup <- length(unique(mortality$age))

load("temp_data/HZmatrices.Rdata")

S1 = t(H1 %*% Z1)
S2 = t(H2 %*% Z2)

#Reference mortality rates and main dataset

germany <- readRDS("temp_data/germany.rds")

mortality <- mortality %>%
  mutate(deaths = deaths, 
         sex = ifelse(sex == "male", 1, 2))


#spline basis
  
age  <-  0:89
nage <-  length(age)

B  <-  bs(age, knots=c(0,1,10,20,40,70), degree=1)

nalpha = ncol(B)

#correct order

target_order = geo_structure$AGS

#age groups

W = matrix(0, nrow=ngroup, ncol=nage)

boundary_ages = c(unique(mortality$age), max(mortality$age)+15)
ix = cbind(as.numeric( cut( age, breaks = boundary_ages, right=FALSE) ), seq(age) )

W[ix] = 1
W = prop.table(W,margin=1)

## Start per year loop

years <- unique(mortality$year)
#this.year <- 2003

for (this.year in years) {
  
  #preparing datasets
  
  mortality_data <- filter(mortality, year == this.year)
  
  germany_data <- filter(germany, Year == this.year)
  
  lambda_star <- cbind(germany_data$logsmoothed_male, 
                       germany_data$logsmoothed_female)
  
  std_sex_diff <- filter(priors.sex, Year == this.year) %>%
    select(GDR.smooth,FRG.smooth,DEU.smooth)
  
  std_sex_diff  <- as.matrix(std_sex_diff)
  
  #ordering
  
  this.pop <- mortality_data %>%
    mutate(ix = match(AGS, target_order))
  
  this.pop = arrange(this.pop, ix, age, sex)
  
  D = array(this.pop$deaths, c(2,ngroup,nkreis))
  N = array(this.pop$pop, c(2,ngroup,nkreis))
  
  #sex diff prior
  
  regime <-  east_west$region
  dstar <-  std_sex_diff[, regime]
  
  #stan data
  
  stanDataList <- list(
    A=nage,
    G=ngroup,
    R=nkreis,
    K=nalpha,
    
    B = B,
    
    n1 = nrow(S1),
    n2 = nrow(S2),

    S1 = S1,
    S2 = S2,
    
    W = W,

    lambda_star = lambda_star,
    dstar = dstar,

    N = N,
    D = D
  )
  
  #stan model
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = 4)
  
  this.model = stan_model(file = 'analysis/04b_main_model.stan', 
                          model_name = 'main_model')
  
  #stan initialization
  
  A = nage
  R = nkreis
  K = nalpha
  
  stanInit = function() {
    delta = .02
    n1 = nrow(S1)
    n2 = nrow(S2)

    list(
      eps1 = array( runif(2*K*n1, -delta, +delta), c(2,K,n1)),
      eps2 = array( runif(2*K*n2, -delta, +delta), c(2,K,n2)),
      mu  = matrix( runif(2*K,-delta,+delta), 2, K),
      sigma_hier = runif(2,.01,.10),
      sigma_sex  = runif(1,.01,.10)
    )
  }
  
  nchain  =  4  
  niter   =  700  
  nwarmup =  200  
  
  fit = sampling(this.model, data=stanDataList, init=stanInit,
                 pars=c('alpha','sigma_hier', 'sigma_sex'),
                 chains=nchain, 
                 iter=niter, 
                 warmup=nwarmup,
                 refresh=25,
                 control=list(adapt_delta  = 0.90, 
                              max_treedepth= 10,
                              stepsize     = 5e-4)
  )
  
  AGSvals = unique(geo_structure$AGS)
  
  nsim = nrow(as.matrix(fit))
  
  z = array(as.matrix(fit,'alpha'), c(nsim, 2,K, R),
            dimnames=list(paste0('sample',1:nsim),
                          c('Male','Female'),NULL, AGSvals))
  
  L = array(NA, c(nsim, A, R,2),
            dimnames=list(paste0('sample',1:nsim), age,
                          AGSvals, c('Male','Female')))
  
  
  M_standard = stanDataList$lambda_star[,1]
  F_standard = stanDataList$lambda_star[,2]
  
  for (this.samp in 1:nsim) {
    for (this.ags in paste(AGSvals)) {
      L[this.samp,,this.ags,'Male']   = M_standard + B %*% z[this.samp,'Male',,this.ags]
      L[this.samp,,this.ags,'Female'] = F_standard + B %*% z[this.samp,'Female',,this.ags]
      
    }
  }
  
  stub = paste0('result_',this.year)
  path  = paste0('results/', stub)
  fname = paste0( paste( path,format(Sys.time(), "%Y-%m-%d-%H%M"),sep='-'), '.Rdata')
  
  pretty_std = matrix(c(M_standard,F_standard), nrow=90, ncol=2,
                       dimnames=list(age, c('Male','Female')))
  
  pretty_L = aperm(L, c(2,1,3,4))  # A x nsim x R x 2
  
  result = list(standard= pretty_std,
                lambda  = pretty_L,
                type='sdp.hier.nocar.mcmc',
                fit=fit)  
  
  save(result, file=fname)
}
