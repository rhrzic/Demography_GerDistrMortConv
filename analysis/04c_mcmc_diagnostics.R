require(rstan)
require(bayesplot)
require(cowplot)

rm(result)

file_to_load <- list.files("results", pattern = "^result_*")

which.file = file_to_load[sample(1:length(file_to_load), 1)]

print(which.file)

load(paste0("results/", which.file))


#spline alpha values

which.trace = paste0("alpha[", sample(1:2, 1), ",", sample(1:7, 1), ",", sample(1:401, 1), "]")

a = mcmc_trace(result$fit, pars =which.trace)
b =mcmc_acf(result$fit, pars =which.trace)
c =mcmc_hist(result$fit, pars = which.trace)
d =mcmc_dens_overlay(result$fit, pars = which.trace)

plot_grid(a,b,c,d, nrow =2, align = "hv", axis = "tblr")

#sex difference

a= mcmc_trace(result$fit, pars = 'sigma_sex')
b=mcmc_acf(result$fit, pars ='sigma_sex')
c=mcmc_hist(result$fit, pars = 'sigma_sex')
d=mcmc_dens_overlay(result$fit, pars = 'sigma_sex')

plot_grid(a,b,c,d, nrow =2, align = "hv", axis = "tblr")


#hierarchy

a=mcmc_trace(result$fit, pars = 'sigma_hier[1]')
b=mcmc_acf(result$fit, pars ='sigma_hier[1]')
c=mcmc_hist(result$fit, pars = 'sigma_hier[1]')
d=mcmc_dens_overlay(result$fit, pars = 'sigma_hier[1]')

plot_grid(a,b,c,d, nrow =2, align = "hv", axis = "tblr")

a=mcmc_trace(result$fit, pars = 'sigma_hier[2]')
b=mcmc_acf(result$fit, pars ='sigma_hier[2]')
c=mcmc_hist(result$fit, pars = 'sigma_hier[2]')
d=mcmc_dens_overlay(result$fit, pars = 'sigma_hier[2]')

plot_grid(a,b,c,d, nrow =2, align = "hv", axis = "tblr")

