rm(list=ls())
library(tidyverse)

geo_structure <- readRDS("temp_data/geo_structure.rds")
mortality <- readRDS("temp_data/mortality.rds")

# Function to estimate life expectancy at birth based on log_e death rates mx
# more or less following Preston et al. (2001)

e0.fun <- function(logmx) {
  mx = exp(logmx)
  ax = rep(0.5, length(mx))
  ax[1] <- 0.1
  qx = mx/(1+(1-ax)*mx)
  qx[qx < 0] <- 0
  qx[qx > 1] <- 1
  px = 1 - qx
  lx <- 100000 * cumprod(c(1, px))[1:(length(px))]
  dx <- c(-diff(lx), lx[length(lx)])
  Lx <- c(lx[-1],0) + ax * dx
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx
  return(ex[1])
}  

# the Kannisto() function extends a logmx schedule to
# very old ages by fitting a regression to the logits
# of high ages

Kannisto = function(logmx, 
                    ages        = 0:89,        
                    fit_ages    = 70:89,
                    extrap_ages = 90:119) {
  
  fit_mx     = exp(logmx[ages %in% fit_ages])
  fit_logits = log(fit_mx / (1-fit_mx))
  logit_coef = coef( lm( fit_logits ~ fit_ages) )
  loga       = logit_coef[1]
  b          = logit_coef[2]
  
  extrap_logits = loga + b*extrap_ages
  extrap_logmx  = log( 1 / (1+exp(-extrap_logits)) )
  
  tmp        = c(logmx, extrap_logmx)
  names(tmp) = c(ages,extrap_ages)
  
  return(tmp)
} 

require(parallel)
parallel_processing = TRUE
ncores = 16
cl = makePSOCKcluster(ncores)
clusterExport(cl,list('e0.fun','Kannisto'))

years <- unique(mortality$year)

for (this.year in years) {
  
  print(paste0("Starting ", this.year))

  file_to_load <- list.files("results", pattern = paste0("^result_", this.year, "-*"))  

  load(paste0("results/",file_to_load))
  
  e0_array = parApply(cl, result$lambda, 2:4,
                      function(logmx) {
                        e0.fun( Kannisto(logmx) )
                      })
  
  rm(result)

# Q will initially be 7 x 402 x 2
  Q = apply(e0_array, 2:3, quantile, 
          probs=c(.025,.10,.50,.90,.975))
  
  rm(e0_array)

# for historical compatibility, desired output is 
# a 804 x 10 data frame, 
# ordered by AGS within sex
  e0.our.model = data.frame(
    AGS  = as.numeric(dimnames(Q)[[2]]),
    Sex  = rep(1:2, c(401,401)),
    sex = rep(c('male','female'), c(401,401)),
    pct025 = as.vector(Q['2.5%',,]),
    pct10 = as.vector(Q['10%',,]),
    pct50 = as.vector(Q['50%',,]),
    pct90 = as.vector(Q['90%',,]),
    pct975 = as.vector(Q['97.5%',,])
  )

  stub = paste0('e0_result_',this.year)
  path  = paste0('results/', stub)
  fname = paste0( paste( path,format(Sys.time(), "%Y-%m-%d-%H%M"),sep='-'), '.Rdata')

  save(e0.our.model, file=fname)
  
  rm(Q)
  rm(e0.our.model)
}
