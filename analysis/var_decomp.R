Theil <- function(df, ple) {
  
  y_ave <- mean(df[[ple]], na.rm = T)
  N <- length( df[[ple]] )
  
  Ttotal = (1 / N) * sum( (df[[ple]] / y_ave) * log(df[[ple]] / y_ave), na.rm = T)
  
  return(data.frame(Ttotal = Ttotal))
  #return(data.frame(Ttotal = Ttotal, Tnormal = Tnormal, N = N, Tmax = log(N)))
}


Theil_w <- function(df, ple, pop) {
  
  y_ave_w <- weighted.mean(df[[ple]], df[[pop]])
  N <- sum(df[[pop]])
  
  Ttotal =  sum( (df[[pop]] / N) * (df[[ple]] / y_ave_w) * log(df[[ple]] / y_ave_w) )
  
  return(data.frame(Ttotal = Ttotal))
}

onestage.Theil.decomp <- function(df, macroregion, ple) {
  
  y_ave <- mean(df[[ple]], na.rm = T)
  
  N <- length(df[[ple]])

  Ttotal = Theil(df, ple)[[1]]
  
  by_macroregion <- split(df, df[[macroregion]])
  
  Ni <- sapply(by_macroregion, function(x){length(x[[ple]])})
  yi_ave <- sapply(by_macroregion, function(x){mean(x[[ple]], na.rm = T)})

  Tmi <- sapply(by_macroregion, function(x){
    
    Theil(x,ple)[[1]]
  })
  
  Twithin <-  sum( (Ni/N) * (yi_ave/y_ave) * Tmi)
  Tbetween <-  sum( (Ni/N) * (yi_ave/y_ave) * log( yi_ave / y_ave ) )
  
  result <- data.frame(Tbetween = Tbetween, Twithin = Twithin, Ttotal = Ttotal, Additive = Twithin + Tbetween)
  
  return(result)
}

onestage.variance.decomp <- function(df, macroregion, ple) {
  
  y_ave <- mean(df[[ple]])
  N <- length(df[[ple]])
  var.total = var(df[[ple]])*((N-1)/N)
  
  
  by_macroregion <- split(df, df[[macroregion]])
  
  Ni <- sapply(by_macroregion, function(x){length(x[[ple]])})
  yi_ave <- sapply(by_macroregion, function(x){mean(x[[ple]])})
  vari <- sapply(by_macroregion, function(x){var(x[[ple]])})
  vari*((Ni-1)/Ni)
  
  
  var.within <-  sum( (Ni/N) * vari)
  
  var.between <-  sum( (Ni/N) * (yi_ave-y_ave)^2)
  
  
  result <- data.frame(var.total, var.within, var.between, Additive = var.within + var.between)
  
  return(result)
}