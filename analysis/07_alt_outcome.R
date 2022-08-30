rm(list=ls())
library(tidyverse)

geo_structure <- readRDS("temp_data/geo_structure.rds")

mort_est <- readRDS("temp_data/mort_est.rds")%>%
  mutate(sex = str_to_lower(sex),
         age = as.numeric(Age)-1) %>%
  filter(between(age, 25, 74))
  

# Function to estimate life expectancy at birth based on log_e death rates mx
# more or less following Preston et al. (2001)

e25.75.fun <- function(logmx) {
  logmx = c(logmx, 0)
  mx = exp(logmx)
  ax = rep(0.5)
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


e25.75 <- mort_est %>%
  group_by(year, sex, AGS) %>%
  summarise(e25.75 = e25.75.fun(`50%`),
         e25.75_up = e25.75.fun(`2.5%`),
         e25.75_low = e25.75.fun(`97.5%`))

ggplot(e25.75, aes(x = year, group = interaction(AGS, sex)))+
  geom_line(aes(y = e25.75))

saveRDS(e25.75, "temp_data/e25.75.rds")
