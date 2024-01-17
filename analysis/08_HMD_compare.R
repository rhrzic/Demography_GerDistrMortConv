rm(list=ls())

require(tidyverse)

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


e0 <- readRDS("temp_data/e0.rds")

pop <- readRDS("temp_data/mid_year_pop.rds") %>%
  group_by(year, AGS, geo, sex) %>%
  summarise(pop = sum(pop))

e0 <- left_join(e0, pop, by = c("year", "AGS", "geo", "sex"))

e25.75 = readRDS("temp_data/e25.75.rds")
e25.75 <- left_join(e25.75, pop, by = c("year", "AGS", "sex"))

mort_est <- readRDS("temp_data/mort_est.rds")%>%
  mutate(sex = str_to_lower(sex),
         age = as.numeric(Age)-1)


## Comparison with HMD results

require(HMDHFDplus)

source("00e_HMD_login.R")
#This is a file containing the username and password for HMD


e0_tot <- HMDHFDplus::readHMDweb(
  CNTRY="DEUTNP",
  "E0per",
  username = myusername, 
  password = mypassword)


e0_tot <- e0_tot %>% 
  filter(Year %in% 1997:2016) %>%
  select(Year:Male) %>%
  pivot_longer(cols = Female:Male, names_to = "sex", values_to = "e0") %>%
  mutate(year = Year,
         sex = str_to_lower(sex), 
         east = "Total")


germany_est <- mort_est %>%
  left_join(., pop, by = c("year", "AGS", "sex")) %>%
  group_by(sex, year, age) %>%
  summarise(logmx = weighted.mean(`50%`, pop),
            up = weighted.mean(`97.5%`, pop),
            low = weighted.mean(`2.5%`, pop))

germany_e0_est <- germany_est %>%
  group_by(sex, year) %>%
  summarise(e0 = e0.fun(Kannisto(logmx)),
            up = e0.fun(Kannisto(up)),
            low = e0.fun(Kannisto(low)))

supp_figA3 = ggplot(germany_e0_est)+
  geom_line(aes(x = year, y = e0, color = str_to_title(sex)))+
  geom_line(aes(x = year, y = up, color = str_to_title(sex)), linetype = "dashed")+
  geom_line(aes(x = year, y = low, color = str_to_title(sex)), linetype = "dashed")+
  geom_line(data = e0_tot, aes(x = Year, y = e0, color = str_to_title(sex)), size = 2) +
  xlab("Year")+
  ylab("Life expectancy at birth")+
  theme_bw()+
  labs(color = NULL)

ggsave('figures/supp_figA3.png', supp_figA3)
ggsave('figures/supp_figA3.pdf', supp_figA3)


## 25-75

expos <- HMDHFDplus::readHMDweb(
  CNTRY="DEUTNP",
  "Exposures_1x1",
  username = myusername, 
  password = mypassword)

dths <- HMDHFDplus::readHMDweb(
  CNTRY="DEUTNP",
  "Deaths_1x1",
  username = myusername, 
  password = mypassword)

germany_hmd <-  left_join(expos, dths, by = c("Year", "Age")) %>%
  mutate(logmx_male = log(Male.y/Male.x),
         logmx_female = log(Female.y/Female.x)) %>%
  select(age = Age, year = Year, male = logmx_male, female = logmx_female) %>%
  pivot_longer(male:female, names_to = "sex", values_to = "logmx")


germany_hmd.e25.75 <- germany_hmd %>%
  filter(between(age, 25, 74), between(year, 1997, 2016)) %>%
  group_by(year, sex) %>%
  mutate(e25.75 = e25.75.fun(logmx))

germany_est.e25.75 <- germany_est %>%
  filter(between(age, 25, 74)) %>%
  group_by(year, sex) %>%
  summarise(e25.75 = e25.75.fun(logmx),
            up = e25.75.fun(up),
            low = e25.75.fun(low))

supp_figA5 = ggplot(germany_est.e25.75)+
  geom_line(aes(x = year, y = e25.75, color = str_to_title(sex)))+
  geom_line(aes(x = year, y = up, color = str_to_title(sex)), linetype = "dashed")+
  geom_line(aes(x = year, y = low, color = str_to_title(sex)), linetype = "dashed")+
  geom_line(data = germany_hmd.e25.75, aes(x = year, y = e25.75, color = str_to_title(sex)), size = 2) +
  xlab("Year")+
  ylab("Years lived 25-75")+
  theme_bw()+
  labs(color = NULL)

ggsave('figures/supp_figA5.png', supp_figA5)
ggsave('figures/supp_figA5.pdf', supp_figA5)