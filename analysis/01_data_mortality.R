rm(list=ls())

library(tidyverse)

deaths <- readRDS("temp_data/deaths.rds")
pop <- readRDS("temp_data/mid_year_pop.rds")

mortality <- left_join(pop, deaths, by = c("year", "AGS", "geo", "age", "sex")) %>%
  filter(year %in% 1997:2016)

saveRDS(mortality, file = "temp_data/mortality.rds")

# Missing data and distribution checks

mortality <- readRDS("temp_data/mortality.rds")

#Replacing 0s with 1e-6 to avoid inf values

mortality <- mortality %>%
  mutate(deaths = deaths, mx = deaths/pop, mx = ifelse(mx == 0, 1e-6, mx), lmx = log(mx))

mortality %>%
  group_by(sex) %>%
  summarise(sum(!is.finite(lmx)))


#HMD

require(HMDHFDplus)
require(MortalitySmooth)

myusername <- #Insert your username
mypassword <- #Insert your password

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

#Raw age mortality schedule three ways

germany_hmd <- left_join(expos, dths, by = c("Year", "Age")) %>%
  mutate(Female.mx = Female.y/Female.x, Male.mx = Male.y/Male.x) %>%
  select(year = Year, age = Age, Female.mx, Male.mx) %>%
  pivot_longer(cols = Female.mx:Male.mx, names_to = c("sex"), names_pattern = "(.*).mx", values_to = "mx") %>%
  mutate(sex = str_to_lower(sex), lmx = log(mx)) %>%
  filter(year %in% 1997:2016 & age %in% 0:75)
  
mortality %>%
  group_by(sex, year, age) %>%
  summarise(mean_lmx = weighted.mean(lmx, pop)) %>%
  ggplot(aes(x = year, y = age, fill = mean_lmx))+
  geom_tile()+
  facet_grid(sex ~ .)

mortality %>%
  group_by(sex, year, age) %>%
  summarise(mean_lmx = weighted.mean(lmx, pop)) %>%
  ggplot()+
  geom_line(aes(x = age, y = mean_lmx, group = interaction(year, sex), color = sex, alpha = year))+
  geom_line(data = germany_hmd, aes(x = age, y = lmx, group = interaction(year, sex), color = sex, alpha = year), linetype = "dashed")

mortality %>%
  group_by(sex, year, age) %>%
  summarise(mean_lmx = weighted.mean(lmx, pop)) %>%
  ggplot() +
  geom_line(aes(x = year, y = mean_lmx, group = age, color = age))+
  facet_grid(sex ~ .)
