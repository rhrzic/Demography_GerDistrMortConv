rm(list=ls())

library(tidyverse)

geo_structure <- readRDS("temp_data/geo_structure.rds")
mortality <- readRDS("temp_data/mortality.rds") %>%
  mutate(deaths = deaths, mx = deaths/pop, mx = ifelse(mx == 0, 1e-6, mx), logmx = log(mx))

years <- unique(mortality$year)

mort_est <- data.frame()

for (this.year in years) {

  print(paste0("Starting ", this.year))

  file_to_load <- list.files("results", pattern = paste0("^result_", this.year, "-*"))

  load(paste0("results/",file_to_load))

  temp = plyr::adply(result$lambda, c(1,3,4), quantile, probs=c(.025, .10, .50, .90, .975), .id = c("Age", "AGS", "sex"))
  temp$year <- as.numeric(this.year)

  rm(result)

  mort_est <- rbind(mort_est, temp)

  rm(temp)
}

saveRDS(mort_est, "temp_data/mort_est.rds")


## Comparing to raw data

mort_est <- readRDS("temp_data/mort_est.rds")%>%
  mutate(sex = str_to_lower(sex),
         age = as.numeric(Age)-1)

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
  
  tmp = data.frame(logmx =  c(logmx, extrap_logmx), age = c(ages,extrap_ages))
  
  return(tmp)
}


test = mort_est %>%
  select(AGS, year, sex, `50%`) %>%
  nest_by(AGS, year, sex) %>%
  mutate(extrapolated = purrr::map(.x = data, .f = Kannisto)) %>%
  select(-data) %>%
  unnest(cols = c(extrapolated))

tmp = left_join(test, mortality, by = c('AGS', 'year', 'sex', 'age')) %>%
  rename(model_logmx = logmx.x, raw_logmx = logmx.y) %>%
  mutate(raw_logmx = ifelse(is.na(raw_logmx), zoo::na.locf(raw_logmx, na.rm = F), raw_logmx),
         age_group = case_when(age %in% 0:9 ~ 0,
                               age %in% 10:14 ~ 10,
                               age %in% 15:19 ~ 15,
                               age %in% 20:24 ~ 20,
                               age %in% 25:29 ~ 25,
                               age %in% 30:34 ~ 30,
                               age %in% 35:39 ~ 35,
                               age %in% 40:44 ~ 40,
                               age %in% 45:49 ~ 45,
                               age %in% 50:54 ~ 50,
                               age %in% 55:59 ~ 55,
                               age %in% 60:64 ~ 60,
                               age %in% 65:74 ~ 65, 
                               age %in% 75:119 ~ 75),
         lead_age = ifelse(age_group != 75, lead(age), 119)) %>%
  group_by(AGS, year, sex, age_group) %>%
  mutate(model_logmx_grp = mean(model_logmx),
         model_count_grp = exp(model_logmx_grp)*pop,
         diff_count = 100*(model_count_grp-deaths)/deaths) %>%
  ungroup()


ggplot()+
  geom_line(data = filter(tmp, AGS == '02000'), aes(x = age, y = model_logmx, color = year, group = year))+
  geom_segment(data = filter(tmp, AGS == '02000'), 
               aes(x = age_group, xend = lead_age, y = raw_logmx, yend = raw_logmx, color = year))+
  facet_grid(. ~ sex)


ggplot()+
  geom_col(data = filter(tmp, year == 2000 & AGS == '02000'), aes(x = age_group, y = diff_count))+
  facet_grid(. ~ sex)+
  ylab('Difference in estimated and observed death counts (%)')




pop <- readRDS("temp_data/mid_year_pop.rds") %>%
  group_by(year, AGS, geo, sex) %>%
  summarise(pop = sum(pop))

germany <- readRDS("temp_data/germany.rds") %>%
  pivot_longer(logsmoothed_female:logsmoothed_male, names_to = "sex") %>%
  mutate(sex = ifelse(sex == "logsmoothed_female", "female", "male")) %>%
  filter(Year %in% 1997:2016) %>%
  select(year = Year, age = Age, sex, lmx = value)

mortality <- mortality %>%
  mutate(deaths = deaths, mx = deaths/pop, mx = ifelse(mx == 0, 1e-6, mx), lmx = log(mx))

## Age-mortality schedule compared to raw data

sample <- sample(unique(mort_est$AGS), 1)
this.mort <- filter(mort_est, AGS == sample)
ggplot(this.mort, aes(x = age, group = year, colour = year)) +
  geom_line(aes(x = age, y = `50%`)) +
  geom_point(data = filter(mortality, AGS == sample), aes(x = age, y = lmx))+
  ggtitle(sample)+
  facet_grid(sex ~ .)

## Age-specific mortality in comparison with German rates

ggplot(filter(mort_est, age %in% c(0, 10, 20, 40, 60, 80))) +
  geom_line(aes(x = year, y = `50%`, group = AGS), size = 0.1, alpha = 0.5)+
  geom_line(data = filter(germany, age %in% c(0, 10, 20, 40, 60, 80)), aes(x = year, y = lmx), color = "red")+
  facet_grid(sex ~ age)

# Reconstructing the German average based on the estimate

mort_est %>%
  filter(age %in% c(0, 10, 20, 40, 60, 80)) %>%
  left_join(., pop, by = c("year", "AGS", "sex")) %>%
  group_by(sex, year, age) %>%
  summarise(lmx = weighted.mean(`50%`, pop),
            up = weighted.mean(`97.5%`, pop),
            down = weighted.mean(`2.5%`, pop)) %>%
  ggplot()+
    geom_line(aes(x = year, y = lmx))+
    geom_line(aes(x = year, y = up), linetype = "dashed")+
    geom_line(aes(x = year, y = down), linetype = "dashed")+
    geom_line(data = filter(germany, age %in% c(0, 10, 20, 40, 60, 80)), aes(x = year, y = lmx), color = "red")+
    facet_grid(sex ~ age)


supp_fig3 %>%
  left_join(., pop, by = c("year", "AGS", "sex")) %>%
  group_by(sex, year, age) %>%
  summarise(lmx = weighted.mean(`50%`, pop),
            up = weighted.mean(`97.5%`, pop),
            down = weighted.mean(`2.5%`, pop)) %>%
  ggplot()+
    geom_line(aes(x = age, y = lmx, color = sex)) +
    geom_line(aes(x = age, y = up, color = sex), linetype = "dashed") +
    geom_line(aes(x = age, y = down, color = sex), linetype = "dashed") +
    geom_line(data = germany, aes(x = age, y = lmx, group = sex))+
  facet_wrap(. ~ year)

ggsave("figures/supp_fig3.png", supp_fig3, width = 250, height = 190, unit = 'mm')


supp_fig4 %>%
  left_join(., pop, by = c("year", "AGS", "sex")) %>%
  group_by(sex, year, age) %>%
  summarise(lmx = weighted.mean(`50%`, pop),
            up = weighted.mean(`97.5%`, pop),
            low = weighted.mean(`2.5%`, pop)) %>%
  left_join(., germany, by = c("year", "age", "sex")) %>%
  mutate(lmx_diff = lmx.x-lmx.y) %>%
  ggplot(aes(x = year, y = age, fill = lmx_diff))+
  geom_raster()+
  scale_fill_gradient2()+
  facet_grid(. ~ str_to_title(sex))+
  xlab("Year")+
  ylab("Age")+
  labs(fill = "Difference in\nlog-mortality rates")+
  theme_bw()

ggsave("figures/supp_fig4.png", supp_fig4, width = 250, height = 190, unit = 'mm')
  
  