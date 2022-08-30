rm(list=ls())

library(tidyverse)
library(RcppRoll)

#Death counts

deaths <- read.csv("raw_data/deaths/12613-02-02-4.csv", header = FALSE, sep = ";", stringsAsFactors = F,
                   encoding = "latin1", na.strings = ".", skip = 7) %>%
  slice(., 1:(n()-4)) %>% 
  select(year = V1, AGS = V2, geo = V3, age = V4, male_deaths = V6, female_deaths = V7) %>%
  mutate(AGS = case_when(AGS == "02" ~ "02000",
                         AGS == "11" ~ "11000",
                         TRUE ~ AGS),
         year = as.numeric(year),
         geo = trimws(geo)) %>%
  filter(str_length(AGS) == 5 | AGS =="05334002") %>%
  pivot_longer(cols = male_deaths:female_deaths, names_to = "sex", names_pattern = "(.*)_deaths", values_to = "deaths")


#Missing data

missing_deaths = deaths %>%
  mutate(age = case_when(age %in% c("unter 1 Jahr") ~ 0,
                         age %in% c("1 bis unter 5 Jahre") ~ 1,
                         age %in% c("5 bis unter 10 Jahre") ~ 5,
                         age %in% c("10 bis unter 15 Jahre") ~ 10,
                         age %in% c("15 bis unter 20 Jahre") ~ 15,
                         age %in% c("20 bis unter 25 Jahre") ~ 20,
                         age %in% c("25 bis unter 30 Jahre") ~ 25,
                         age %in% c("30 bis unter 35 Jahre") ~ 30,
                         age %in% c("35 bis unter 40 Jahre") ~ 35,
                         age %in% c("40 bis unter 45 Jahre") ~ 40,
                         age %in% c("45 bis unter 50 Jahre") ~ 45,
                         age %in% c("50 bis unter 55 Jahre") ~ 50,
                         age %in% c("55 bis unter 60 Jahre") ~ 55,
                         age %in% c("60 bis unter 65 Jahre") ~ 60,
                         age %in% c("65 bis unter 70 Jahre") ~ 65,
                         age %in% c("70 bis unter 75 Jahre") ~ 70,
                         age %in% c("75 bis unter 80 Jahre") ~ 75,
                         age %in% c("80 bis unter 85 Jahre") ~ 80,
                         age %in% c("85 Jahre und mehr") ~ 85,
                         age == 'Insgesamt' ~ 99)) %>%
  mutate(count = ifelse(deaths == '-', 0, deaths))

summary_missing_deaths = missing_deaths %>%
  filter(is.na(count))

open_age_missing = summary_missing_deaths %>%
  filter(age > 74) %>%
  group_by(age, year, sex) %>%
  summarise(missing = n_distinct(geo))
  

censoring = summary_missing_deaths %>%
  filter(age < 74) %>%
  group_by(sex, year) %>%
  summarise(missing = n_distinct(geo))


# There are errors in the coding of zeros and missings. Normally . = missing and - = 0. However, for some reformed districts, - = missing. 
# Solution: if the Insgesamt category = 0 (impossible), then the whole district is missing. 

missing_total <- deaths %>%
  group_by(year, geo, AGS, sex) %>%
  filter(age == "Insgesamt") %>%
  mutate(missing_total = ifelse(deaths == "-", 1, 0)) %>%
  select(year, geo, AGS, sex, missing_total)

deaths_missing <- deaths %>%
  left_join(., missing_total, by = c("year", "geo", "AGS", "sex")) %>%
  mutate(deaths = ifelse(missing_total == 1, NA, deaths))

deaths_missing %>%
  group_by(geo, age, sex, year) %>%
  summarise(count = case_when(is.na(deaths) ~ "Missing",
                              deaths == '-' ~ "Possibly 0",
                              deaths > 0 ~ "More than 0")) %>%
  ggplot(aes(x = geo, y = age, fill = count))+
  facet_grid(sex ~ year)+
  geom_raster()

#In addition, some low death counts are censored (.) (e.g., young age missings for individual ages)
#Solution: in districts where the insgesamt is known, set missings to 1.5 for ages under 75

deaths_missing_2 <- deaths_missing %>%
  mutate(deaths = ifelse(is.na(deaths) & !(age %in% c("75 bis unter 80 Jahre", "80 bis unter 85 Jahre", "85 Jahre und mehr")) & missing_total == 0, "-", deaths))

deaths_missing_2 %>%
  group_by(geo, age, sex, year) %>%
  summarise(count = case_when(is.na(deaths) ~ "Missing",
                              deaths == '-' ~ "Possibly 0",
                              deaths > 0 ~ "More than 0")) %>%
  ggplot(aes(x = geo, y = age, fill = count))+
  facet_grid(sex ~ year)+
  geom_raster()

deaths <- deaths_missing_2 %>%
  mutate(deaths = ifelse(deaths == "-", 1.5, deaths),
         deaths = as.numeric(deaths))


#Now to handle the districts with missings at 75+

deaths_total <- deaths %>%
  filter(age == "Insgesamt") %>%
  mutate(deaths_total = deaths*1) %>%
  select(-deaths)

deaths_redistribute <- deaths %>% 
  filter(age != "Insgesamt") %>%
  group_by(year, AGS, geo, sex) %>%
  summarise(deaths_sum = sum(deaths, na.rm = T)) %>%
  left_join(., deaths_total, by = c("year", "AGS", "geo", "sex")) %>%
  mutate(deaths_diff = deaths_total-deaths_sum,
         to_redistribute = ifelse(deaths_diff == 0, 0, 1)) %>%
  select(year, AGS, geo, sex, deaths_diff, to_redistribute)

deaths_redistributed <- deaths %>%
  filter(age != "Insgesamt") %>%
  mutate(age = case_when(age %in% c("unter 1 Jahr", "1 bis unter 5 Jahre", "5 bis unter 10 Jahre") ~ 0,
                         age %in% c("10 bis unter 15 Jahre") ~ 10,
                         age %in% c("15 bis unter 20 Jahre") ~ 15,
                         age %in% c("20 bis unter 25 Jahre") ~ 20,
                         age %in% c("25 bis unter 30 Jahre") ~ 25,
                         age %in% c("30 bis unter 35 Jahre") ~ 30,
                         age %in% c("35 bis unter 40 Jahre") ~ 35,
                         age %in% c("40 bis unter 45 Jahre") ~ 40,
                         age %in% c("45 bis unter 50 Jahre") ~ 45,
                         age %in% c("50 bis unter 55 Jahre") ~ 50,
                         age %in% c("55 bis unter 60 Jahre") ~ 55,
                         age %in% c("60 bis unter 65 Jahre") ~ 60,
                         age %in% c("65 bis unter 70 Jahre", "70 bis unter 75 Jahre") ~ 65,
                         age %in% c("75 bis unter 80 Jahre", "80 bis unter 85 Jahre", "85 Jahre und mehr") ~ 75)) %>%
  group_by(year, AGS, geo, age, sex) %>%
  summarise(deaths = sum(deaths)) %>%
  left_join(., deaths_redistribute, by = c("year", "AGS", "geo", "sex")) %>%
  mutate(deaths = ifelse(is.na(deaths) & to_redistribute == 1, deaths_diff, deaths))

deaths_redistributed %>%
  mutate(deaths = ifelse(deaths == 0, 1e-1, deaths)) %>%
  ggplot(aes(x = geo, y = age, fill = log(deaths)))+
  facet_grid(sex ~ year)+
  geom_raster()
  

#Implementing reforms to recreate the district state in 2017

reformed_areas <- c("13071", "13072", "13076", "13074", "13073", "13075", "15001", "15086", "15083",  "15089", 
                    "15085", "15087", "15091", "15082", "15088", "15084", "05334", "03159")

redundant_areas <- c("15303", "15202", "15370", "15363", "14272", "14292", "14264",
                     "14171", "14191", "14188", "14181", "14284", "14286", "14263",
                     "14379", "14383", "14280", "14285", "14182", "14177", "14375",
                     "14374", "14389", "14287", "14290", "14178", "14166", "14173",
                     "14193", "14167", "14161", "14262", "14365", "13002", "13056", 
                     "13055", "13052", "13051", "13053", "13060", "13054", "13058", 
                     "13006", "13005", "13057", "13061", "13001", "13059", "13062",
                     "15101", "15358", "15362", "15355", "15153", "15352", "15367",
                     "15357", "15369", "15364", "15171", "15151", "15159", "15154",
                     "15265", "15261", "15256", "15268", "05354", "05334002", "15266",
                     "15260", "03152", "03156")

deaths_reformed <- deaths_redistributed %>%
  mutate(code_new = case_when(AGS %in% c("13002", "13056", "13055", "13052") ~ "13071",
                              AGS %in% c("13051", "13053") ~ "13072",
                              AGS %in% c("13060", "13054") ~ "13076",
                              AGS %in% c("13058", "13006") ~ "13074",
                              AGS %in% c("13005", "13057", "13061") ~ "13073",
                              AGS %in% c("13001", "13059", "13062") ~ "13075",
                              AGS %in% c("15101") ~ "15001",
                              AGS %in% c("15358") ~ "15086",
                              AGS %in% c("15362", "15355") ~ "15083",
                              AGS %in% c("15153", "15352", "15367") ~ "15089",
                              AGS %in% c("15357", "15369", "15364") ~ "15085",
                              AGS %in% c("15171") ~ "15091",
                              AGS %in% c("15151", "15159", "15154") ~ "15082",
                              AGS %in% c("15266", "15260") ~ "15087",
                              AGS %in% c("15265", "15261") ~ "15088",
                              AGS %in% c("15256", "15268") ~ "15084",
                              AGS %in% c("05354", "05334002") ~ "05334",
                              AGS %in% c("03152", "03156") ~ "03159",
                              TRUE ~ AGS)) %>%
  group_by(year, code_new, age, sex) %>%
  mutate(deaths_new = ifelse((is.na(deaths) & AGS %in% reformed_areas), sum(deaths, na.rm = T), deaths)) %>% 
  filter(!(AGS %in% redundant_areas)) %>%
  select(year, AGS = code_new, geo, age, sex, deaths = deaths_new)


deaths_reformed %>%
  mutate(deaths = ifelse(deaths == 0, 1e-1, deaths)) %>%
  ggplot(aes(x = geo, y = age, fill = log(deaths)))+
  facet_grid(sex ~ year)+
  geom_raster()


## Pooling deaths to match with exposures

deaths_pooled <- deaths_reformed %>%
  group_by(AGS, geo, age, sex) %>%
  mutate(deaths_pool = RcppRoll::roll_sum(deaths, n = 3L, align = "center", fill = NA)) %>%
  select(year, AGS, geo, age, sex, deaths = deaths_pool) %>%
  filter(year %in% 1996:2016)

saveRDS(deaths_pooled, file = "temp_data/deaths.rds")

