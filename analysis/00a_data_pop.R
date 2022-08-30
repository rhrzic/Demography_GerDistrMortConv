rm(list=ls())

library(tidyverse)
library(RcppRoll)

#Population size

population <- data.frame()

for (i in 1995:2019) {
  temp <- read.csv(paste0("raw_data/population/12411-02-03-4 (",i,").csv"), header = FALSE, sep = ";", stringsAsFactors = F,
                     encoding = "latin1", na.strings = c("-", "."), skip = 7) %>%
    slice(., 1:(n()-4))
  
  temp$year <- i
  population <- rbind(population, temp)
  rm(temp)
  print(i)
}

population <- population %>%
  select(year, AGS = V1, geo = V2, age = V3, male_pop = V5, female_pop = V6) %>%
  mutate(AGS = case_when(AGS == "02" ~ "02000",
                          AGS == "11" ~ "11000",
                         TRUE ~ AGS),
         geo = trimws(geo)) %>%
  filter(str_length(AGS) == 5 | AGS =="05334002",
         !age == "Insgesamt") %>%
  pivot_longer(cols = male_pop:female_pop, names_to = "sex", names_pattern = "(.*)_pop", values_to = "pop")


population %>%
  group_by(geo, year, sex) %>%
  summarise(missing = is.na(pop)) %>%
  ggplot(aes(x = geo, y = year, fill = missing))+
  geom_raster()+
  facet_grid(. ~ sex)

population %>%
  group_by(geo, age, sex) %>%
  summarise(missing = is.na(pop)) %>%
  ggplot(aes(x = geo, y = age, fill = missing))+
  geom_raster()+
  facet_grid(. ~ sex)

population %>%
  group_by(year, age, sex) %>%
  summarise(missing = is.na(pop)) %>%
  ggplot(aes(x = year, y = age, fill = missing))+
  geom_raster()+
  facet_grid(. ~ sex)

# Population is only missing as a result of administrative reforms. No difference between sexes. All available ages are fully accounted for.

# Merging age categories:
# 0-9.9; 10-14.9; 15-19.9, ... 70-74.9, 75+

population <- population %>%
  mutate(age = case_when(age %in% c("unter 3 Jahre", "3 bis unter 6 Jahre", "6 bis unter 10 Jahre") ~ 0,
                             age %in% c("10 bis unter 15 Jahre") ~ 10,
                             age %in% c("15 bis unter 18 Jahre", "18 bis unter 20 Jahre") ~ 15,
                             age %in% c("20 bis unter 25 Jahre") ~ 20,
                             age %in% c("25 bis unter 30 Jahre") ~ 25,
                             age %in% c("30 bis unter 35 Jahre") ~ 30,
                             age %in% c("35 bis unter 40 Jahre") ~ 35,
                             age %in% c("40 bis unter 45 Jahre") ~ 40,
                             age %in% c("45 bis unter 50 Jahre") ~ 45,
                             age %in% c("50 bis unter 55 Jahre") ~ 50,
                             age %in% c("55 bis unter 60 Jahre") ~ 55,
                             age %in% c("60 bis unter 65 Jahre") ~ 60,
                             age %in% c("65 bis unter 75 Jahre") ~ 65,
                             age %in% c("75 Jahre und mehr") ~ 75)) %>%
  group_by(year, AGS, geo, age, sex) %>%
  summarise(pop = sum(pop))

population %>%
  group_by(geo, year, sex) %>%
  summarise(missing = is.na(pop)) %>%
  ggplot(aes(x = geo, y = year, fill = missing))+
  geom_raster()+
  facet_grid(. ~ sex)

## 2011 MV Kreisgebietsreform (1995 - 2010 data are missing)
# 13002 (Neubrandenburg), 13056 (Müritz), 13055 (Mecklenburg-Strelitz), 13052 (Demmin)  --> 13071 (Seenplatte)
# 13051 (Bad Doberan), 13053 (Güstrow) --> 13072 (Rostock)
# 13060 (Parchim), 13054 (Ludwigslust) --> 13076 (Ludwigslust-Parchim	)
# 13058 (Nordwestmecklenburg), 13006 (Wismar) --> 13074 (Nordwestmecklenburg)
# 13005 (Stralsund), 13057 (Nordvorpommern), 13061 (Rügen) --> 13073 (Vorpommern-Rügen)
# 13001 (Greifswald), 13059 (Ostvorpommern), 13062 (Uecker-Randow) --> 13075 (Vorpommern-Greifswald)

## 2007 SA Kreisreform (2000 starts the new regime, has deathcounts for new regime since 1995)
# HAS OVERLAPPING YEARS WHERE THERE IS DATA FOR BOTH REGIMES
# 15303 (old Magdeburg) --> 15003 (Magdeburg); old may be deleted
# 15101 (old Dessau) --> 15001 (Dessau-Roßlau); some error inevitable
# 15202 (old Halle) --> 15002 (Halle); old may be deleted
# 15370 (old Salzwedel) --> 15081 (Salzwedel); old may be deleted
# 15363 (old Stendal) --> 15090 (Stendal); old may be deleted
# 15358 (old Jerichower Land) --> 15086 (Jerichower Land); some error inevitable
# 15362 (Ohrekreis), 15355 (Bördekreis) --> 15083 (Börde)
# 15153 (Bernburg), 15352 (Aschersleben-Staßfurt), 15367 (Schönebeck)  --> 15089 (Salzlandkreis)
# 15357 (Halberstadt), 15369 (Wernigerode), 15364 (Quedlinburg) --> 15085 (Harz)
# 15171 (Wittenberg) --> 15091 (Wittenberg)
# 15151 (Anhalt-Zerbs), 15159 (Köthen), 15154 (Bitterfeld) --> 15082 (Anhalt-Bitterfeld)
# 15265 (Saalkreis), 15261 (Merseburg-Querfurt) --> 15088 (Saalekreis)
# 15256 (Burgenlandkreis), 15268 (Weißenfels) --> 15084 (Burgenlandkreis)
# 15266 (Sangerhausen) + 15260 (Mansfelder Land) --> 15087 (Mansfeld-Südharz)

## 2009 NRW
# 05354 (Aachen) + ? --> 05334 (Städteregion Aachen)

## 2016 Eingliederung des Landkreises Osterode am Harz in den Landkreis Göttingen
# 03152 (Göttingen), 03156 (Osterode am Harz) --> 03159 (Göttingen)

## Kreisreform Sachsen 2008

# 14272 (old Bautzen) + 14292 (Kamenz) + 14264 (Hoyerswerda) --> 14625 (Bautzen); complete, old parts may be deleted
# 14171 (Annaberg) + 14191 (Aue-Schwarzenberg) + 14188 (Stollberg) + 14181 (Mittlerer Erzgebirgskreis) --> 14521 (Erzgebirgskreis); complete, old parts may be deleted
# 14284 (Niederschlesischer Oberlausitzkreis) + 14286 (Löbau-Zittau) + 14263 (Görlitz) --> 14626 (Görlitz); complete, old parts may be deleted
# 14379 (Leipziger Land) + 14383 (Muldentalkreis) --> 14729 (Leipzig, Landkreis); complete old parts may be deleted
# 14280 (old Meißen) + 14285 (Riesa-Großenhain) --> 14627 (Meißen); complete, old may be deleted
# 14182 (Mittweida) + 14177 (Freiberg) + 14375 (Döbeln) --> 14522 (Mittelsachsen); already complete, old ones may be deleted
# 14374 (Delitzsch) + 14389 (Torgau-Oschatz) --> 14730 (Nordsachsen); already complete
# 14287 (Sächsische Schweiz) + 14290 (Weißeritzkreis) --> 14628	(Sächsische Schweiz-Osterzgebirge); complete
# 14178 (old Vogtlandkreis) + 14166 (Plauen) --> 14523 (Vogtlandkreis); complete
# 14173 (Chemnitzer Land) + 14193 (Zwickauer Land) + 14167 (Stadt Zwickau) --> 14524 (Landkreis Zwickau); complete
# 14161 (old Chemnitz) --> 14511 (Chemnitz, Stadt), delete old
# 14262 (old Dresden) --> 14612 (Dresden), delete old
# 14365 (old Leipzig) --> 14713 (Leipzig), delete old

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

population_reformed <- population %>%
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
  mutate(pop_new = ifelse(is.na(pop) & AGS %in% reformed_areas, sum(pop, na.rm = T), pop)) %>% 
  filter(!(AGS %in% redundant_areas)) %>%
  select(year, AGS = code_new, geo, age, sex, pop_new)

population_reformed %>%
  group_by(geo, year, sex) %>%
  summarise(missing = is.na(pop_new)) %>%
  ggplot(aes(x = geo, y = year, fill = missing))+
  geom_raster()+
  facet_grid(. ~ sex)

mid_year_population <- population_reformed %>%
  group_by(AGS, geo, age, sex) %>%
  mutate(pop = RcppRoll::roll_mean(pop_new, n = 2L, align = "right", fill = NA)) %>%
  select(year, AGS, geo, age, sex, pop) %>%
  filter(year %in% 1996:2019)


mid_year_population_pooled <- mid_year_population %>%
  group_by(AGS, geo, age, sex) %>%
  mutate(pop_pool = RcppRoll::roll_sum(pop, n = 3L, align = "center", fill = NA)) %>%
  select(year, AGS, geo, age, sex, pop = pop_pool) %>%
  filter(year %in% 1997:2018)
  
                              
saveRDS(mid_year_population_pooled, file = "temp_data/mid_year_pop.rds")
