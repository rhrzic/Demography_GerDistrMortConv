rm(list=ls())

## German reference mortality rates

require(HMDHFDplus)
require(MortalitySmooth)

myusername <- "rok.hrzic@gmail.com"
mypassword <- "VmwyCJiKwVu9n4u"

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

the.ages <- 0:89
the.years <- 1997:2016

exposures <- expos %>%
  filter(Age %in% the.ages & Year %in% the.years) %>%
  select(Year, Age, Female, Male)

deaths <- dths %>%
  filter(Age %in% the.ages & Year %in% the.years) %>%
  select(Year, Age, Female, Male)

create.ref.pop <- function(the.sex, the.year) {
  
  print(the.sex)
  print(the.year)
  
  x.raw <- exposures[exposures$Year == the.year,"Age"]
  Dx.raw <- deaths[deaths$Year == the.year,the.sex]
  Nx.raw <- exposures[exposures$Year == the.year,the.sex]
  
  require(MortalitySmooth)
  age.P.spline <- x.raw[-1]
  Expo.P.spline <- Nx.raw[-1]    
  Dth.P.spline <- Dx.raw[-1]
  
  fit1D <- Mort1Dsmooth(x=age.P.spline, y=Dth.P.spline, offset=log(Expo.P.spline))
  
  reference.mortality <- c(Dx.raw[1], fit1D$fitted.values) / Nx.raw
  
  returnlist <- data.frame(Year=the.year, Age=x.raw, smoothed=reference.mortality)
  return(returnlist)
}

GermanStandardMale <- NULL

for (i in the.years) {
  GermanStandardMale_new <- create.ref.pop("Male", i)
  GermanStandardMale <- rbind(GermanStandardMale, GermanStandardMale_new)
}

GermanStandardFemale <- NULL

for (i in the.years) {
  GermanStandardFemale_new <- create.ref.pop("Female", i)
  GermanStandardFemale <- rbind(GermanStandardFemale, GermanStandardFemale_new)
}

GermanStandardMale <- GermanStandardMale %>%
  mutate(logsmoothed_male = log(smoothed))

GermanStandardFemale <- GermanStandardFemale %>%
  mutate(logsmoothed_female = log(smoothed))

germany <- full_join(GermanStandardFemale, GermanStandardMale, by = c("Year", "Age")) %>%
  select(Year, Age, starts_with("logsmoothed"))

saveRDS(germany, file="temp_data/germany.rds")

## Male-Female mortality difference

LTB.DEU.males <- HMDHFDplus::readHMDweb(
  CNTRY="DEUTNP",
  "mltper_1x1",
  username = myusername, 
  password = mypassword)

LTB.DEU.females <- HMDHFDplus::readHMDweb(
  CNTRY="DEUTNP",
  "fltper_1x1",
  username = myusername, 
  password = mypassword)

LTB.GDR.males <- HMDHFDplus::readHMDweb(
  CNTRY="DEUTE",
  "mltper_1x1",
  username = myusername, 
  password = mypassword)
LTB.GDR.females <- HMDHFDplus::readHMDweb(
  CNTRY="DEUTE",
  "fltper_1x1",
  username = myusername, 
  password = mypassword)

LTB.FRG.males <- HMDHFDplus::readHMDweb(
  CNTRY="DEUTW",
  "mltper_1x1",
  username = myusername, 
  password = mypassword)
LTB.FRG.females <- HMDHFDplus::readHMDweb(
  CNTRY="DEUTW",
  "fltper_1x1",
  username = myusername, 
  password = mypassword)

LTB.DEU.females <- LTB.DEU.females[LTB.DEU.females$Year %in% the.years,]
LTB.DEU.males <- LTB.DEU.males[LTB.DEU.males$Year %in% the.years,]
LTB.GDR.females <- LTB.GDR.females[LTB.GDR.females$Year %in% the.years,]
LTB.GDR.males <- LTB.GDR.males[LTB.GDR.males$Year %in% the.years,]
LTB.FRG.females <- LTB.FRG.females[LTB.FRG.females$Year %in% the.years,]
LTB.FRG.males <- LTB.FRG.males[LTB.FRG.males$Year %in% the.years,]

MF.diff <- data.frame(Year = LTB.DEU.females$Year,
                      Age=LTB.DEU.females$Age,
                      DEU=log(LTB.DEU.males$mx) - log(LTB.DEU.females$mx),
                      GDR=log(LTB.GDR.males$mx) - log(LTB.GDR.females$mx),
                      FRG=log(LTB.FRG.males$mx) - log(LTB.FRG.females$mx))

library(mgcv)

priors.sex <- MF.diff %>%
  mutate(GDR = ifelse(is.infinite(GDR), 0, GDR)) %>%
  group_by(Year) %>%
  mutate(DEU.smooth = fitted(gam(DEU ~ s(Age))),
         FRG.smooth = fitted(gam(FRG ~ s(Age))),
         GDR.smooth = fitted(gam(GDR ~ s(Age)))) %>%
  filter(Age %in% 0:89) %>%
  select(Year, Age, GDR.smooth, FRG.smooth, DEU.smooth) %>%
  ungroup()

saveRDS(priors.sex, file = "temp_data/sex_diff.rds")

#East-West-Berlin indicator

geo_structure <- readRDS("temp_data/geo_structure.rds")

east_west <- geo_structure %>%
  mutate(region = case_when(AGS1 == 11 ~ 3,
                            AGS1 > 11 ~ 1,
                            AGS1 < 11 ~ 2))

saveRDS(east_west, file="temp_data/east_west.rds")
