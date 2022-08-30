rm(list=ls())

library(tidyverse)
library(RcppRoll)

longterm_unemployed <- read.csv2("raw_data/context/Longterm_unemployed.csv", header=FALSE, skip=2, dec = ",") %>%
  select(AGS = V1, Year_1998 = V4, Year_1999 = V5, Year_2000 = V6, Year_2001 = V7, 
           Year_2002 = V8, Year_2003 = V9, Year_2004 = V10, Year_2005 = V11,
           Year_2006 = V12, Year_2007 = V13, Year_2008 = V14, Year_2009 = V15,
           Year_2010 = V16, Year_2011 = V17, Year_2012 = V18, Year_2013 = V19,
           Year_2014 = V20, Year_2015 = V21, Year_2016 = V22) %>%
  pivot_longer(Year_1998:Year_2016, names_to = c("sex", "year"), names_sep = "_", values_to = "longterm_unemployed") %>%
  mutate(year = as.numeric(year)) %>%
  select(-sex)

saveRDS(unemployed, "temp_data/longterm_unemployed.rds")


setClass('finance')
setAs("character","finance", function(from) as.numeric(gsub(",","\\.",gsub("\\.","",from))))


tax_potential <- read.csv2("raw_data/context/Tax_potential.csv", header=FALSE, skip=2, stringsAsFactors=FALSE, 
                          colClasses = c("character", "character", "character", rep("finance", 23))) %>%
  select(AGS = V1, Year_1995 = V4, Year_1996 = V5, Year_1997 = V6, Year_1998 = V7, 
         Year_1999 = V8, Year_2000 = V9, Year_2001 = V10, Year_2002 = V11,
         Year_2003 = V12, Year_2004 = V13, Year_2005 = V14, Year_2006 = V15,
         Year_2007 = V16, Year_2008 = V17, Year_2009 = V18, Year_2010 = V19,
         Year_2011 = V20, Year_2012 = V21, Year_2013 = V22, Year_2014 = V23,
         Year_2015 = V24, Year_2016 = V25) %>%
  pivot_longer(Year_1995:Year_2016, names_to = c("sex", "year"), names_sep = "_", values_to = "tax_potential") %>%
  mutate(year = as.numeric(year), 
         AGS = as.numeric(AGS)) %>%
  select(-sex)

saveRDS(tax_potential, "temp_data/tax_potential.rds")


average_age <- read.csv2("raw_data/context/AverageAge.csv", header=FALSE, skip=2, dec = ",") %>%
  select(AGS = V1, geo = V2, Year_1997 = V4, Year_1998 = V5,  
         Year_1999 = V6, Year_2000 = V7, Year_2001 = V8, Year_2002 = V9,
         Year_2003 = V10, Year_2004 = V11, Year_2005 = V12, Year_2006 = V13,
         Year_2007 = V14, Year_2008 = V15, Year_2009 = V16, Year_2010 = V17,
         Year_2011 = V18, Year_2012 = V19, Year_2013 = V20, Year_2014 = V21,
         Year_2015 = V22, Year_2016 = V23) %>%
  pivot_longer(Year_1997:Year_2016, names_to = c("sex", "year"), names_sep = "_", values_to = "average_age") %>%
  mutate(year = as.numeric(year))%>%
  select(-sex)

saveRDS(average_age, "temp_data/average_age.rds")


