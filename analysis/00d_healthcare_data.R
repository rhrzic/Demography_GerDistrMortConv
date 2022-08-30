rm(list=ls())

#Hospitals dataset

hospitals1 <- read.csv(paste0("raw_data/healthcare/23111-01-02-4.csv"), header = FALSE, sep = ";", stringsAsFactors = F,
                      encoding = "latin1", na.strings = c("-", "."), skip = 5) %>%
  slice(., 1:(n()-4)) %>%
  select(year = V1, AGS = V2, geo = V3, hospitals = V4)

hospitals2 <- read.csv(paste0("raw_data/healthcare/23111-01-04-4.csv"), header = FALSE, sep = ";", stringsAsFactors = F,
                       encoding = "latin1", na.strings = c("-", "."), skip = 8) %>%
  slice(., 1:(n()-4)) %>%
  select(year = V1, AGS = V2, geo = V3, hospitals = V4)

hospitals <- rbind(hospitals1, hospitals2) %>%
  mutate(year = str_sub(year, -4),
         year = as.numeric(year),
         geo = trimws(geo),
         AGS = case_when(AGS == "02" ~ "02000",
                         AGS == "11" ~ "11000",
                         TRUE ~ AGS)) %>%
  filter(str_length(AGS) == 5 | AGS =="05334002")
  
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

hospitals_reformed <- hospitals %>%
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
  group_by(year, code_new) %>%
  mutate(hospitals_new = ifelse(is.na(hospitals) & AGS %in% reformed_areas, sum(hospitals, na.rm = T), hospitals)) %>%
  filter(!(AGS %in% redundant_areas)) %>%
  select(year, AGS = code_new, geo, hospitals = hospitals_new)


standard_pop_w <- data.frame(age = c(0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 75),
                             w = c(0.105, 0.055, 0.055, 0.06, 0.06, 0.065, 0.07, 0.07, 0.07, 0.07,0.065, 0.06, 0.105, 0.09))

pop <- readRDS("temp_data/mid_year_pop.rds") %>%
  group_by(year, AGS, geo) %>%
  summarise(pop = sum(pop))

hospitals_rate <- left_join(pop, hospitals_reformed, by = c("year", "AGS", "geo")) %>%
  filter(!is.na(hospitals)) %>%
  mutate(hospitals_rate = 100000*hospitals/pop)
  

saveRDS(hospitals_rate, "temp_data/hospitals_rate.rds")
