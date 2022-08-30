rm(list=ls())

require(tidyverse)

e0_df <- data.frame()

e0_results <- list.files("results", pattern = "^e0_result_*")

for (this.result in e0_results) {
  
  load(paste0("results/",this.result))
  temp <- e0.our.model
  temp$year <- as.numeric(substr(this.result, start = 11, stop = 14))
  
  e0_df <- rbind(e0_df, temp)
  
  rm(e0.our.model)
}

geo_structure <- readRDS("temp_data/geo_structure.rds") %>%
  select(AGS, geo, state = AGS1)

state_names <- data.frame(state_code = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
                                         "11", "12", "13", "14", "15", "16"),
                          state_name = c("Schleswig-Holstein", "Hamburg", "Niedersachsen", "Bremen", 
                                    "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz",
                                    "Baden-Württemberg", "Bayern", "Saarland", "Berlin", "Brandenburg",
                                    "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen"))

e0_df$AGS = str_pad(e0_df$AGS, 5, side = "left", pad = "0")

e0_df <- left_join(e0_df, geo_structure, by ="AGS")

e0_df <- left_join(e0_df, state_names, by = c("state" = "state_code"))

e0_df <- e0_df %>%
  mutate(east = case_when(state %in% c("12", "13", "14", "15", "16") ~ "East",
                          state %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10") ~ "West",
                          TRUE ~ "Berlin"),
         sex = ifelse(Sex == 1, "male", "female"))

saveRDS(e0_df, "temp_data/e0.rds")

## Alternative outcome

e25.75=readRDS("temp_data/e25.75.rds")
e25.75_df = left_join(e25.75, geo_structure, by = "AGS")
e25.75_df <- left_join(e25.75_df, state_names, by = c("state" = "state_code"))

e25.75_df <- e25.75_df %>%
  mutate(east = case_when(state %in% c("12", "13", "14", "15", "16") ~ "East",
                          state %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10") ~ "West",
                          TRUE ~ "Berlin"))

saveRDS(e25.75_df, "temp_data/e25.75_df.rds")

