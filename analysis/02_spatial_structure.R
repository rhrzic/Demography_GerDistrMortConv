rm(list=ls())

library(tidyverse)

#Construct the hierarchical matrices H and Z, simplified to two levels

mortality <- readRDS("temp_data/mortality.rds")

geo_structure <- data.frame(AGS = unique(mortality$AGS), 
                            geo = unique(mortality$geo)) 

geo_structure <- geo_structure %>%
  filter(str_length(AGS) == 5) %>%
  mutate(AGS1 = str_sub(AGS, 1,2),
         AGS3 = str_sub(AGS, 4,5))

geo_structure <- geo_structure %>%
  group_by(AGS1) %>%
  mutate(number_of_districts = n_distinct(geo)) %>%
  arrange(number_of_districts) %>%
  ungroup() %>%
  mutate(ix1 = as.numeric(AGS1),
         ix2 = ifelse(AGS1 %in% c("02", "11"), 0, 1:399))

saveRDS(geo_structure, "temp_data/geo_structure.rds")

tmp <- geo_structure %>%
  mutate(effect1 = ix1,
         parent1 = 1,
         effect2 = ix2,
         parent2 = (effect2 != 0)*ix1)

nkreise = nrow(tmp)

# LEVEL 1 - state random effects

sel        = (tmp$effect1 > 0)
parents    = tmp$parent1[sel]
effects    = tmp$effect1[sel]

H1 = matrix(0, nrow=nkreise, ncol=length(unique(effects)) )
ix = cbind( which(sel), effects)
H1[ix] = 1

G1 = 1* (table(parents, effects) > 0)


# LEVEL 2 - district random effects

sel        = (tmp$effect2 > 0)
parents    = tmp$parent2[sel]
effects    = tmp$effect2[sel]

H2 = matrix(0, nrow=nkreise, ncol=length(unique(effects)) )
ix = cbind( which(sel), effects)
H2[ix] = 1

G2 = 1* (table(parents, effects) > 0)

Z1 = eigen( crossprod(G1))$vectors[, -(1:nrow(G1))]
Z2 = eigen( crossprod(G2))$vectors[, -(1:nrow(G2))]

save(G1, G2, H1, H2, Z1, Z2, file='temp_data/HZmatrices.Rdata')