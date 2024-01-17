rm(list=ls())

require(tidyverse)
require(sf)
require(cowplot)
require(directlabels)
require(grid)
require(remotes)
require(Cairo)

#remotes::install_github("coolbutuseless/ggpattern")
require(ggpattern)


kreise <- st_read("raw_data/vg2500/vg2500_krs.shp")

lander <- st_read("raw_data/vg2500/vg2500_lan.shp") %>%
  mutate(east = ifelse(GEN %in% c('Brandenburg', 'Mecklenburg-Vorpommer', 'Sachsen', 'Sachscen-Anhalt'), 'East', 'West'))


e0_diff <- readRDS("temp_data/e0_diff.rds")

e0_district_convergence <- e0_diff %>%
  group_by(sex, year) %>%
  mutate(mean_e0_start = mean(e0), mean_diff = mean(e0_diff)) %>%
  mutate(scenario = case_when(e0 < mean_e0_start & e0_diff > mean_diff ~ "Decreasing disadvantage",
                              e0 < mean_e0_start & e0_diff < mean_diff ~ "Increasing disadvantage",
                              e0 > mean_e0_start & e0_diff > mean_diff ~ "Increasing advantage",
                              e0 > mean_e0_start & e0_diff < mean_diff ~ "Decreasing advantage"),
         scenario = factor(scenario, levels = c('Increasing disadvantage', 'Decreasing disadvantage',
                                                'Decreasing advantage', 'Increasing advantage'))) %>%
  separate(scenario, c("scenario1", "scenario2"), remove = F)

saveRDS(e0_district_convergence, 'temp_data/e0_district_convergence.rds')

e0_district_convergence = readRDS('temp_data/e0_district_convergence.rds')

e0_district_convergence %>%
  filter(year == '1997-2016') %>%
  group_by(sex, scenario) %>%
  summarise(n_distinct(AGS))

e0_district_convergence %>%
  filter(year == '1997-2016') %>%
  group_by(sex, scenario) %>%
  summarise(n_distinct(AGS)/401)

e0_district_convergence %>%
  filter(year == '1997-2016') %>%
  group_by(sex, east) %>%
  summarise(n_distinct(AGS))

e0_district_convergence %>%
  filter(year == '1997-2016') %>%
  group_by(sex, east, scenario) %>%
  summarise(n_distinct(AGS))

e0_district_convergence %>%
  filter(year == '1997-2016' & east == 'West') %>%
  group_by(sex, east, state, scenario) %>%
  summarise(n_distinct(AGS)) %>%
 filter(scenario %in% c('Increasing disadvantage', 'Increasing advantage'))


map_e0_district_convergence <- left_join(e0_district_convergence, kreise, by = c("AGS" = "ARS")) %>%
  filter(year == '1997-2016')

p4 <- ggplot(map_e0_district_convergence)+
  geom_sf_pattern(aes(pattern = scenario, pattern_fill = scenario, pattern_color = scenario, geometry = geometry), 
                  pattern_spacing = 0.01, fill = 'white', color = 'black', size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.position = 'top',
        strip.text = element_text(face = 'bold'),
        text = element_text(size = 16)) +
  facet_grid(. ~ str_to_title(sex))+
  scale_pattern_fill_manual(values =c('Increasing disadvantage'='darkred', 'Decreasing disadvantage'='lightcoral',
                             'Decreasing advantage'='lightskyblue', 'Increasing advantage' = 'navyblue'), name = '')+
  scale_pattern_color_manual(values =c('Increasing disadvantage'='darkred', 'Decreasing disadvantage'='lightcoral',
                                       'Decreasing advantage'='lightskyblue', 'Increasing advantage' = 'navyblue'), name = '')+
  scale_pattern_manual(values = c('Increasing disadvantage'='stripe', 'Decreasing disadvantage'='stripe',
                                  'Decreasing advantage'='circle', 'Increasing advantage' = 'circle'), name = '')

ggsave("figures/p4.png", p4, width = 250, height = 180, unit = 'mm', dpi = 600)

ggsave("figures/p4.pdf", p4, width = 250, height = 180, unit = 'mm', family = 'Arial', device = cairo_pdf)

ggsave("figures/p4.eps", p4, width = 250, height = 180, unit = 'mm', device = 'eps')



## Explaining the scenarios

scenarios <- data.frame(scenario = c(rep("Decreasing disadvantage",4), rep("Decreasing advantage", 4),
                                     rep('Increasing disadvantage', 4), rep("Increasing advantage", 4)),
                        Unit = c(rep(c("District", "District", "Average", "Average"), 4)), time = rep(c(0, 1), 8),
                        value = c(0.6, 0.95, 0.8, 1, 0.95, 1.05, 0.8, 1, 0.7, 0.8, 0.8, 1, 0.9, 1.2, 0.8, 1),
                        annotation = c('District life expectancy', '', 'Average life expectancy', '', rep('', 12)),
                        angle =  c(50, 0, 35, 0, rep(0, 12))) %>%
  mutate(scenario = factor(scenario, levels = c('Decreasing disadvantage', 'Decreasing advantage',
                                                'Increasing disadvantage', 'Increasing advantage')))

p1 = ggplot(scenarios, aes(x = time, y = value, linetype = Unit))+
  geom_line(show.legend = F, linewidth = 1.5)+
  geom_text(aes(label = annotation, angle = angle, x = 0.01, y = (value+0.04)), hjust = 'left')+
  facet_wrap(. ~ scenario, nrow = 1)+
  xlab("Time (in years)")+
  ylab("Life Expectancy at Birth")+
  theme(panel.border = element_rect(colour = 'black', fill = NA),
      panel.grid.major = element_line(color = 'transparent'),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.background = element_rect(fill = 'transparent'),
      text = element_text(size = 12),
      axis.title = element_text(face = 'bold'),
      strip.text = element_text(face = 'bold'))


ggsave("figures/p1.png", p1, width = 250, height = 150, unit = 'mm', dpi = 600)

ggsave("figures/p1.pdf", p1, width = 250, height = 150, unit = 'mm', device = 'pdf')


## scenarios with alternative outcome

# e25.75_diff <- readRDS("temp_data/e25.75_diff.rds")
# 
# 
# e25.75_district_convergence <- e25.75_diff %>%
#   group_by(sex, year) %>%
#   mutate(mean_e25.75_start = mean(e25.75), mean_diff = mean(e25.75_diff)) %>%
#   mutate(scenario = case_when(e25.75 < mean_e25.75_start & e25.75_diff > mean_diff ~ "Decreasing disadvantage",
#                               e25.75 < mean_e25.75_start & e25.75_diff < mean_diff ~ "Increasing disadvantage",
#                               e25.75 > mean_e25.75_start & e25.75_diff > mean_diff ~ "Increasing advantage",
#                               e25.75 > mean_e25.75_start & e25.75_diff < mean_diff ~ "Decreasing advantage"))
# 
# 
# saveRDS(e25.75_district_convergence, 'temp_data/e25.75_district_convergence.rds')

e25.75_district_convergence = readRDS('temp_data/e25.75_district_convergence.rds')


map_e25.75_district_convergence <- left_join(e25.75_district_convergence, kreise, by = c("AGS" = "ARS"))


supp_figA8 = ggplot(map_e25.75_district_convergence)+
  geom_sf(aes(fill = scenario, geometry = geometry, color = after_scale(fill))) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", size = 1.2, fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.position = 'top') +
  facet_grid(str_to_title(sex) ~ year)+
  scale_fill_manual(values =c('Increasing disadvantage'='darkred', 'Decreasing disadvantage'='lightcoral', 'Decreasing advantage'='cornflowerblue', 'Increasing advantage' = 'navyblue'))+
  scale_alpha(guide = 'none')+
  labs(fill = NULL)

ggsave("figures/supp_figA8.png", supp_figA8, width = 250, height = 190, unit = 'mm')
ggsave("figures/supp_figA8.pdf", supp_figA8, width = 250, height = 190, unit = 'mm')

