rm(list=ls())

require(tidyverse)
require(viridis)
require(sf)
require(cowplot)
require(gridExtra)
require(Cairo)

#How big are the confidence intervals?

e0 <- readRDS("temp_data/e0.rds") %>%
  select(AGS, sex, e0_low = pct025, e0 = pct50, e0_high = pct975, year, geo, state = state_name, east)

e0_ranges = e0 %>%
  mutate(e0_range = e0_high - e0_low)

min(e0_ranges$e0_range)
max(e0_ranges$e0_range)

supp_figA1 = ggplot(filter(e0_ranges, east != 'Berlin'), aes(x = year, y = e0_range, group = year))+
  geom_boxplot()+
  facet_grid(east ~ str_to_title(sex))+
  xlab('Year')+
  ylab('Size of the 95% confidence interval')+
  theme_bw()

ggsave('figures/supp_figA1.png', supp_figA1)
ggsave('figures/supp_figA1.pdf', supp_figA1)


ggplot(filter(e0_ranges, east != 'Berlin'), aes(x = year, y = e0, group = year))+
  geom_boxplot()+
  facet_grid(east ~ str_to_title(sex))+
  xlab('Year')+
  ylab('Distribution of e0')+
  theme_bw()


e0_distr = e0 %>%
  group_by(sex, year) %>%
  summarise(mean_e0 = mean(e0),
            sd = sd(e0),
            low_e0=mean(e0)-1.96*sd,
            high_e0=mean(e0)+1.96*sd,
            range_e0 = high_e0-low_e0)


e0_ranges_plot = e0_ranges %>%
  group_by(sex, year) %>%
  summarise(mean_ranges = mean(e0_range),
            sd = sd(e0_range),
            low_range=mean(e0_range)-1.96*sd,
            high_range=mean(e0_range)+1.96*sd,
            range_e0 = high_range-low_range)

supp_figA2 = ggplot()+
  geom_point(data = e0_distr, aes(x = year, y = range_e0, group = year, color = 'Median estimate'))+
  geom_point(data =e0_ranges_plot, aes(x = year, y = range_e0, group = year, color = 'Uncertainty around\nmedian estimate'))+
  facet_grid(. ~ str_to_title(sex))+
  xlab('Year')+
  ylab('Average size of the 95% confidence interval')+
  theme_bw()+
  labs(color = '')+
  theme(legend.position = 'top')

ggsave('figures/supp_figA2.png', supp_figA2)
ggsave('figures/supp_figA2.pdf', supp_figA2)

  

kreise <- st_read("raw_data/vg2500/vg2500_krs.shp")

lander <- st_read("raw_data/vg2500/vg2500_lan.shp")

e0_map = e0 %>%
  filter(year %in% c(1997, 2016)) %>%
  left_join(., kreise, by = c("AGS" = "ARS"))

e0_diff = e0 %>%
  filter(year %in% c(1997, 2006, 2007, 2016)) %>%
  arrange(AGS, sex, year) %>%
  group_by(AGS, sex) %>%
  mutate(e0_diff = lead(e0)-e0,
         year = paste0(year, "-", lead(year))) %>%
  ungroup()%>%
  filter(!is.na(e0_diff) & year %in% c('1997-2006', '2007-2016'))

e0_diff_total = e0 %>%
  filter(year %in% c(1997, 2016)) %>%
  arrange(AGS, sex, year) %>%
  group_by(AGS, sex) %>%
  mutate(e0_diff = lead(e0)-e0,
         year = paste0(year, "-", lead(year))) %>%
  ungroup()%>%
  filter(!is.na(e0_diff))

e0_diff = rbind(e0_diff, e0_diff_total) %>%
  mutate(year = factor(year, levels = c('1997-2016', '1997-2006', '2007-2016')))


saveRDS(e0_diff, "temp_data/e0_diff.rds")


e0_map_diff = e0_diff %>%
  left_join(., kreise, by = c("AGS" = "ARS"))


a1 = ggplot(filter(e0_map, sex == "male", year == 1997))+
  geom_sf(aes(fill = e0, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
    scale_fill_viridis_c()+
    guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(bold(paste("Male ", bolditalic(e[0]), " in 1997"))))

a2 = ggplot(filter(e0_map, sex == "male", year == 2016))+
  geom_sf(aes(fill = e0, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(bold(paste("Male ", bolditalic(e[0]), " in 2016"))))


a3 = ggplot(filter(e0_map_diff, sex == "male" & year == '1997-2016'))+
  geom_sf(aes(fill = e0_diff, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(bold(paste("Change in male ", bolditalic(e[0]), " 1997\u20132016"))))

b1 = ggplot(filter(e0_map, sex == "female" & year == 1997))+
  geom_sf(aes(fill = e0, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
  #scale_fill_gradient2(midpoint = mean(filter(map_df, sex == "male" & year == 1997)$e0))+
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(bold(paste("Female ", bolditalic(e[0]), " in 1997"))))

b2 = ggplot(filter(e0_map, sex == "female" & year == 2016))+
  geom_sf(aes(fill = e0, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
  #scale_fill_gradient2(midpoint = mean(filter(map_df, sex == "male" & year == 1997)$e0))+
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(bold(paste("Female ", bolditalic(e[0]), " in 2016"))))


b3 = ggplot(filter(e0_map_diff, sex == "female" & year == '1997-2006'))+
  geom_sf(aes(fill = e0_diff, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
  #scale_fill_gradient2(midpoint = mean(filter(map_df, sex == "male" & year == 1997)$e0))+
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(bold(paste("Change in female ", bolditalic(e[0]), " 1997\u20132016"))))



Cairo::CairoFonts(    # for some reason I seem to need the `pkgname::` format
  regular="Arial:style=Regular",
  bold="Arial:style=Bold",
  italic="Arial:style=Italic",
  bolditalic="Arial:style=Bold Italic, BoldItalic",
  symbol="Symbol"
)

p2 <- plot_grid(a1,a2,a3,b1,b2,b3, ncol = 3, nrow = 2, align = "hv", axis = "tblr")

ggsave("figures/p2.png", p2, width = 250, height = 190, unit = 'mm')

ggsave("figures/p2.pdf", p2, width = 250, height = 190, unit = 'mm', family = 'Arial', device = cairo_pdf)


## Alternative outcome

e25.75 <- readRDS("temp_data/e25.75_df.rds")

e25.75_diff = e25.75 %>%
  filter(year %in% c(1997, 2006, 2007, 2016)) %>%
  arrange(AGS, sex, year) %>%
  group_by(AGS, sex) %>%
  mutate(e25.75_diff = lead(e25.75)-e25.75,
         year = paste0(year, "-", lead(year))) %>%
  ungroup()%>%
  filter(!is.na(e25.75_diff) & year %in% c('1997-2006', '2007-2016'))

e25.75_diff_total = e25.75 %>%
  filter(year %in% c(1997, 2016)) %>%
  arrange(AGS, sex, year) %>%
  group_by(AGS, sex) %>%
  mutate(e25.75_diff = lead(e25.75)-e25.75,
         year = paste0(year, "-", lead(year))) %>%
  ungroup()%>%
  filter(!is.na(e25.75_diff))

e25.75_diff = rbind(e25.75_diff, e25.75_diff_total) %>%
  mutate(year = factor(year, levels = c('1997-2006', '2007-2016', '1997-2016')))

saveRDS(e25.75_diff, "temp_data/e25.75_diff.rds")


mean_e25.75 <- e25.75 %>%
  group_by(sex, year) %>%
  summarise(mean_e25.75 = mean(e25.75),
            mean_low = mean(e25.75_low),
            mean_up = mean(e25.75_up))

ggplot(e25.75, aes(x = year), key_glyph = draw_key_rect)+
  geom_line(aes(y = e25.75, group = interaction(AGS, sex)), size = 0.1, alpha = 0.1, show.legend = FALSE)+
  geom_line(data = mean_e25.75, aes(y = mean_e25.75, group = sex), size = 1.2)+
  geom_line(data = mean_e25.75, aes(y = mean_low, group = sex), size = 1.2, linetype = "dashed")+
  geom_line(data = mean_e25.75, aes(y = mean_up, group = sex), size = 1.2, linetype = "dashed")+
  geom_text(data = filter(mean_e25.75, year == 1997), aes(x = year-1, y = mean_e25.75, label = paste0(str_to_title(sex),"\n", round(mean_e25.75,1),"\n(",round(mean_low,1),", ",round(mean_up,1),")")), size = 3, show.legend = FALSE)+
  geom_text(data = filter(mean_e25.75, year == 2016), aes(x = year+1, y = mean_e25.75, label = paste0(str_to_title(sex),"\n", round(mean_e25.75,1),"\n(",round(mean_low,1),", ",round(mean_up,1),")")), size = 3, show.legend = FALSE)+
  xlab("Year")+
  ylab(expression(paste(e[25]^75)))+
  theme_bw()+
  guides(fill = "none")


map_e25.75 = e25.75 %>%
  filter(year %in% c(1997, 2016)) %>%
  left_join(., kreise, by = c("AGS" = "ARS"))

map_e25.75_diff <- left_join(e25.75_diff, kreise, by = c("AGS" = "ARS"))


a1 = ggplot(filter(map_e25.75, sex == "male" & year == 1997))+
  geom_sf(aes(fill = e25.75, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
  #scale_fill_gradient2(midpoint = mean(filter(map_df, sex == "male" & year == 1997)$e0))+
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(paste("Male ", e[25]^75, " in 1997")))


a2 = ggplot(filter(map_e25.75, sex == "male" & year == 2016))+
  geom_sf(aes(fill = e25.75, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
  #scale_fill_gradient2(midpoint = mean(filter(map_df, sex == "male" & year == 1997)$e0))+
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(paste("Male ", e[25]^75, " in 2016")))



a3 = ggplot(filter(map_e25.75_diff, sex == "male" & year == '1997-2006'))+
  geom_sf(aes(fill = e25.75_diff, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(paste("Change in male ", e[25]^75, " 1997-2016")))


b1 = ggplot(filter(map_e25.75, sex == "female" & year == 1997))+
  geom_sf(aes(fill = e25.75, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(paste("Female ", e[25]^75, " in 1997")))

b2 = ggplot(filter(map_e25.75, sex == "female" & year == 2016))+
  geom_sf(aes(fill = e25.75, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(paste("Female ", e[25]^75, " in 2016")))

b3 = ggplot(filter(map_e25.75_diff, sex == "female" & year=='1997-2016'))+
  geom_sf(aes(fill = e25.75_diff, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar('',barwidth = 1, barheight = 6))+
  labs(subtitle = expression(paste("Change in female ", e[25]^75, " 1997-2016")))


supp_figA8 <- plot_grid(a1,a2,a3,b1,b2,b3, ncol = 3, nrow = 2, align = "hv", axis = "tblr")

ggsave("figures/supp_figA8.png", supp_figA8, width = 250, height = 190, unit = 'mm')
ggsave("figures/supp_figA8.pdf", supp_figA8, width = 250, height = 190, unit = 'mm')

