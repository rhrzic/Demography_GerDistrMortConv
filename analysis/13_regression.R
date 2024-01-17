rm(list=ls())

require(tidyverse)
require(nnet)
require(purrr)
require(broom)
require(sf)
require(cowplot)
require(scales)
require(flextable)
require(Cairo)

kreise <- st_read("raw_data/vg2500/vg2500_krs.shp")

lander <- st_read("raw_data/vg2500/vg2500_lan.shp")

## Dependent var

e0_district_convergence = readRDS("temp_data/e0_district_convergence.rds")


## Independent vars

longterm_unemployed <- readRDS("temp_data/longterm_unemployed.rds") %>%
  group_by(AGS) %>%
  mutate(diff_unemployment = lead(longterm_unemployed) - longterm_unemployed) %>%
  summarise(year = '1997-2016', 
            ave_unemployment = mean(longterm_unemployed), 
            diff_unemployment = mean(diff_unemployment, na.rm = T)) %>%
  mutate(ave_unemployment = scale(ave_unemployment), diff_unemployment = scale(diff_unemployment),
         AGS = str_pad(AGS, 5, "left", pad = 0)) 


longterm_unemployed.1997 <- readRDS("temp_data/longterm_unemployed.rds") %>%
  filter(year < 2007) %>%
  group_by(AGS) %>%
  mutate(diff_unemployment = lead(longterm_unemployed) - longterm_unemployed) %>%
  summarise(year = '1997-2006',
            ave_unemployment = mean(longterm_unemployed), 
            diff_unemployment = mean(diff_unemployment, na.rm = T)) %>%
  mutate(ave_unemployment = scale(ave_unemployment), diff_unemployment = scale(diff_unemployment),
         AGS = str_pad(AGS, 5, "left", pad = 0)) 

longterm_unemployed.2007 <- readRDS("temp_data/longterm_unemployed.rds") %>%
  filter(year > 2006) %>%
  group_by(AGS) %>%
  mutate(diff_unemployment = lead(longterm_unemployed) - longterm_unemployed) %>%
  summarise(year = '2007-2016', 
            ave_unemployment = mean(longterm_unemployed), 
            diff_unemployment = mean(diff_unemployment, na.rm = T)) %>%
  mutate(ave_unemployment = scale(ave_unemployment), diff_unemployment = scale(diff_unemployment),
         AGS = str_pad(AGS, 5, "left", pad = 0)) 

longterm_unemployed = rbind(longterm_unemployed, longterm_unemployed.1997, longterm_unemployed.2007)
rm(longterm_unemployed.1997, longterm_unemployed.2007)


tax_potential <- readRDS("temp_data/tax_potential.rds") %>%
  filter(tax_potential > 0) %>%
  mutate(tax_potential = log(tax_potential)) %>%
  group_by(AGS) %>%
  mutate(diff_tax = lead(tax_potential) - tax_potential) %>%
  summarise(year = '1997-2016',
            ave_tax = mean(tax_potential), 
            diff_tax = mean(diff_tax, na.rm = T)) %>%
  mutate(ave_tax = scale(ave_tax), diff_tax = scale(diff_tax),
         AGS = str_pad(AGS, 5, "left", pad = 0)) 

tax_potential.1997 <- readRDS("temp_data/tax_potential.rds") %>%
  filter(tax_potential > 0 & year < 2007) %>%
  mutate(tax_potential = log(tax_potential)) %>%
  group_by(AGS) %>%
  mutate(diff_tax = lead(tax_potential) - tax_potential) %>%
  summarise(year = '1997-2006',
            ave_tax = mean(tax_potential),
            diff_tax = mean(diff_tax, na.rm = T)) %>%
  mutate(ave_tax = scale(ave_tax), diff_tax = scale(diff_tax),
         AGS = str_pad(AGS, 5, "left", pad = 0)) 

tax_potential.2007 <- readRDS("temp_data/tax_potential.rds") %>%
  filter(tax_potential > 0 & year > 2006) %>%
  mutate(tax_potential = log(tax_potential)) %>%
  group_by(AGS) %>%
  mutate(diff_tax = lead(tax_potential) - tax_potential) %>%
  summarise(year = '2007-2016',
            ave_tax = mean(tax_potential),
            diff_tax = mean(diff_tax, na.rm = T)) %>%
  mutate(ave_tax = scale(ave_tax), diff_tax = scale(diff_tax),
         AGS = str_pad(AGS, 5, "left", pad = 0)) 

tax_potential = rbind(tax_potential, tax_potential.1997, tax_potential.2007)
rm(tax_potential.1997, tax_potential.2007)


hospitals_rate <- readRDS("temp_data/hospitals_rate.rds")  %>%
  select(year, AGS, hospitals_rate) %>%
  group_by(AGS) %>%
  mutate(diff_hosp = lead(hospitals_rate) - hospitals_rate) %>%
  summarise(year = '1997-2016',
            ave_hosp = mean(hospitals_rate),
            diff_hosp = mean(diff_hosp, na.rm = T)) %>%
  mutate(ave_hosp = scale(ave_hosp), diff_hosp = scale(diff_hosp))

hospitals_rate.1997 <- readRDS("temp_data/hospitals_rate.rds")  %>%
  select(year, AGS, hospitals_rate) %>%
  filter(year < 2007) %>%
  group_by(AGS) %>%
  mutate(diff_hosp = lead(hospitals_rate) - hospitals_rate) %>%
  summarise(year = '1997-2006',
            ave_hosp = mean(hospitals_rate),
            diff_hosp = mean(diff_hosp, na.rm = T)) %>%
  mutate(ave_hosp = scale(ave_hosp), diff_hosp = scale(diff_hosp))

hospitals_rate.2007 <- readRDS("temp_data/hospitals_rate.rds")  %>%
  select(year, AGS, hospitals_rate) %>%
  filter(year > 2006) %>%
  group_by(AGS) %>%
  mutate(diff_hosp = lead(hospitals_rate) - hospitals_rate) %>%
  summarise(year = '2007-2016',
            ave_hosp = mean(hospitals_rate),
            diff_hosp = mean(diff_hosp, na.rm = T)) %>%
  mutate(ave_hosp = scale(ave_hosp), diff_hosp = scale(diff_hosp))

hospitals_rate = rbind(hospitals_rate, hospitals_rate.1997, hospitals_rate.2007)
rm(hospitals_rate.1997, hospitals_rate.2007)


average_age = readRDS("temp_data/average_age.rds") %>%
  select(year, AGS, average_age) %>%
  group_by(AGS) %>%
  mutate(diff_age = lead(average_age) - average_age) %>%
  summarise(year = '1997-2016',
            ave_age = mean(average_age),
            diff_age = mean(diff_age, na.rm = T)) %>%
  mutate(ave_age = scale(ave_age), diff_age = scale(diff_age),
         AGS = str_pad(AGS, 5, "left", pad = 0))

average_age.1997 = readRDS("temp_data/average_age.rds") %>%
  select(year, AGS, average_age) %>%
  filter(year < 2007) %>%
  group_by(AGS) %>%
  mutate(diff_age = lead(average_age) - average_age) %>%
  summarise(year = '1997-2006',
            ave_age = mean(average_age),
            diff_age = mean(diff_age, na.rm = T)) %>%
  mutate(ave_age = scale(ave_age), diff_age = scale(diff_age),
         AGS = str_pad(AGS, 5, "left", pad = 0))

average_age.2007 = readRDS("temp_data/average_age.rds") %>%
  select(year, AGS, average_age) %>%
  filter(year > 2006) %>%
  group_by(AGS) %>%
  mutate(diff_age = lead(average_age) - average_age) %>%
  summarise(year = '2007-2016',
            ave_age = mean(average_age),
            diff_age = mean(diff_age, na.rm = T)) %>%
  mutate(ave_age = scale(ave_age), diff_age = scale(diff_age),
         AGS = str_pad(AGS, 5, "left", pad = 0))

average_age = rbind(average_age, average_age.1997, average_age.2007)
rm(average_age.1997, average_age.2007)

independent_vars = full_join(longterm_unemployed, tax_potential, by = c('year', 'AGS')) %>%
  full_join(., hospitals_rate, by = c('year', 'AGS')) %>%
  full_join(., average_age, by = c('year', 'AGS')) %>%
  pivot_longer(ave_unemployment:diff_age, names_to =c('measure','variable'),
               names_pattern = '(.*)_(.*)', values_to = 'value') %>%
  mutate(year = factor(year, levels = c('1997-2016', '1997-2006', '2007-2016')))
  


## maps of independent variables

map.independent_vars <- left_join(independent_vars, kreise, by = c("AGS" = "ARS"))

supp_figA9 = ggplot(map.independent_vars)+
  geom_sf(aes(fill = value, geometry = geometry), color = NA, size = 0.01) +
  geom_sf(data = lander, aes(geometry = geometry), color = "black", fill = NA)+
  theme(panel.grid.major = element_line(color = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "transparent"))+
  scale_fill_gradient2(limits = c(-3, 3), oob = squish)+
  scale_alpha(guide = 'none')+
  labs(fill = NULL)+
  facet_wrap(variable + measure ~ year, ncol = 6)

ggsave("figures/supp_figA9.png", supp_figA9, width = 190, height = 250, unit = 'mm')
ggsave("figures/supp_figA9.pdf", supp_figA9, width = 190, height = 250, unit = 'mm')


## Regression df

regression_df_long <- left_join(e0_district_convergence, independent_vars, by = c("AGS", 'year')) %>%
  dplyr::select(AGS, sex, year, state, east, scenario, measure, variable, value) %>%
  mutate(scenario = factor(scenario, levels = c('Increasing disadvantage', 'Decreasing disadvantage', 'Decreasing advantage', 'Increasing advantage')))

regression_df_wide = regression_df_long %>%
  pivot_wider(names_from = c(measure, variable), values_from = value)


averages_regression = function(data){multinom(scenario ~ state + ave_unemployment +
                                            ave_tax + ave_hosp + ave_age, data = data)}

extract_coeff <- function(mod){broom::tidy(mod, conf.int = T, exponentiate = T) %>%
    filter(term %in% c('ave_unemployment', 'ave_tax', 'ave_hosp', 'ave_age'))}


averages_model = regression_df_wide %>%
  group_by(year, sex) %>%
  nest() %>%
  mutate(models = map(data, averages_regression),
         results = map(models, extract_coeff)) %>%
  select(year, sex, results) %>%
  unnest(col = results) %>%
  mutate(model = 'averages')

## bivariate regression

bivar_regression = function(data){multinom(scenario ~ state + value, data = data, model = T)}

extract_coeff <- function(mod){broom::tidy(mod, conf.int = T, exponentiate = T) %>% filter(term == 'value')}

bivariate_models = regression_df_long %>%
  group_by(year, sex, measure, variable) %>%
  nest() %>%
  mutate(models = map(data, bivar_regression),
         results = map(models, extract_coeff)) %>%
  select(year, sex, measure, variable, results) %>%
  unnest(col = results)


## full regression

full_regression = function(data){multinom(scenario ~ state + ave_unemployment + diff_unemployment + 
                                             ave_tax + diff_tax + ave_hosp + diff_hosp + ave_age + diff_age, data = data, model = T)}

extract_coeff <- function(mod){broom::tidy(mod, conf.int = T, exponentiate = T) %>% 
    filter(term %in% c('ave_unemployment', 'diff_unemployment', 'ave_tax', 'diff_tax', 'ave_hosp', 'diff_hosp', 'ave_age', 'diff_age'))}


full_model = regression_df_wide %>%
  group_by(year, sex) %>%
  nest() %>%
  mutate(models = map(data, full_regression),
         results = map(models, extract_coeff)) %>%
  select(year, sex, results) %>%
  unnest(col = results) %>%
  mutate(model = 'full')



models = rbind(averages_model, full_model) %>%
  mutate(sex = str_to_sentence(sex),
         sex = factor(sex, levels = c('Male', 'Female')),
         term = recode(term, 
                       diff_unemployment = 'Change in unemployment',
                       diff_tax = 'Change in tax base',
                       diff_hosp = 'Change in hospital density',
                       diff_age = 'Change in average age',
                       ave_unemployment = 'Average unemployment',
                       ave_tax = 'Average tax base',
                       ave_hosp = 'Average hospital density',
                       ave_age = 'Average age'),
         model = recode(model,
                        averages = 'Model containing\naverage variables',
                        full = 'Model containing average\nand change variables'),
         model = factor(model, levels = c('Model containing\naverage variables', 
                                          'Model containing average\nand change variables'))) %>%
  arrange(desc(sex))



models$y.level = factor(models$y.level, levels = c("Decreasing disadvantage",  
                                                   "Decreasing advantage", 
                                                   "Increasing advantage"))

p5 = models %>%
  filter(term %in% c('Average tax base', 'Average unemployment', 'Average hospital density', 'Average age') &
           year == '1997-2016' & model == 'Model containing\naverage variables') %>%
ggplot(aes(y = estimate, ymin = conf.low, ymax = conf.high, x = term))+
  geom_point(position = position_dodge2(0.75))+
  geom_linerange(position = position_dodge2(0.75), key_glyph = 'path')+
  geom_hline(aes(yintercept = 1))+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 1, 2, 4), labels = c('0', '0.25', '0.5', '1', '2', '4'), trans = 'log')+
  facet_grid(sex ~ y.level)+
  xlab(NULL)+
  ylab('Odds Ratio')+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = 'top',
        axis.title = element_text(face = 'bold', color = 'black'),
        strip.text = element_text(face = 'bold'),
        axis.text = element_text(color = 'black'))

ggsave("figures/p5.png", p5, width = 300, height = 190, unit = 'mm')

ggsave("figures/p5.pdf", p5, width = 250, height = 190, unit = 'mm',family = 'Arial', device = cairo_pdf)




supp_table2 = models %>%
  ungroup() %>%
  mutate(star = ifelse(p.value < 0.05, '*', ''),
         year = factor(year, levels = c('1997-2016', '1997-2006', '2007-2016'))) %>%
  arrange(sex, year, model, term) %>%
  mutate_if(is.numeric, ~round(.x, digits = 2)) %>%
  mutate(result = paste0(estimate, star, '\n(', conf.low, ', ', conf.high, ')')) %>%
  select(sex, year, model, y.level, term, result) %>%
  pivot_wider(names_from = y.level, values_from = result) %>%
  mutate(`Increasing disadvantage` = '1 (Ref.)') %>%
  select(sex:term, `Increasing disadvantage`, `Decreasing disadvantage`, `Decreasing advantage`, `Increasing advantage`) %>%
  flextable(.) %>%
  merge_v(j = c('sex', 'year', 'model'))%>%
  set_header_labels(sex = 'Sex', year = 'Period', model = 'Model', term = 'Variable') %>%
  valign(valign = 'top') %>%
  footnote(value = as_paragraph(c('Notes: *p < 0.05')), ref_symbols = c('')) %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 8, part = 'all') %>%
  fontsize(size = 8, part = 'header')

save_as_docx(supp_table2, path = 'tables/supp_table2.docx')



### With the alternative outcome

e25.75_district_convergence = readRDS("temp_data/e25.75_district_convergence.rds")

regression_df_wide_e25.75 <- left_join(e25.75_district_convergence, independent_vars, by = c("AGS", 'year')) %>%
  dplyr::select(AGS, sex, year, state, east, scenario, measure, variable, value) %>%
  mutate(scenario = factor(scenario, levels = c('Increasing disadvantage', 'Decreasing disadvantage', 'Decreasing advantage', 'Increasing advantage'))) %>%
  pivot_wider(names_from = c(measure, variable), values_from = value)

averages_regression = function(data){multinom(scenario ~ state + ave_unemployment +
                                                ave_tax + ave_hosp + ave_age, data = data)}

extract_coeff <- function(mod){broom::tidy(mod, conf.int = T, exponentiate = T) %>%
    filter(term %in% c('ave_unemployment', 'ave_tax', 'ave_hosp', 'ave_age'))}


averages_model_e25.75 = regression_df_wide_e25.75 %>%
  group_by(year, sex) %>%
  nest() %>%
  mutate(models = map(data, averages_regression),
         results = map(models, extract_coeff)) %>%
  select(year, sex, results) %>%
  unnest(col = results) %>%
  mutate(model = 'averages',
         dependent = 'e25.75')

full_regression = function(data){multinom(scenario ~ state + ave_unemployment + diff_unemployment + 
                                            ave_tax + diff_tax + ave_hosp + diff_hosp + ave_age + diff_age, data = data, model = T)}

extract_coeff <- function(mod){broom::tidy(mod, conf.int = T, exponentiate = T) %>% 
    filter(term %in% c('ave_unemployment', 'diff_unemployment', 'ave_tax', 'diff_tax', 'ave_hosp', 'diff_hosp', 'ave_age', 'diff_age'))}


full_model_e25.75 = regression_df_wide_e25.75 %>%
  group_by(year, sex) %>%
  nest() %>%
  mutate(models = map(data, full_regression),
         results = map(models, extract_coeff)) %>%
  select(year, sex, results) %>%
  unnest(col = results) %>%
  mutate(model = 'full',
         dependent = 'e25.75')

models.e25.75 = rbind(averages_model_e25.75,full_model_e25.75) %>%
  mutate(sex = str_to_sentence(sex),
         sex = factor(sex, levels = c('Male', 'Female')),
         term = recode(term, 
                       diff_unemployment = 'Change in unemployment',
                       diff_tax = 'Change in tax base',
                       diff_hosp = 'Change in hospital density',
                       diff_age = 'Change in average age',
                       ave_unemployment = 'Average unemployment',
                       ave_tax = 'Average tax base',
                       ave_hosp = 'Average hospital density',
                       ave_age = 'Average age'),
         model = recode(model,
                        averages = 'Model containing\naverage variables',
                        full = 'Model containing average\nand change variables'),
         model = factor(model, levels = c('Model containing\naverage variables', 
                                          'Model containing average\nand change variables'))) %>%
  arrange(sex)

models.e25.75$y.level = factor(models.e25.75$y.level, levels = c("Decreasing disadvantage",  
                                                   "Decreasing advantage", 
                                                   "Increasing advantage"))


supp_table3 = models.e25.75 %>%
  ungroup() %>%
  mutate(star = ifelse(p.value < 0.05, '*', ''),
         year = factor(year, levels = c('1997-2016', '1997-2006', '2007-2016'))) %>%
  arrange(sex, year, model, term) %>%
  mutate_if(is.numeric, ~round(.x, digits = 2)) %>%
  mutate(result = paste0(estimate, star, '\n(', conf.low, ', ', conf.high, ')'),
         sex = str_to_title(sex)) %>%
  select(sex, year, model, y.level, term, result) %>%
  pivot_wider(names_from = y.level, values_from = result) %>%
  mutate(`Increasing disadvantage` = '1 (Ref.)') %>%
  select(sex:term, `Increasing disadvantage`, `Decreasing disadvantage`, `Decreasing advantage`, `Increasing advantage`) %>%
  flextable(.) %>%
  merge_v(j = c('sex', 'year', 'model'))%>%
  set_header_labels(sex = 'Sex', year = 'Period', model = 'Model', term = 'Variable') %>%
  valign(valign = 'top') %>%
  footnote(value = as_paragraph(c('Notes: *p < 0.05')), ref_symbols = c('')) %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 8, part = 'all') %>%
  fontsize(size = 8, part = 'header')

save_as_docx(supp_table3, path = 'tables/supp_table3.docx')


models_combined = models %>%
  mutate(dependent = 'e0') %>%
  rbind(models.e25.75)

supp_figA10 = models_combined %>%
  filter(term %in% c('Average tax base', 'Average unemployment', 'Average hospital density', 'Average age')) %>%
  ggplot(aes(y = estimate, ymin = conf.low, ymax = conf.high, x = term, color = dependent, shape = year, linetype = model))+
  geom_point(position = position_dodge2(0.75))+
  geom_linerange(position = position_dodge2(0.75), key_glyph = 'path')+
  geom_hline(aes(yintercept = 1))+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 1, 2, 4), labels = c('0', '0.25', '0.5', '1', '2', '4'), trans = 'log')+
  facet_grid(sex ~ y.level)+
  xlab(NULL)+
  ylab('Odds ratio')+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = 'top')
#  ggtitle(expression(paste('Association of district ',e[0],' and ',e[25]^75,' trajectory groups with district characteristics')))

ggsave("figures/supp_figA10.png", supp_figA10, width = 250, height = 210, unit = 'mm')
ggsave("figures/supp_figA10.pdf", supp_figA10, width = 250, height = 210, unit = 'mm')



