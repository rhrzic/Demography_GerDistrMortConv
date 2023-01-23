rm(list=ls())

require(tidyverse)
require(directlabels)
require(cowplot)
require(scales)
require(Hmisc)
require(RcppRoll)
require(flextable)
require(Cairo)

source("analysis/var_decomp.R")

e0 <- readRDS("temp_data/e0.rds") %>%
  select(AGS, sex, e0 = pct50, year, geo, state = state_name, east)

e0_diff = readRDS('temp_data/e0_diff.rds')



pop <- readRDS("temp_data/mid_year_pop.rds") %>%
  group_by(year, AGS, geo, sex) %>%
  summarise(pop = sum(pop)) %>%
  group_by(AGS, geo, sex) %>%
  summarise(pop = mean(pop))


e0_diff <- left_join(e0_diff, pop, by = c("AGS", "geo", "sex"))


average_changes = e0_diff %>%
  group_by(sex, year) %>%
  summarise(location = 'Germany', 
            e0_diff=mean(e0_diff))

average_changes_east = e0_diff %>%
  filter(east != 'Berlin') %>%
  group_by(sex, year, east) %>%
  summarise(e0_diff=mean(e0_diff)) %>%
  rename(location = east)

average_changes_table= rbind(average_changes, average_changes_east)

average_changes_table = average_changes_table %>% 
  mutate(comparison = paste(location, sex, sep = ' - ')) %>%
  ungroup() %>%
  select(comparison, year, e0_diff) %>%
  pivot_wider(names_from = year, values_from = e0_diff) %>%
  select(comparison, `1997-2006`, `2007-2016`)


## Beta convergence

unconditional_beta <- function(data){lm(e0_diff ~ e0, data = data)}

unconditional_beta_weighted <- function(data){lm(e0_diff ~ e0, data = data, weights = pop)}

extract_coeff <- function(mod){round(coef(summary(mod))[2,1], 2)}
extract_CI <- function(mod){round(confint(mod, "e0", level = 0.95),2)}
extract_p <- function(mod){round(coef(summary(mod))[2,4], 2)}

beta_models <- e0_diff %>%
  group_by(sex, year) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta),
         beta = map(models, extract_coeff),
         ci = map(models, extract_CI),
         pval = map(models, extract_p)) %>%
  select(sex, year, beta, ci, pval) %>%
  mutate(location = 'Germany')

beta_models_w <- e0_diff %>%
  group_by(sex, year) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta_weighted),
         beta = map(models, extract_coeff),
         ci = map(models, extract_CI),
         pval = map(models, extract_p)) %>%
  mutate(location = 'Germany')

beta_models_east <- e0_diff %>%
  filter(east != 'Berlin') %>%
  group_by(sex, year, east) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta),
         beta = map(models, extract_coeff),
         ci = map(models, extract_CI),
         pval = map(models, extract_p)) %>%
  select(sex, year, location = east, beta, ci, pval)

beta_models_w_east <- e0_diff %>%
  filter(east != 'Berlin') %>%
  group_by(sex, year, east) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta_weighted),
         beta = map(models, extract_coeff),
         ci = map(models, extract_CI),
         pval = map(models, extract_p)) %>%
  select(sex, year, location = east, beta, ci, pval)

beta_models_table = rbind(beta_models, beta_models_east)

beta_models_table = beta_models_table %>% 
  mutate(star = ifelse(pval < 0.05, '*', ''), 
         ci = str_remove(as.character(ci), 'c'),
         result = paste0(beta, star, '\n', ci),
         comparison = paste(location, sex, sep = ' - ')) %>%
  ungroup() %>%
  select(comparison, year, result) %>%
  pivot_wider(names_from = year, values_from = result)


e0_diff$year_lab = e0_diff$year
levels(e0_diff$year_lab) = c('1997\u20132016', '1997\u20132006', '2007\u20132016')


a <- ggplot(filter(e0_diff, east != 'Berlin' & year == '1997-2006'), aes(x = e0, y = e0_diff))+
  geom_point(aes(color = east, shape = east), alpha = 0.3, show.legend = F)+
  geom_smooth(aes(group = interaction(sex, east), color = east), method = "lm", se = F, show.legend = F)+
  geom_dl(aes(label = str_to_title(sex)), method = 'smart.grid')+
  geom_text(aes(x = 71.7, y = 4.7, label = 'Eastern'), color = '#F8766D')+
  geom_text(aes(x = 72.3, y = 2.5, label = 'Western'), color = '#00BFC4')+
  xlab(expression(paste(bold('Starting '), bolditalic(e[0]))))+
  ylab(expression(paste(bold("Change in "), bolditalic(e[0]))))+
  theme_bw()+
  theme(legend.position = c(.85, .80),
        legend.title = element_blank(),
        text = element_text(size = 11),
        axis.title = element_text(face = 'bold'),
        strip.text = element_text(face = 'bold'))+
  guides(shape = guide_legend(override.aes = list(alpha = 1)))+
  facet_wrap(year_lab ~ .)

b <- ggplot(filter(e0_diff, east != 'Berlin' & year == '2007-2016'), aes(x = e0, y = e0_diff))+
  geom_point(aes(color = east, shape = east), alpha = 0.3, show.legend = F)+
  geom_smooth(aes(group = interaction(sex, east), color = east), method = "lm", se = F, show.legend = F)+
  geom_dl(aes(label = str_to_title(sex)), method = 'smart.grid')+
  xlab(expression(paste(bold('Starting '), bolditalic(e[0]))))+
  ylab(expression(paste(bold("Change in "), bolditalic(e[0]))))+
  theme_bw()+
  theme(legend.position = c(.85, .80),
        legend.title = element_blank(),
        text = element_text(size = 11),
        axis.title = element_text(face = 'bold'),
        strip.text = element_text(face = 'bold'))+
  guides(shape = guide_legend(override.aes = list(alpha = 1)))+
  facet_wrap(year_lab ~ .)

## Sigma convergence


e0_var <- e0 %>%
  filter(east != "Berlin") %>%
  group_by(sex, year) %>%
  do(onestage.variance.decomp(., macroregion = "east", ple = "e0")) %>%
  select(-Additive)%>%
  pivot_longer(var.total:var.between, values_to = 'variance', names_to = 'component', names_prefix = 'var.')


c <- ggplot(filter(e0_var, sex == 'male'), aes(x = year, y = variance, group = component, color = component, shape = component))+
  geom_vline(xintercept = 2006.5, alpha = 0.5)+
  geom_point(show.legend = F)+
  annotate(geom = 'text', x =2005, y = 1, label = 'Early period', hjust = 'right', size = 3, alpha = 0.5)+
  annotate(geom = 'text', x =2008, y = 1, label = 'Late period', hjust = 'left', size = 3, alpha = 0.5)+
  geom_smooth(se = FALSE, show.legend = F)+
  geom_dl(aes(label = str_to_title(component)), method = 'smart.grid')+
  ylim(c(0,1))+
  xlab("Year")+
  ylab(expression(paste(bold("Variance of "), bolditalic(e[0]))))+
  theme_bw()+
  theme(text = element_text(size = 12),
        axis.title = element_text(face = 'bold'),
        strip.text = element_text(face = 'bold'))+
  facet_wrap(. ~ str_to_title(sex))

d <- ggplot(filter(e0_var, sex =='female'), aes(x = year, y = variance, group = component, color = component, shape = component))+
  geom_vline(xintercept = 2006.5, alpha = 0.5)+
  geom_point(show.legend = F)+
  geom_smooth(se = FALSE, show.legend = F)+
  geom_dl(aes(label = str_to_title(component)), method = 'smart.grid')+
  ylim(c(0,1))+
  xlab("Year")+
  ylab(expression(paste(bold("Variance of "), bolditalic(e[0]))))+
  theme_bw()+
  theme(text = element_text(size = 12),
        axis.title = element_text(face = 'bold'),
        strip.text = element_text(face = 'bold'))+
  facet_wrap(. ~ str_to_title(sex))

set_null_device("png")

p3 <- plot_grid(a,b,c, d, ncol = 2, nrow = 2, align = "hv", axis = "tblr", labels = c('a', 'b', 'c', 'd'))

ggsave("figures/p3.png", p3, width = 190, height = 200, unit = 'mm')

ggsave("figures/p3.pdf", p3, width = 190, height = 200, unit = 'mm', family = 'Arial', device = cairo_pdf)


e0_var_east = e0 %>%
  filter(east != "Berlin") %>%
  group_by(sex, year, east) %>%
  do(onestage.variance.decomp(., macroregion = "east", ple = "e0")) %>%
  select(-Additive)%>%
  pivot_longer(var.total:var.between, values_to = 'variance', names_to = 'component', names_prefix = 'var.')

e0_var_table = e0_var %>% 
  mutate(east = 'Germany') %>%
  rbind(., e0_var_east) %>%
  filter(year %in% c(1997, 2006, 2007, 2016) & component == 'total') %>%
  select(-component) %>%
  pivot_wider(id_cols = c(sex, east), names_from = 'year', values_from = 'variance') %>%
  mutate(`1997-2006` = 100*(`2006` - `1997`) / (`1997` * 10),
         `2007-2016` = 100*(`2016` - `2007`) / (`2007` * 10),
         `1997-2016` = 100*(`2016` - `1997`) / (`1997` * 20)) %>%
  mutate(comparison = paste(east, sex, sep = ' - ')) %>%
  ungroup() %>%
  select(-c(`1997`, `2006`, `2007`, `2016`, sex, east))
  

table2 = left_join(average_changes_table, beta_models_table, by = 'comparison') %>%
  left_join(., e0_var_table, by = 'comparison') %>%
  select(-`1997-2016.x`, -`1997-2016.y`) %>%
  mutate(comparison = factor(comparison, levels = c('Germany - male', 'Germany - female', 'East - male', 'West - male', 'East - female', 'West - female'))) %>%
  arrange(comparison) %>%
  mutate_if(is.numeric, ~round(.x, digits = 2)) %>%
  flextable(.) %>%
  set_header_labels(comparison = NA,  `1997-2006` = 'Early period:\n1997-2006',  `2007-2016` = 'Late period:\n2007-2016', `1997-2006.x` = 'Early period:\n1997-2006',  `2007-2016.x` = 'Late period:\n2007-2016',
                    `1997-2006.y` = 'Early period:\n1997-2006', `2007-2016.y` = 'Late period:\n2007-2016')%>%
  add_header_row(values = c('','Average life expectancy change', 'Beta convergence coefficient (95% CI)', 
                            'Annual relative change in the variance (%)'),
                 colwidths = c(1,2,2,2)) %>%
  footnote(value = as_paragraph(c('Notes: *p < 0.05. If the beta coefficient is negative, life expectancy improved fastest in districts with the lowest life expectancy at the start.')), ref_symbols = c('')) %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 10, part = 'all') %>%
  align(align = 'left', part = 'all')

save_as_docx(table2, path = 'tables/table2.docx')


#Supplementary (Theil and weighted)


pop <- readRDS("temp_data/mid_year_pop.rds") %>%
  group_by(year, AGS, geo, sex) %>%
  summarise(pop = sum(pop))

e0 <- left_join(e0, pop, by = c("year", "AGS", "geo", "sex"))

e0_T <- e0 %>%
  group_by(sex, year) %>%
  do(Theil(df = ., ple = "e0")) %>%
  rename(theil = Ttotal)

e0_T_w <- e0 %>%
  group_by(sex, year) %>%
  do(Theil_w(df = ., ple = "e0", pop = 'pop')) %>%
  ungroup() %>%
  rename(w.theil = Ttotal)

e0_var_w <- e0 %>%
  group_by(sex, year) %>%
  summarise(w.variance = wtd.var(e0, pop))

e0_var_sens = e0_var %>%
  filter(component == 'total') %>%
  select(-component) %>%
  left_join(., e0_var_w, by = c('year', 'sex')) %>%
  left_join(., e0_T,  by = c('year', 'sex')) %>%
  left_join(., e0_T_w, by = c('year', 'sex')) %>%
  pivot_longer(variance:w.theil, names_to = 'measure', values_to = 'dispersion')
  


supp_p6 = ggplot(filter(e0_var_sens, measure %in% c('variance', 'w.variance')), aes(x = year, y = dispersion, group = measure, color = measure))+
  geom_point(show.legend = F)+
  geom_smooth(se = FALSE, show.legend = F)+
  geom_dl(aes(label = measure), method = 'smart.grid')+
  xlab("Year")+
  ylab(expression(paste("Variance of ", e[0])))+
  theme_bw()+
  facet_wrap(. ~ str_to_title(sex))

ggsave('figures/supp_p6.png', supp_p6)


supp_p7 = ggplot(filter(e0_var_sens, measure %in% c('theil', 'w.theil')), aes(x = year, y = dispersion, group = measure, color = measure))+
  geom_point(show.legend = F)+
  geom_smooth(se = FALSE, show.legend = F)+
  geom_dl(aes(label = measure), method = 'smart.grid')+
  xlab("Year")+
  ylab(expression(paste("Theil index of ", e[0])))+
  theme_bw()+
  facet_wrap(. ~ str_to_title(sex))

ggsave('figures/supp_p7.png', supp_p7)


## Repeating the analysis using alternative outcome

e25.75_diff <- readRDS("temp_data/e25.75_diff.rds")
e25.75 <- readRDS("temp_data/e25.75_df.rds")


average_changes_e25.75 = e25.75_diff %>%
  group_by(sex, year) %>%
  summarise(location = 'Germany', 
            e25.75_diff=mean(e25.75_diff))

average_changes_east_e25.75 = e25.75_diff %>%
  filter(east != 'Berlin') %>%
  group_by(sex, year, east) %>%
  summarise(e25.75_diff=mean(e25.75_diff)) %>%
  rename(location = east)

average_changes_table_e25.75 = rbind(average_changes_e25.75, average_changes_east_e25.75)

average_changes_table_e25.75 = average_changes_table_e25.75 %>% 
  mutate(comparison = paste(location, sex, sep = ' - ')) %>%
  ungroup() %>%
  select(comparison, year, e25.75_diff) %>%
  pivot_wider(names_from = year, values_from = e25.75_diff) %>%
  select(comparison, `1997-2006`, `2007-2016`)


unconditional_beta_e25.75 <- function(data){lm(e25.75_diff ~ e25.75, data = data)}

unconditional_beta_weighted_e25.75 <- function(data){lm(e25.75_diff ~ e25.75, data = data, weights = pop)}

extract_coeff <- function(mod){round(coef(summary(mod))[2,1], 2)}
extract_CI <- function(mod){round(confint(mod, "e25.75", level = 0.95),2)}
extract_p <- function(mod){round(coef(summary(mod))[2,4], 2)}

beta_models_e25.75 <- e25.75_diff %>%
  group_by(sex, year) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta_e25.75),
         beta = map(models, extract_coeff),
         ci = map(models, extract_CI),
         pval = map(models, extract_p)) %>%
  select(sex, year, beta, ci, pval) %>%
  mutate(location = 'Germany')


beta_models_east_e25.75 <- e25.75_diff %>%
  filter(east != 'Berlin') %>%
  group_by(sex, year, east) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta_e25.75),
         beta = map(models, extract_coeff),
         ci = map(models, extract_CI),
         pval = map(models, extract_p)) %>%
  select(sex, year, location = east, beta, ci, pval)

beta_models_table_e25.75 = rbind(beta_models_e25.75, beta_models_east_e25.75)

beta_models_table_e25.75 = beta_models_table_e25.75 %>% 
  mutate(star = ifelse(pval < 0.05, '*', ''), 
         ci = str_remove(as.character(ci), 'c'),
         result = paste0(beta, star, '\n', ci),
         comparison = paste(location, sex, sep = ' - ')) %>%
  ungroup() %>%
  select(comparison, year, result) %>%
  pivot_wider(names_from = year, values_from = result)


e25.75_var <- e25.75 %>%
  filter(east != "Berlin") %>%
  group_by(sex, year) %>%
  do(onestage.variance.decomp(., macroregion = "east", ple = "e25.75")) %>%
  select(-Additive)%>%
  pivot_longer(var.total:var.between, values_to = 'variance', names_to = 'component', names_prefix = 'var.')

e25.75_var_east = e25.75 %>%
  filter(east != "Berlin") %>%
  group_by(sex, year, east) %>%
  do(onestage.variance.decomp(., macroregion = "east", ple = "e25.75")) %>%
  select(-Additive)%>%
  pivot_longer(var.total:var.between, values_to = 'variance', names_to = 'component', names_prefix = 'var.')

e25.75_var_table = e25.75_var %>% 
  mutate(east = 'Germany') %>%
  rbind(., e25.75_var_east) %>%
  filter(year %in% c(1997, 2006, 2007, 2016) & component == 'total') %>%
  select(-component) %>%
  pivot_wider(id_cols = c(sex, east), names_from = 'year', values_from = 'variance') %>%
  mutate(`1997-2006` = 100*(`2006` - `1997`) / (`1997` * 10),
         `2007-2016` = 100*(`2016` - `2007`) / (`2007` * 10),
         `1997-2016` = 100*(`2016` - `1997`) / (`1997` * 20)) %>%
  mutate(comparison = paste(east, sex, sep = ' - ')) %>%
  ungroup() %>%
  select(-c(`1997`, `2006`, `2007`, `2016`, sex, east))



supp_table1 = left_join(average_changes_table_e25.75, beta_models_table_e25.75, by = 'comparison') %>%
  left_join(., e25.75_var_table, by = 'comparison') %>%
  mutate(comparison = factor(comparison, levels = c('Germany - male', 'Germany - female', 'East - male', 'West - male', 'East - female', 'West - female'))) %>%
  arrange(comparison) %>%
  mutate_if(is.numeric, ~round(.x, digits = 2)) %>%
  flextable(.) %>%
  set_header_labels(comparison = NA,  `1997-2006` = 'Early period:\n1997-2006',  `2007-2016` = 'Late period:\n2007-2016', `1997-2006.x` = 'Early period:\n1997-2006',  `2007-2016.x` = 'Late period:\n2007-2016', `1997-2016.x` = 'Total period:\n1997-2016',
                    `1997-2006.y` = 'Early period:\n1997-2006', `2007-2016.y` = 'Late period:\n2007-2016', `1997-2016.y` = 'Total period:\n1997-2016')%>%
  add_header_row(values = c('','Average life expectancy change', 'Beta convergence coefficient (95% CI)', 
                            'Annual relative change in total dispersion, measured by variance (%)'),
                 colwidths = c(1,2,3,3)) %>%
  footnote(value = as_paragraph(c('Notes:*p < 0.05. If the beta coefficient is negative, life expectancy improved fastest in districts with the lowest life expectancy at the start.')), ref_symbols = c('')) %>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 10, part = 'all') %>%
  align(align = 'left', part = 'all')

save_as_docx(supp_table1, path = 'tables/supp_table1.docx')

