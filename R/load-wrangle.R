
# load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(cowplot)
library(lubridate)
library(scales)
library(patchwork)


# set colors --------------------------------------------------------------

site_colours <- c(`Site 1` = '#5b9bd5',
                  `Site 2` = '#70ad47',
                  `Site 3` = '#a5a5a5',
                  `Site 4` = '#ffc000',
                  `Site 5` = '#ed7d31',
                  `Site 6` = '#4472c4'
)

# 01 load data ---------------------------------------------------------------

dat <- readxl::read_xlsx(here::here('data', 'SH-data.xlsx'),
                         sheet = 'sh-env') %>%
  janitor::clean_names()

sjrwmd_dat <- readxl::read_xlsx(here::here('data', 'SJRWMD_Marineland_Data_2010-2020.xlsx'),
                                sheet = "SJRWMD_Data_by_Parameter") %>%
  janitor::clean_names()


# 02 wrangle and tidy data ---------------------------------------------------

## sh nut
dat <- dat %>%
        filter(dredge != "pre") %>%
        mutate(site = factor(site,
                             levels = c('RI',
                                        'MI',
                                        'SB',
                                        'DC',
                                        'ML',
                                        'BL')),
               site_long = factor(site_long,
                                  levels = c('Rattlesnake Island',
                                             'Matanzas Inlet',
                                             'Summerhaven Bridge',
                                             'Dolphin Creek',
                                             'Marineland',
                                             'Bings Landing')),
               dredge = factor(dredge,
                               levels = c('pre',
                                          'dredge',
                                          'post dredge')),
               site_number = as.character(site_number),
               site_no = paste0('Site ', site_number))

## sjr nut
sjr_new <- sjrwmd_dat %>%
            rename(datetime = sample_collection_date_and_time) %>%
            select(datetime, parameter, unit_of_measure, measured_value, mdl, pql) %>%
            mutate(month = month(datetime),
                   day = day(datetime),
                   year = as.character(year(datetime))) %>%
            group_by(month, parameter) %>%
            summarise(mean = mean(measured_value, na.rm = TRUE),
                      sd = sd(measured_value, na.rm = TRUE)) %>%
            ungroup()


sjr_2017 <- sjr_new %>%
  mutate(day = 2,
         year = 2017,
         date = lubridate::ymd(paste0(year, '-', month, '-', day)),
         date = as.POSIXct(date, tz = "America/NewYork")
  )

sjr_2018 <- sjr_new %>%
  mutate(day = 2,
         year = 2018,
         date = lubridate::ymd(paste0(year, '-', month, '-', day)),
         date = as.POSIXct(date, tz = "America/NewYork")
  ) %>%
  filter(date < '2018-10-31')

sjr_dat <- bind_rows(sjr_2017, sjr_2018) %>%
  mutate(site_no = "SJRWMD 2010-2016")

rm(sjr_2017, sjr_2018)


# 03 create boxplots -------------------------------------------------------------

## 03.1 all sites, dredge timeframes ----

# single param, site, pre/post dredge

# chlorophyll a
dat %>%
  ggplot(aes(x = site_no, y = chl_a_ug_l)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  scale_fill_manual(name = "Dredge Period", values = c('#00798c','#edae49')) +
  ggpubr::theme_classic2() +
  theme(text = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 12, color = 'black')) +
  labs(x = "",
       y = "Chlorophyll a (\U00B5g/L)")

#dissolved oxygen
dat %>%
  ggplot(aes(x = site_no, y = d_o_mg_l)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  # geom_point(aes(fill = dredge), size = 2, shape = 21, position = position_jitterdodge()) +
  scale_fill_manual(name = "Dredge Period", values = c('#00798c','#edae49')) +
  ggpubr::theme_classic2() +
  theme(text = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 12, color = 'black')) +
  labs(x = "",
       y = "Dissolved Oxygen mg/L")

# din
dat %>%
  ggplot(aes(x = site_no, y = din_u_m)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  # geom_point(aes(fill = dredge), size = 2, shape = 21, position = position_jitterdodge()) +
  scale_fill_manual(name = "Dredge Period", values = c('#00798c','#edae49')) +
  ggpubr::theme_classic2() +
  theme(text = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 12, color = 'black')) +
  labs(x = "",
       y = "Dissolved Inorganic Nitrogen \U00B5M")

# tss
tss <- dat %>%
  ggplot(aes(x = site_no, y = corr_tss_mg_l)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  scale_fill_manual(name = "Dredge Period", values = c('#00798c','#edae49')) +
  scale_y_continuous(breaks = seq(0, 120, by = 10), expand= c(0,0), limits = c(0,130)) +
  theme_classic() +
  theme(text = element_text(family = "sans"),
        panel.grid.major.y = element_line(colour = c("white")),
        axis.text.y = element_text(colour = c("black", NA, NA, NA), size = 12), # NA built in to create gaps in axis
        axis.text.x = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 20, face = "bold")) +
  labs(x = "",
       y = "Total Suspended Solids (mg/L)")


# ammonium
nh4 <- dat %>%
  ggplot(aes(x = site_no, y = ammonium_u_m)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  scale_fill_manual(name = "Dredge Period", values = c('#00798c','#edae49')) +
  scale_y_continuous(breaks = seq(0, 6, by = 1), expand= c(0,0), limits = c(0,7)) +
  theme_classic() +
  theme(text = element_text(family = "sans"),
        panel.grid.major.y = element_line(colour = c("white")),
        axis.text.y = element_text(colour = c("black", NA), size = 12),
        axis.text.x = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 20, face = "bold")) +
  labs(x = "",
       y = "Ammonium \U00B5M")



## turb
dat %>%
  ggplot(aes(x = site_no, y = turbidiity_ntu)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  scale_fill_manual(name = "Dredge Period", values = c('#00798c','#edae49')) +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  ggpubr::theme_classic2() +
  theme(text = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 12, color = 'black')) +
  labs(x = "",
       y = "Turbidity (NTU)")

## SALINITY
sal <- dat %>%
  ggplot(aes(x = site_no, y = average_sal_pss)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  scale_fill_manual(name = "Dredge Period", values = c('#00798c','#edae49')) +
  scale_y_continuous(breaks = seq(0, 40, by = 1), expand= c(0,0), limits = c(10,41)) +
  theme_classic() +
  theme(text = element_text(family = "sans"),
        panel.grid.major.y = element_line(colour = c("white")),
        axis.text.y = element_text(colour = c("black", NA, NA, NA, NA), size = 12),
        axis.text.x = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 20, face = "bold"))+
  labs(x = "",
       y = "Salinity (psu)")




# individual sites, dredge timeframes

dat %>%
  filter(site == 'SB' & dredge != "pre") %>%
  ggplot(aes(x = dredge, y = chl_a_ug_l)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8, notch = TRUE) +
  theme_cowplot()


## 03.2 only Site 5 and sig params: SAL, NH4, TSS ----

sjr_comp <- sjrwmd_dat %>%
  filter(parameter %in% c('NH4-D', 'TSS', 'Salinity') & measured_value > 0) %>%
  select(parameter, measured_value) %>%
  mutate(dredge = "SJRWMD 2010-2016")

sjr_nh4 <- sjr_comp %>%
  filter(parameter == "NH4-D") %>%
  mutate(ammonium_u_m = (measured_value*1000/14.01)) %>%
  select(-parameter)

sjr_sal <- sjr_comp %>%
  filter(parameter == "Salinity") %>%
  rename(average_sal_pss = measured_value) %>%
  select(-parameter)

sjr_tss <- sjr_comp %>%
  filter(parameter == "TSS") %>%
  rename(corr_tss_mg_l = measured_value) %>%
  select(-parameter)

# NH4
nh4_site5 <- dat %>%
  filter(site_number == 5 & dredge != "pre") %>%
  select(ammonium_u_m, dredge) %>%
  bind_rows(sjr_nh4) %>%
  ggplot(aes(x = dredge, y = ammonium_u_m)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  scale_fill_manual(values = c('#00798c','#edae49', 'gray')) +
  scale_y_continuous(breaks = seq(0, 6, by = 1), expand= c(0,0), limits = c(0,7)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "sans"),
        panel.grid.major.y = element_line(colour = c("white")),
        axis.text.y = element_text(colour = c("black", NA), size = 12),
        axis.text.x = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 20, face = "bold")) +
  theme(legend.title = element_blank()) +
  labs(x = '',
       y = 'Ammonium \U00B5M')

# TSS
tss_site5 <- dat %>%
  filter(site_number == 5 & dredge != "pre") %>%
  select(corr_tss_mg_l, dredge) %>%
  bind_rows(sjr_tss) %>%
  ggplot(aes(x = dredge, y = corr_tss_mg_l)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  scale_fill_manual(values = c('#00798c','#edae49', 'gray')) +
  scale_y_continuous(breaks = seq(0, 120, by = 10), expand= c(0,0), limits = c(0,130)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "sans"),
        panel.grid.major.y = element_line(colour = c("white")),
        axis.text.y = element_text(colour = c("black", NA, NA, NA), size = 12),
        axis.text.x = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 20, face = "bold")) +
  labs(x = '',
       y = 'Total Suspended Solids (mg/L)')

# Salinity
sal_site5 <- dat %>%
  filter(site_number == 5 & dredge != "pre") %>%
  select(average_sal_pss, dredge) %>%
  bind_rows(sjr_sal) %>%
  ggplot(aes(x = dredge, y = average_sal_pss)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  scale_fill_manual(values = c('#00798c','#edae49', 'gray')) +
  scale_y_continuous(breaks = seq(0, 40, by = 1), expand= c(0,0), limits = c(10,41)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "sans"),
        panel.grid.major.y = element_line(colour = c("white")),
        axis.text.y = element_text(colour = c("black", NA, NA, NA), size = 12),
        axis.text.x = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 20, face = "bold")) +
  labs(x = '',
       y = 'Salinity (psu)')

# 03.3 multiplots ----
multiplot_1 <-
(sal +
   labs(title = "Salinity (psu)") +
   theme(legend.position = "none",
         axis.title.y = element_blank(),
         axis.text.x = element_blank(),
         title = element_text(size = 14))) /
  (tss +
     labs(title = "Total Suspended Solids (mg/L)") +
     theme(legend.position = "none",
           axis.text.x = element_blank(),
           axis.title.y = element_blank(),
           title = element_text(size = 14))) /
    (nh4 +
       labs(title = "Ammonium (\U00B5M)") +
       theme(legend.position = "bottom",
             axis.title.y = element_blank(),
             title = element_text(size = 14)))

# multiplot_2 <-
#   (sal_site5 +
#      labs(title = "Salinity (psu)") +
#      theme(legend.position = "none",
#            axis.title.y = element_blank(),
#            axis.text.x = element_blank(),
#            title = element_text(size = 14))) /
#   (tss_site5 +
#      labs(title = "Total Suspended Solids (mg/L)") +
#      theme(legend.position = "none",
#            axis.text.x = element_blank(),
#            axis.title.y = element_blank(),
#            title = element_text(size = 14))) /
#   (nh4_site5 +
#      labs(title = "Ammonium (\U00B5M)") +
#      theme(legend.position = "bottom",
#            axis.title.y = element_blank(),
#            title = element_text(size = 14)))

# 04 save plots -----------------------------------------------------------

## single param, all sites
ggsave(here::here('output','tss_v2.png'), plot = tss, dpi = 300)
ggsave(here::here('output','ammonium_v2.png'), plot = nh4, dpi = 300)
ggsave(here::here('output','salinity_v2.png'), plot = sal, dpi = 300)
## single param, site 5 with SJRWMD data
ggsave(here::here('output', 'nh4_site5_v2.png'), plot = nh4_site5, dpi = 300)
ggsave(here::here('output', 'tss_site5_v2.png'), plot = tss_site5, dpi = 300)
ggsave(here::here('output', 'sal_site5_v2.png'), plot = sal_site5, dpi = 300)
## multiplot of single param, all sites
ggsave(here::here('output','multiplot_v1.png'), plot = multiplot, dpi = 300)
