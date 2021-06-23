
# load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(cowplot)
library(lubridate)
library(scales)


# set colors --------------------------------------------------------------

site_colours <- c(`Site 1` = '#5b9bd5',
                  `Site 2` = '#70ad47',
                  `Site 3` = '#a5a5a5',
                  `Site 4` = '#ffc000',
                  `Site 5` = '#ed7d31',
                  `Site 6` = '#4472c4'
)

# load data ---------------------------------------------------------------

dat <- readxl::read_xlsx(here::here('data', 'SH-data.xlsx'),
                         sheet = 'sh-env') %>%
  janitor::clean_names()

sjrwmd_dat <- readxl::read_xlsx(here::here('data', 'SJRWMD_Marineland_Data_2010-2020.xlsx'),
                                sheet = "SJRWMD_Data_by_Parameter") %>%
  janitor::clean_names()

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
                                          'post')),
               site_number = as.character(site_number),
               site_no = paste0('Site ', site_number))

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


# boxplots -------------------------------------------------------------

# all sites, dredge timeframes

# set up new strip title names
dredge_labs <- c("Dredge", "Post-Dredge")
names(dredge_labs) <- c('dredge', 'post')

dat %>%
  ggplot(aes(x = site_no, y = average_sal_pss, fill = site_no)) +
  geom_boxplot() +
  facet_grid(. ~ dredge,
             labeller = labeller(dredge = dredge_labs)) +
  scale_fill_manual(values = site_colours) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 12, color = 'black')) +
  labs(x = '',
       y = 'Salinity (psu)')

# single param, site, pre/post dredge

# create label for chlorophyll plots
chla_y_title <- expression(paste("Chlorophyll a  ", mu*"g/L"))
# all sites
dat %>%
  ggplot(aes(x = site_no, y = chl_a_ug_l)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  scale_fill_manual(name = "Dredge Period", values = c('#00798c','#edae49')) +
  ggpubr::theme_classic2() +
  theme(text = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 12, color = 'black')) +
  labs(x = "",
       y = chla_y_title)

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
din_y_title <- expression(paste("Dissolved Inorganic Nitrogen   ", mu*"M"))
dat %>%
  ggplot(aes(x = site_no, y = din_u_m)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  # geom_point(aes(fill = dredge), size = 2, shape = 21, position = position_jitterdodge()) +
  scale_fill_manual(name = "Dredge Period", values = c('#00798c','#edae49')) +
  ggpubr::theme_classic2() +
  theme(text = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 12, color = 'black')) +
  labs(x = "",
       y = din_y_title)

# tss
dat %>%
  ggplot(aes(x = site_no, y = corr_tss_mg_l)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8) +
  scale_fill_manual(name = "Dredge Period", values = c('#00798c','#edae49')) +
  ggpubr::theme_classic2() +
  theme(text = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 12, color = 'black')) +
  labs(x = "",
       y = "Total Suspended Solids (mg/L)")

# turb
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


# individual sites, dredge timeframes

dat %>%
  filter(site == 'SB' & dredge != "pre") %>%
  ggplot(aes(x = dredge, y = chl_a_ug_l)) +
  geom_boxplot(aes(fill = dredge), alpha = 0.8, notch = TRUE) +
  theme_cowplot()



# line graphs -------------------------------------------------------------

# CHLA
sjr_dat_c <- sjr_dat %>% filter(parameter == "Chl-a")
chla_y_title <- expression(paste("Chlorophyll  ", italic("a "), mu*"g/L"))

dat %>%
  filter(dredge != "pre") %>%
  ggplot() +
  geom_ribbon(data = sjr_dat_c, aes(x = date,
                                    ymin = (mean - sd),
                                    ymax = (mean + sd)),
              fill = "gray90") +
  geom_line(aes(x = date, y = chl_a_ug_l, color = site_no), size = 1) +
  geom_point(aes(x = date, y = chl_a_ug_l, color = site_no), size = 1) +
  geom_line(data = sjr_dat_c, aes(x = date, y = mean),
            color = "black",
            linetype = "longdash",
            size = 1) +
  scale_color_manual(values = site_colours) +
  scale_x_datetime(date_breaks = "months", date_labels = "%y-%b") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        legend.title = element_blank(),
        text = element_text(size = 12)) +
  labs(x = "Date (Year-Month)",
       y = chla_y_title)













dat %>%
  # filter(dredge != "pre") %>%
  ggplot(aes(x = date, y = chl_a_ug_l, color = site)) +
  geom_line(size = 1) +
  # geom_point() +
  scale_x_datetime(date_breaks = "months", date_labels = "%Y-%b") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))
