
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
dat %>%
  filter(dredge != "pre") %>%
  ggplot(aes(x = site_no, y = average_sal_pss)) +
  geom_boxplot(aes(fill = site_no), alpha = 0.8) +
  facet_grid(. ~ dredge) +
  scale_color_manual(values = site_colours) +
  scale_fill_manual(values = site_colours) +
  theme_bw() +
  theme(legend.title = element_blank())

# create label for chlorophyll plots
chla_y_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L"))
# all sites
dat %>%
  dplyr::filter(dredge != "pre") %>%
  ggplot(aes(x = site, y = chl_a_ug_l)) +
  geom_boxplot(aes(fill = dredge)) +
  scale_fill_manual(name = "Dredge Period", values = c('darkturquoise','darkorange')) +
  theme_cowplot() +
  labs(x = "Site",
       y = chla_y_title)

#dissolved oxygen
dat %>%
  dplyr::filter(dredge != "pre") %>%
  ggplot(aes(x = site, y = d_o_mg_l)) +
  geom_boxplot(aes(fill = dredge)) +
  scale_fill_manual(name = "Dredge Period", values = c('darkturquoise','darkorange')) +
  theme_cowplot() +
  labs(x = "Site",
       y = "Dissolved Oxygen mg/L")



# individual sites, dredge timeframes

dat %>%
  filter(site == 'SB') %>%
  ggplot(aes(x = dredge, y = chl_a_ug_l)) +
  geom_boxplot(aes(color = dredge, fill = dredge), alpha = 0.5) +
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
              fill = "gray88") +
  geom_line(data = sjr_dat_c, aes(x = date, y = mean),
            color = "black",
            size = 1) +
  geom_line(aes(x = date, y = chl_a_ug_l, color = site_no), size = 1) +
  geom_point(aes(x = date, y = chl_a_ug_l, color = site_no), size = 1) +
  scale_color_manual(values = site_colours) +
  scale_x_datetime(date_breaks = "months", date_labels = "%Y-%b") +
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
