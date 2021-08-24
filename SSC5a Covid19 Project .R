library(tidyverse)
library(ggplot2)
library(dplyr)

theme_set(theme_minimal())

covid_global <- read_csv('https://covid.ourworldindata.org/data/owid-covid-data.csv')

covid_global %>%
  filter(iso_code == 'GBR') %>%
ggplot(aes(x = date, y = total_deaths)) + geom_line() +
scale_x_date(date_breaks = "1 month", date_labels = '%d %b')


covid_global %>%
  filter(iso_code == 'GBR') %>%
  ggplot(aes(x = date, y = new_deaths)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D')
