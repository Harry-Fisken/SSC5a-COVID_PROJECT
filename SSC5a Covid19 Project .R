library(tidyverse)
library(ggplot2)

theme_set(theme_minimal())

covid_global <- read_csv('https://covid.ourworldindata.org/data/owid-covid-data.csv')


covid_UK <- covid_global %>% filter(iso_code == 'GBR')


#Total Number of Deaths 
covid_UK %>%
  ggplot(aes(x = date, y = total_deaths)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D')

#New Deaths
covid_global %>%
  filter(iso_code == 'GBR') %>%
  ggplot(aes(x = date, y = new_deaths)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'Confirmed New Deaths (n)', title = 'Number of Confirmed New COVID-19 Deaths in the UK')

covid_global %>%
  filter(iso_code == 'GBR') %>%
  ggplot(aes(x = date, y = )) + geom_line() +
  scale_x_date(date_breaks = "", date_labels = '%D')

#Daily COVID-19 Tests 
covid_UK %>%
  ggplot(aes(x = date, y = new_tests)) + geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'New COVID-19 Tests (n)', title = 'Number of Daily COVID-19 Tests in the UK from March 2020 - August 2021')


#Number of Hopsital Patients 
covid_UK %>%
  
  covid_UK %>% filter(!is.na(weekly_hosp_admissions)) %>% 
  
  ggplot(aes(x = date, y = weekly_hosp_admissions)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'Hospital Patients (n)', title = 'Number of COVID-19 Hospital Patients in the UK Between March 2020 - August 2021')

#Number of Hopsital Patients 
covid_global %>%
  filter(iso_code == 'GBR') %>%
  ggplot(aes(x = date, y = icu_patients)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'ICU Patients (n)', title = 'Number of COVID-19 ICU Patients in the UK Between March 2020 - August 2021')

##Weekly Hospital Admissions###

covid_UK %>% filter(!is.na(weekly_hosp_admissions)) %>% 
  
  ggplot(aes(x = date, y = weekly_hosp_admissions)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'Hospital Patients (n)', title = 'Number of COVID-19 Hospital Patients in the UK Between March 2020 - August 2021')









##Regression Discontinuity##

#Model 0

covid_UK %>%
  model0 <-glm(new-hosp_admissions ~ 2020-06-19 + 2020-07-24 + 2020-08-28, family="poisson", data=covid_UK, subset=year01==0)


