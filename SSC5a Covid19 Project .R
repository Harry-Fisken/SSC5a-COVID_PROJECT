library(tidyverse)
library(ggplot2)

theme_set(theme_minimal())

covid_global <- read_csv('https://covid.ourworldindata.org/data/owid-covid-data.csv')


covid_UK <- covid_global %>% filter(iso_code == 'GBR')


Colnames.Covid <- colnames(covid_UK)
View(Colnames.Covid)

#Total Number of Deaths 
covid_UK %>%
  ggplot(aes(x = date, y = total_deaths)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D')

#New Deaths#
# Steven: You already created covid_UK - use it
#covid_global %>%
#  filter(iso_code == 'GBR') %>%
covid_UK %>%  ggplot(aes(x = date, y = new_deaths)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'Confirmed New Deaths (n)', title = 'Number of Confirmed New COVID-19 Deaths in the UK')

# covid_global %>%
#   filter(iso_code == 'GBR') %>%
covid_UK %>% ggplot(aes(x = date, y = )) + geom_line() +
  scale_x_date(date_breaks = "", date_labels = '%D')

#Daily COVID-19 Tests# 
covid_UK %>%
  ggplot(aes(x = date, y = new_tests)) + geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'New COVID-19 Tests (n)', title = 'Number of Daily COVID-19 Tests in the UK from March 2020 - August 2021')


#Number of Hopsital Patients# 
covid_UK %>%
  
covid_UK %>% filter(!is.na(weekly_hosp_admissions)) %>% 
  ggplot(aes(x = date, y = weekly_hosp_admissions)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'Hospital Patients (n)', title = 'Number of COVID-19 Hospital Patients in the UK Between March 2020 - August 2021')

#Number of Hopsital Patients# 
# covid_global %>%
#   filter(iso_code == 'GBR') %>%
covid_UK %>%
  ggplot(aes(x = date, y = icu_patients)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'ICU Patients (n)', title = 'Number of COVID-19 ICU Patients in the UK Between March 2020 - August 2021')

###Weekly Hospital Admissions###
# Steven : this is a repetition of what came above
covid_UK %>% filter(!is.na(weekly_hosp_admissions)) %>% 
  ggplot(aes(x = date, y = weekly_hosp_admissions)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'Hospital Patients (n)', title = 'Number of COVID-19 Hospital Patients in the UK Between March 2020 - August 2021')





##Regression Discontinuity##

##Effects of Mask Mandate in the UK##

start_time = as.Date('2020-06-19')

change_time = as.Date('2020-07-24')

end_time = as.Date('2020-08-23')

covid_UK <- filter(covid_UK, date >= start_time & date <= end_time)

covid_UK <- mutate(covid_UK,
                   day = as.numeric(date - start_time),
                   change = ifelse(date > change_time, 1, 0),
                   days_postchange = day * change)




model0 <-glm(new_deaths ~ day+change+days_postchange, family="poisson", data = covid_UK)
summary(model0)

model1 <-glm(new_cases ~ day+change+days_postchange, family="poisson", data = covid_UK)
summary(model1)

model2 <-glm(weekly_hosp_admissions ~ day+change+days_postchange, family="poisson", data = covid_UK)
summary(model2)

model3 <-glm (weekly_icu_admissions ~ day+change+days_postchange, family="poisson", data = covid_UK)
summary(model3)

##model3 isnt working##
  

##Effects of Lockdown 1 (March 23 2020)##  

start_time = as.Date('2020-02-24')

change_time = as.Date('2020-03-23')

end_time = as.Date('2020-04-13')

covid_UK <- filter(covid_UK, date >= start_time & date <= end_time)

covid_UK <- mutate(covid_UK,
                   day = as.numeric(date - start_time),
                   change = ifelse(date > change_time, 1, 0),
                   days_postchange = day * change)

model4 <-glm(new_deaths ~ day+change+days_postchange, family="poisson", data = covid_UK)
summary(model4)

model5 <-glm(new_cases ~ day+change+days_postchange, family="poisson", data = covid_UK)
summary(model5)

model6 <-glm(weekly_hosp_admissions ~ day+change+days_postchange, family="poisson", data = covid_UK)
summary(model6)

model7 <- glm(weekly_icu_admissions ~ day+change+days_postchange, family = "poisson", data = covid_UK)
summary(model7)

##model 7 isnt working - clearly doesnt like ICU admissions lol##


