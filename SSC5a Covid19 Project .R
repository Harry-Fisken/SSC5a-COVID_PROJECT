library(tidyverse)
library(ggplot2)

theme_set(theme_minimal())

covid_global <- read_csv('https://covid.ourworldindata.org/data/owid-covid-data.csv')


covid_UK <- covid_global %>% filter(iso_code == 'GBR')


Colnames.Covid <- colnames(covid_UK, do.NULL = TRUE, prefix = "col")
View(Colnames.Covid)

#Total Number of Deaths 
covid_UK %>%
  ggplot(aes(x = date, y = total_deaths)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D')

#New Deaths#
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
covid_global %>%
  filter(iso_code == 'GBR') %>%
  ggplot(aes(x = date, y = icu_patients)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'ICU Patients (n)', title = 'Number of COVID-19 ICU Patients in the UK Between March 2020 - August 2021')

###Weekly Hospital Admissions###

covid_UK %>% filter(!is.na(weekly_hosp_admissions)) %>% 
  
  ggplot(aes(x = date, y = weekly_hosp_admissions)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'Hospital Patients (n)', title = 'Number of COVID-19 Hospital Patients in the UK Between March 2020 - August 2021')





##Regression Discontinuity##

start_time = as.Date(2020-06-19)

change_time = as.Date(2020-07-24)

end_time = as.Date(2020-08-23)

covid_UK <- filter(covid_UK, date >= start_time & date <= end_time)

covid_UK <- mutate(covid_UK,
                   day = as.numeric(date - start_time),
                   change = ifelse(date > change_time, 1, 0),
                   days_postchange = day * change)

Keep getting this error - Error in as.Date.numeric(2020 - 6 - 19) : 'origin' must be supplied for the first three lines of code, i have tried to fix it as below but have had no luck.

##Model 0

model0 <-glm(new_deaths ~ day+change+days_postchange, family="poisson", data = covid_UK)


### Attempt to fix the problem with no luck as of yet  ###
  
options(date.origin = "1970-01-01")
as.date <- function(x, origin = getOption("date.origin"))

start_time <- 2020-06-19
start_date <- as.Date(start_time, format = "%y-%m-%d")

change_time <- 2020-07-24
change_date <- as.Date(change_time, format = "%d-%m-%y")
  
end_time <- 2020-08-23
end_date <- as.Date(end_time, format = "%d-%m-%y")


covid_UK <- filter(covid_UK, date >= start_date & date <= end_date)

covid_UK <- mutate(covid_UK, 
                   day = as.numeric(date - start_date),
                   change = ifelse(date > change_date, 1, 0),
                   days_postchange = day * change)




Colnames.Covid2 <- colnames(covid_UK, do.NULL = TRUE, prefix = "col")
View(Colnames.Covid2)
view(covid_UK)

#Model 0

model0 <-glm(new_deaths ~ day+change+days_postchange, family="poisson", data = covid_UK)




