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
  scale_x_date(date_breaks = "40 days", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'Confirmed New Deaths (n)', title = 'Number of Confirmed New COVID-19 Deaths in the UK') 

# covid_global %>%
#   filter(iso_code == 'GBR') %>%
#covid_UK %>% ggplot(aes(x = date, y = )) + geom_line() +
 # scale_x_date(date_breaks = "", date_labels = '%D')

#Daily COVID-19 Tests# 
covid_UK %>%
  ggplot(aes(x = date, y = new_tests)) + geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'New COVID-19 Tests (n)', title = 'Number of Daily COVID-19 Tests in the UK from March 2020 - August 2021')


#Number of Hopsital Patients# 
#covid_UK %>%
  
covid_UK %>% filter(!is.na(weekly_hosp_admissions)) %>% 
  ggplot(aes(x = date, y = weekly_hosp_admissions)) + geom_line() +
  scale_x_date(date_breaks = "30 days", date_labels = '%D') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = 'Hospital Patients (n)', title = 'Number of COVID-19 Hospital Patients in the UK Between March 2020 - August 2021')

#Number of Hopsital Patients# 
# covid_global %>%
#   filter(iso_code == 'GBR') %>%
covid_UK %>%
  ggplot(aes(x = date, y = icu_patients)) + geom_line() +
  scale_x_date(date_breaks = "30 days", date_labels = '%D') +
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

covid_UK_1 <- filter(covid_UK, date >= start_time & date <= end_time)

covid_UK_1 <- mutate(covid_UK_1,
                   day = as.numeric(date - start_time),
                   change = ifelse(date > change_time, 1, 0),
                   days_postchange = day * change)




model0 <-glm(new_deaths ~ day+change+days_postchange, family="poisson", data = covid_UK_1)
summary(model0)

model1 <-glm(new_cases ~ day+change+days_postchange, family="poisson", data = covid_UK_1)
summary(model1)

model2 <-glm(weekly_hosp_admissions ~ day+change+days_postchange, family="poisson", data = covid_UK_1)
summary(model2)

#Present the predictors of the model
#Shows how the model fits the data for models 0-2 (mask mandate)

##model0##

covid_UK_1 <- covid_UK_1 %>%
  mutate(prediction_model0=predict(model0,type="response"))

# covid_UK_1 <- covid_UK_1 %>%
#   mutate(prediction_model0=0)

# covid_UK_1$prediction_model0=predict(model0,type="response")

ggplot(covid_UK_1, aes(x=date, col=new_deaths)) +
  geom_line(aes(y=prediction_model0)) +
  scale_y_log10() +
  geom_point(aes(y=new_deaths))+
  labs(x = "Date", y = 'COVID-19 Deaths by Day (n)')
  

##model1##
covid_UK_1 <- covid_UK_1 %>%
  mutate(prediction_model1=predict(model1,type="response"))


# covid_U_1K <- covid_UK_1 %>%
#   mutate(prediction_model1=0)
# 
# covid_UK_1$prediction_model1=predict(model1,type="response")



ggplot(covid_UK_1, aes(x=date, col=new_cases)) +
  geom_line(aes(y=prediction_model1)) +
  scale_y_log10() +
  geom_point(aes(y=new_cases)) +
  labs(x = "Date", y = 'COVID-19 Cases by Day (n)')


##model2##

covid_UK_1 <- covid_UK_1 %>%
  mutate(prediction_model2=predict(model2,type="response"))
# 
# covid_UK_1 <- covid_UK_1 %>%
#   mutate(prediction_model2=0)
# 
# covid_UK_1$prediction_model2=predict(model2,type="response")

ggplot(covid_UK_1, aes(x=date, col=weekly_hosp_admissions)) +
  geom_line(aes(y=prediction_model2)) +
  scale_y_log10() +
  geom_point(aes(y=weekly_hosp_admissions))+
  labs(x = "Date", y = 'Weekly Hospital Admissions (n)')

##keep getting an error here 
  
##Effects of Lockdown 1 (March 23 2020)##  

start_time = as.Date('2020-02-24')

change_time = as.Date('2020-03-23')

end_time = as.Date('2020-04-13')

covid_UK_2 <- filter(covid_UK, date >= start_time & date <= end_time)

covid_UK_2 <- mutate(covid_UK_2,
                   day = as.numeric(date - start_time),
                   change = ifelse(date > change_time, 1, 0),
                   days_postchange = day * change)

model4 <-glm(new_deaths ~ day+change+days_postchange, family="poisson", data = covid_UK_2)
summary(model4)

model5 <-glm(new_cases ~ day+change+days_postchange, family="poisson", data = covid_UK_2)
summary(model5)

model6 <-glm(weekly_hosp_admissions ~ day+change+days_postchange, family="poisson", data = covid_UK_2)
summary(model6)

#Present the predictors of the model
#Shows how the model fits the data for models 4-6 (first lockdown)

##Model4##



prediction_model4 = data.frame(predict(model4,type="response")) 

prediction_model4 = prediction_model4 %>%
  mutate(day = as.numeric(row.names(prediction_model4)))

names(prediction_model4) = c('prediction_model4', 'day')

covid_UK_2 <- covid_UK_2 %>%
  left_join(prediction_model4)

# covid_UK_2 <- covid_UK_2 %>%
#   mutate(prediction_model4=0)
# 
# covid_UK_2$prediction_model4=predict(model4,type="response")

ggplot(covid_UK_2, aes(x=date, col=new_deaths)) +
  geom_line(aes(y=prediction_model4)) +
  scale_y_log10() +
  geom_point(aes(y=new_deaths))




##model5##
covid_UK_2 <- covid_UK_2 %>%
  mutate(prediction_model5=predict(model5,type="response"))

# covid_UK_2 <- covid_UK_2 %>%
#   mutate(prediction_model5=0)
# 
# covid_U_2K$prediction_model5=predict(model5,type="response")

ggplot(covid_UK_2, aes(x=date, col=new_cases)) +
  geom_line(aes(y=prediction_model5)) +
  scale_y_log10() +
  geom_point(aes(y=new_cases))+
  labs(x = "Date", y = 'COVID-19 Cases by Day (n)')

  

##Model6##



prediction_model6 = data.frame(predict(model6,type="response")) 

prediction_model6 = prediction_model6 %>%
  mutate(day = as.numeric(row.names(prediction_model6)))

names(prediction_model6) = c('prediction_model6', 'day')

covid_UK_2 <- covid_UK_2 %>%
  left_join(prediction_model6)

# covid_UK_2 <- covid_UK_2 %>%
#   mutate(prediction_model6=predict(model6,type="response"))

# covid_UK_2 <- covid_UK_2 %>%
#   mutate(prediction_model6=0)
# 
# covid_UK_2$prediction_model6=predict(model6,type="response")


ggplot(covid_UK_2, aes(x=date, col=weekly_hosp_admissions)) +
  geom_line(aes(y=prediction_model6)) +
  scale_y_log10() +
  geom_point(aes(y=weekly_hosp_admissions))


##Effects of Lockdown 2 31st October 2020##

start_time = as.Date('2020-10-3')

change_time = as.Date('2020-10-31')

end_time = as.Date('2020-11-28')

covid_UK_3 <- filter(covid_UK, date >= start_time & date <= end_time)

covid_UK_3 <- mutate(covid_UK_3,
                   day = as.numeric(date - start_time),
                   change = ifelse(date > change_time, 1, 0),
                   days_postchange = day * change)

model8 <-glm(new_deaths ~ day+change+days_postchange, family="poisson", data = covid_UK_3)
summary(model8)

model9 <-glm(new_cases ~ day+change+days_postchange, family="poisson", data = covid_UK_3)
summary(model9)

model10 <-glm(weekly_hosp_admissions ~ day+change+days_postchange, family="poisson", data = covid_UK_3)
summary(model10)

#Present the predictors of the model
#Shows how the model fits the data for Models 8-10 (Second Lockdown)

##Model8##

Covid_UK_3 <- covid_UK_3 %>%
  mutate(prediction_model8=predict(model8,type="response"))

# covid_UK_3 <- covid_UK_3 %>%
#   mutate(prediction_model8=0)
# 
# covid_UK_3$prediction_model8=predict(model8,type="response")

ggplot(covid_UK_3, aes(x=date, col=new_deaths)) +
  geom_line(aes(y=prediction_model8)) +
  scale_y_log10() +
  geom_point(aes(y=new_deaths))+
  labs(x = "Date", y = 'COVID-19 Deaths by Day (n)')

##Model9##

covid_UK_3 <- covid_UK_3 %>%
  mutate(prediction_model9=predict(model9,type="response"))
# 
# covid_UK_3 <- covid_UK_3 %>%
#   mutate(prediction_model9=0)
# 
# covid_UK_3$prediction_model9=predict(model9,type="response")

ggplot(covid_UK_3, aes(x=date, col=new_cases)) +
  geom_line(aes(y=prediction_model9)) +
  scale_y_log10() +
  geom_point(aes(y=new_cases))+
  labs(x = "Date", y = 'COVID-19 Cases by Day (n)')


##Model10##

covid_UK_3 <- covid_UK_3 %>%
  mutate(prediction_model10=predict(model10,type="response"))

covid_UK_3 <- covid_UK_3 %>%
  mutate(prediction_model10=0)

covid_UK_3$prediction_model10=predict(model10,type="response")

ggplot(covid_UK_3, aes(x=date, col=weekly_hosp_admissions)) +
  geom_line(aes(y=prediction_model10)) +
  scale_y_log10() +
  geom_point(aes(y=weekly_hosp_admissions)) +
  labs(x = "Date", y = 'Weekly Hospital Admissions (n)')



# Stevne: None of the code form here on in will work properly
# you are using the dataframe restricted between certain times (e.g covid_UK_2, covid_UK_3)
# to make the predictions. Youre then trying to insert thse predictions into a dataframe
# that covers a much lager time period. Hence the error messages

#Present the predictors of the model
#Shows how the model fits the data

##model0##

covid_UK <- covid_UK %>%
  mutate(prediction_model0=predict(model0,type="response"))

covid_UK <- covid_UK %>%
  mutate(prediction_model0=0)

covid_UK$prediction_model0=predict(model0,type="response")

ggplot(covid_UK, aes(x=date, col=new_deaths)) +
  geom_line(aes(y=prediction_model0)) +
  scale_y_log10() +
  geom_point(aes(y=new_deaths))

##model1##
covid_UK <- covid_UK %>%
  mutate(prediction_model1=predict(model1,type="response"))

covid_UK <- covid_UK %>%
  mutate(prediction_model1=0)

covid_UK$prediction_model1=predict(model1,type="response")

ggplot(covid_UK, aes(x=date, col=new_cases)) +
  geom_line(aes(y=prediction_model1)) +
  scale_y_log10() +
  geom_point(aes(y=new_cases))

##model2##

covid_UK <- covid_UK %>%
  mutate(prediction_model2=predict(model2,type="response"))

covid_UK <- covid_UK %>%
  mutate(prediction_model2=0)

covid_UK$prediction_model2=predict(model2,type="response")

ggplot(covid_UK, aes(x=date, col=weekly_hosp_admissions)) +
  geom_line(aes(y=prediction_model2)) +
  scale_y_log10() +
  geom_point(aes(y=weekly_hosp_admissions))

##Model4##

covid_UK <- covid_UK %>%
  mutate(prediction_model4=predict(model4,type="response"))

covid_UK <- covid_UK %>%
  mutate(prediction_model4=0)

covid_UK$prediction_model4=predict(model4,type="response")

ggplot(covid_UK, aes(x=date, col=new_cases)) +
  geom_line(aes(y=prediction_model4)) +
  scale_y_log10() +
  geom_point(aes(y=new_deaths)) +
  labs(x = "Date", y = 'Hospital Patients (n)', title = 'Number of COVID-19 Hospital Patients in the UK Between March 2020 - August 2021')




##model5##
covid_UK <- covid_UK %>%
  mutate(prediction_model5=predict(model5,type="response"))

covid_UK <- covid_UK %>%
  mutate(prediction_model5=0)

covid_UK$prediction_model5=predict(model5,type="response")

ggplot(covid_UK, aes(x=date, col=new_cases)) +
  geom_line(aes(y=prediction_model5)) +
  scale_y_log10() +
  geom_point(aes(y=new_cases))

##Model6##

Covid_UK <- covid_UK %>%
  mutate(prediction_model6=predict(model6,type="response"))

covid_UK <- covid_UK %>%
  mutate(prediction_model6=0)

covid_UK$prediction_model6=predict(model6,type="response")

ggplot(covid_UK, aes(x=date, col=weekly_hosp_admissions)) +
  geom_line(aes(y=prediction_model6)) +
  scale_y_log10() +
  geom_point(aes(y=weekly_hosp_admissions))

##Model8##

Covid_UK <- covid_UK %>%
  mutate(prediction_model8=predict(model8,type="response"))

covid_UK <- covid_UK %>%
  mutate(prediction_model8=0)

covid_UK$prediction_model8=predict(model8,type="response")

ggplot(covid_UK, aes(x=date, col=new_deaths)) +
  geom_line(aes(y=prediction_model8)) +
  scale_y_log10() +
  geom_point(aes(y=new_deaths))

##Model9##

Covid_UK <- covid_UK %>%
  mutate(prediction_model9=predict(model9,type="response"))

covid_UK <- covid_UK %>%
  mutate(prediction_model9=0)

covid_UK$prediction_model8=predict(model9,type="response")

ggplot(covid_UK, aes(x=date, col=new_cases)) +
  geom_line(aes(y=prediction_model9)) +
  scale_y_log10() +
  geom_point(aes(y=new_cases))

##Model10##

Covid_UK <- covid_UK %>%
  mutate(prediction_model10=predict(model10,type="response"))

covid_UK <- covid_UK %>%
  mutate(prediction_model10=0)

covid_UK$prediction_model8=predict(model10,type="response")

ggplot(covid_UK, aes(x=date, col=weekly_hosp_admissions)) +
  geom_line(aes(y=prediction_model10)) +
  scale_y_log10() +
  geom_point(aes(y=weekly_hosp_admissions))

##Calculating IRR for each model##

##Mask Mandate##
exp(coef(model0))
round(exp(coef((model0))),2)
round(cbind(RR = exp(coef(model0)),exp(confint(model0))),2)

exp(coef(model1))
round(exp(coef((model1))),2)
round(cbind(RR = exp(coef(model1)),exp(confint(model1))),2)

exp(coef(model2))
round(exp(coef((model2))),2)
round(cbind(RR = exp(coef(model2)),exp(confint(model2))),2)

##1st Lockdown##
exp(coef(model4))
round(exp(coef((model4))),2)
round(cbind(RR = exp(coef(model4)),exp(confint(model4))),2)

exp(coef(model5))
round(exp(coef((model5))),2)
round(cbind(RR = exp(coef(model5)),exp(confint(model5))),2)


exp(coef(model6))
round(exp(coef((model6))),2)
round(cbind(RR = exp(coef(model6)),exp(confint(model6)), p),2)

##2nd Lockdown##
exp(coef(model9))
round(exp(coef((model9))),2)
round(cbind(RR = exp(coef(model9)),exp(confint(model9))),2)


covid_mask<-covid_UK %>%
  filter(date >= as.Date('2020-06-19') & date <= as.Date('2020-08-23'))

filter


