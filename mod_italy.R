# Data: https://github.com/DavideMagno/ItalianCovidData.git
library(tidyverse)
library(lubridate)
library(nlstools)
source('f_fitfunctions.r')


############
# Read Data
############

firstday <- ymd("2020-02-24")
lastday  <- today() - 1
dataURL<-'https://github.com/pcm-dpc/COVID-19/tree/master/dati-regioni'
gitURL<-'https://github.com/sandrocalmanti/covid'
cal <- as.Date(firstday - 1 + seq(firstday:lastday))
fileURL <- paste0("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-",
                  paste0(year(cal),sprintf("%02d",month(cal)),sprintf("%02d",day(cal))),
                  ".csv")

###############################
#  Prepare data sable
###############################

tbw_dpc <- as_tibble(bind_rows(map(fileURL,read.csv)) ) %>%
  group_by(data)  %>%
  select(-stato,-codice_regione,-denominazione_regione,-lat,-long) %>%
  summarise_all(sum) %>%
  mutate(day = as.numeric(as.Date(data)-min(as.Date(data))),
         Country='Italy')

#################
# Non-linear fit
#################

fit1 <- fit.gompertz(tbw_dpc$deceduti,tbw$day,tbw$Country)
fit2 <- fit.logistic(tbw_dpc$deceduti,tbw$day,tbw$Country)
#Check residulas
nr1 <- nlsResiduals(fit1)
nr2 <- nlsResiduals(fit2)
plot(nr1)
plot(nr2)

#Extract coefficients and confidence
fit1_dpc <- broom::tidy(fit1) %>%  gather("coef", "value", 2:ncol(fit1_tbc)) %>%    spread(term, value)
fit2_dpc <- broom::tidy(fit2) %>%  gather("coef", "value", 2:ncol(fit1_tbc)) %>%    spread(term, value)


###################
# Compute forecasts
###################

forecast <- tibble(day=(seq(90)-1)) %>%
  mutate(data=firstday+day,
         Gompertz=f.gompertz(day,fit1_dpc$a[1],fit1_dpc$mu[1],fit1_dpc$lambda[1]),
         Logistic=f.logistic(day,fit2_dpc$a[1],fit2_dpc$mu[1],fit2_dpc$lambda[1]))%>%
  gather('Fit','Deceduti',-c(day,data)) %>%
  group_by(Fit)

maxfit <- forecast %>% select(-day,-data) %>% summarize_all(max)

##################
# PLOT
##################

p<-ggplot(forecast,aes(data,Deceduti,color=Fit)) +
  geom_line() +
  geom_point(data=tbw_dpc,aes(as.Date(data),deceduti),color='black') +
  coord_trans(y='log10') +
  xlab('')+
  labs(
    title = 'COVID-19 Italia - Previsione',
    subtitle = paste0('Aggiornamento: ',today()),
    caption = paste0('Data: ', dataURL ,'\n Rproject:',gitURL)
  ) +
  annotate('text',x=as.Date('2020-05-01'),y=100,label=paste0('Max. Gompertz ~ ',floor(dplyr::pull(filter(maxfit,Fit=='Gompertz'),Deceduti))))+
  annotate('text',x=as.Date('2020-05-01'),y=50,label=paste0('Max. Logistic ~ ',floor(dplyr::pull(filter(maxfit,Fit=='Logistic'),Deceduti))))+
  theme_light()
p
ggsave('./PLOT/COVID19_previsione_italia_log.png',p)

p<-ggplot(forecast,aes(data,Deceduti,color=Fit)) +
  geom_line() +
  geom_point(data=tbw_dpc,aes(as.Date(data),deceduti),color='black') +
  xlab('')+
  labs(
    title = 'COVID-19 Italia - Previsione',
    subtitle = paste0('Aggiornamento: ',today()),
    caption = paste0('Data: ', dataURL ,'\n Rproject:',gitURL)
  ) +
  annotate('text',x=as.Date('2020-05-01'),y=11000,label=paste0('Max. Gompertz ~ ',floor(dplyr::pull(filter(maxfit,Fit=='Gompertz'),Deceduti))))+
  annotate('text',x=as.Date('2020-05-01'),y=9000,label=paste0('Max. Logistic ~ ',floor(dplyr::pull(filter(maxfit,Fit=='Logistic'),Deceduti))))+
  theme_light()
p
ggsave('./PLOT/COVID19_forecast_italy_lin.png',p)


#EN
p<-ggplot(forecast,aes(data,Deceduti,color=Fit)) +
  geom_line() +
  geom_point(data=tbw_dpc,aes(as.Date(data),deceduti),color='black') +
  coord_trans(y='log10') +
  xlab('')+
  labs(
    title = 'COVID-19 Italy - Forecast',
    subtitle = paste0('Update: ',today()),
    caption = paste0('Data: ', dataURL ,'\n Rproject:',gitURL)
  ) +
  annotate('text',x=as.Date('2020-05-01'),y=100,label=paste0('Max. Gompertz ~ ',floor(dplyr::pull(filter(maxfit,Fit=='Gompertz'),Deceduti))))+
  annotate('text',x=as.Date('2020-05-01'),y=50,label=paste0('Max. Logistic ~ ',floor(dplyr::pull(filter(maxfit,Fit=='Logistic'),Deceduti))))+
  theme_light()
p
ggsave('./PLOT/COVID19_forecast_italy_log.png',p)

p<-ggplot(forecast,aes(data,Deceduti,color=Fit)) +
  geom_line() +
  geom_point(data=tbw_dpc,aes(as.Date(data),deceduti),color='black') +
  xlab('')+
    labs(
      title = 'COVID-19 Italy - Forecast',
      subtitle = paste0('Update: ',today()),
      caption = paste0('Data: ', dataURL ,'\n Rproject:',gitURL)
    ) +
  annotate('text',x=as.Date('2020-05-01'),y=11000,label=paste0('Max. Gompertz ~ ',floor(dplyr::pull(filter(maxfit,Fit=='Gompertz'),Deceduti))))+
  annotate('text',x=as.Date('2020-05-01'),y=9000,label=paste0('Max. Logistic ~ ',floor(dplyr::pull(filter(maxfit,Fit=='Logistic'),Deceduti))))+
  theme_light()
p
ggsave('./PLOT/COVID19_forecast_italy_lin.png',p)


