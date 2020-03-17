# Data: https://github.com/DavideMagno/ItalianCovidData.git
library(tidyverse)
library(lubridate)
library(nlstools)
library(patchwork)
library(moments)
source('f_fitfunctions.r')


############
# Read Data
############

firstday <- ymd("2020-02-26")
lastday  <- today() - 1 
dataURL<-'https://github.com/pcm-dpc/COVID-19/tree/master/dati-regioni'
gitURL<-'https://github.com/sandrocalmanti/covid'
cal <- as.Date(firstday - 1 + seq(firstday:lastday))
fileURL <- paste0("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-",
                  paste0(year(cal),sprintf("%02d",month(cal)),sprintf("%02d",day(cal))),
                  ".csv")

###############################
#  Prepare data table
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

fit1 <- fit.gompertz(tbw_dpc$deceduti,tbw_dpc$day,tbw_dpc$Country)
fit2 <- fit.logistic(tbw_dpc$deceduti,tbw_dpc$day,tbw_dpc$Country)
#Check residulas
#nr1 <- nlsResiduals(fit1)
#nr2 <- nlsResiduals(fit2)
#plot(nr1)
#plot(nr2)

#Extract coefficients and confidence
fit1_dpc <- broom::tidy(fit1) %>%  gather("coef", "value", 2:ncol(broom::tidy(fit1))) %>%    spread(term, value)
fit2_dpc <- broom::tidy(fit2) %>%  gather("coef", "value", 2:ncol(broom::tidy(fit2))) %>%    spread(term, value)

#########################################################################################
#                             
#                                   F O R E C A S T S
#
#########################################################################################


#Init time
forecast <- tibble(day=(seq(90)-1)) %>% mutate(data=firstday+day)

###################
# Compute simple forecasts
###################

# forecast <- forecast %>% mutate(                                               
#     Gompertz=f.gompertz(day,fit1_dpc$a[1],fit1_dpc$mu[1],fit1_dpc$lambda[1]),
#     Logistic=f.logistic(day,fit2_dpc$a[1],fit2_dpc$mu[1],fit2_dpc$lambda[1]))
# maxfit <- forecast %>% select(-day,-data) %>% summarize_all(max)


##########
# Jacknife
##########

for ( i in seq(1:1000)) {
  print(i)
tbw_dpc_jkn <- sample_frac(tbw_dpc,0.5, replace=FALSE)
fit1 <- fit.gompertz(tbw_dpc_jkn$deceduti,tbw_dpc_jkn$day,tbw_dpc_jkn$Country)
fit1_dpc <- broom::tidy(fit1) %>%  gather("coef", "value", 2:ncol(broom::tidy(fit1))) %>%    spread(term, value)
forecast <- forecast %>% mutate(
  !!as.name(paste0('f',i)) := f.gompertz(day,
                                         a=fit1_dpc$a[1],
                                         mu=fit1_dpc$mu[1],
                                         lambda=fit1_dpc$lambda[1])
)
}

##################
# PLOT
##################

#Comput max
maxfit <- forecast %>% select(-day,-data) %>% 
  summarize_all(max) %>% 
  gather('Fit','Max') %>% 
  dplyr::filter(Max<200000)

#Gather data frame
forecastp <- forecast %>%
  gather('Fit','Deceduti',-c(day,data)) %>%
  group_by(Fit)


p1<-ggplot(forecastp,aes(data,Deceduti,group=Fit)) +
  geom_line(color='red',alpha=0.1) +
  geom_point(data=tbw_dpc,aes(as.Date(data),deceduti,group=NULL),color='black') +
  xlab('')+
  scale_y_continuous(limits=c(1,500000), trans='log10') +
  labs(
    title = 'COVID-19 Italia - Previsione',
    subtitle = paste0('Aggiornamento: ',lastday),
    caption = paste0('Data: ', dataURL ,'\n Rproject:',gitURL)
  ) +
  theme_light() 

p2 <- ggplot(maxfit, aes(x=Max)) + 
  scale_x_continuous(limits=c(1,500000), trans='log10') +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  coord_flip() +
  annotate('text',x=1000,y=0.1,label=paste0('Mean: ',floor(mean(maxfit$Max)),'\n',
                                            'Median: ',floor(median(maxfit$Max)),'\n',
                                            'St.dev: ',floor(sd(maxfit$Max)),  '\n',
                                            'Skewness: ',sprintf('%0.2f',skewness(maxfit$Max)),'\n',
                                            'Fit: y = A*exp(-B*exp(-C*t))'
  ),hjust=0,vjust=1)+
  theme_void()


p <- p1 + p2 + plot_layout(ncol=2,widths=c(2,1))

ggsave(paste0('./PLOT/COVID19_previsione_italia_jacknife_log_',lastday,'.png'),p)

