rm(list = ls())

 library(ggplot2)
library(dplyr)
# library(multiApply)
# library(ncdf4)
# library(CSTools)
# library(zeallot)
# library(lubridate)
# library(SPEI)
# library(MASS)
# library(eneaR)

#setwd("C:/Users/marcello/Google Drive/ZZ_WORK/covid19italia-master/")

start_date=5
end_date=13
da = read.table("./covid-19-it-protezione-civile.txt", header=TRUE)
dat = as.data.frame(da)
tempo=seq(1,length(dat$date))
dat = cbind(dat, tempo)
dat$date = as.Date(dat$date)
#plot(as.Date(dat$date),as.numeric(dat$cases))

#TAMPONI
aa = read.csv2("Daily_Covis19_Italian_Data_Cumulative.csv", sep=",")
tamponi = aa %>% group_by(Date) %>% summarise(somma =sum(Tests)) 
tamponi2=dat$cases*0
tamponi2[5:length(dat$cases)]=tamponi$somma
tamponi_n=tamponi2
tamponi_n=tamponi_n/sum(tamponi_n)

dat_n = dat
dat_n$cases=dat_n$cases/sum(dat_n$cases)
dat_n$deaths=dat_n$deaths/sum(dat_n$deaths)
dat_n$healed=dat_n$healed/sum(dat_n$healed)
dat_n$critical=dat_n$critical/sum(dat_n$critical)
dat_n$severe=dat_n$severe/sum(dat_n$severe)

tempo2=dat$tempo[start_date:end_date]
LL=(dat$cases[start_date:end_date])
fit = lm(log(LL) ~ (tempo2))
fit$coefficients

LL_deaths=(dat$deaths[start_date:end_date])
fit_deaths = lm(log(LL_deaths) ~ (tempo2))
fit_deaths$coefficients

LL_critical=(dat$critical[start_date:end_date])
fit_critical = lm(log(LL_critical) ~ (tempo2))
fit_critical$coefficients

LL_tamponi=(tamponi2[start_date:end_date])
fit_tamponi = lm(log(LL_tamponi) ~ (tempo2))
fit_tamponi$coefficients


#fit_normalizzati
LL=(dat_n$cases[start_date:end_date])
fit_n = lm(log(LL) ~ (tempo2))
fit_n$coefficients

LL_deaths=(dat_n$deaths[start_date:end_date])
fit_deaths_n = lm(log(LL_deaths) ~ (tempo2))
fit_deaths_n$coefficients

LL_critical=(dat_n$critical[start_date:end_date])
fit_critical_n = lm(log(LL_critical) ~ (tempo2))
fit_critical_n$coefficients

LL_tamponi=(tamponi_n[start_date:end_date])
fit_tamponi_n = lm(log(LL_tamponi) ~ (tempo2))
fit_tamponi_n$coefficients

#Plot the fitted line

#GG=data.frame(as.Date(dat$date),dat$cases,dat$deaths,dat$critical)
rfit_cases =exp(fit$coefficients[1] + dat$tempo * fit$coefficients[2])
rfit_deaths =exp(fit_deaths$coefficients[1] + dat$tempo * fit_deaths$coefficients[2])
rfit_critical =exp(fit_critical$coefficients[1] + dat$tempo * fit_critical$coefficients[2])
rfit_tamponi =exp(fit_tamponi$coefficients[1] + dat$tempo * fit_tamponi$coefficients[2])

#estraploazioni normalizzate
nrfit_cases =exp(fit_n$coefficients[1] + dat$tempo * fit_n$coefficients[2])
nrfit_deaths =exp(fit_deaths_n$coefficients[1] + dat$tempo * fit_deaths_n$coefficients[2])
nrfit_critical =exp(fit_critical_n$coefficients[1] + dat$tempo * fit_critical_n$coefficients[2])
nrfit_tamponi =exp(fit_tamponi_n$coefficients[1] + dat$tempo * fit_tamponi_n$coefficients[2])

GG=data.frame(dat$date,tempo,dat$cases,dat$deaths,dat$critical,tamponi2, 
              rfit_cases,rfit_deaths,rfit_critical, rfit_tamponi)
names(GG)=c("data","tempo", "cases", "deaths","critical","tamponi", 
            "fitted_cases", "fitted_deaths", "fitted_critical", "fitted_tamponi")
GG_n=data.frame(dat_n$date,tempo,dat_n$cases,dat_n$deaths,dat_n$critical,tamponi_n, 
              nrfit_cases,nrfit_deaths,nrfit_critical, nrfit_tamponi)
names(GG_n)=c("data","tempo", "cases", "deaths","critical","tamponi", 
            "fitted_cases", "fitted_deaths", "fitted_critical", "fitted_tamponi")

ggplot(data=GG, aes(x=data,y= value, color=variable))+ 
  scale_y_continuous(trans='log2')+
  geom_point(aes(y = cases, col = "cases")) + 
  geom_point(aes(y = deaths, col = "deaths")) + 
  geom_point(aes(y = critical, col = "critical"))+
#  geom_point(aes(y = critical, col = "tamponi"))+
  geom_line(aes(y = fitted_cases, col = "fitted_cases")) + 
  geom_line(aes(y = fitted_deaths, col = "fitted_deaths")) + 
  geom_line(aes(y = fitted_critical, col = "fitted_critical"))
#geom_line(aes(y = fitted_critical, col = "fitted_tamponi"))
ggplot(data=GG, aes(x=data,y= value, color=variable))+ 
  scale_y_continuous(trans='log2')+
  geom_point(aes(y = critical, col = "tamponi"))+
 geom_line(aes(y = fitted_critical, col = "fitted_tamponi"))

ggplot(data=GG_n, aes(x=data,y= value, color=variable))+ 
  scale_y_continuous(trans='log2')+
  geom_point(aes(y = cases, col = "cases")) + 
  geom_point(aes(y = deaths, col = "deaths")) + 
  geom_point(aes(y = critical, col = "critical"))+
    geom_point(aes(y = critical, col = "tamponi"))+
  geom_line(aes(y = fitted_cases, col = "fitted_cases")) + 
  geom_line(aes(y = fitted_deaths, col = "fitted_deaths")) + 
  geom_line(aes(y = fitted_critical, col = "fitted_critical"))+
geom_line(aes(y = fitted_critical, col = "fitted_tamponi"))

plot(GG_n$data,(GG_n$tamponi-GG_n$fitted_tamponi)/(GG_n$cases-GG_n$fitted_cases))

plot(GG_n$data,     (GG_n$cases-GG_n$fitted_cases),xlab="")
lines(GG_n$data,(GG_n$tamponi-GG_n$fitted_tamponi))



# 
# 
# plot(dat$date[start_date:length(dat$date)], (dat$cases[start_date:length(dat$date)]))
# lines(dat$date[start_date:length(dat$date)],  
#       exp(fit$coefficients[1] + dat$tempo[start_date:length(dat$date)] * fit$coefficients[2]), col = "red")
# 
# plot(dat$date, (dat$cases), log="y")
# lines(dat$date,  
#       exp(fit$coefficients[1] + dat$tempo * fit$coefficients[2]), col = "red", log="y")
# 
# plot(dat$date, (dat$deaths), log="y")
# lines(dat$date,  
#       exp(fit_deaths$coefficients[1] + dat$tempo * fit_deaths$coefficients[2]), col = "red", log="y")
# 
# plot(dat$date, (dat$critical), log="y")
# lines(dat$date,  
#       exp(fit_critical$coefficients[1] + dat$tempo * fit_critical$coefficients[2]), col = "red", log="y")
# 
