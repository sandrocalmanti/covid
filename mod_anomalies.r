rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)

lfit <- function(x,t,n=0) {
  #Length of forecasting set
  xlm <- x[1:(length(x)-n)]
  tlm <- t[1:(length(t)-n)]
  lf <- lm(log(xlm) ~ tlm)
  a <- lf$coefficients[1]
  b <- lf$coefficients[2]
  y <- exp( a + t*b)
  return(y)
}

############################
# Length of the forecast set
############################

nf <- 2


df <- read.csv("./DATA/Daily_Covis19_Italian_Data_Cumulative.csv", sep=",") %>%
  group_by(Date)  %>%
  select(-Region) %>%
  summarise_all(sum) %>%
  mutate(Cases=Hospitalised + In.ICU + Home.Isolation + Healed + Dead)

#Normalize Tests and Cases using max, add time [days]
df <- df  %>% mutate(nTests=Tests/max(Tests), 
                     nCases=Cases/max(Cases), 
                     Time=as.integer(ymd(Date)-ymd(Date[1])) )

df <- df %>% mutate(
  fCases = lfit(nCases,Time,n=nf),
  fTests = lfit(nTests,Time,n=nf)
)

p<- ggplot(data=df,aes(Date,100*(nCases-fCases),color='Casi accertati'), color='black') +
  geom_point() +
  geom_point(data=df,aes(Date,100*(nTests-fTests), color='Tamponi')) +
  ylab('Differenza Osservati - Previsti [%]') +
  xlab('Data') +
  labs(color="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
p

ggsave('COVID_osservazioni-previsioni.png',height=4,width=8)





