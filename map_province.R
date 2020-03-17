library(dplyr)
library(lubridate)
library(ggplot2)
library(maps)
library(purrr)
library(ggforce)
library(readr)
source('f_df_map.r')

IT <- map_data("world") %>% filter(region=="Italy")

firstday <- ymd("2020-02-24")
lastday  <- today() - 1
cal <- as.Date(firstday - 1 + seq(firstday:lastday))

fileURL <- paste0("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-",
                  paste0(year(cal),sprintf("%02d",month(cal)),sprintf("%02d",day(cal))),
                  ".csv")

df <- map(fileURL,read.csv) 
df_scale <- 1000.*max(unlist(map(df,pull,'totale_casi')),na.rm=TRUE)

p <- map(df,df_map,df_bor=IT,df_scale)


dfanim <- dplyr::bind_rows(df)
names(dfanim)[1] <- 'time'
dfanim$time <- as.Date(dfanim$time)

ggplot() +
#  geom_polygon(data = IT, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
#  geom_circle( data=dfanim,aes(x0=long, y0=lat, r=(log(totale_casi)/(log(df_scale)))), fill='red',alpha=0.5) +
  geom_circle( data=dfanim,aes(x0=long, y0=lat, r=1), fill='red',alpha=0.5) +
  # geom_circle(aes(x0=18,y0=46,r=log(1000)/(log(df_scale))), fill='red',alpha=0.5)  +
  # geom_circle(aes(x0=18,y0=45,r=log(100)/(log(df_scale))), fill='red',alpha=0.5)  +
  # geom_circle(aes(x0=18,y0=44.2,r=log(10)/(log(df_scale))), fill='red',alpha=0.5)  +
  ggtitle(paste0("COVID-19  Totale Casi per provincia - ")) +
  annotate("text",x=10,y=36.5,label="https://github.com/sandrocalmanti/covid") +
  annotate("text",x=17,y=46,label="1000") +
  annotate("text",x=17,y=45,label="100") +
  annotate("text",x=17,y=44.2,label="10") +
  labs(size="Totale casi") +
  theme_void() + 
  theme(legend.position = "none") +
  transition_states(
    time,
    transition_length = 2,
    state_length = 1
  ) +
  coord_fixed() 

