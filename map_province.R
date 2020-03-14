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

df <- map(fileURL,read_csv) 
df_scale <- max(unlist(map(df,pull,'totale_casi')))

p <- map(df,df_map,df_bor=IT,df_scale)

unlink("./PLOTcovid19.mpeg")
system("convert -delay 10 './PLOT/COVID-19_map_*.png' ./PLOT/covid19.mpeg")


