library(tidyverse)
library(patchwork)
source('f_fitfunctions.r')

################
# Set parameters
################

n0 <- 4     #Number of deaths to trigger time counting in each country

############
# Read Data
############

dataURL <- "https://covid.ourworldindata.org/data/total_deaths.csv"

#Use tibbles instead of dataframes 
#tibbles are advanced dataframes (https://r4ds.had.co.nz/tibbles.html)
tbw <- as_tibble(read.csv(dataURL))

#Reduce wide tibbles to a long tible with single records in each row 
tbl <- gather(tbw, "Country", "Deaths", -date)

#Group records by country, add an auxiliary column ID=Country, which is useful to work with purrr
tbl <- tbl %>% 
  mutate(ID=Country) %>% 
  group_by(Country)

#################
# Filter coutries
#################

tbl <- tbl %>% 
  #Filter empty records
  dplyr::filter(!is.na(Deaths)) %>%
  #Filter regords smaller than the trigger
  dplyr::filter(Deaths>n0) %>%
  #Remove world total
  dplyr::filter(Country!='World',
                Country!='International',
                Country!='Iraq',
                Country!='Germany',
                Country!='Netherlands',
                Country!='Switzerland',
                Country!='United.Kingdom') %>%
  #Add column with days from start of COVID in each country
  group_map( ~ mutate(.x, day = as.numeric(as.Date(date)-min(as.Date(date))) ) )


############
# Make graph
############

#Graph n.1
p1 <-ggplot(tbl,aes(x=day,y=Deaths,group=Country,color=Country))+
  geom_point() +
  geom_line() +
  coord_trans(y='log10') +
  xlim(0,20) +
  labs(
    title = 'COVID19 - Total deaths by country',
    subtitle = paste0('Time starts at n>',n0,' deaths'),
    caption = paste0('Data: ', dataURL)
  ) +
  theme_light()

#Graph n.2
#p2


#Join graphs in a single layout
p <- p1 +
 plot_layout(ncol=1)


ggsave('./PLOT/COVID19.png',p1)



#####################################
# Model Fit
#####################################


tbl <- tbl %>% dplyr::filter(Country %in% c('China','Italy'))

tbl_fit_gompertz <- tbl %>% group_map(~ broom::tidy(fit.gompertz(data=.x$Deaths, time=.x$day, country=.x$ID)) ) %>%
  mutate(fun='fit.gompertz')
tbl_fit_logistic <- tbl %>% group_map(~ broom::tidy(fit.logistic(data=.x$Deaths, time=.x$day, country=.x$ID)) )  %>%
  mutate(fun='fit.logistic')
tbl_fit <- bind_rows(tbl_fit_gompertz,tbl_fit_logistic)


tbl <- tbl %>% dplyr::filter(Country %in% c('Italy'))
fit1 <- fit.gompertz(tbl$Deaths,tbl$day,tbl$Country)
fit2 <- fit.logistic(tbl$Deaths,tbl$day,tbl$Country)
plot(tbl$day,tbl$Deaths)
lines(tbl$day,predict(fit1))
lines(tbl$day,predict(fit2))





