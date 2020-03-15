library(tidyverse)
library(patchwork)

################
# Set parameters
################

n0 <- 6     #Number of deaths to trigger time counting in each country

############
# Read Data
############

dataURL <- "https://covid.ourworldindata.org/data/total_deaths.csv"
tbw <- as_tibble(read.csv(fileURL))
tbl <- gather(tbw, "Country", "Deaths", -date)
tbl <- tbl %>% group_by(Country)

#################
# Filter coutries
#################

tbl <- tbl %>% 
  dplyr::filter(!is.na(Deaths)) %>%
  dplyr::filter(Deaths>n0) %>%
  dplyr::filter(Country!='World')

tbl <- tbl %>%
  group_map( ~ mutate(.x,day = as.numeric(as.Date(date)-min(as.Date(date))) ) )

p0a < -ggplot(tbl,aes(x=day,y=Deaths,group=Country,color=Country)) +
  geom_point() +
  geom_line() +
  coord_trans(y='log10') +
  labs(
    title = 'COVID19 - Total Deaths by country',
    subtitle = paste0('Day 0 in each country corresponds to n>',n0,' deaths'),
    caption = paste0('Data: ', dataURL)
  ) +
  theme_light()

p0 <- p0a +
 plot_layout(ncol=1)

ggsave('./PLOT/COVID19.png',p0)

#####################################
# Model Fit
#####################################


tbl <- tbl %>% dplyr::filter(Country %in% c('China'))
tbl <- tbl %>% mutate(day=as.numeric(as.Date(date)-min(as.Date(date))) )







