
########################################################################
########################################################################

df_map = function (df,df_bor,df_scale=1) {
  
  df <- df %>% filter(totale_casi>0) %>% filter(lat>0)
  df_ymd <- ymd(as.Date(unique(df$'ï..data')))
  p <- ggplot() +
    geom_polygon(data = df_bor, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
    geom_circle( data=df,aes(x0=long, y0=lat, r=(log(totale_casi)/(log(df_scale)))), fill='red',alpha=0.5) +
    geom_circle(aes(x0=18,y0=46,r=log(1000)/(log(df_scale))), fill='red',alpha=0.5)  +
    geom_circle(aes(x0=18,y0=45,r=log(100)/(log(df_scale))), fill='red',alpha=0.5)  +
    geom_circle(aes(x0=18,y0=44.2,r=log(10)/(log(df_scale))), fill='red',alpha=0.5)  +
    ggtitle(paste0("COVID-19  Totale Casi per provincia - ",df_ymd)) +
    annotate("text",x=10,y=36.5,label="https://github.com/sandrocalmanti/covid") +
    annotate("text",x=17,y=46,label="1000") +
    annotate("text",x=17,y=45,label="100") +
    annotate("text",x=17,y=44.2,label="10") +
    labs(size="Totale casi") +
    theme_void() + 
    theme(legend.position = "none") +
    coord_fixed() 
  
  ggsave(file=paste0('./PLOT/COVID-19_map_',df_ymd,'.png'),plot=p,height=7,width=7)
  
  return(p)
}

