
########################################################################
########################################################################

df_map = function (df,df_bor,df_scale=1) {
  
  df <- df %>% filter(totale_casi>0) %>% filter(lat>0)
  df_ymd <- ymd(as.Date(unique(df$data)))
  p <- ggplot() +
    geom_polygon(data = df_bor, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
    geom_circle( data=df,aes(x0=long, y0=lat, r=(totale_casi/df_scale)), fill='red',alpha=0.5) +
    ggtitle(paste0("COVID-19  Totale Casi - Giorno: ",df_ymd)) +
    annotate("text",x=10,y=36.5,label="https://github.com/sandrocalmanti/covid") +
    labs(size="Totale casi") +
    theme_void() + 
    theme(legend.position = "none") +
    coord_fixed() 
  
  ggsave(file=paste0('./PLOT/COVID-19_map_',df_ymd,'.png'),plot=p,height=7,width=7)
  
  return(p)
}

