# combinando resultados das metricas de biod. com expansao agricola

#---- pacotes ------------------------------------------------------------------

library(readr)
library(dplyr)

#-------------------------------------------------------------------------------

lu_change <- read_csv("output_tables/resultados_lu_change_cenarios.csv")

metricas <- read_csv("output_tables/resultados_metricas_cenarios.csv")

names(metricas)[c(2,3)] <-c("metric_label","metrics_value") 

comb_df <- left_join(lu_change,metricas)

comb_df$label_scen <-factor(comb_df$label_scen,levels = rev(c("exacerb. lib. + BTC baseline","tarif.elim.+BTC baseline","transp.cost. red + BTC baseline","frict.&reconfig. + BTC baseline","BAU")))



l2 <-expression(paste("Agriculture expansion ("~km^2,"x1000 ) ",sep=""))

metrica <-unique(comb_df$metric)

metricasxarea <- list()

c <- 1

for(m in metrica){
  df <- comb_df %>%
    # agriculture
    filter(name== "AGR"|name== "PAS")%>%
    # juntando past e agri
    group_by(label_scen,metric_label,metric)%>%
    summarise(value=sum(value),metrics_value=max(metrics_value))%>%
    filter(metric == m)%>%
    mutate(land_use_change=value/1000)
  #filter(region == "Latin America and the Caribbean")%>%
  # adatpando escala
  #mutate(value=value/1000)%>%
  gfco <- ggplot(df , aes(x=land_use_change, y=metrics_value,color=label_scen,)) + 
    geom_point(position = position_dodge(width =100),size=4)+
    #scale_y_continuous(trans=pseudolog10_trans ) +
    theme_classic()+
    xlab(l2)+
    ylab(m)+
    scale_color_brewer(palette = "Spectral",name="label_scen")+
    theme(legend.title = element_blank())
  
  metricasxarea[[c]] <- gfco
  c <- c+1
  
}

metrics_area_plot <- ggarrange(plotlist = metricasxarea[2:4],ncol=3,common.legend = T)
25.4/2

ggsave(filename = "figures/agri_expansion_vs_bio_metrics.jpeg",width = 25.4,height = 9,units = "cm",plot = metrics_area_plot,bg ="white")
