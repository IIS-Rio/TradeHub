#---- pacotes ------------------------------------------------------------------

library(dplyr)
library(ggpubr)
library(purrr)
library(scales)
library(sf)
library(ggmap)
library(ggthemes)
library(RColorBrewer)
library(viridis)
library(ggrepel)
library(scico)

#-------------------------------------------------------------------------------


# fazer tb figura com mudanca % nao apenas absoluta! pq isso  enfatiza diferencas

# df com valores sem mudança climatica

df <- read.csv("/dados/pessoal/francisco/TradeHub/output_tables/resultados_cenarios_regional_analysis.csv")%>%
  # remove bd
  filter(name!="bd.val")

# com mudança climatica

bd_df <- unique(read.csv("/dados/pessoal/francisco/TradeHub/output_tables/updated_results/agg_bd_climate_env.csv")%>%
  # adjust names
  dplyr::rename(region=regions,
         scenario=scens,
         value=mean_bd)%>%
  mutate(name="bd.val",
         variable="Extinction debt reduction")%>%
  # add columns
  left_join(df[,c(1,2,6,7)])
)

# df completo

df <- rbind(df,bd_df)


levels_regions <- c("Global","SSA","LAC","CSI","SEA","EAS","MNA","OCE","SAS","EUR","USA","CAN")


bio <- df%>%
  filter(name%in%c("bd.val","it.val","ec.val"))#%>%
  #remover bd antigo
  #filter(name!="bd.val")



bsline <- filter(bio,label_scen=="BAU")%>%rename(value_BAU=value)
trde <- filter(bio,conservation!="C"&label_scen!="BAU")
con <- filter(bio,conservation=="C"&label_scen!="BAU")
# 
trde <- left_join(trde,bsline[,c(1,3,4,5,7)])%>%
  # calculate relative value
  mutate(relative_to_BAU_2050=(value-value_BAU)/abs(value_BAU))

con <- left_join(con,bsline[,c(1,3,4,5)])%>%
  # calculate relative value
  mutate(relative_to_BAU_2050=(value-value_BAU)/abs(value_BAU))
 
trde <- rbind(trde,con)

# #ordenar 

trde$label_scen <-factor(trde$label_scen,levels = (c("ETL","Ta","Tr","Fr","Trade-base","BAU")))



trde$variable <- recode_factor(trde$variable, 'Ecoregion vulnerability' = "Ecoregion vulnerability reduction")


trde$variable <- factor(trde$variable,levels = (c("Extinction debt reduction","Ecoregion vulnerability reduction","Ecossistem integrity reduction","Carbon","Land opportunity cost")))

# falta ordenar por regiao e entender melhor as metricas: qndo eh reducao, qndo eh aumento!

trde$region <- factor(trde$region,levels=rev(c("LAC","SSA","CSI","SEA","MNA","SAS","EUR","EAS","USA","OCE","CAN")))


trde$name <- gsub(pattern = ".val",replacement = "",x = trde$name)

#ordenando siglas das variaveis


trde$name <- factor(trde$name,levels=c("bd","ec","it","cb","oc"))

metricas_grid_BTC_base <- trde%>%
  #filter(label_scen=="BAU")%>%
  filter(!conservation=="C")%>%
  # scale data above a certain treshdold
  mutate(relative_to_BAU_2050_skwd=if_else(relative_to_BAU_2050 <(-2),-2,relative_to_BAU_2050))%>%
  #mutate(relative_to_BAU_2050=relative_to_BAU_2050/max(relative_to_BAU_2050))%>%
  ggplot(aes(y = region, x=name)) +
  geom_raster(aes(fill = relative_to_BAU_2050_skwd))+
  #scale_fill_viridis(name = "Rel. BAU",breaks = my_breaks,trans=scales::pseudo_log_trans(sigma = 0.001))+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
  #scale_fill_viridis(limit = limit)+
  theme_bw()+
  #scale_fill_distiller(type = "div", limit = limit)+
  scale_fill_scico(palette = "roma",limits=c(-2,2))+ 
  facet_wrap(~label_scen)+
  rotate_x_text(angle = 90)+
  xlab("")+
  ylab("")+
  ggtitle("BTC-base") +
  theme(legend.position = "none")+
  geom_text(aes(label = round(relative_to_BAU_2050,2)), size = 2 )+
  theme(axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        strip.text.y = element_text(size = 7),
        strip.text.x = element_text(size = 7),
        plot.title = element_text(size = 8),
        axis.title = element_text(size=7))



metricas_grid_C <- trde%>%
  #filter(!label_scen=="BAU")%>%
  filter(conservation=="C")%>%
  mutate(relative_to_BAU_2050_skwd=if_else(relative_to_BAU_2050 <(-2),-2,relative_to_BAU_2050))%>%
  mutate(relative_to_BAU_2050_skwd=if_else(relative_to_BAU_2050_skwd >2,2,relative_to_BAU_2050_skwd))%>%
  ggplot(aes(y = region, x=name)) +
  geom_raster(aes(fill = relative_to_BAU_2050_skwd))+
  # scale_fill_viridis(name = "Rel. BAU",breaks = my_breaks,trans=scales::pseudo_log_trans(sigma = 0.001))+
  theme_bw()+
  scale_fill_scico(palette = "roma", limit = c(-2,2),name = "trade/BAU")+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
  facet_wrap(~label_scen)+
  rotate_x_text(angle = 90)+
  xlab("")+
  ylab("")+
  ggtitle("C")+
  theme(legend.position = c(.85, .2))+
  geom_text(aes(label = round(relative_to_BAU_2050,2)), size = 2 )+
  theme(axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        strip.text.y = element_text(size = 7),
        strip.text.x = element_text(size = 7),
        plot.title = element_text(size = 8),
        axis.title = element_text(size=7),
        legend.title = element_text(size=7),
        legend.text = element_text(size=7))


#option="plasma"

tiles_plot <- ggarrange(metricas_grid_BTC_base,metricas_grid_C,widths = c(2,3))


ggsave(filename = "/dados/pessoal/francisco/TradeHub/figures_paper_new_versions/heatmap_regional_biodiv_with_clim_change.jpeg",width = 15,height = 10,units = "cm",plot = tiles_plot,bg ="white",scale=1)


