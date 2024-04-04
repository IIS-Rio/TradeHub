# plotando resultado das metricas agregado por cenario


#---- pacotes ------------------------------------------------------------------

library(ggplot2)
library(ggpubr)
library(viridis)
library(ggrepel)
library(scico)
library(dplyr)

#-------------------------------------------------------------------------------

val_l <- read.csv("output_tables/resultados_metricas_cenarios.csv")

# scenario names

metricas <- unique(val_l$metric)


val_l <- val_l %>%
  filter(metric== unique(metric)) %>%
  # calculando prop. em relacao ao BAU em modulo
  mutate(relative_to_BAU_2050=(value-value[label_scen=="BAU"])/abs(value[label_scen=="BAU"]))

# adicionando coluna pra separar os cenarios

val_l$scen_type <- NA

val_l$scen_type[c(grep(pattern = "+ C",x = val_l$label_scen))] <- "C"
val_l$scen_type[c(grep(pattern = "+ C",x = val_l$label_scen,invert = T))] <- "baseline"


# order by converted area

val_l$label_scen <-factor(val_l$label_scen,levels = rev(c("exacerb. lib. + BTC baseline","tarif.elim.+ BTC baseline","transp.cost. red + BTC baseline","frict.&reconfig. + BTC baseline","BAU","exacerb. lib. + C","tarif.elim.+ C","transp.cost. red + C","frict.&reconfig. + C","BAU + C")))


# excluindo bd

val_l2 <- filter(val_l,name!="bd.val")

# ajustando nome baseline

val_l2$scenario_name[val_l2$scenario_name=="globiom_iiasa_baseline"] <- "TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2"

val_l2$scenario_name <- gsub("globiom_iiasa_","",val_l2$scenario_name)


bd <- read.csv("output_tables/updated_results/agg_bd_climate_env_global_newcolumns.csv")

dic <- unique(bd[,c(1,3,4)])

# ajustando colunas

val_l3 <- val_l2%>%
  rename(label_scen_old=label_scen,
         scens=scenario_name,
         variable=metric)%>%
  left_join(dic)


# fundindo

val_l4 <- rbind(val_l3[,c(1:4,6,8,9)],bd)

# eliminando outras variaveis

val_l4 <- filter(val_l4,name %in% c("bd.val","ec.val","it.val"))

# order by converted area

val_l4$label_scen <-factor(val_l4$label_scen,levels = rev(c("ETL","Ta","Tr","Fr","BAU","Trade-base")))

# plotando cenarios sem ações de conservacao!

# labels legenda (nem precisa)

l <- c("Fr","Tr","Ta","ETL")


#ordenando siglas das variaveis
val_l4$name <- gsub(".val",replacement = "",x = val_l4$name)
val_l4$name <- factor(val_l4$name,levels=c("bd","ec","it","cb","oc"))


# trocando etl pelo ta pq o delta rest foi gerado trocado; depois vou refazer e corrigir isso!

val_2_ETL <-  val_l4$value[val_l4$label_scen=="Ta"&val_l4$conservation=="BTC-base"&val_l4$name=="bd"]

val_2_ta <- val_l4$value[val_l4$label_scen=="ETL"&val_l4$conservation=="BTC-base"&val_l4$name=="bd"] 


val_l4$value[val_l4$label_scen=="Ta"&val_l4$conservation=="BTC-base"&val_l4$name=="bd"] <- val_2_ta


val_l4$value[val_l4$label_scen=="ETL"&val_l4$conservation=="BTC-base"&val_l4$name=="bd"] <- val_2_ETL

val_2_ETL_rel <-  val_l4$relative_to_BAU_2050[val_l4$label_scen=="Ta"&val_l4$conservation=="BTC-base"&val_l4$name=="bd"]

val_2_ta_rel <- val_l4$relative_to_BAU_2050[val_l4$label_scen=="ETL"&val_l4$conservation=="BTC-base"&val_l4$name=="bd"] 


val_l4$relative_to_BAU_2050[val_l4$label_scen=="Ta"&val_l4$conservation=="BTC-base"&val_l4$name=="bd"] <- val_2_ta_rel


val_l4$relative_to_BAU_2050[val_l4$label_scen=="ETL"&val_l4$conservation=="BTC-base"&val_l4$name=="bd"] <- val_2_ETL_rel


#- fim da correcao (seque o jogo) ----------------------------------------------

metricas_grid_BTC_base <- val_l4%>%
  filter(!label_scen=="BAU")%>%
  filter(!conservation=="C")%>%
  #mutate(relative_to_BAU_2050=relative_to_BAU_2050/max(relative_to_BAU_2050))%>%
  ggplot(aes(y = label_scen, x=name)) +
  geom_raster(aes(fill = relative_to_BAU_2050))+
  scale_fill_scico(palette = "roma",limits=c(-1.6,1.6),name="Rel. var BAU")+ 
  #scale_y_discrete(labels=l)+
  #scale_fill_viridis(name = "Rel. BAU",breaks = my_breaks,trans=scales::pseudo_log_trans(sigma = 0.001))+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
  #scale_fill_viridis(limit = limit)+
  theme_bw()+
  #scale_fill_distiller(type = "div", limit = limit)+
  
  #facet_wrap(~label_scen)+
  #rotate_x_text(angle = 90)+
  xlab("")+
  ylab("")+
  ggtitle("BTC-base") +
  theme(legend.position = "top")+
  geom_text(aes(label = round(relative_to_BAU_2050,2)), size = 2 )

l2 <- c("Trade-base","Fr","Tr","Ta","ETL")

metricas_grid_c <- val_l4%>%
  filter(!label_scen=="BAU")%>%
  filter(conservation=="C")%>%
  #mutate(relative_to_BAU_2050=relative_to_BAU_2050/max(relative_to_BAU_2050))%>%
  ggplot(aes(y = label_scen, x=name)) +
  geom_raster(aes(fill = relative_to_BAU_2050))+
  scale_fill_scico(palette = "roma", limits=c(-1.6,1.6),name="Rel. var BAU")+ 
  scale_y_discrete(labels=l2)+
  #scale_fill_viridis(name = "Rel. BAU",breaks = my_breaks,trans=scales::pseudo_log_trans(sigma = 0.001))+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
  #scale_fill_viridis(limit = limit)+
  theme_bw()+
  #scale_fill_distiller(type = "div", limit = limit)+
  
  #facet_wrap(~label_scen)+
 # rotate_x_text(angle = 90)+
  xlab("")+
  ylab("")+
  ggtitle("C") +
  theme(legend.position = "none")+
  geom_text(aes(label = round(relative_to_BAU_2050,2)), size = 2 )


legend <- val_l%>%
  filter(!label_scen=="BAU")%>%
  filter(scen_type=="C")%>%
  #mutate(relative_to_BAU_2050=relative_to_BAU_2050/max(relative_to_BAU_2050))%>%
  ggplot(aes(y = label_scen, x=name)) +
  geom_raster(aes(fill = relative_to_BAU_2050_sc))+
  scale_fill_scico(palette = "roma",limits=limit , name="Rel. var BAU")+ 
  scale_y_discrete(labels=l2)+
  #scale_fill_viridis(name = "Rel. BAU",breaks = my_breaks,trans=scales::pseudo_log_trans(sigma = 0.001))+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
  #scale_fill_viridis(limit = limit)+
  theme_bw()+
  #scale_fill_distiller(type = "div", limit = limit)+
  
  #facet_wrap(~label_scen)+
  # rotate_x_text(angle = 90)+
  xlab("")+
  ylab("")+
  ggtitle("C") +
  theme(legend.position = "top")


legend2 <- get_legend(legend)

# library(egg)
# 
# detach(package:egg,unload=TRUE)

final <- ggarrange(metricas_grid_BTC_base,metricas_grid_c,common.legend = T,nrow = 2,align =  "hv")


ggsave(filename = "figures_paper_new_versions/heatmap_global_biodiv_with_clim_change.jpeg",width = 7,height = 14,units = "cm",plot = final,bg ="white",scale = 1)


