# plotando resultado das metricas agregado por cenario


#---- pacotes ------------------------------------------------------------------

library(ggplot2)
library(ggpubr)
library(viridis)
library(ggrepel)
library(scico)

#-------------------------------------------------------------------------------

val_l <- read.csv("output_tables/resultados_metricas_cenarios.csv")

# scenario names

metricas <- unique(val_l$metric)


val_l <- val_l %>%
  filter(metric== unique(metric)) %>%
  # calculando prop. em relacao ao BAU em modulo
  mutate(relative_to_BAU_2050=value/abs(value[label_scen=="BAU"]))
 

# adicionando coluna pra separar os cenarios

val_l$scen_type <- NA

val_l$scen_type[c(grep(pattern = "+ C",x = val_l$label_scen))] <- "C"
val_l$scen_type[c(grep(pattern = "+ C",x = val_l$label_scen,invert = T))] <- "baseline"


# subtraindo 1 do BTC base fica boa a relacao
val_l$relative_to_BAU_2050_rc <- NA

val_l$relative_to_BAU_2050_rc[val_l$scen_type=="baseline"]<-  val_l$relative_to_BAU_2050[val_l$scen_type=="baseline"] +1


val_l$relative_to_BAU_2050_rc[val_l$scen_type!="baseline"]<-  val_l$relative_to_BAU_2050[val_l$scen_type=="baseline"] 

# order by converted area

val_l$label_scen <-factor(val_l$label_scen,levels = rev(c("exacerb. lib. + BTC baseline","tarif.elim.+ BTC baseline","transp.cost. red + BTC baseline","frict.&reconfig. + BTC baseline","BAU","exacerb. lib. + C","tarif.elim.+ C","transp.cost. red + C","frict.&reconfig. + C","BAU + C")))



# plotando cenarios sem ações de conservacao!

# labels legenda (nem precisa)

l <- c("Fr","Tr","Ta","ETL")


# primeiro, escalar cada variavel separadamente, ente -1 e 1


val_l <- val_l %>%
  group_by(across(4))%>%
  filter(metric==metric)%>%
  mutate(relative_to_BAU_2050_sc=relative_to_BAU_2050/max(abs(relative_to_BAU_2050)))%>%
  mutate(diff= relative_to_BAU_2050_sc - relative_to_BAU_2050_sc[label_scen=="BAU"] )


val_l$name <- gsub(pattern = ".val",replacement = "",x = val_l$name)

limit <- max(abs(val_l$relative_to_BAU_2050_sc)) * c(-1, 1)

#ordenando siglas das variaveis


val_l$name <- factor(val_l$name,levels=c("bd","ec","it","cb","oc"))


metricas_grid_BTC_base <- val_l%>%
  filter(!label_scen=="BAU")%>%
  filter(!scen_type=="C")%>%
  #mutate(relative_to_BAU_2050=relative_to_BAU_2050/max(relative_to_BAU_2050))%>%
  ggplot(aes(y = label_scen, x=name)) +
  geom_raster(aes(fill = relative_to_BAU_2050_rc))+
  scale_fill_scico(palette = "roma",limits=limit ,name="Rel. var BAU")+ 
  scale_y_discrete(labels=l)+
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
  theme(legend.position = "top")

l2 <- c("Trade-base","Fr","Tr","Ta","ETL")

metricas_grid_c <- val_l%>%
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
  theme(legend.position = "none")


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

library(egg)

detach(package:egg,unload=TRUE)

final <- ggarrange(metricas_grid_BTC_base,metricas_grid_c,common.legend = T,nrow = 2,align =  "hv")


ggsave(filename = "figures_paper/exploratory_Trade_relative_values_heatmap.jpeg",width = 9,height = 18,units = "cm",plot = final,bg ="white")


# continuar! ainda falta ajustar detalhes pq a escala de -1,1 nao ta funcionando. e preciso rever como calcular a diferença!! mas no geral, pra esse global, as barras sao melhores pra vizualizar