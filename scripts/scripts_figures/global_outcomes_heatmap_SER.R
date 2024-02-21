# plotando resultado das metricas agregado por cenario


#---- pacotes ------------------------------------------------------------------

library(ggplot2)
library(ggpubr)
library(viridis)
library(ggrepel)
library(scico)
library(dplyr)

#-------------------------------------------------------------------------------

val_l <- read.csv("/dados/pessoal/francisco/TradeHub/output_tables/resultados_metricas_cenarios.csv")

# scenario names

metricas <- unique(val_l$metric)


#Percentage Relative to BAU = ((Opportunity Cost of Scenario - Opportunity Cost of BAU) / Opportunity Cost of BAU) * 100


# baseline

baseline_l <- val_l%>%
  filter(label_scen=="BAU")%>%
  rename(value_BAU=value)

trd_l <- val_l%>%
  filter(label_scen!="BAU")


trd_l <- left_join(trd_l,baseline_l[,c(2,3,4)])%>%
  # calculate relative value
  mutate(relative_to_BAU_2050=(value-value_BAU)/abs(value_BAU))

# cehcar como fazer os sinais fazerem sentido! acho q ta certo agora!

# val_l <- val_l %>%
#   filter(metric== unique(metric)) %>%
#   # calculando prop. em relacao ao BAU em modulo e invertendo sinal
#   mutate(
#     relative_to_BAU_2050=((value-value[label_scen=="BAU"])/(value[label_scen=="BAU"])*100*-1))


# adicionando coluna pra separar os cenarios

# mudei val_l por trd_l daqui pra frente:

trd_l$scen_type <- NA

trd_l$scen_type[c(grep(pattern = "+ C",x = trd_l$label_scen))] <- "C"
trd_l$scen_type[c(grep(pattern = "+ C",x = trd_l$label_scen,invert = T))] <- "baseline"


# order by converted area

trd_l$label_scen <-factor(trd_l$label_scen,levels = rev(c("exacerb. lib. + BTC baseline","tarif.elim.+ BTC baseline","transp.cost. red + BTC baseline","frict.&reconfig. + BTC baseline","BAU","exacerb. lib. + C","tarif.elim.+ C","transp.cost. red + C","frict.&reconfig. + C","BAU + C")))

# plotando cenarios sem ações de conservacao!

# labels legenda (nem precisa)

l <- c("Fr","Tr","Ta","ETL")


# primeiro, escalar cada variavel separadamente, ente -1 e 1
# so variaveis bio

trd_l <- filter(trd_l,name%in%c("bd.val","it.val","ec.val"))

trd_l <- trd_l %>%
  group_by(across(4))%>%
  filter(metric==metric)%>%
  mutate(
    #relative_to_BAU_2050_sc=relative_to_BAU_2050/max(abs(relative_to_BAU_2050))
    # esse metodo esta errado, precisa ser repensado, pq 
    relative_to_BAU_2050_sc = scale(relative_to_BAU_2050, center = min(relative_to_BAU_2050), scale = max(relative_to_BAU_2050) - min(relative_to_BAU_2050)) * 2 - 1)
  


trd_l$name <- gsub(pattern = ".val",replacement = "",x = trd_l$name)

limit1 <- c(min(trd_l$relative_to_BAU_2050[trd_l$scen_type=="baseline"]), max(trd_l$relative_to_BAU_2050[trd_l$scen_type=="baseline"]))

#ordenando siglas das variaveis


trd_l$name <- factor(trd_l$name,levels=c("bd","ec","it","cb","oc"))


metricas_grid_BTC_base <- trd_l%>%
  filter(!label_scen=="BAU")%>%
  filter(!scen_type=="C")%>%
  #mutate(relative_to_BAU_2050=relative_to_BAU_2050/max(relative_to_BAU_2050))%>%
  ggplot(aes(y = label_scen, x=name)) +
    # fill com valores escalados entre -1,1
    #geom_raster(aes(fill = relative_to_BAU_2050_sc))+
    geom_raster(aes(fill = relative_to_BAU_2050))+
    scale_fill_scico(palette = "roma",limits=c(-1.6,1.6) ,name="trade/BAU")+ 
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
    theme(legend.position = "top") +
  geom_text(aes(label = round(relative_to_BAU_2050,2)), size = 2 )

l2 <- c("Trade-base","Fr","Tr","Ta","ETL")

limit2<- c(min(trd_l$relative_to_BAU_2050[trd_l$scen_type!="baseline"]), max(trd_l$relative_to_BAU_2050[trd_l$scen_type!="baseline"]))


metricas_grid_c <- trd_l%>%
  filter(!label_scen=="BAU")%>%
  filter(scen_type=="C")%>%
  #mutate(relative_to_BAU_2050=relative_to_BAU_2050/max(relative_to_BAU_2050))%>%
  ggplot(aes(y = label_scen, x=name)) +
  geom_raster(aes(fill = relative_to_BAU_2050))+
  scale_fill_scico(palette = "roma",limits=c(-1.6,1.6) , name="Rel. var BAU")+ 
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


legend <- trd_l%>%
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
  ggtitle("C") 


legend2 <- get_legend(legend)

# library(egg)
# 
# detach(package:egg,unload=TRUE)

# vertival

final <- ggarrange(metricas_grid_BTC_base,metricas_grid_c,common.legend = T,nrow = 2,align =  "hv")


# final2 <- ggarrange(metricas_grid_BTC_base,metricas_grid_c,common.legend = T,nrow = 2,align =  "hv")

# ajustar aqui!
ggsave(filename = "/dados/pessoal/francisco/TradeHub/figures_paper_new_versions/heatmap_global_biodiv.jpeg",width = 8,height = 15,units = "cm",plot = final,bg ="white",scale = 1)


# continuar! ainda falta ajustar detalhes pq a escala de -1,1 nao ta funcionando. e preciso rever como calcular a diferença!! mas no geral, pra esse global, as barras sao melhores pra vizualizar
