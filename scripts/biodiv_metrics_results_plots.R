# plotando resultado das metricas agregado por cenario


#---- pacotes ------------------------------------------------------------------

library(ggplot2)
library(ggpubr)

#-------------------------------------------------------------------------------

val_l <- read.csv("output_tables/resultados_metricas_cenarios.csv")

# scenario names

metricas <- unique(val_l$metric)


# funcao pra plotar eixos em notacao base 10

scientific_10 <- function(x) {
  parse(text=gsub("e", "%*% 10^", scales::scientific_format()(x)))
}

###############################################################################
# versao com valores absolutos
###############################################################################


# labels legenda (nem precisa)

l <- c("BAU","frictions and reconfigurations + conservation","frictions and reconfigurations + BTC baseline","baseline + IAP","exacerbated liberalization + IAP")


lista_todos_plots <- list()

c=1
for(metrica in metricas){
  
  gfico <- val_l %>%
    filter(metric== metrica)%>%
    ggplot( aes(x=scenario_name, y=value,fill=label_scen)) + 
    geom_bar(stat = "identity",position = position_dodge())+
    scale_y_continuous(label=scientific_10 ) +
    theme_classic()+
    xlab(metrica)+
    ylab("Absol. variation (baseline 2020)")+
    scale_fill_brewer(palette = "Spectral",name="name")+
    geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
    coord_flip()+
    theme(legend.position = 'bottom', legend.direction = "horizontal",
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    guides(fill=guide_legend(nrow=2,byrow=TRUE,title = ""))
  lista_todos_plots[[c]] <- gfico
  c = c+1
}

todos_plots <- ggarrange(plotlist = lista_todos_plots,common.legend = T)

# salvando
ggsave(filename = "figures/exploratory_Trade_scen.jpeg",width = 25.4,height = 14.288,units = "cm",plot = todos_plots,bg ="white")


###############################################################################
# versao com valores relativos ao BAu 2050
###############################################################################


val_l <- val_l %>%
  filter(metric== unique(metric)) %>%
  mutate(relative_to_BAU_2050=value/value[label_scen=="BAU"])


val_l$label_scen <-factor(val_l$label_scen,levels = rev(c("exacerb. lib. + BTC baseline","transp.cost. red + BTC baseline","tarif.elim.+BTC baseline","BAU","frict.&reconfig. + BTC baseline")))



lista_todos_relative <- list()

c=1

metricas_sem_oc <- metricas[-5]

for(metrica in metricas_sem_oc){
  
  gfico <- val_l %>%
    filter(metric== metrica)%>%
    # tirando BAU
    filter(label_scen != "BAU")%>%
    ggplot( aes(x=label_scen, y=relative_to_BAU_2050,fill=label_scen)) + 
    geom_bar(stat = "identity",position = position_dodge())+
    #scale_y_continuous(label=scientific_10 ) +
    theme_classic()+
    xlab(metrica)+
    ylab("Relat. variation (BAU 2050)")+
    scale_fill_brewer(palette = "Spectral",name="name")+
    geom_hline(yintercept=1, linetype="dashed",color = "darkgray", size=1)+
    coord_flip()+
    theme(legend.position = 'bottom', legend.direction = "horizontal",
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    guides(fill=guide_legend(nrow=2,byrow=TRUE,title = ""))
  lista_todos_relative[[c]] <- gfico
  c = c+1
}

todos_plots_relative <- ggarrange(plotlist = lista_todos_relative[2:4],common.legend = T,ncol = 3)

ggsave(filename = "figures/exploratory_Trade_relative_values_no_oc.jpeg",width = 25.4,height = 9,units = "cm",plot = todos_plots_relative,bg ="white")
