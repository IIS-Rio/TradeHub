library(readr)

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/"

# resultados agregados plangea

resultado_cenarios <- read_csv("output_tables/resultado_cenarios.csv")

resultado_cenarios <- resultado_cenarios%>%
  filter(metric== unique(metric)) %>%
  mutate(relative_to_BAU_2050=value/value[label_scen=="BAU"])

# metricas calculadas

metricas <- unique(resultado_cenarios$metric)


lista_todos_relative <- list()

c=1

cenarios_comercio <- unique(resultado_cenarios$label_scen)[c(1,3,5,6,8)]

metricas_sem_oc <- metricas[-5]


# ordenando cenarios em ordem de expansao agricola

resultado_cenarios$label_scen <-factor(resultado_cenarios$label_scen,levels = rev(c("exacerb. lib. + BTC baseline","transp.cost. red + BTC baseline","tarif.elim.+BTC baseline","BAU","frict.&reconfig. + BTC baseline","exacerb. lib. + IAP","frict.&reconfig. + conservation")))


lista_todos_relative <- list()

c=1

for(metrica in metricas_sem_oc){
  
  gfico <- resultado_cenarios %>%
    filter(metric== metrica)%>%
    # tirando cenarios de conservacao
    filter(label_scen %in% cenarios_comercio)%>%
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

todos_plots_relative <- ggarrange(plotlist = lista_todos_relative,common.legend = T)


ggsave(filename = "figures/exploratory_Trade_relative_values_no_oc.jpeg",width = 25.4,height = 14.288,units = "cm",plot = todos_plots_relative,bg ="white")
