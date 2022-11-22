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


# order by converted area

val_l$label_scen <-factor(val_l$label_scen,levels = rev(c("exacerb. lib. + BTC baseline","tarif.elim.+ BTC baseline","transp.cost. red + BTC baseline","frict.&reconfig. + BTC baseline","BAU","exacerb. lib. + C","tarif.elim.+ C","transp.cost. red + C","frict.&reconfig. + C","BAU + C")))

# adicionando coluna pra separar os cenarios

val_l$scen_type <- NA

val_l$scen_type[c(grep(pattern = "+ C",x = val_l$label_scen))] <- "C"
val_l$scen_type[c(grep(pattern = "+ C",x = val_l$label_scen,invert = T))] <- "baseline"

# plotando cenarios sem ações de conservacao!


# labels legenda (nem precisa)

l <- c("Fr","Tr","Ta","ETL")

val_l$relative_to_BAU_2050_red <- 1 - val_l$relative_to_BAU_2050

lista_todos_relative_baseline <- list()

c=1

metricas_sem_oc <- metricas[-5]

for(metrica in metricas_sem_oc){
  
  gfico <- val_l %>%
    filter(metric== metrica)%>%
    filter(scen_type=="baseline")%>%
    # tirando BAU
    filter(label_scen != "BAU")%>%
    ggplot( aes(x=label_scen, y=relative_to_BAU_2050_red,fill=label_scen)) + 
    geom_bar(stat = "identity",position = position_dodge())+
    #scale_y_continuous(label=scientific_10 ) +
    theme_classic()+
    xlab(metrica)+
    ylab("Relat. variation (BAU 2050)")+
    scale_fill_brewer(palette = "Spectral",name="",labels=l)+
    geom_hline(yintercept=0, linetype="dashed",color = "darkgray", size=1)+
    ylim(-0.5,0.5)+
    coord_flip()+
    theme(legend.position = 'bottom', legend.direction = "horizontal",
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
    #guides(fill=guide_legend(nrow=2,byrow=TRUE,title = ""))
  lista_todos_relative_baseline[[c]] <- gfico
  c = c+1
}

todos_plots_relative <- ggarrange(plotlist = lista_todos_relative_baseline[2:4],common.legend = T,ncol = 3,labels = c("A","B","C"))

# ggsave(filename = "figures/exploratory_Trade_relative_values_no_oc.jpeg",width = 25.4,height = 9,units = "cm",plot = todos_plots_relative,bg ="white")

# plotando cenarios com ações de conservacao!


# pra manter reducao em relacao a x

lista_todos_relative_conservacao <- list()

c=1

metricas_sem_oc <- metricas[-5]



for(metrica in metricas_sem_oc){
  
  
  valores_BAU_C <- val_l$relative_to_BAU_2050_red[val_l$label_scen=="BAU + C"&val_l$metric==metrica]
  valores_BAU_C <- valores_BAU_C-1
  gfico <- val_l %>%
    filter(metric== metrica)%>%
    filter(scen_type=="C")%>%
    # tirando BAU
    filter(label_scen != "BAU + C")%>%
    mutate(relative_to_BAU_2050_red=relative_to_BAU_2050_red-1)%>%
    ggplot( aes(x=label_scen, y=relative_to_BAU_2050_red,fill=label_scen)) + 
    geom_bar(stat = "identity",position = position_dodge())+
    #scale_y_continuous(label=scientific_10 ) +
    theme_classic()+
    xlab(metrica)+
    ylab("Relat. variation (BAU 2050)")+
    scale_fill_brewer(palette = "Spectral",name="",labels=l)+
    geom_hline(yintercept=0, linetype="dashed",color = "darkgray", size=1)+
    geom_hline(yintercept=valores_BAU_C, linetype="dashed",color = "black", size=1)+
    ylim(-0.6,0.6)+
    coord_flip()+
    theme(legend.position = 'none', legend.direction = "horizontal",
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
    #guides(fill=guide_legend(nrow=2,byrow=TRUE,title = ""))
  lista_todos_relative_conservacao[[c]] <- gfico
  c = c+1
}

todos_plots_relative_c <- ggarrange(plotlist = lista_todos_relative_conservacao[2:4],ncol = 3,labels = c("D","E","F"))


combinando_bau_conservation <- ggarrange(todos_plots_relative,todos_plots_relative_c,nrow = 2)

ggsave(filename = "figures/exploratory_Trade_relative_values_biodiv.jpeg",width = 22,height = 16,units = "cm",plot = combinando_bau_conservation,bg ="white")
