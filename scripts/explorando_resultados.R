# explorando resultado

# ignorar o confidence level, ta errado!
# o lance eh juntar scenarios numa tabela e comparar!
# possivelmente com barplot mesmo

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/"

#"globiom_iiasa_test2/results/post_processed/tables/global"

# listando pastas

scenarios <- list.files(p,pattern = "globiom_iiasa",recursive = F)


# listando resultados

resultados <- list.files(file.path(p,scenarios),pattern = ".csv",recursive = T,full.names = T)


# resultados[[3]] <-  list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/plangea_results_wrong/globiom_iiasa_TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2_2/results/post_processed/tables/global",full.names = T)


# abrindo, nomeando cenario 

f <- function(tabelas){
  counter <<- counter + 1
  df <- read.csv(tabelas)[1,]
  df$scenario_name <- scenarios[counter]
  return(df)
  
}

counter <- 0

dfs <- lapply(resultados,f)

df <- do.call(rbind,dfs)

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggallin) # pseudolog trans
library(viridis)
library(RColorBrewer)
library(scales)

val_l <- df[,-c(2:6,26:29)] %>%
  pivot_longer(cols = grep(pattern = ".val",names(df[1,-c(2:6,26:29)])))

val_l <- val_l[,c(1,16:17)]


# adicionando nome da variavel

# bd = débito de extinção de espécies
# cb = carbon
# ec = vulnerabilidade de ecorregiões
# it = integridade de ecossistemas
# oc = custo de oportunidade

met_name <- data.frame(name = unique(val_l$name),metric=c("Carbon","Ecossistem's integrity","Ecoregion's vulnerability","Extinction debt","Land opportunity cost"))


val_l <- left_join(val_l,met_name)

# val_l$legend <- c("carbon","ecosystem integrity","ecoregion vulnerability","extinction risk","opportunity cost")


# plot do resultado das metricas (em relacao ao presente!)


scientific_10 <- function(x) {
  parse(text=gsub("e", "%*% 10^", scales::scientific_format()(x)))
}


l <- c("BAU","frictions and reconfigurations + conservation","frictions and reconfigurations + BTC baseline","baseline + IAP","exacerbated liberalization + IAP")


# bd = débito de extinção de espécies
# cb = carbon
# ec = vulnerabilidade de ecorregiões
# it = integridade de ecossistemas
# oc = custo de oportunidade

#-------------------------------------------------------------------------------

# Painel por variavel

#-------------------------------------------------------------------------------


# rodar c+_ss
# 1.todas de comercio com baseline de conservacao.
# todas de conservacao com baseline de comercio

# usar valores relativos ao bau 2050

# pensar em rodar plangea  2020 BAU 

metricas <- unique(val_l$metric)



l <- c("BAU","frict.&reconfig. + conservation","frict.&reconfig. + BTC baseline","baseline + IAP","transp.cost. red + BTC baseline","tarif.elim.+BTC baseline","exacerb. lib. + IAP","exacerb. lib. + BTC baseline")

nomes_scen <- data_frame(label_scen =l,scenario_name=unique(val_l$scenario_name))

val_l <- left_join(val_l,nomes_scen)

###############################################################################
# versao com valores absolutos
###############################################################################

lista_todos_plots <- list()
c=1
for(metrica in metricas){
  
  gfico <- val_l %>%
    filter(metric== metrica)%>%
  ggplot( aes(x=scenario_name, y=value,fill=scenario_name)) + 
    geom_bar(stat = "identity",position = position_dodge())+
    scale_y_continuous(label=scientific_10 ) +
    theme_classic()+
    xlab(metrica)+
    ylab("Absol. variation (baseline 2020)")+
    scale_fill_brewer(palette = "Spectral",name="name",labels=l)+
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

ggsave(filename = "figures/exploratory_8scen.jpeg",width = 25.4,height = 14.288,units = "cm",plot = todos_plots,bg ="white")

# apenas cenarios comercio

BTC <- c("BIOD_NOTECH_NODEM_SPA0_SSP2",
         "BIOD_TECH_NODEM_SPA0_SSP2",
         "BIOD_TECH_DEM_SPA0_SSP2")

# selecionando BTC scen
conserv <- grep(pattern = paste(BTC,collapse = "|"),x = val_l$scenario_name,value = T)


# eliminando o baseline
conserv_sub <- grep(pattern = "NOBIOD_NOTECH_NODEM_SPA0_SSP2",conserv,value = T,invert = T)



lista_comercio <- list()

c=1

for(metrica in metricas){
  
  gfico <- val_l %>%
    filter(metric== metrica)%>%
    # tirando cenarios de conservacao
    filter(!scenario_name %in% conserv_sub)%>%
    ggplot( aes(x=label_scen, y=value,fill=label_scen)) + 
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
    lista_comercio[[c]] <- gfico
  c = c+1
}

comercio_plots <- ggarrange(plotlist = lista_comercio,common.legend = T)

ggsave(filename = "figures/exploratory_Trade_scen.jpeg",width = 25.4,height = 14.288,units = "cm",plot = comercio_plots,bg ="white")

###############################################################################
# versao com valores relativos ao BAu 2050
###############################################################################


val_l <- val_l %>%
  filter(metric== unique(metric)) %>%
  mutate(relative_to_BAU_2050=value/value[label_scen=="BAU"])

lista_todos_relative <- list()

c=1

for(metrica in metricas){
  
  gfico <- val_l %>%
    filter(metric== metrica)%>%
    # tirando cenarios de conservacao
    filter(!scenario_name %in% conserv_sub)%>%
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

ggsave(filename = "figures/exploratory_Trade_scen.jpeg",width = 25.4,height = 14.288,units = "cm",plot = comercio_plots,bg ="white")
