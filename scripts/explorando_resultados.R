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

val_l <- df[,-c(2:6,26:29)] %>%
  pivot_longer(cols = grep(pattern = ".val",names(df[1,-c(2:6,26:29)])))

val_l <- val_l[,c(1,16:17)]


# val_l$legend <- c("carbon","ecosystem integrity","ecoregion vulnerability","extinction risk","opportunity cost")


# plot do resultado das metricas (em relacao ao presente!)


scientific_10 <- function(x) {
  parse(text=gsub("1e", "10^", scales::scientific_format()(x)))
}


l <- c("BAU","frictions and reconfigurations + conservation","frictions and reconfigurations + BTC baseline","baseline + IAP","exacerbated liberalization + IAP")

# l <- c("BAU","frictions and reconfigurations + conservation","baseline + IAP","exacerbated liberalization + IAP")

plot <- ggplot(val_l, aes(x=name, y=value,fill=scenario_name)) + 
  geom_bar(stat = "identity",position = position_dodge())+
  scale_y_continuous(trans = pseudolog10_trans,breaks = c(10^2,10^4,10^6,10^8,10^10,0,-10^2,-10^4,-10^6,-10^8,-10^10,-10^12),label=scientific_10 ) +
  theme_classic()+
  xlab("plangea's metrics")+
  ylab("Variation relative to present")+
  scale_fill_viridis_d(name="scenario",labels=l)



# bd = débito de extinção de espécies
# cb = carbon
# ec = vulnerabilidade de ecorregiões
# it = integridade de ecossistemas
# oc = custo de oportunidade

# Painel por variavel


ggplot(val_l, aes(x=name, y=value,fill=scenario_name)) + 
  geom_bar(stat = "identity",position = position_dodge())+
  scale_y_continuous(trans = pseudolog10_trans,breaks = c(10^2,10^4,10^6,10^8,10^10,0,-10^2,-10^4,-10^6,-10^8,-10^10,-10^12),label=scientific_10 ) +
  theme_classic()+
  xlab("plangea's metrics")+
  ylab("Variation relative to present")+
  scale_fill_viridis_d(name="scenario")+
  coord_flip()+
  facet_grid("scenario_name")

ggplot(val_l, aes(x=scenario_name, y=value,fill=scenario_name)) + 
  geom_bar(stat = "identity",position = position_dodge())+
  scale_y_continuous(trans = pseudolog10_trans,breaks = c(10^2,10^4,10^6,10^8,10^10,0,-10^2,-10^4,-10^6,-10^8,-10^10,-10^12),label=scientific_10 ) +
  theme_classic()+
  xlab("plangea's metrics")+
  ylab("Variation relative to present")+
  scale_fill_viridis_d(name="scenario",labels=l)+
  scale_x_discrete(labels= l)+
  coord_flip()+
  facet_grid("name")+
  theme(legend.position = 'bottom', legend.direction = "horizontal")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
 

# rodar c+_ss
# 1.todas de comercio com baseline de conservacao.
# todas de conservacao com baseline de comercio

# usar valores relativos ao bau 2050

# pensar em rodar plangea  2020 BAU 