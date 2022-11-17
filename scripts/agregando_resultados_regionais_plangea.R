#---- pacotes ------------------------------------------------------------------

library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/regional_analysis/"

# listando regioes
regions <- list.files(p,recursive = F)

# listando scen 

scen <- list.files(file.path(p,"CAN"),recursive = F)


# listando resultados e inserindo nome do cenario e regiao


lista_df <- list()
c <- 1
for(i in 1:length(regions)){
  for(j in 1:length(scen)){
  df <- read.csv(list.files(path = file.path(p,regions[i],scen[j]),pattern = ".csv",full.names = T,recursive = T),row.names = 1)
  
  df <- df%>%
    mutate(region=regions[i],scenario=scen[j])%>%
    filter(row.names(df)=="Future land-use")
  lista_df[[c]] <- df
  c <- c+1

  }
}

# combinando em um df unico

df <- do.call(rbind,lista_df)

# mudando pra formato long

val_l <- df[,c(seq(6,14,2),16:24,27,29,30)] %>%
  pivot_longer(cols = 1:15)

# adicionando nome da variavel

# bd = débito de extinção de espécies
# cb = carbon
# ec = vulnerabilidade de ecorregiões
# it = integridade de ecossistemas
# oc = custo de oportunidade

# alterar nomes 
met_name <- data.frame(name = grep(pattern = ".val",unique(val_l$name),value = T),variable=c("Carbon","Ecossistem integrity reduction","Ecoregion vulnerability","Extinction debt reduction","Land opportunity cost"))


val_l <- left_join(val_l,met_name)

val_l$variable[is.na(val_l$variable)] <- "lulcc"


# tem q ter apenas os cenarios rodados, na ordem certa

l <- rep(c("Fr",
       "Trade-base",
       "Tr",
       "Ta",
       "ETL"),each=2)

nomes_scen <- data.frame(label_scen =l,scenario=unique(val_l$scenario))

nomes_scen[4,1] <- "BAU"

# adicionando info. de conservacao ou Base-BTC

nomes_scen$conservation <- rep(c("C","BTC-base"),5)

val_l <- left_join(val_l,nomes_scen)

write.csv(x = val_l,file = "/dados/pessoal/francisco/TradeHub/output_tables/resultados_cenarios_regional_analysis.csv",row.names = F)


