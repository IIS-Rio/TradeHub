# explorando resultado (agora com conservacao junto)

#---- pacotes ------------------------------------------------------------------

library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/"

#"globiom_iiasa_test2/results/post_processed/tables/global"

# listando pastas

scenarios <- list.files(p,pattern = "globiom_iiasa",recursive = F)

# listando resultados

resultados <- list.files(file.path(p,scenarios),pattern = ".csv",recursive = T,full.names = T)

# abrindo, nomeando cenario 

f <- function(tabelas){
  counter <<- counter + 1
  df <- read.csv(tabelas)[1,]
  df$scenario_name <- scenarios[counter]
  return(df)
  
}

# contador pra funcao
counter <- 0

# abrindo dfs de resultados

dfs <- lapply(resultados,f)

# combinando em um df unico

df <- do.call(rbind,dfs)

# mudando pra formato long

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

# scenario names

metricas <- unique(val_l$metric)

# tem q ter apenas os cenarios rodados, na ordem certa

l <- c("BAU",
       "frict.&reconfig. + C",
       "frict.&reconfig. + BTC baseline",
       "BAU + C",
       "transp.cost. red + C",
       "transp.cost. red + BTC baseline",
       "tarif.elim.+ C",
       "tarif.elim.+ BTC baseline",
       "exacerb. lib. + C",
       "exacerb. lib. + BTC baseline")

nomes_scen <- data.frame(label_scen =l,scenario_name=unique(val_l$scenario_name))

val_l <- left_join(val_l,nomes_scen)

write.csv(x = val_l,file = "/dados/pessoal/francisco/TradeHub/output_tables/resultados_metricas_cenarios.csv",row.names = F)


# mudancas de uso da terra


keep <- c(1,17:25,28)

df_s <- df %>%
  dplyr::select(keep) %>%
  pivot_longer(cols = 2:10)%>%
  # nome simplificado dos cenarios
  left_join(nomes_scen)


df_s <- left_join(df_s,nomes_scen)



write.csv(x = df_s,file = "/dados/pessoal/francisco/TradeHub/output_tables/resultados_lu_change_cenarios.csv",row.names = F)


l2 <-expression(paste("Agriculture expansion ("~km^2," ) ",sep=""))

# parece q considerando o resultado mesmo do plangea, nao tem dif. quase entre o transp. cost e o tarif. elimn

 df_s %>%
  filter(name== "AGR")%>%
  # tirando BAU
  #filter(label_scen != "BAU")%>%
  # adatpando escala
  mutate(value=value/1000)%>%
  ggplot( aes(x=label_scen, y=value,fill=label_scen)) + 
  geom_bar(stat = "identity",position = position_dodge())+
  #scale_y_continuous(label=scientific_10 ) +
  theme_classic()+
  xlab("Scenarios")+
  ylab(l2)+
  scale_fill_brewer(palette = "Spectral",name="name")+
  #geom_hline(yintercept=1, linetype="dashed",color = "darkgray", size=1)+
  coord_flip()+
  theme(legend.position = 'bottom', legend.direction = "horizontal",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill=guide_legend(nrow=2,byrow=TRUE,title = ""))
 
 