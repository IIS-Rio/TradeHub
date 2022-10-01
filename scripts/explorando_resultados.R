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

metricas_sem_oc <- metricas[-5]

for(metrica in metricas_sem_oc){
  
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

ggsave(filename = "figures/exploratory_Trade_relative_values_no_oc.jpeg",width = 25.4,height = 14.288,units = "cm",plot = todos_plots_relative,bg ="white")



write.csv(x = val_l,"output_tables/resultado_cenarios.csv",row.names = F)


#-------------------------------------------------------------------------------

# extraindo expansão área agrícola por regiao!


keep <- c(1,17:25,28)

df_s <- df %>%
  select(keep) %>%
  pivot_longer(cols = 2:10)%>%
  # nome simplificado dos cenarios
  left_join(nomes_scen)


l <-expression(paste("Agriculture expansion ("~km^2," ) ",sep=""))

expansao_agricola_total <- df_s %>%
  filter(name== "AGR")%>%
  # tirando cenarios de conservacao
  filter(!scenario_name %in% conserv_sub)%>%
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


ggsave(filename = "figures/exploratory_Trade_agri_expansion_total.jpeg",width = 25.4,height = 14.288,units = "cm",plot = expansao_agricola_total,bg ="white")


write.csv(x = df_s,"output_tables/lu_change_global.csv",row.names = F)

# extracting area expanded per country


library(giscoR)
library(countrycode)
library(sf)

world <- gisco_get_countries()

plot(st_geometry(world))

# Add the subregion

world$region <- countrycode(world$ISO3_CODE,
                            origin = "iso3c",
                            destination = "un.regionsub.name")





subworld <- world %>% 
  group_by(region) %>%
  # Mock the data field
  summarise(data=n())

ggplot(subworld) +
  geom_sf(aes(fill=region))

# open agricultural rasters

p_land_2050 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050"

scenarios_full <- grep(pattern = "SSP2",x = list.files(p_land_2050),value = T)

library(raster)

for(s in 1:length(scenarios_full)){
  
  scen <- scenarios_full[s]
  
  agri <- raster(file.path(p_land_2050,scen,"agriculture.tif"))
  
  # calculando area
  
  pixel_area <- (50100 * 61800)/10^6
  
  # convertendo pra area
  
  agri_km2 <- agri*pixel_area
  
  area_per_region <- data.frame(scen=extract(x = agri_km2,y = subworld,fun= sum,na.rm=TRUE ))
  
  names(area_per_region) <- scen
  
  
  subworld <- cbind(subworld,area_per_region)

}


subworld_df <- subworld

nomes_scen2 <- nomes_scen 

nomes_scen2$scenario_name[nomes_scen2$scenario_name=="globiom_iiasa_baseline"] <- "globiom_iiasa_TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2"



nomes_scen2$scenario_name[nomes_scen2$scenario_name=="globiom_iiasa_TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2_2"] <- "globiom_iiasa_TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2"

nomes_scen2$scenario_name[nomes_scen2$scenario_name=="globiom_iiasa_TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2_2"] <- "globiom_iiasa_TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2"


st_geometry(subworld_df) <- NULL

subworld_df_l <- subworld_df%>%
  pivot_longer(cols = 3:10)%>%
  mutate(name=paste0("globiom_iiasa_",name))%>%
  rename(scenario_name=name)%>%
  left_join(nomes_scen2)

conserv_sub2 <- conserv_sub

conserv_sub2[1:5] <- "globiom_iiasa_TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2"

conserv_sub2[11:15] <- "globiom_iiasa_TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2"

l2 <-expression(paste("Agriculture expansion ("~km^2,"x1000 ) ",sep=""))


expansao_agricola_regional <- subworld_df_l %>%
  #filter(name== "AGR")%>%
  # tirando cenarios de conservacao
  filter(!scenario_name %in% conserv_sub2)%>%
  # tirando BAU
  #filter(label_scen != "BAU")%>%
  # filter na
  filter(!is.na(region))%>%
  # adaptando escala
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
  guides(fill=guide_legend(nrow=2,byrow=TRUE,title = ""))+
  facet_wrap("region")

# globiom_iiasa_TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2
# globiom_iiasa_TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2


write.csv(x = subworld_df_l,"output_tables/agri_expansion_global_regions.csv",row.names = F)


ggsave(filename = "figures/exploratory_Trade_agri_expansion_per_region.jpeg",width = 25.4,height = 14.288,units = "cm",plot = expansao_agricola_regional,bg ="white")

#----------------------------------------------------------

# extrair metricas por pais pra ver se faz sentido alguma relacao entre expansao agricola e uma dada metrica

# acho q da pra somar!

# piloto com bd

metrica <- c("bd","ec","it")

# isso aqui ta errado, deu tudo igual! as metricas ficam com mesmo valor por pais pra todos os cenarios. sera q eh pq eh input?? deve seR!!

for(i in 1:length(scenarios)) {
  
    for(mt in metrica){
    # caminho pros rasters de metricas
    m_path <- list.files(path = file.path(p,scenarios[i],"/results/post_processed/input_variables/"),pattern = paste0(mt,".+.tif"),full.names = T)
    # abrindo raster
    m <- raster(m_path)
    # extraindo valor metrica por regiao
    m_per_region <- data.frame(value=extract(x = m,y = subworld,fun= sum,na.rm=TRUE ))
    # colocando nome correto
    names(m_per_region) <- mt
    
    # faltou nomear os cenarios
    
    # adicionando no shape de regioes
    subworld <- cbind(subworld,m_per_region)
    
    }
}


subworld_df <- subworld

st_geometry(subworld_df) <- NULL

names(subworld_df)[3:26] <- paste0(rep(metrica,3),"_",rep(scenarios,each=3))

subworld_df_l <- pivot_longer(subworld_df,cols =3:26 )

subworld_df_l <- subworld_df_l%>% 
  separate(name,c("metric","scenario_name"),sep = "_globiom_iiasa_")%>%
  filter(!is.na(region))


# combine with agricultural expansion

library(readr)

agri_expansion_global_regions <- read_csv("/dados/pessoal/francisco/TradeHub/output_tables/agri_expansion_global_regions.csv")


# padronizando nomes

# TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2_2
# TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2_2
# TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2_2

subworld_df_l$scenario_name <- gsub(pattern = "TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2_2",replacement = "TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2",x = subworld_df_l$scenario_name)

subworld_df_l$scenario_name <- gsub(pattern = "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2_2",replacement = "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2",x = subworld_df_l$scenario_name)

subworld_df_l$scenario_name <- gsub(pattern = "baseline",replacement = "TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",x = subworld_df_l$scenario_name)

subworld_df_l$scenario_name <- paste0("globiom_iiasa_",subworld_df_l$scenario_name)


names(agri_expansion_global_regions)[4] <- "agricultural_expansion"

#names(subworld_df_l)[4] <- "scenario_name"

subworld_df_l_2 <- left_join(subworld_df_l,agri_expansion_global_regions,)


write.csv("/dados/pessoal/francisco/TradeHub/output_tables/metrics_agri_exp.csv",row.names = F,x = subworld_df_l_2)

subworld_df_l_2 %>%
  filter(metric== metrica[1])%>%
  # tirando cenarios de conservacao
  filter(!scenario_name %in% conserv_sub)%>%
  filter(region == "Latin America and the Caribbean")%>%
  # adatpando escala
  #mutate(value=value/1000)%>%
  ggplot( aes(x=value, y=agricultural_expansion,color=label_scen)) + 
  geom_point()+
  #scale_y_continuous(label=scientific_10 ) +
  theme_classic()+
  #xlab("Scenarios")+
  #ylab(l2)+
  scale_color_brewer(palette = "Spectral",name="name")+
  #geom_hline(yintercept=1, linetype="dashed",color = "darkgray", size=1)+
  #coord_flip()+
  theme(legend.position = 'bottom', legend.direction = "horizontal",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill=guide_legend(nrow=2,byrow=TRUE,title = ""))

for(mt in metrica){
  
  print(mt)
}

list.files(path = file.path(p,scenarios[i],"/results/post_processed/input_variables/"),pattern = paste0(mt,".+.tif"),full.names = T)

# explorar grafico de expansao agricola e metricas, por cenario

p <- "/dados/pessoal/francisco/TradeHub"

library(readr)

resultado_cenarios <- read_csv("/dados/pessoal/francisco/TradeHub/output_tables/resultado_cenarios.csv")

lu_change_global <- read_csv("/dados/pessoal/francisco/TradeHub/output_tables/lu_change_global.csv")


names(lu_change_global)[c(3,4)] <- c("LU","land_use_change")

agri_metrics <- left_join(lu_change_global,resultado_cenarios)


# not great yet!

# plot metrica x agri

l2 <-expression(paste("Agriculture expansion ("~km^2,"x1000 ) ",sep=""))

metrica <-unique(agri_metrics$metric)[2:4]

metricasxarea <- list()
c <- 1
for(m in metrica){
    df <- agri_metrics %>%
    # agriculture
    filter(LU== "AGR")%>%
    # tirando cenarios de conservacao
    filter(!scenario_name %in% conserv_sub)%>%
    # filter(!name == "oc.val")%>%
    # filter(!name == "cb.val")%>%
    filter(metric == m)%>%
    mutate(land_use_change=land_use_change/1000)
    #filter(region == "Latin America and the Caribbean")%>%
    # adatpando escala
    #mutate(value=value/1000)%>%
  gfco <- ggplot(df , aes(x=land_use_change, y=value,color=label_scen,)) + 
  geom_point(position = position_dodge(width =100))+
  #scale_y_continuous(trans=pseudolog10_trans ) +
  theme_classic()+
  xlab(l2)+
  ylab(m)+
  #scale_color_brewer(palette = "Spectral",name="label_scen")+
    theme(legend.title = element_blank())
  
  metricasxarea[[c]] <- gfco
  c <- c+1
# +
#   theme(legend.position = 'top', legend.direction = "horizontal",
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),legend.title = element_text(""))+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE,title = ""))+
 # facet_wrap("name")

}

metrics_area_plot <- ggarrange(plotlist = metricasxarea,common.legend = T)

ggsave(filename = "figures/exploratory_Trade_agrixbio_metrics.jpeg",width = 25.4,height = 14.288,units = "cm",plot = metrics_area_plot,bg ="white")

#----------------------------------------------------------------

# calcular expansao de pastagem e total de habitats naturais em cd cenario (esse pode ser um grafico barra stacked)
