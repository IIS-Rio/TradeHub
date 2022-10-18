library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(viridis)
library(maptools)
library(raster)

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agriculture_expansion_baseline_2050"



agri_expan_files <-list.files(p,pattern = "agricultural")[-2] 
pasture_expan_files <- list.files(p,pattern = "pasture")[-2]

scen <- c("frictions and reconfig.","transp. cost. red","tariff elim.","exacerb. lib")


# limites globo

data(wrld_simpl)

# prohetando
wrld_transf <- spTransform(wrld_simpl, crs(agri_expan_r))


# legenda

l <- "agriculture expansion (>1%)/baseline 2050"


# lista pra guardar mapas (tem q excluir o baseline, q da 0)

maps <- list()
dfs <- list()

for (i in 1:length(agri_expan_files)){
  agri_expan_r <- raster(file.path(p,agri_expan_files[i]))
  past_expan_r <- raster(file.path(p,pasture_expan_files[i]))
  #omando os 2 usos
  soma <- agri_expan_r + past_expan_r
  # binarizando (cob agri > 1%)
  expan_df <- as.data.frame(soma, xy=TRUE)%>%
    filter(layer>0.01)%>%
    mutate(bin=ifelse(layer<0.01,0,1))
  
  names(expan_df)[3] <- "agriculture_expansion"
  expan_df$scen <- scen[i]
  dfs[[i]] <- expan_df
  map <- expan_df%>%
    filter(!is.na(agriculture_expansion))%>%
    #filter(!agriculture_expansion<0.05)%>%
    ggplot()+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
    geom_raster(aes(x = x,y = y,fill=bin))+
    #scale_fill_viridis_d("Agriculture expansion (>0.01)")+
    #scale_fill_manual(values = c ("red","NA"))+
    #scale_fill_gradient(low="red", high="green")+
    labs(
      title = "",
      x = "",
      y = "",
      caption = scen[i])+
    #ggtitle(paste(,scen[i]))+
    theme_map()+
    theme( legend.position = "none")
  l<- get_legend(map, position = NULL)
  
  
  
  #map <- map  + theme(legend.position="none")
  
  maps[[i]] <- map


}

global_maps <- ggarrange(plotlist =  maps,labels = "AUTO")

ggsave(filename = "figures/exploratory_Trade_agri_expansion_bin_relaviteBaseline2050.jpeg",width = 25.4,height = 14.288,units = "cm",plot = global_maps,bg ="white")


#-------------------------------------------------------------------------------

# fazer o mapa usando o raster de mascara pro indice de biodiv


p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/"

#"globiom_iiasa_test2/results/post_processed/tables/global"

# listando pastas

scenarios <- list.files(p2,pattern = "globiom_iiasa",recursive = F)


# listando resultados

resultados <- list.files(file.path(p2,scenarios),pattern = ".csv",recursive = T,full.names = T)

#### espacializando IT #########################################################



it <- raster(file.path(p2,scenarios[1],"results/post_processed/input_variables","it_2022-10-05.tif"))

it_df <- as.data.frame(it,xy=TRUE)

names(it_df)[3] <- "it"



maps <- list()
dfs <- list()

for (i in 1:length(agri_expan_files)){
  agri_expan_r <- raster(file.path(p,agri_expan_files[i]))
  past_expan_r <- raster(file.path(p,pasture_expan_files[i]))
  #omando os 2 usos
  soma <- agri_expan_r + past_expan_r
  
  # binarizando (cob agri > 1%)
  expan_df <- as.data.frame(soma, xy=TRUE)
  names(expan_df)[3] <- "agriculture_expansion"
  expand_it <- left_join(expan_df,it_df)%>%
    filter(agriculture_expansion>0.01)%>%
    mutate(bin=ifelse(agriculture_expansion<0.01,0,1))
  # continuar
  expand_it$scen <- scen[i]
  dfs[[i]] <- expan_df
  map <- expand_it%>%
    filter(!is.na(agriculture_expansion))%>%
    #filter(!agriculture_expansion<0.05)%>%
    ggplot()+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
    geom_raster(aes(x = x,y = y,fill=it))+
    scale_fill_viridis(option="turbo","it")+
    #scale_fill_manual(values = c ("red","NA"))+
    #scale_fill_gradient(low="red", high="green")+
    labs(
      title = "",
      x = "",
      y = "",
      caption = scen[i])+
    #ggtitle(paste(,scen[i]))+
    theme_map()
    #theme( legend.position = "none")
  l<- get_legend(map, position = NULL)
  
  
  
  #map <- map  + theme(legend.position="none")
  
  maps[[i]] <- map
  
  
}


global_maps_it <- ggarrange(plotlist =  maps,labels = "AUTO",common.legend = T)



ggsave(filename = "figures/exploratory_Trade_agri_expansion_bin_relaviteBaseline2050_IT.jpeg",width = 25.4,height = 14.288,units = "cm",plot = global_maps_it,bg ="white")


#### espacializando EC #########################################################



ec <- raster(file.path(p2,scenarios[1],"results/post_processed/input_variables","ec_2022-10-05.tif"))

ec_df <- as.data.frame(ec,xy=TRUE)

names(ec_df)[3] <- "ec"

ec_df$ec_log10 <- log10(ec_df$ec)

# normalizando valores de 0 a 1

range01 <- function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}

ec_df <- ec_df%>%
  mutate(scaled_ec = range01(ec_log10))

maps <- list()
dfs <- list()

for (i in 1:length(agri_expan_files)){
  agri_expan_r <- raster(file.path(p,agri_expan_files[i]))
  past_expan_r <- raster(file.path(p,pasture_expan_files[i]))
  #omando os 2 usos
  soma <- agri_expan_r + past_expan_r
  
  # binarizando (cob agri > 1%)
  expan_df <- as.data.frame(soma, xy=TRUE)
  names(expan_df)[3] <- "agriculture_expansion"
  expand_ec <- left_join(expan_df,ec_df)%>%
    filter(agriculture_expansion>0.01)%>%
    mutate(bin=ifelse(agriculture_expansion<0.01,0,1))
  # continuar
  expand_ec$scen <- scen[i]
  dfs[[i]] <- expan_df
  map <- expand_ec%>%
    filter(!is.na(agriculture_expansion))%>%
    #filter(!agriculture_expansion<0.05)%>%
    ggplot()+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
    geom_raster(aes(x = x,y = y,fill=scaled_ec))+
    scale_fill_viridis(option="turbo","ec")+
    #scale_fill_manual(values = c ("red","NA"))+
    #scale_fill_gradient(low="red", high="green")+
    labs(
      title = "",
      x = "",
      y = "",
      caption = scen[i])+
    #ggtitle(paste(,scen[i]))+
    theme_map()
  #theme( legend.position = "none")
  l<- get_legend(map, position = NULL)
  
  
  
  #map <- map  + theme(legend.position="none")
  
  maps[[i]] <- map
  
  
}


global_maps_ec <- ggarrange(plotlist =  maps,labels = "AUTO",common.legend = T)

global_maps_ec2 <- ggarrange(plotlist =  maps,labels = "AUTO",common.legend = T,
                             nrow = 4)

global_maps_ec3 <- ggarrange(plotlist =  maps[1:2],labels = "AUTO",common.legend = T,nrow = 2)

ggsave(filename = "figures/exploratory_Trade_agri_expansion_bin_relaviteBaseline2050_EC.jpeg",width = 25.4,height = 14.288,units = "cm",plot = global_maps_ec,bg ="white")


ggsave(filename = "figures/exploratory_Trade_agri_expansion_bin_relaviteBaseline2050_EC_vert.jpeg",width = 14.288,height = 25.4,units = "cm",plot = global_maps_ec2,bg ="white")


ggsave(filename = "figures/exploratory_Trade_agri_expansion_bin_relaviteBaseline2050_EC_vert_subset.jpeg",width = 15,height = 15,units = "cm",plot = global_maps_ec3,bg ="white")

#### espacializando EC #########################################################



bd <- raster(file.path(p2,scenarios[1],"results/post_processed/input_variables","bd_2022-10-05.tif"))

bd_df <- as.data.frame(bd,xy=TRUE)

names(bd_df)[3] <- "bd"

bd_df$bd_log10 <- log10(bd_df$bd)

# normalizando valores de 0 a 1

range01 <- function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}

bd_df <- bd_df%>%
  mutate(scaled_bd = range01(bd_log10))

maps <- list()
dfs <- list()

for (i in 1:length(agri_expan_files)){
  agri_expan_r <- raster(file.path(p,agri_expan_files[i]))
  past_expan_r <- raster(file.path(p,pasture_expan_files[i]))
  #omando os 2 usos
  soma <- agri_expan_r + past_expan_r
  
  # binarizando (cob agri > 1%)
  expan_df <- as.data.frame(soma, xy=TRUE)
  names(expan_df)[3] <- "agriculture_expansion"
  expand_bd <- left_join(expan_df,bd_df)%>%
    filter(agriculture_expansion>0.01)%>%
    mutate(bin=ifelse(agriculture_expansion<0.01,0,1))
  # continuar
  expand_bd$scen <- scen[i]
  dfs[[i]] <- expan_df
  map <- expand_bd%>%
    filter(!is.na(agriculture_expansion))%>%
    #filter(!agriculture_expansion<0.05)%>%
    ggplot()+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
    geom_raster(aes(x = x,y = y,fill=scaled_bd))+
    scale_fill_viridis(option="turbo","bd")+
    #scale_fill_manual(values = c ("red","NA"))+
    #scale_fill_gradient(low="red", high="green")+
    labs(
      title = "",
      x = "",
      y = "",
      caption = scen[i])+
    #ggtitle(paste(,scen[i]))+
    theme_map()
  #theme( legend.position = "none")
  l<- get_legend(map, position = NULL)
  
  
  
  #map <- map  + theme(legend.position="none")
  
  maps[[i]] <- map
  
  
}

global_maps_bd <- ggarrange(plotlist =  maps,labels = "AUTO",common.legend = T)



ggsave(filename = "figures/exploratory_Trade_agri_expansion_bin_relaviteBaseline2050_BD.jpeg",width = 25.4,height = 14.288,units = "cm",plot = global_maps_bd,bg ="white")
