#-------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(viridis)
library(maptools)
library(raster)
#-------------------------------------------------------------------------------

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agriculture_expansion_baseline_2050"


# filtrar so NOBIOD

agri_expan_files <-grep(pattern = "NOBIOD",x = list.files(p,pattern = "agricultural"),value = T)[-2]
pasture_expan_files <- grep(pattern = "NOBIOD",list.files(p,pattern = "pasture"),value = T)[-2]

# abrindor raster modelo

r_m <- raster(file.path(p,agri_expan_files[1]))

scen <- c("Fr","Tr","Ta","ETL")

maptools::wrld_simpl
# limites globo

data(wrld_simpl)

# excluding Antartica

wrld_simpl <- wrld_simpl[-which(wrld_simpl$NAME == "Antarctica"),]

# projetando

wrld_transf <- spTransform(wrld_simpl, crs(r_m))


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

# lendo dado de hotspots

hp <- st_make_valid(st_read("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/external_data/hotspots_2016_1.shp"))

# filtrando dados

hp2 <- hp%>%
  filter(Type=="hotspot area")%>%
  filter(!NAME=="Polynesia-Micronesia")

# ajustar projecao

hp2 <- st_transform(hp2,crs = crs(r_m))

hp3 <- hp2

cali <- hp2%>%
  filter(NAME=="Mesoamerica")

plot(st_geometry(cali))

st_geometry(hp3) <- NULL

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
    geom_sf(data = hp2, fill = "transparent", color = "black") + # Add the hotspots as hollow polygons with dotted black lines
    scale_fill_viridis(option="turbo","it")+
    #scale_fill_manual(values = c ("red","NA"))+
    #scale_fill_gradient(low="red", high="green")+
    labs(
      title = "",
      x = "",
      y = "",
      #caption = scen[i] desliga captios pra ver se melhora tamanho
      )+
    #ggtitle(paste(,scen[i]))+
    theme_map()+
    #theme(plot.margin = margin(-0.5,0.1,0,-0.5, "cm")) 
    theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme( legend.position = "none")
  l<- get_legend(map, position = NULL)
  
  
  
  #map <- map  + theme(legend.position="none")
  
  maps[[i]] <- map
  
  
}


global_maps_it <- ggarrange(plotlist =  maps,common.legend = T,labels = c("Fr","Tr","Ta","ETL"))

#versao pro SUPMAT

legend <- get_legend(maps[[1]])


global_maps_it_2 <- ggarrange(plotlist =  maps,nrow = 4,common.legend = T,legend="top")+
  theme(plot.margin = margin(0,0,0,0, "cm")) 


# figure paper

ggsave(filename = "figures_paper/agri_expansion_IT.jpeg",width = 16,height = 20,units = "cm",plot = global_maps_it_2,bg ="white")
  
# figure presentation

ggsave(filename = "presentations/wp5/agri_expansion_IT_map_hotspots.jpeg",width = 32,height = 18,units = "cm",plot = global_maps_it,bg ="white")


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
    geom_sf(data = hp2, fill = "transparent", color = "black") + # Add the hotspots as hollow polygons with dotted black lines
    scale_fill_viridis(option="turbo","ec")+
    #scale_fill_manual(values = c ("red","NA"))+
    #scale_fill_gradient(low="red", high="green")+
    labs(
      title = "",
      x = "",
      y = "",
      caption = scen[i])+
    #ggtitle(paste(,scen[i]))+
    theme_map()+
    theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  #theme( legend.position = "none")
  l<- get_legend(map, position = NULL)
  
  
  
  #map <- map  + theme(legend.position="none")
  
  maps[[i]] <- map
  
  
}


# global_maps_ec <- ggarrange(plotlist =  maps,labels = "AUTO",common.legend = T)

global_maps_ec2 <- ggarrange(plotlist =  maps,common.legend = T,
                             nrow = 4)+
  theme(plot.margin = margin(0,0,0,0, "cm")) 

# global_maps_ec3 <- ggarrange(plotlist =  maps[1:2],labels = "AUTO",common.legend = T,nrow = 2)

# ggsave(filename = "figures/exploratory_Trade_agri_expansion_bin_relaviteBaseline2050_EC.jpeg",width = 25.4,height = 14.288,units = "cm",plot = global_maps_ec,bg ="white")
# 
# 
# ggsave(filename = "figures/exploratory_Trade_agri_expansion_bin_relaviteBaseline2050_EC_vert.jpeg",width = 14.288,height = 25.4,units = "cm",plot = global_maps_ec2,bg ="white")


ggsave(filename = "figures_paper/agri_expansion_EC.jpeg",width = 16,height = 20,units = "cm",plot = global_maps_ec2,bg ="white")

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
  map <- expand_bd %>%
    filter(!is.na(agriculture_expansion))%>%
    #filter(!agriculture_expansion<0.05)%>%
    ggplot()+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
    geom_raster(aes(x = x,y = y,fill=scaled_bd))+
    geom_sf(data = hp2, fill = "transparent", color = "transparent") + # Add the hotspots as hollow polygons with dotted black lines
    scale_fill_viridis(option="turbo","bd")+
    #scale_fill_manual(values = c ("red","NA"))+
    #scale_fill_gradient(low="red", high="green")+
    labs(
      title = "",
      x = "",
      y = "",
      #caption = scen[i]
      )+
    #ggtitle(paste(,scen[i]))+
    theme_map()+
    theme(plot.margin = margin(-3, -2, -3, -3, "cm"))
  #theme( legend.position = "none")
  l<- get_legend(map, position = NULL)
  
  
  
  #map <- map  + theme(legend.position="none")
  
  maps[[i]] <- map
  
  
}

global_maps_bd <- ggarrange(plotlist =  maps,common.legend = T,nrow = 4)+
  theme(plot.margin = margin(0,0,0,0, "cm"))

global_maps_bd <- ggarrange(plotlist =  maps,common.legend = T,labels = c("Fr","Tr","Ta","ETL"))

ggsave(filename = "figures_paper/agri_expansion_BD.jpeg",width = 16,height = 20.288,units = "cm",plot = global_maps_bd,bg ="white")


# figure presentation

ggsave(filename = "presentations/wp5/agri_expansion_BD_map_hotspots.jpeg",width = 32,height = 18,units = "cm",plot = global_maps_bd,bg ="white")

ggsave(filename = "presentations/wp5/agri_expansion_BD_map_nohotspots.jpeg",width = 32,height = 18,units = "cm",plot = global_maps_bd,bg ="white")
