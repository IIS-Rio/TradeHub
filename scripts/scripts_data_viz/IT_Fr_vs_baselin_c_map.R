library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(viridis)
library(maptools)
library(raster)

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agriculture_expansion_baseline_2050"

p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/"


# limites globo

data(wrld_simpl)

# prohetando
wrld_transf <- spTransform(wrld_simpl, crs(agri_expan_r))


# legenda

l <- "agriculture expansion (>1%)/baseline 2050"


# listando pastas

scenarios <- list.files(p2,pattern = "globiom_iiasa",recursive = F)

# subset usar so fr + C e baseline + C

agri_expan_files <-list.files(p,pattern = "agricultural")[c(1,3)] [-4]

pasture_expan_files <- list.files(p,pattern = "pasture")[c(1,3)] [-4]




scen <- c("Fr","trade-base")

it <- raster(file.path(p2,scenarios[1],"results/post_processed/input_variables","it_2022-10-05.tif"))

it_df <- as.data.frame(it,xy=TRUE)

names(it_df)[3] <- "it"



maps <- list()
dfs <- list()

for (i in 1:length(agri_expan_files)){
  agri_expan_r <- raster(file.path(p,agri_expan_files[i]))
  past_expan_r <- raster(file.path(p,pasture_expan_files[i]))
  #omando os 2 usos
  
  if(is.na(sum(agri_expan_r[]))){
  
  soma <-  past_expan_r
  
  } else{
    
    soma <- agri_expan_r + past_expan_r
    
  }
  
  
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


global_maps_it <- ggarrange(plotlist =  maps,labels = "AUTO",common.legend = T,nrow = 2)


ggsave(filename = "figures_paper/exploratory_Trade_agri_expansion_bin_relaviteBaseline2050_it_Fr_trade_base.jpeg",width = 18,height = 14.288,units = "cm",plot = global_maps_it,bg ="white")

