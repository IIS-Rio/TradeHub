# calcular as areas de expansão agricola em cada cenario e plotar sobre os países


#--- gerando rasters de expansao agrícola -------------------------------------

# rasters area agricola 2050

p_land_2050 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050"

# cenarios rodados

scenarios_full <- grep(pattern = "SSP2",x = list.files(p_land_2050,full),value = T)

# cenarios so comercio

scenarios_trade <-  grep(pattern = "NOBIOD_NOTECH_NODEM_SPA0_SSP2",x =  scenarios_full,value = T)
  
  
# cenario BAU (usado pra comparacao de todas as metricas)

for(i in 1:length(scenarios_trade)){
  baseline_2020 <- raster(file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use","agriculture.tif"))
  
  
  scen_2050 <- raster(file.path(p_land_2050,scenarios_trade[i],"agriculture.tif"))
  
  # essa eh a diferenca; soh oq for maior que zero eh expansao. Teve tb reducao em alguns locais.
  
  dif <- scen_2050 - baseline_2020
  
  # SO EXPANSAO
  
  expan <-  reclassify(dif, cbind(-1, 0, 0))
  
  writeRaster(x = expan,filename = file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agricultural expansion",paste0(scenarios_trade[i],"_agricultural_expansion.tif")) )
}


#---- plotando por cenário ----------------------------------------------------

library(ggplot2)
library(ggpubr)
library(ggthemes)
library(viridis)
library(maptools)

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agricultural expansion"

agri_expan_files <-list.files(p) 

scen <- c("frictions and reconfig.","BAU","transp. cost. red","tariff elim.","exacerb. lib")

# limites globo

data(wrld_simpl)

# prohetando
wrld_transf <- spTransform(wrld_simpl, crs(agri_expan_r))

# lista pra guardar mapas

maps <- list()
dfs <- list()
for (i in 1:length(agri_expan_files)){
  
  agri_expan_r <- raster(file.path(p,agri_expan_files[i]))

  agri_expan_df <- as.data.frame(agri_expan_r, xy=TRUE)

  names(agri_expan_df)[3] <- "agriculture_expansion"
  agri_expan_df$scen <- scen[i]
  dfs[[i]] <- agri_expan_df
  map <- agri_expan_df%>%
    filter(!is.na(agriculture_expansion))%>%
    filter(!agriculture_expansion<0.05)%>%
    ggplot()+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
    geom_raster(aes(x = x,y = y,fill=agriculture_expansion))+
    scale_fill_viridis(option="turbo","Agriculture expansion (%)")+
    ggtitle(scen[i])+
    theme_map()+
    theme( legend.direction = "horizontal")
  l<- get_legend(map, position = NULL)
    
  #map <- map  + theme(legend.position="none")
  
  maps[[i]] <- map

}


maps[[6]] <- l


global_maps <- ggarrange(plotlist =  maps,ncol=2,nrow=3)

#ggarrange(global_maps,l,widths = c(5,1),heights = c(5,1))

ggsave(filename = "figures/agri_expansion_Trade_map.jpeg",width = 25.4,height = 14.288,units = "cm",plot = global_maps,bg ="white")

# adicionar figura da expansao agricola total no mapa!



lu_change_global <- do.call(rbind,dfs) %>%
  mutate(agri_expansion= agriculture_expansion *(50100*61800)/10^6)%>%
  group_by(scen)%>%
  summarise(total_agri_expansion=sum(agri_expansion,na.rm = T))


l <-expression(paste("Agriculture expansion ("~km^2,"x1000 ) ",sep=""))

trade <- unique(lu_change_global$label_scen)[c(1,3,5,6,8)]

library(forcats)

expansao_agricola_total <- lu_change_global %>%
  mutate(value=total_agri_expansion/1000)%>%
  mutate(name = fct_reorder(scen, value)) %>%
  ggplot( aes(x=name, y=value)) + 
  geom_bar(stat = "identity",position = position_dodge(),fill="lightgray")+
  #scale_y_continuous(label=scientific_10 ) +
  theme_classic()+
  xlab("")+
  ylab(l)+
  #scale_fill_brewer(palette = "Spectral",name="name")+
  #geom_hline(yintercept=1, linetype="dashed",color = "darkgray", size=1)+
  coord_flip()

# maps2 <- maps[1:5]
# 
maps2[[6]] <- expansao_agricola_total


# reordenando toscamente pra ficar em ordem de expansao

maps3 <- list(maps2[[5]],maps2[[3]],maps2[[4]],maps2[[2]],maps2[[1]],maps2[[6]])

global_maps2 <- ggarrange(plotlist =  maps3,common.legend = T,ncol=2,nrow=3)

ggsave(filename = "figures/agri_expansion_Trade_map_2.jpeg",width = 25.4,height = 14.288,units = "cm",plot = global_maps2,bg ="white")

# pq o impacto nas metricas nao eh diretamente proporcional à expansao agricola

# como interfere nas areas mais sensiveis do BD: agri+pastagem em locais sensíveis!

# olhar tb pro desmatamento nas ecorregioes!
