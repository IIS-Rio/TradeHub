#-pacotes-----------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(viridis)
library(maptools)
library(raster)
library(sf)
library(tidyverse)
#-------------------------------------------------------------------------------

# rasters dos cenarios

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agriculture_expansion_baseline_2050"


# filtrar so NOBIOD

agri_expan_files <-grep(pattern = "NOBIOD",x = list.files(p,pattern = "agricultural"),value = T)[-2]
pasture_expan_files <- grep(pattern = "NOBIOD",list.files(p,pattern = "pasture"),value = T)[-2]

# abrindor raster modelo

r_m <- raster(file.path(p,agri_expan_files[1]))

# limites globo

data(wrld_simpl)

# excluding Antartica

wrld_simpl <- wrld_simpl[-which(wrld_simpl$NAME == "Antarctica"),]

# projetando

wrld_transf <- spTransform(wrld_simpl, crs(r_m))

# lendo dado de hotspots

hp <- sf::st_make_valid(st_read("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/external_data/hotspots_2016_1.shp"))

# filtrando dados

hp2 <- hp%>%
  filter(Type=="hotspot area")%>%
  filter(!NAME=="Polynesia-Micronesia")

# ajustar projecao

hp2 <- st_transform(hp2,crs = crs(r_m))

hp2 <- st_simplify(hp2)


maps <- list()
dfs <- list()

scens <- c("Fr","Tr","Ta","ETL")

# calculando como grafico de barra, computando expansao em hotspots de biodiversidade! Calcular por regiao, aquelas 11.


# eixo x cenarios, y area, stackeado por regiao!

# cruzar hotspots com regiao!

# 11 regions

regions <- st_read(file.path("/dados/pessoal/francisco/TradeHub/country_boundaries","world_11regions_boundaries.shp"))

regions_pj <-st_transform(regions,crs = crs(r_m))
regions_pj <- st_simplify(regions_pj)

regions_hotspots <- st_intersection(regions_pj,hp2)

# convertendo soma em area

area_pixel <- 50100* 61800/10^6


for (i in 1:length(agri_expan_files)){

  agri_expan_r <- raster(file.path(p,agri_expan_files[i]))
  past_expan_r <- raster(file.path(p,pasture_expan_files[i]))
  #somando os 2 usos
  soma <- agri_expan_r + past_expan_r
  soma_area <- soma*area_pixel
  zonal_result <-data.frame(scen=extract(soma_area, regions_hotspots, fun = sum))
  names(zonal_result) <- scens[i]
  dfs[[i]] <- zonal_result
  # df pra plotar
  expan_df <- as.data.frame(soma, xy=TRUE)
  names(expan_df)[3] <- "agriculture_expansion"
  map <- expan_df%>%
    filter(!is.na(agriculture_expansion))%>%
    filter(agriculture_expansion>0.005)%>%
    ggplot()+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
    geom_raster(aes(x = x,y = y,fill=agriculture_expansion))+
    # add country limits
    geom_sf(data = hp2, fill = "transparent", color = "black") + # Add the hotspots as hollow polygons with dotted black lines
    scale_fill_viridis(option="inferno","agriculture_expansion",direction = -1)+
      labs(
      title = "",
      x = "",
      y = "",
      
    )+
    theme_map()+
    theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme( legend.position = "none")
    l<- get_legend(map, position = NULL)
    
    maps[[i]] <- map
    dfs[[i]] <- zonal_result
}

global_maps<- ggarrange(plotlist =  rev(maps),common.legend = T,labels = rev(c("Fr","Tr","Ta","ETL")))



dfs <- list()

for (i in 1:length(agri_expan_files)){
  soma_area <- soma*area_pixel
  zonal_result <-data.frame(scen=extract(soma_area, regions_hotspots, fun = sum))
  names(zonal_result) <- scens[i]
  dfs[[i]] <- zonal_result
}

# juntando tudo

expansao_hotspots <- do.call(cbind,dfs)

# agregando info. localizacao

st_geometry(regions_hotspots) <- NULL

expansao_hotspots <- cbind(expansao_hotspots,regions_hotspots)

# area total por regiao

expansao_hotspots_sum <- expansao_hotspots%>%
  pivot_longer(cols = 1:4)%>%
  group_by(AggrgtR,name)%>%
  summarise(total_area=sum(value,na.rm = T))

# ordenando

expansao_hotspots_sum$AggrgtR <- factor(expansao_hotspots_sum$AggrgtR , levels=(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))

expansao_hotspots_sum$name <- factor(expansao_hotspots_sum$name , levels=c("Fr","Tr","Ta","ETL"))

# ajustando escala

expansao_hotspots_sum$total_area <- expansao_hotspots_sum$total_area/10^6

# barplot

hotspot_area <- ggplot(expansao_hotspots_sum, aes(x = name, y = total_area, fill = AggrgtR)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "inferno", direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = expression(paste("Agricultural expansion area overlapping hotspots (", Km^2, " ", "x 10"^"-6", ")", sep = "")), fill = "") +
  coord_flip()


final <- ggarrange(global_maps,hotspot_area,nrow = 2,heights = c(2,1))


ggsave(plot = final,filename="/dados/pessoal/francisco/TradeHub/figures_paper/agri_expansion_hotspot_areas.jpg",width = 19,height = 24,units = "cm" )
