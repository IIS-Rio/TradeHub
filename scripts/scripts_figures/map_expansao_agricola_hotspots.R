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

hp <- (st_read("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/external_data/hotspots_2016_1.shp"))

#hp <- st_make_valid(hp)

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

regions_hotspots$area <- as.numeric(st_area(regions_hotspots)/10^6)

regions_hotspots2 <- st_drop_geometry(regions_hotspots)

# salvandot tabela

write.csv(regions_hotspots2,"/dados/pessoal/francisco/TradeHub/output_tables/updated_results/overlap_hotspots_11_regions.csv",row.names = F)


# convertendo soma em area

area_pixel <- 50100* 61800/10^6

# Calculate the centroid of the multipolygons
centroid <- st_centroid(regions_pj)
centroid <- cbind(centroid,st_coordinates(centroid))



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
    # add hotspots limits
    geom_sf(data = hp2, fill = "transparent", color = "black") + # Add the hotspots as hollow polygons with dotted black lines
    # add regions
    geom_sf(data = regions_pj, fill = "transparent", color = "darkgray",lwd=0.5)+
    
    scale_fill_viridis(option="inferno","agriculture_expansion",direction = -1)+
      labs(
      title = "",
      x = "",
      y = "",
      
    )+
    # geom_sf_text(data = regions_pj,aes(label = AggrgtR),color="white", size = 2.5,) +
    # geom_sf_text(data = regions_pj,aes(label = AggrgtR),color="black", size = 2.2) +
    geom_text_repel(data = centroid,aes(x = X, y = Y, label = AggrgtR),
                    hjust = 0, nudge_x = 1, nudge_y = 4,
                    size = 2, color = "black", fontface = "bold",bg.color = "white",bg.r = 0.25)+
    theme_map()+
    theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme( legend.position = "none")
    l<- get_legend(map, position = NULL)
    
    maps[[i]] <- map
    dfs[[i]] <- zonal_result
}


# legenda

legenda <- expan_df%>%
  filter(!is.na(agriculture_expansion))%>%
  filter(agriculture_expansion>0.005)%>%
  ggplot()+
  geom_raster(aes(x = x,y = y,fill=agriculture_expansion))+
  # add hotspots limits
  scale_fill_viridis(option="inferno","Agri.exp.(%)",direction = -1)+
  theme( legend.position = "top")

l <- get_legend(legenda)

plot(l)

global_maps<- ggarrange(plotlist =  rev(maps),common.legend = F,labels = rev(c("Fr","Tr","Ta","ETL")))

global_maps2 <- ggarrange(l,global_maps,nrow = 2,heights = c(1,6))

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


# aqui mantem o nome do hotspot!
expansao_hotspots_2 <- expansao_hotspots%>%
  pivot_longer(cols = 1:4)%>%
  group_by(AggrgtR,NAME,name)%>%
  summarise(total_area=sum(value,na.rm = T))

write.csv(expansao_hotspots_2,"/dados/pessoal/francisco/TradeHub/output_tables/updated_results/overlap_hotspots_11_regions_agriexp.csv",row.names = F)


# add can==0
name <- unique(expansao_hotspots_sum[,c(2)])
AggrgtR <- "CAN"
can <- expand_grid(AggrgtR,name)
can$total_area <- 0

expansao_hotspots_sum <- rbind(expansao_hotspots_sum,can)
# ordenando

expansao_hotspots_sum$AggrgtR <- factor(expansao_hotspots_sum$AggrgtR , levels=(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))

expansao_hotspots_sum$name <- factor(expansao_hotspots_sum$name , levels=c("Fr","Tr","Ta","ETL"))

# ajustando escala

expansao_hotspots_sum$total_area <- expansao_hotspots_sum$total_area/10^6

# barplot
# padronizar cor regioes!!
hotspot_area <- ggplot(expansao_hotspots_sum, aes(x = name, y = total_area, fill = AggrgtR)) +
  geom_bar(stat = "identity") +
  #scale_fill_viridis_d(option = "inferno", direction = -1) +
  scale_fill_brewer(palette = "Set3", name="",direction=-1)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = expression(paste("Agricultural expansion area overlapping hotspots (", Km^2, " ", "x 10"^"-6", ")", sep = "")), fill = "") +
  coord_flip()


final <- ggarrange(global_maps2,hotspot_area,nrow = 2,heights = c(2,1))


ggsave(plot = final,filename="/dados/pessoal/francisco/TradeHub/figures_paper_new_versions/agri_expansion_hotspot_areas.jpg",width = 19,height = 24,units = "cm" )


regions_hotspotsdf <- sf::st_drop_geometry(regions_hotspots)


