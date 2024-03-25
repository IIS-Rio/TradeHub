# mapa de uso do solo pra 2020
# ler tudo, transformar em df e plotar.
# definir cores tb. Simplificar agrupando algumas classes
library(terra)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(maptools)
library(sf)

ls_lu <- list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use/",full.names = T)

# limites globo

data(wrld_simpl)

# excluding Antartica

wrld_simpl <- wrld_simpl[-which(wrld_simpl$NAME == "Antarctica"),]

# projetando

wrld_transf <- spTransform(wrld_simpl, crs(rast(ls_lu[3])))

regions <- st_read(file.path("/dados/pessoal/francisco/TradeHub/country_boundaries","world_11regions_boundaries.shp"))

regions_pj <-st_transform(regions,crs = crs(rast(ls_lu[3])))
regions_pj <- st_simplify(regions_pj)



# mapa com as % e figura com as areas restauradas e convertidas em cada cenarios


forest <- rast(ls_lu[3])
agriculture <- rast(ls_lu[1])
pasture <- rast(ls_lu[9])
ice_rocks_urban <- rast(ls_lu[5])+rast(ls_lu[6])+rast(ls_lu[11])
desert <- rast(ls_lu[2])
grassland <- rast(ls_lu[4])
shrubland <- rast(ls_lu[10])
wetland <- rast(ls_lu[12])
other_nat <- rast(ls_lu[7]) + rast(ls_lu[8])

# dfs

forest_df <- as.data.frame(forest,xy=T)%>%
  filter(sum!=0,
         !is.na(sum))

agriculture_df <- as.data.frame(agriculture,xy=T)%>%
  filter(sum!=0,
         !is.na(sum))

pasture_df <- as.data.frame(pasture,xy=T)%>%
  filter(sum!=0,
         !is.na(sum))



ignored_df <- as.data.frame(desert + ice_rocks_urban,xy=T)%>%
  filter(sum!=0,
         !is.na(sum))


grassland_df <- as.data.frame(grassland,xy=T)%>%
  filter(sum!=0,
         !is.na(sum))



shrubland_df <- as.data.frame(shrubland,xy=T)%>%
  filter(sum!=0,
         !is.na(sum))

wetland_df <- as.data.frame(wetland,xy=T)%>%
  filter(sum!=0,
         !is.na(sum))


remainin_nat <- as.data.frame(other_nat,xy=T)%>%
  filter(sum!=0,
         !is.na(sum))


plot_list <- list()


agriculture_plot <- ggplot(agriculture_df)+
  geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
  geom_raster(aes(x = x,y = y,fill=sum))+
  #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
  scale_fill_gradient(low = "lightgray", high = "orange")+
  theme_map()+
  labs(
    title = "",
    x = "",
    y = "",
    fill="agriculture (%)")+
  #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme(  legend.text = element_text(size=4.5),
          legend.title = element_text(size=4.5)) 



plot_list[[1]] <- agriculture_plot 

pasture_plot <- ggplot(pasture_df)+
  geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
  geom_raster(aes(x = x,y = y,fill=sum))+
  #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
  scale_fill_gradient(low = "lightgray", high = "red")+
  theme_map()+
  labs(
    title = "",
    x = "",
    y = "",
    fill="pastureland (%)")+
  #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme(  legend.text = element_text(size=4.5),
          legend.title = element_text(size=4.5)) 



plot_list[[2]] <- pasture_plot

forest_plot <- ggplot(forest_df)+
  geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
  geom_raster(aes(x = x,y = y,fill=sum))+
  #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
  scale_fill_gradient(low = "lightgray", high = "darkgreen")+
  theme_map()+
  labs(
    title = "",
    x = "",
    y = "",
    fill="forests (%)")+
  #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme(  legend.text = element_text(size=4.5),
          legend.title = element_text(size=4.5)) 



plot_list[[3]] <- forest_plot 



grassland_plot <- ggplot(grassland_df)+
  geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
  geom_raster(aes(x = x,y = y,fill=sum))+
  #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
  scale_fill_gradient(low = "lightgray", high = "lightgreen")+
  theme_map()+
  labs(
    title = "",
    x = "",
    y = "",
    fill="grasslands (%)")+
  #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme(  legend.text = element_text(size=4.5),
          legend.title = element_text(size=4.5)) 


plot_list[[4]] <- grassland_plot

shrubland_plot <- ggplot(shrubland_df)+
  geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
  geom_raster(aes(x = x,y = y,fill=sum))+
  #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
  scale_fill_gradient(low = "lightgray", high = "#CCCC00")+
  theme_map()+
  labs(
    title = "",
    x = "",
    y = "",
    fill="shrubland (%)")+
  #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme(  legend.text = element_text(size=4.5),
          legend.title = element_text(size=4.5)) 



plot_list[[5]] <- shrubland_plot 

wetland_plot <- ggplot(wetland_df)+
  geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
  geom_raster(aes(x = x,y = y,fill=sum))+
  #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
  scale_fill_gradient(low = "lightgray", high = "blue")+
  theme_map()+
  labs(
    title = "",
    x = "",
    y = "",
    fill="wetland (%)")+
  #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme(  legend.text = element_text(size=4.5),
          legend.title = element_text(size=4.5)) 



plot_list[[6]] <- wetland_plot 

otn_plot <- ggplot(remainin_nat)+
  geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
  geom_raster(aes(x = x,y = y,fill=sum))+
  #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
  scale_fill_gradient(low = "lightgray", high = "#00FfCC")+
  theme_map()+
  labs(
    title = "",
    x = "",
    y = "",
    fill="other nat. areas (%)")+
  #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme(  legend.text = element_text(size=4.5),
          legend.title = element_text(size=4.5)) 


plot_list[[7]] <- otn_plot


ignored_plot <- ggplot(ignored_df)+
  geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
  geom_raster(aes(x = x,y = y,fill=sum))+
  #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
  scale_fill_gradient(low = "lightgray", high = "darkgray")+
  theme_map()+
  labs(
    title = "",
    x = "",
    y = "",
    fill="ignored (%)")+
  #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme(  legend.text = element_text(size=4.5),
          legend.title = element_text(size=4.5)) 


plot_list[[8]] <- ignored_plot


pannel_lu <- ggarrange(plotlist = plot_list,labels = "AUTO",ncol = 2,nrow=4)


ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/lulc_2020.png",pannel_lu,scale = 1,width = 16,height = 20,units = "cm")
