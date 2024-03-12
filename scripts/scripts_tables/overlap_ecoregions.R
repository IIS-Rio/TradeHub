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

# 11 regions

regions <- st_read(file.path("/dados/pessoal/francisco/TradeHub/country_boundaries","world_11regions_boundaries.shp"))

regions_pj <-st_transform(regions,crs = crs(r_m))
regions_pj <- st_simplify(regions_pj)

# # limites globo
# 
# data(wrld_simpl)
# 
# # excluding Antartica
# 
# wrld_simpl <- wrld_simpl[-which(wrld_simpl$NAME == "Antarctica"),]
# 
# # projetando
# 
# wrld_transf <- spTransform(wrld_simpl, crs(r_m))

# lendo dado ecorregioes

ec_r <- raster("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/ecoregions_2017_1000m_moll_resampled_50km.tif")

unique(ec_r)

ec_p <- rasterToPolygons(ec_r)

ec_p2 <- st_as_sf(ec_p)%>%
  group_by(ecoregions_2017_1000m_moll)%>%
  summarise()


names(ec_p2)[1] <- "BIOME_NUM"
ec_p2$BIOME_NAME <- as.character(ec_p2$BIOME_NAME)
ec <- st_read("/dados/bd_iis/ecoregions_esa_2017/Ecoregions2017_moll.shp")%>%
  st_transform(crs(r_m))

ec_df <- st_drop_geometry(ec)%>%
  group_by(BIOME_NAME,BIOME_NUM)%>%
  summarise(area=sum(SHAPE_AREA))

ec_df$BIOME_NUM <- as.character(ec_df$BIOME_NUM)
ec_p3 <- left_join(ec_p2,ec_df)

st_write(ec_p3,"/dados/projetos_andamento/TRADEhub/trade_hub_plangea/variables/ecoregions.shp")

# cruzar com as areas agricolas seguindo o mapa de hotspots!

ec_p3 <- st_read("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/variables/ecoregions.shp")[-1,]


# rasters dos cenarios

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agriculture_expansion_baseline_2050"


# filtrar so NOBIOD

agri_expan_files <-grep(pattern = "NOBIOD",x = list.files(p,pattern = "agricultural"),value = T)[-2]
pasture_expan_files <- grep(pattern = "NOBIOD",list.files(p,pattern = "pasture"),value = T)[-2]

# abrindor raster modelo

r_m <- raster(file.path(p,agri_expan_files[1]))

# convertendo soma em area

area_pixel <- 50100* 61800/10^6
dfs <- list()
scens <- c("Fr","Tr","Ta","ETL")
f <- function(x)sum(x,na.rm = T)

for (i in 1:length(agri_expan_files)){
  
  agri_expan_r <- raster(file.path(p,agri_expan_files[i]))
  past_expan_r <- raster(file.path(p,pasture_expan_files[i]))
  #somando os 2 usos
  soma <- agri_expan_r + past_expan_r
  soma_area <- soma*area_pixel
  zonal_result <-data.frame(scen=extract(soma_area, ec_p3, fun = sum))
  names(zonal_result) <- scens[i]
  dfs[[i]] <- zonal_result
  # df pra plotar
  expan_df <- as.data.frame(soma, xy=TRUE)
  names(expan_df)[3] <- "agriculture_expansion"
  dfs[[i]] <- zonal_result
}


# juntando tudo

expansao_ecorregioes <- do.call(cbind,dfs)


# usando os shapes nem vale a pena pq sao so 3 biomas q sao desmatados!