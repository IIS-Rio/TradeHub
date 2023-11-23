# Required libraries -----------------------------------------------------------

library(terra)
library(dplyr)
library(tidyverse)
library(sf)
library(raster)
library(fasterize)

# Reading original zonings -----------------------------------------------------

ipcc_climatic = rast("bd_iis/IPCC/IPCC_Climate_Zones_Map_raster/ipcc_zones_2017.tif")
# adjust pj
rbase <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use/agriculture.tif")
rbase <- rbase/rbase
# ajustando projecao

ipcc_climatic_pj <- terra::project(ipcc_climatic,rbase,method="near")

writeRaster(ipcc_climatic_pj,"/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/ipcc_zones_2017.tif",overwrite=T)

cont_div = st_read("bd_iis/NatureMap/ContinentalDivision/continents_20210725.shp")%>%
  st_transform(crs(rbase))

cont_div$ID <- seq(1,5,1)

cont_div_df <- st_drop_geometry(cont_div)

write.csv(cont_div_df,"/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/continents_20210725.csv",row.names = F)

# rasterize
rbase <- raster(rbase)
cont_div <- fasterize(sf = cont_div,raster = rbase,field = "ID")

writeRaster(cont_div,"/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/continents_20210725.tif",overwrite=T)

geo_eco =st_read("bd_iis/GEZ/gez_2010_wgs84.shp") %>%
  st_transform(crs(rbase))

st_write(geo_eco,"/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/gez_2010.shp")


geo_eco_r <- fasterize(geo_eco,rbase,field="gez_code")

writeRaster(geo_eco_r,"/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/GlobalEcoZones.tif",overwrite=T)

geo_eco_df <- st_drop_geometry(geo_eco)

write.csv(geo_eco_df,"/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/gez_2010.csv",row.names = F)

