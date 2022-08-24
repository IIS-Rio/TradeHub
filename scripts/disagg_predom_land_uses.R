
library(raster)
library(terra)

# defining path


dir.create("/dados/projetos_andamento/TRADEhub/trade_hub_plangea")


# IIASA scen

p <- "/dados/projetos_andamento/TRADEhub/GLOBIOM/atualizacao/scen_desagregados"

# ESA land-uses

p2 <- "/dados/projetos_andamento/TRADEhub/ESA_CCI_2015_dominant_land_uses"


dominant_land <- raster(list.files(p2,pattern = "tif",full.names = T))

plot(dominant_land)


# # change projection
# 
# crs <- "+proj=longlat +datum=WGS84 +no_defs "
# 
# dominant_land_latlong <- projectRaster(from = dominant_land,crs = crs)

###############################################################################
# separate land-cover
###############################################################################

# path to save 

p3 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/dominant_uses_disagg"

dir.create(p3)


# reclassify to forest = 3

m_f <- c(0, 2, 0,2,3,1,3,9,0)

rclmat_f <- matrix(m_f, ncol=3, byrow=TRUE)

forest <- reclassify(dominant_land, rclmat_f)



writeRaster(forest,filename = file.path(p3,"forest_ESA_CCI_2015.tif"),
            overwrite=T)



# reclassify to shrubland = 4

m <- c(0, 3, 0,3,4,1,4,9,0)

rclmat <- matrix(m, ncol=3, byrow=TRUE)

shrubland <- reclassify(dominant_land, rclmat)

writeRaster(shrubland,filename = file.path(p3,"shrubland_ESA_CCI_2015.tif"),
            overwrite=T)

# reclassify to wetlands

m_wet <- c(0, 4, 0,4,5,1,5,9,0)

rclmat_wet <- matrix(m_wet, ncol=3, byrow=TRUE)

wetland <- reclassify(dominant_land, rclmat_wet)

writeRaster(wetland,filename = file.path(p3,"wetland_ESA_CCI_2015.tif"),
            overwrite=T)

# reclassify to deserts

m_des <- c(0, 5, 0,5,6,1,6,9,0)

rclmat_des <- matrix(m_des, ncol=3, byrow=TRUE)

desert <- reclassify(dominant_land, rclmat_des)

writeRaster(desert,filename = file.path(p3,"desert_ESA_CCI_2015.tif"),
            overwrite=T)

# reclassify to ignored

m_ignored <- c(0, 7, 0,7,8,1,8,9,0)

rclmat_ignored <- matrix(m_ignored, ncol=3, byrow=TRUE)

ignored <- reclassify(dominant_land, rclmat_ignored)

#plot(ignored)

writeRaster(ignored,filename = file.path(p3,"urban_rock_ice_ESA_CCI_2015.tif"),
            overwrite=T)

# reclassify to natural grasslands

m_grass <- c(0, 6, 0,6,7,1,7,9,0)

rclmat_grass <- matrix(m_grass, ncol=3, byrow=TRUE)

grass <- reclassify(dominant_land, rclmat_grass)

writeRaster(grass,filename = file.path(p3,"natural_grasslands_ESA_CCI_2015.tif"),
            overwrite=T)

###############################################################################
# aggregate land-covers to 0.5
###############################################################################

# agregate function to get to 0.5 res

#res_iiasa <- 0.5


other_iiasa <- list.files(file.path(p,"other_7"),full.names = T)


scen_iiasa <- stack(other_iiasa[2])

scen_iiasa_proj <- projectRaster(scen_iiasa,crs ="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" )

ratio <- res(scen_iiasa_proj)/res(shrubland)

# listing raster files 

lu <- list.files(p3,full.names = T)


dir.create("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/dominant_use_fraction")

p4 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/dominant_use_fraction"

types <- c("desert","natural_grasslands","shrubland","urban_rock_ice","wetland","forest")

i=6

for(i in 1:length(lu))  {
  
  # opening raster
  
  r <- raster(lu[i])
  
  # ratio used to aggregate
  
  #ratio <- res(scen_iiasa_proj)/res(shrubland)
  
  # ressampe
  
  dominant_land_fraction <- resample(r,scen_iiasa_proj)
  
  # save to new folder
  
  writeRaster(dominant_land_fraction,filename = file.path(p4,paste0(types[i],
            "_dominant_landuse_fraction_2015_05res.tif")),overwrite=T)
  
}


# teste de soma 1

check_sum <- lapply(list.files(p4,full.names = T),stack)

check_sum_2 <- Reduce("+",check_sum)



