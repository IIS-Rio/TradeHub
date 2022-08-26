# Merging diferent classes of forest, exporting other natural areas

# --- libraries ----------------------------------------------------------------

library(raster)

#-------------------------------------------------------------------------------


################################################################################
# listing files with forest
################################################################################

p <- "/dados/projetos_andamento/TRADEhub/GLOBIOM/atualizacao/scen_desagregados"

# I combined managed forests into forests!

# forest
forest_files <- list.files(path = p,pattern = "forest*",recursive = T,full.names = T)

# restored areas

rest <- list.files(path = p,pattern = "restored*",recursive = T,full.names = T)


# list of scenarios (20 in total)

scen <- gsub("_abn_cropland_2Gbioen_10.tif","",
             list.files(file.path(p,"abn_cropland_2Gbioen_10"),pattern = "55"))

# 4 ja foi, faltam os outros!

scen_to_keep <- c("TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2",
                  "TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2")

# excluir o 4 que ja foi
scen_to_keep <- scen_to_keep[-4]

# 5 scenarios

scen_subset <- grep(pattern =paste(scen_to_keep,collapse = "|"),x = scen,value = T )

# exclude i=1 (ja rodei tudo como piloto)

scen_subset <- scen_subset[-1]

# folder to save the results

for(i in 1:length(scen_subset)){
  path_res <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i])
  
  dir.create(path_res)
  
  # folder for forests
  
  dir.create(file.path(path_res,"forest"))
  
  forest_sub <- grep(forest_files,pattern = scen_to_keep[i],value = T) 
  
  # opening rasters
  
  forest_sub_r <- lapply(forest_sub,stack)
  
  # sum forests!
  
  forests <- Reduce("+",forest_sub_r)
  
  pj = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
  
  forests_pj <- projectRaster(forests,crs=pj)
  
  # saving with scenario name
  
  writeRaster(x = forests_pj,filename =file.path(path_res,"forest",paste0(scen_subset[i],"_forest_managedANDunmanaged.tif")),overwrite=TRUE)
  
  
  # folder for restored
  
  dir.create(file.path(path_res,"restored"))
  
  # scenario subset restored areas
  
  rest_sub <- grep(rest,pattern = scen_subset[i],value = T) 
  
  # opening rasters
  
  rest_sub_r <- lapply(rest_sub,stack)
  
  # sum 
  
  restored <- Reduce("+",rest_sub_r)
  
  restored_pj <- projectRaster(restored,crs=pj)
  
  writeRaster(x = restored_pj,filename =file.path(path_res,"restored",paste0(scen_subset[i],"_restored.tif")),overwrite=TRUE)
  
  
  }







