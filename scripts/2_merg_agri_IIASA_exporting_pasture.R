# Merging agricultural land-uses from IIASA scenarios, exporting pasturelands

# --- libraries ----------------------------------------------------------------

library(raster)

#-------------------------------------------------------------------------------

################################################################################
# croplands = agriculture
################################################################################

# listing files with cropland


p <- "/dados/projetos_andamento/TRADEhub/GLOBIOM/atualizacao/scen_desagregados"


agri <- list.files(path = p,pattern = "crop*",recursive = T,full.names = T)


# list of scenarios (20 in total)

scen <- gsub("_abn_cropland_2Gbioen_10.tif","",
        list.files(file.path(p,"abn_cropland_2Gbioen_10"),pattern = "55"))


################################################################################

# *** checar se o vetor scen_to_keep e scen_subset estão na mesma ordem! ******

################################################################################


scen_to_keep <- c("TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2" ,
                  "TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2",
                  "TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2")


# 5 scenarios

scen_subset <- grep(pattern =paste(scen_to_keep,collapse = "|"),x = scen,value = T )

# como ja terminei o cen "TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2" exclui ele do loop

scen_to_keep <- scen_to_keep[c(-1,-4)]

scen_subset <- scen_subset[c(-1,-4)]

for(i in 1:length(scen_subset)){

  # folder to save the results
  
  path_res <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i])
  
  # dir.create(path_res)
  
  # folder for agriculture
  
  dir.create(file.path(path_res,"agriculture"))
  
  # scenario subset - beggining of the loop
  
  agri_sub <- grep(agri,pattern = scen_subset[i],value = T) 
  
  # opening rasters
  
  agri_sub_r <- lapply(agri_sub,stack)
  
  # sum agricultural uses!
  
  agriculture <- Reduce("+",agri_sub_r)
  
  crs  <-  "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
  
  agriculture_pj <- projectRaster( agriculture,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")
  
    # saving with scenario name
  
  writeRaster(x = agriculture_pj,filename =file.path(path_res,"agriculture",paste0(scen_subset[i],"_agriculture.tif")),overwrite=TRUE)
  
  # pastureland
  
  
  pasture <- list.files(path = p,pattern = "grass*",recursive = T,full.names = T)
  
  # inicio do loop
  
  pasture_sub <- grep(pasture,pattern = scen_subset[i],value = T) 
  
  # opening rasters
  
  pasture_sub_r <- lapply(pasture_sub,stack)
  
  # sum agricultural uses!
  
  pasture <- Reduce("+",pasture_sub_r)
  
  pasture_pj <- projectRaster( pasture,crs = crs)
  
  # folder for pastureland
  
  dir.create(file.path(path_res,"pasture"))
  
  writeRaster(x = pasture_pj,filename =file.path(path_res,"pasture",
    paste0(scen_subset[i],"_pasture.tif")),overwrite=TRUE)

}
