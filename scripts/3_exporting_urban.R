# exporting urban land-use from IIASA scenarios
# --- libraries ----------------------------------------------------------------

library(raster)

#-------------------------------------------------------------------------------

################################################################################
# croplands = agriculture
################################################################################

# listing files with cropland


p <- "/dados/projetos_andamento/TRADEhub/GLOBIOM/atualizacao/scen_desagregados"


urban <- list.files(path = p,pattern = "built*",recursive = T,full.names = T)


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
  
  path_res <-file.path( "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i])
  
  # dir.create(path_res)-- ja criei no script exporting nat_areas.r
  
  urban_sub <- grep(urban,pattern = scen_subset[i],value = T) 
  
  # opening rasters
  
  urban_sub_r <- lapply(urban_sub,stack)
  
  urban_final_pj <- projectRaster(urban_sub_r[[1]],
        crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")
  
  # folder for urban
  
  dir.create(file.path(path_res,"urban"))
  
  writeRaster(x = urban_final_pj,filename =file.path(path_res,"urban",
              paste0(scen_subset[i],"_urban.tif")),overwrite=TRUE)

}
