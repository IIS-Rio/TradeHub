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


scen_to_keep <- c("TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2",
                  "TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2")

# 5 scenarios

scen_subset <- grep(pattern =paste(scen_to_keep,collapse = "|"),x = scen,value = T )


# folder to save the results

path_res <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses"

dir.create(path_res)

# scenario subset - beggining of the loop

i=1

urban_sub <- grep(urban,pattern = scen_subset[i],value = T) # here I can replace with an index

# opening rasters

urban_sub_r <- lapply(urban_sub,stack)

urban_final <- Reduce("+",urban_sub_r)

urban_final_pj <- projectRaster(urban_final,
      crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")

# plot(urban_final)

# folder for agriculture

dir.create(file.path(path_res,"urban"))

writeRaster(x = urban_final_pj,filename =file.path(path_res,"urban",
            paste0(scen[i],"_urban.tif")),overwrite=TRUE)
