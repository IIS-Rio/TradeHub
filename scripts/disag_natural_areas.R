# disagregating natural areas

# --- libraries ----------------------------------------------------------------

library(raster)

#-------------------------------------------------------------------------------

# defining path

p <- "/dados/projetos_andamento/TRADEhub/GLOBIOM/atualizacao/scen_desagregados"


# list of scenarios (20 in total)

scen <- gsub("_abn_cropland_2Gbioen_10.tif","",
             list.files(file.path(p,"abn_cropland_2Gbioen_10"),pattern = "55"))


# selecting 5 priority scenarios

## baseline_TRADE + baseline_BTC : TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2
## exacerbated trade liberalization + IAP_BTC: TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2
## frictions and reconfigurations + baseline_BTC: TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2
## frictions and reconfigurations + C_BTC: TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2
## baseline_TRADE + IAP_BTC: TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2

scen_to_keep <- c("TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                 "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2",
                 "TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                 "TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2",
                 "TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2")

# 5 scenarios

scen_subset <- grep(pattern =paste(scen_to_keep,collapse = "|"),x = scen,value = T )

# excluir o cenario piloto

scen_to_keep <- scen_to_keep[-4]

scen_subset <- scen_subset[-1]


#############################################################################
# separating water, ice and deserts from the class other
#############################################################################

# path for masks

p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/dominant_use_fraction"

for(i in 1:length(scen_subset)){

  path_res <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i])
  
  
  other <- stack(file.path(p,"other_7",paste0(scen_subset[i],"_other_7.tif"))) 
  
  # ice_rock (using predominant land-uses ESA-2015)
  
  ice_rock <- raster(file.path(p2,"urban_rock_ice_ESA_CCI_2015_dominant_landuse_fraction_05res.tif"))
  
  # isso aqui gera valores negativos
  
  other_pj <- projectRaster(from = other ,to = ice_rock )
  
  # Removendo negativos
  for (r in 1:5) {
    other_pj [[r]][other_pj [[r]] < 0] = 0
  }
  
  
  # masking  ice_rock_urban from all other bands (years)
  
  other_ice_rock <- other_pj * ice_rock
  
  dir.create(file.path(path_res,"/rock_ice"))
  
  writeRaster(other_ice_rock,file.path(path_res,"rock_ice",
                paste0(scen_subset[i],"_rock_ice.tif")),overwrite=T)
  
  #  deserts
  
  desert <- raster(file.path(p2,"desert_ESA_CCI_2015_dominant_landuse_fraction_05res.tif"))
  
  other_desert <- other_pj * desert
  
  
  dir.create(file.path(path_res,"desert"))
  
  writeRaster(x =other_desert ,filename = file.path(path_res,"desert",
              paste0(scen_subset[i],"_desert.tif")),overwrite=T)
  
  
  # natural_grasslands
  
  grasslands <- raster(file.path(p2,"natural_grasslands_ESA_CCI_2015_dominant_landuse_fraction_05res.tif"))
  
  other_grasslands <- other_pj * grasslands
  
  dir.create(file.path(path_res,"grassland"))
  
  writeRaster(x =other_grasslands ,filename = file.path(path_res,"grassland", 
            paste0(scen_subset[i],"_grassland.tif")),overwrite=T)
  
  # shrubland
  
  shrubland <- raster(file.path(p2,"shrubland_ESA_CCI_2015_dominant_landuse_fraction_05res.tif"))
  
  other_shrubland <- other_pj * shrubland
  
  dir.create(file.path(path_res,"shrubland"))
  
  writeRaster(x =other_shrubland ,filename = file.path(path_res,"shrubland", 
              paste0(scen_subset[i],"_shrubland.tif")),overwrite=T)
  
  
  # wetland
  
  wetland <- raster(file.path(p2,"wetland_ESA_CCI_2015_dominant_landuse_fraction_05res.tif"))
  
  other_wetland <- other_pj * wetland
  
  dir.create(file.path(path_res,"wetland"))
  
  writeRaster(x =other_wetland ,filename = file.path(path_res,"wetland", 
              paste0(scen_subset[i],"_wetland.tif")),overwrite=T)
  
    # oq nao desagregou, colocar como uma classe other
  
  
  disagg <- other_desert + other_grasslands+other_ice_rock+other_shrubland+other_wetland
  
  # gera valores negativos
  
  other_non_disagg <- other_pj - disagg
  
  
  for(j in 1:5){
    
    other_non_disagg[[j]][other_non_disagg[[j]]<0] <- 0
  }
  
  
  dir.create(file.path(path_res,"other"))
  
  writeRaster(x = other_non_disagg ,filename = file.path(path_res,"other",paste0(scen_subset[i],"_other.tif")),overwrite=T)


}
