# Disaggeregating restored areas into the different natural land-uses

# --- libraries ----------------------------------------------------------------

library(raster)


#-------------------------------------------------------------------------------

# path to natural land uses

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/dominant_use_fraction"


# path to restored land uses

p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/restored"

# land use masks
natural_mask <- list.files(path = p,full.names = T)

# scenarios with restored classes disaggregated
restored_uses <-  list.files(path = p2,full.names = T)

# creating new folder to save the outputs

p3 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/restored_disaggregated"

dir.create(p3)



# list of scenarios to start

scen_to_keep <- c("TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2",
                  "TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2")


# other natural lands

p4 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/other_natural_land"

other <- list.files(p4,full.names = T)






i=4
#j=1

# loop trough all the scenarios
for(i in 1:length(scen_to_keep)){
  scen <-  grep(restored_uses,pattern = scen_to_keep[i],value = T)
  other_scen <- grep(other,pattern = scen_to_keep[i],value = T)
  usos <- c("desert","forest","natural_grasslands","shrubland","rock_ice","wetland")
  # scenarios with restored
  r <- stack(scen)
  # scenarios with other natural uses
  other_mascara <- stack(other_scen)
  # crossing restored with other
  r_other <- other_mascara*r
  # saving other natural uses restored
  save_other <- file.path(p3,paste0(scen_to_keep[i],"-24May2022_FinalMask_55x55km_restored_","other_natural_land",".tif"))
  # write raster
  writeRaster(r_other,filename = save_other,overwrite=T)
  # loop to mask using different natural formations!
  for(j in 1:length(natural_mask)){
    # natural formation
    mascara <- raster(natural_mask[j])
    # crossing scenario with natural formation
    r_m <- r * mascara
    # path
    save <- file.path(p3,paste0(scen_to_keep[i],"-24May2022_FinalMask_55x55km_restored_",usos[j],".tif"))
    # saving
    writeRaster(r_m,filename = save,overwrite=T)
    
    
    
  }
  

}


