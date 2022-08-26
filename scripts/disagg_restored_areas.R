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

scen_to_keep <- c("TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",      "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2",          "TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",         "TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2",          "TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2")


# other natural lands

p4 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/other_natland"

#other <- list.files(p4,full.names = T)

#j=1
#c(scen_to_keep[i],"other_nadland")collapse = ",")
#agrep(other,pattern = paste(c(scen_to_keep[i],"other_natland"),collapse = ","))

usos <- c("desert","forest","natural_grasslands","shrubland","rock_ice","wetland")


i=4


# loop trough all the scenarios
for(i in 1:length(scen_to_keep)){
  scen <-  grep(restored_uses,pattern = scen_to_keep[i],value = T)
  # other_scen <- grep(other,pattern = scen_to_keep[i],value = T)
  # selec only other natver
  #other_nat_land <- grep(other_scen,pattern = "natland_other",value = T)
  # scenarios with restored
  r <- stack(scen)
  # scenarios with other natural uses
  #other_mascara <- stack(other_nat_land)
  # crossing restored with other
  #r_other <- other_mascara*r
  # saving other natural uses restored
  # save_other <- file.path(p3,paste0("TradeHubTrack1Prelim-LCproj-GLOBIOM-",scen_to_keep[i],"-24May2022_FinalMask_55x55km_restored_","other_natural_land",".tif"))
  # # write raster
  #writeRaster(r_other,filename = save_other,overwrite=T)
  # loop to mask using different natural formations!
  for(j in 1:length(natural_mask)){
    # natural formation
    mascara <- raster(natural_mask[j])
    # crossing scenario with natural formation
    r_m <- r * mascara
    # path
    save <- file.path(p3,paste0("TradeHubTrack1Prelim-LCproj-GLOBIOM-",scen_to_keep[i],"-24May2022_FinalMask_55x55km_restored_",usos[j],".tif"))
    # saving
    writeRaster(r_m,filename = save,overwrite=T)
    
    
    
  }
  
  

}


# cruzei so com other natural land (oq sobrou dpois de ter desagregado todas as outras classes) e com os dominant land uses de 2015. Faltaria os land uses de 1992 pro que ficar faltanto. Ou pelo menos criar a classe que ficou faltando.

# teria q somar e comparar os resultado. e pelo menos criar uma classe nao desagregado.


restored_sum <- lapply(list.files(p3,full.names = T),stack)

restored_sum_2 <- Reduce("+",restored_sum)

diif_restored <- r - restored_sum_2


# Removendo negativos
for (k in 1:5) {
  diif_restored[[k]][diif_restored[[k]] < 0] = 0
}

p4 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/restored_other_natland"

dir.create(p4)

save <- file.path(p4,paste0("TradeHubTrack1Prelim-LCproj-GLOBIOM-",scen_to_keep[i],"-24May2022_FinalMask_55x55km_restored_","other_natland",".tif"))
# saving
writeRaster(diif_restored,filename = save,overwrite=T)


# cruzando areas restauradas nao desagregadas ainda com os original land-uses
# ESA

# ESA 1992
esa_raster_file_paths = list.files("/dados/rawdata/land-use/past/", full.names = T)

# usos naturais

esa_raster_file_paths_nat <- grep(esa_raster_file_paths,pattern = paste(c("desert","forest","ice","shrubland","wetlands","NatGrass"),collapse = "|"),value = T)

esa_rasters = lapply(esa_raster_file_paths_nat, raster)

# Reamostrando ESA natural

esa_nat_res = lapply(esa_rasters, function(r){raster::resample(x = r, y = diif_restored)})

# Removendo negativos

for (r in 1:length(esa_nat_res)) {
  esa_nat_res[[r]][esa_nat_res[[r]] < 0] = 0
}

# Multiplicação do other restored por cada classe natural

other_nat_rest_mult = lapply(esa_nat_res, function(x){x * diif_restored})

files = gsub("(ESA_landuse_300m_1992_)(.*)(_media.*$)", "\\2", unlist(lapply(esa_nat_res, names)))

files[6] =  "grassland"

l=1

for(l in 1:length(other_nat_rest_mult)){
  save.path <- paste0(p3, "/TradeHubTrack1Prelim-LCproj-GLOBIOM-TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2-24May2022_FinalMask_55x55km_", files[l], "_other_restored.tif")
  writeRaster(other_nat_rest_mult[[l]], save.path, overwrite = T)
}

# finalmante, o restinho de restored que nao deu pra desagregar

all_restored <- list.files(p3,full.names = T)

all_restored_r <- lapply(all_restored,stack)

all_restored_sum <- Reduce("+",all_restored_r)

final_non_disagg <- r - all_restored_sum


# Removendo negativos

for (r in 1:5) {
  final_non_disagg[[r]][final_non_disagg[[r]] < 0] = 0
}



save.path <- paste0(p3, "/TradeHubTrack1Prelim-LCproj-GLOBIOM-TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2-24May2022_FinalMask_55x55km_","other_restored.tif")

writeRaster(final_non_disagg, save.path, overwrite = T)
