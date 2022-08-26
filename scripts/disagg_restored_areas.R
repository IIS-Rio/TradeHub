# Disaggeregating restored areas into the different natural land-uses

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



p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/dominant_use_fraction"

# land use masks

natural_mask <- list.files(path = p2,full.names = T)

usos <- c("desert","forest","natural_grasslands","shrubland","rock_ice","wetland")

# ESA 1992
esa_raster_file_paths = list.files("/dados/rawdata/land-use/past/", full.names = T)

# usos naturais

esa_raster_file_paths_nat <- grep(esa_raster_file_paths,pattern = paste(c("desert","forest","ice","shrubland","wetlands","NatGrass"),collapse = "|"),value = T)

esa_rasters = lapply(esa_raster_file_paths_nat, raster)

# Reamostrando ESA natural (aqui tb tem q ajustar o raster de referencia, pq ele so abre dentro do loop)

esa_nat_res = lapply(esa_rasters, function(x){raster::resample(x = x, y = r)})

# Removendo negativos

for (z in 1:length(esa_nat_res)) {
  esa_nat_res[[z]][esa_nat_res[[z]] < 0] = 0
}


files = gsub("(ESA_landuse_300m_1992_)(.*)(_media.*$)", "\\2", unlist(lapply(esa_nat_res, names)))

files[6] =  "grassland"


for(i in 1:length(scen_subset)){
  # path to restored land uses

  p3 <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i],"restored")
  
  # scenarios with restored classes disaggregated
  
  restored_uses <-  list.files(path = p3,full.names = T)
  
  # creating new folder to save the outputs
  
  p4 <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i],"restored_disaggregated")
  
  dir.create(p4)


  # # other natural lands
  # 
  # p5 <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i],"other")
  r <- stack(restored_uses)
  # loop to mask using different natural formations!
  for(j in 1:length(natural_mask)){
    # natural formation
    mascara <- raster(natural_mask[j])
    # crossing scenario with natural formation
    r_m <- r * mascara
    # path
    save <- file.path(p4,paste0(scen_subset[i],"_restored_",usos[j],".tif"))
    # saving
    writeRaster(r_m,filename = save,overwrite=T)
    
    
    
  }
  # atr aqui ok!
  
  restored_sum <- lapply(list.files(p4,full.names = T),stack)
  
  restored_sum_2 <- Reduce("+",restored_sum)
  
  diif_restored <- r - restored_sum_2
  
  
  # Removendo negativos
  for (k in 1:5) {
    diif_restored[[k]][diif_restored[[k]] < 0] = 0
  }
  
  p5 <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i],"restored_other_natland")
  
  dir.create(p5)
  
  save2 <- file.path(p5,paste0(scen_subset[i],"_restored_","other_natland",".tif"))
  
  # saving
  writeRaster(diif_restored,filename = save2,overwrite=T)


# cruzando areas restauradas nao desagregadas ainda com os original land-uses
# ESA


# Multiplicação do other restored por cada classe natural

  other_nat_rest_mult = lapply(esa_nat_res, function(x){x * diif_restored})

  for(l in 1:length(other_nat_rest_mult)){
    save.path <- paste0(p4,"/",scen_subset[i],"_" ,files[l], "_other_restored.tif")
    writeRaster(other_nat_rest_mult[[l]], save.path, overwrite = T)
  }

# finalmante, o restinho de restored que nao deu pra desagregar

  all_restored <- list.files(p4,full.names = T)
  
  all_restored_r <- lapply(all_restored,stack)
  
  all_restored_sum <- Reduce("+",all_restored_r)
  
  final_non_disagg <- r - all_restored_sum


  # Removendo negativos
  
  for (r in 1:5) {
    final_non_disagg[[r]][final_non_disagg[[r]] < 0] = 0
  }



  save.path <- paste0(p4,"/",scen_subset[i],"_" ,"other_restored.tif")
  
  writeRaster(final_non_disagg, save.path, overwrite = T)

}
