# Disaggeregating restored areas into the different natural land-uses

# --- libraries ----------------------------------------------------------------

library(raster)


#-------------------------------------------------------------------------------

# defining path

p <- "/dados/projetos_andamento/TRADEhub/GLOBIOM/atualizacao/scen_desagregados"


# list of scenarios (20 in total)

scen <- gsub("_abn_cropland_2Gbioen_10.tif","",
             list.files(file.path(p,"abn_cropland_2Gbioen_10"),pattern = "55"))


################################################################################

# *** checar se o vetor scen_to_keep e scen_subset estão na mesma ordem! ******

################################################################################


# scen_to_keep <- c("TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2" ,
#                   "TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
#                   "TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2",
#                   "TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
#                   "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2")

# cenarios com comercio e baseline conservacao que faltam


scen_to_keep <- c("TH_TFBASE_TCREDU_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFELIM_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",                                      "TH_TFELIM_TCREDU_NOBIOD_NOTECH_NODEM_SPA0_SSP2")

scen_subset <- grep(pattern =paste(scen_to_keep,collapse = "|"),x = scen,value = T )


p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/dominant_use_fraction"

# land use masks

natural_mask <- list.files(path = p2,full.names = T)

usos <- c("desert","forest","natural_grasslands","shrubland","rock_ice","wetland")

# ESA 1992

esa_nat_res <- lapply(list.files("/dados/projetos_andamento/TRADEhub/ESA_CCI_1992_original",full.names = T),raster)


files = gsub("(ESA_landuse_1992_)(.*)(_IIASApj.*$)", "\\2", unlist(lapply(esa_nat_res, names)))

files[4] =  "grassland"


for(i in 1:length(scen_subset)){
  # path to restored land uses

  p3 <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i],"restored")
  
  # restored class aggregated
  
  restored_uses <-  list.files(path = p3,full.names = T)
  
  # creating new folder to save the outputs
  
  p4 <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i],"restored_disaggregated")
  
  dir.create(p4)


  # # other natural lands
  
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
