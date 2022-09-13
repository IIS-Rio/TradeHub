# Desambiguando 'other natural lands'

library(raster)


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

scen_to_keep <- c("TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2" ,
                  "TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2",
                  "TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2")

# 5 scenarios

scen_subset <- grep(pattern =paste(scen_to_keep,collapse = "|"),x = scen,value = T )

# excluir o cenario piloto

scen_to_keep <- scen_to_keep[c(-1,-4)]

scen_subset <- scen_subset[c(-1,-4)]


# ESA 1992
esa_raster_file_paths = list.files("/dados/rawdata/land-use/past/", full.names = T)

#---- usos naturais  ESA -------------------------------------------------------

# desmarcar essa parte caso precise gerar de novo os usos esa na mesma projecao
# dos cenarios

# esa_raster_file_paths_nat <- grep(esa_raster_file_paths,pattern = paste(c("desert","forest","ice","shrubland","wetlands","NatGrass"),collapse = "|"),value = T)
# 
# esa_rasters = lapply(esa_raster_file_paths_nat, raster)
# 
# # Teste soma um
# 
# esa_sum = Reduce('+', esa_rasters)

# projetando 

# raster com res correta (pode ser qqer um ja reprojetado)

# ref <- raster(list.files(file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i],"agriculture"),full.names = T))
# 
# esa_nat_res = lapply(esa_rasters, function(r){raster::resample(x = r, y = ref)})
# 
# # Removendo negativos
# 
# for (r in 1:length(esa_nat_res)) {
#   
#   esa_nat_res[[r]][esa_nat_res[[r]] < 0] = 0
# 
#   }
# 
# # salvando pra usar outras vezes!
# 
# dir.create(file.path("/dados/projetos_andamento/TRADEhub","ESA_CCI_1992_original"))
# 
# save_esa <- paste(c("desert","forest","ice","shrubland","wetlands","NatGrass"))
# 
# for(n in 1:length(esa_nat_res)){
# 
#   writeRaster(esa_nat_res[[n]],file.path("/dados/projetos_andamento/TRADEhub","ESA_CCI_1992_original",paste0("ESA_landuse_1992_",save_esa[n],"_IIASApj_Molweide.tif")))
# 
#   }

esa_nat_res <- lapply(list.files("/dados/projetos_andamento/TRADEhub/ESA_CCI_1992_original",full.names = T),raster)



files = gsub("(ESA_landuse_1992_)(.*)(_IIASApj.*$)", "\\2", unlist(lapply(esa_nat_res, names)))

files[4] =  "grassland"


for (i in 1:length(scen_subset)){

  other_nat_ssp2 = stack(list.files(file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i],"other"),full.names = T))
  
  
  # Multiplicação do other por cada classe natural
  
  other_nat_ssp2_mult = lapply(esa_nat_res, function(r){r * other_nat_ssp2})
  
  other_2050_mult_sum = Reduce('+', other_nat_ssp2_mult)
  
  other_non_disagg <- other_nat_ssp2 -  other_2050_mult_sum
  
  # Removendo negativos
  for (r in 1:5) {
    other_non_disagg[[r]][other_non_disagg[[r]] < 0] = 0
  }
  
  
  dir = file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i],"other_natland_disagg")
  
  dir.create(dir)
  
  
  for(r in 1:length(other_nat_ssp2_mult)){
    save.path <- file.path(dir, paste0(scen_subset[i],"_",files[r], "_other.tif"))
    writeRaster(other_nat_ssp2_mult[[r]], save.path, overwrite = T)
  }
  
  writeRaster(other_non_disagg, paste0(dir,"/" ,scen_subset[i], "_other_natland.tif" ))

}
