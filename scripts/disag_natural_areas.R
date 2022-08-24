# disagregating natural areas

# --- libraries ----------------------------------------------------------------

library(raster)
library(dplyr)

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


# folder to save the results

path_res <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses"


dir.create(path_res)

#############################################################################
# separating water, ice and deserts from the class other
#############################################################################

# path for masks

p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/dominant_use_fraction"


# start function here

i=1

other <- stack(file.path(p,"other_7",paste0(scen_subset[i],"_other_7.tif"))) # index here



# ice_rock (using predominant land-uses ESA-2015)

ice_rock <- raster(file.path(p2,"urban_rock_ice_dominant_landuse_fraction_2015_05res.tif"))

other_pj <- projectRaster(from = other ,to = ice_rock )


# # padronizing extent
# 
# ice_rock_res <- resample(ice_rock,other,"ngb")


# masking  ice_rock_urban from all other bands (years)

other_ice_rock <- other_pj * ice_rock


dir.create(file.path(path_res,"/rock_ice"))

writeRaster(other_ice_rock,file.path(path_res,"rock_ice",
              paste0(scen_subset[1],"_rock_ice.tif")))

#  deserts

desert <- raster(file.path(p2,"desert_dominant_landuse_fraction_2015_05res.tif"))

other_desert <- other_pj * desert


dir.create(file.path(path_res,"desert"))

writeRaster(x =other_desert ,filename = file.path(path_res,"desert",
            paste0(scen_subset[1],"_desert.tif")),overwrite=T)


# natural_grasslands

grasslands <- raster(file.path(p2,"natural_grasslands_dominant_landuse_fraction_2015_05res.tif"))

other_grasslands <- other_pj * grasslands

dir.create(file.path(path_res,"grassland"))

writeRaster(x =other_grasslands ,filename = file.path(path_res,"grassland", 
          paste0(scen_subset[1],"_grassland.tif")),overwrite=T)

# shrubland

shrubland <- raster(file.path(p2,"shrubland_dominant_landuse_fraction_2015_05res.tif"))

other_shrubland <- other_pj * shrubland

dir.create(file.path(path_res,"shrubland"))

writeRaster(x =other_shrubland ,filename = file.path(path_res,"shrubland", 
            paste0(scen_subset[1],"_shrubland.tif")),overwrite=T)


# wetland

wetland <- raster(file.path(p2,"wetland_dominant_landuse_fraction_2015_05res.tif"))

other_wetland <- other_pj * wetland

dir.create(file.path(path_res,"wetland"))

writeRaster(x =other_wetland ,filename = file.path(path_res,"wetland", 
            paste0(scen_subset[1],"_wetland.tif")),overwrite=T)


# # all that's left inside the other original class!
# 
# # creating a class  with the disaggregated uses! 
# 
# wt_grass_shrub_desert_ignored <- other_desert+other_grasslands+ other_shrubland +
#   other_wetland+other_minus_ice_rock_urban
# 
# # transform pixels > 0 in 0 and pixels with value 0 in one. Then, when crossing
# # with the original other class, only uncovered pixels will remain!
# 
# 
# m_other <- c(-1, 0, 1,0,2,0)
# 
# rclmat_other <- matrix(m_other, ncol=3, byrow=TRUE)
# 
# wt_grass_shrub_desert_ignored_rec <- reclassify(wt_grass_shrub_desert_ignored, rclmat_other)
# 
# plot(wt_grass_shrub_desert_ignored_rec[[1]])
# 
# other_final <- wt_grass_shrub_desert_ignored_rec*other
# 
# dir.create(file.path(path_res,"other_natural_lands"))
# 
# writeRaster(x =other_final  ,filename = file.path(path_res,"other_natural_lands", 
#           paste0(scen_subset[1],"_other_natural_lands.tif")),overwrite=T)


# plot(other[[1]],main="other")
# plot(other_final[[1]],main="other_final")
# plot(wt_grass_shrub_desert_ignored[[1]],main="natural_lands_disagg")
# plot(other_grasslands[[1]],main="grassland")
# essa classe deveria ser valores q nao foram incluidos em nenhuma outra


################################################################################
#### OBS
################################################################################
# tem pequenas sobreposicoes! grassland com ignored. Mas melhor deixar pra 
# corrigir isso depois!
# desert_grass <- other_desert+other_grasslands #ok
# 
# grass_shrubland <- other_grasslands+other_shrubland # ok!!
# 
# grass_wer <- other_wetland+other_grasslands #ok
# 
# 
# grass_ignored <- other_minus_ice_rock_urban+other_grasslands # esse q da bosta
# 
# 
# mask_overlap
# 
# 
# plot(grass_ignored[[1]]>1)
# 
# 
# dir.create(file.path(path_res,"temp_apagar"))
# 
# writeRaster(x =grass_ignored ,filename = file.path(path_res,"temp_apagar", 
#                   paste0(scen_subset[1],"_shrubland.tif")),overwrite=T)
# 

# acho q uma ideia eh zerar a classe ignored nesses pixeis! ou substituir por 
# valores que deem zero





