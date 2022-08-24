# Merging diferent classes of forest, exporting other natural areas

# --- libraries ----------------------------------------------------------------

library(raster)

#-------------------------------------------------------------------------------


################################################################################
# listing files with forest
################################################################################

p <- "/dados/projetos_andamento/TRADEhub/GLOBIOM/atualizacao/scen_desagregados"

# I combined managed forests into forests!

forest_files <- list.files(path = p,pattern = "forest*",recursive = T,full.names = T)


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

# folder for forests

dir.create(file.path(path_res,"forest"))


# scenario subset

# begining of the loop!

i=1

forest_sub <- grep(forest_files,pattern = scen_subset[i],value = T) # here I can replace with an index

# opening rasters

forest_sub_r <- lapply(forest_sub,stack)

# sum forests!

forests <- Reduce("+",forest_sub_r)

pj = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "

forests_pj <- projectRaster(forests,crs=pj        )


# saving with scenario name

writeRaster(x = forests_pj,filename =file.path(path_res,"forest",
    paste0(scen_subset[i],"_forest_managedANDunmanaged.tif")),overwrite=TRUE)


################################################################################
# listing files with restored (can be any natural area)
################################################################################


rest <- list.files(path = p,pattern = "restored*",recursive = T,full.names = T)

# scenario subset

rest_sub <- grep(rest,pattern = scen_subset[i],value = T) # here I can replace with an index


# opening rasters

rest_sub_r <- lapply(rest_sub,stack)

# sum forests!

restored <- Reduce("+",rest_sub_r)


restored_pj <- projectRaster(restored,crs=pj)


# plot(restored) # apagar!!

# o restored mistura outras classes que nao apenas floresta. teria q separar e
# adicionar as outras classes corretamente!

# folder for forests

dir.create(file.path(path_res,"restored"))


writeRaster(x = restored_pj,filename =file.path(path_res,"restored",
                    paste0(scen[1],"_restored.tif")),overwrite=TRUE)


