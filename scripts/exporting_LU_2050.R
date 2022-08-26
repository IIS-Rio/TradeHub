library(raster)


# criando os rasters de lu pro plangea, pra 2050

# caminho land uses

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses"


# listando pastas com usos finais (descarta usos agregados que foram desagregados; ex: restored e other)

l_dir <- list.files(p)[c(1:5,7,9,11:14)]

l_r <- list.files(file.path(p,l_dir),recursive = T,full.names = T)

# listando raster so com nomes

l_r_names <- tools::file_path_sans_ext(list.files(file.path(p,l_dir),recursive = T))


# exportando ano 2050

p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea"

dir.create(file.path(p2,"land_uses_2050"))


# criando pasta pra cada cenario

dir.create(file.path(p2,"land_uses_2050","TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2"))


p3 <- file.path(p2,"land_uses_2050","TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2")

i=1

for( i in 1:length(l_r)){
  # abrindo raster
  r <- raster(l_r[i],band=5)
  # criando diretorio
  #dir.create(file.path(p2,"land_uses_2050",l_r_lu[i]))
  save_path <- file.path(p3,paste0(basename(l_r_names[i]),"_2050.tif"))
  writeRaster(r,filename = save_path)
  
}


# testando soma 1

soma <- lapply(list.files(p3,full.names = T),raster)
soma_reduced <- Reduce("+",soma)

plot(soma_reduced)

summary(soma_reduced[])

# salvando ultimo layer com fracoes que serao ignoradas

ignored <- 1- soma_reduced


# removendo valor negativo


ignored[ignored<0] <- 0


writeRaster(ignored,filename = file.path(p3,"TradeHubTrack1Prelim-LCproj-GLOBIOM-TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2-24May2022_FinalMask_55x55km_ignored_2050.tif"))


# rename_p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses_backup/other_natland_disaggregated"
# 
# file.rename(from = list.files(rename_p,full.names = T),paste0(tools::file_path_sans_ext(list.files(rename_p,full.names = T)),"_other.tif"))





