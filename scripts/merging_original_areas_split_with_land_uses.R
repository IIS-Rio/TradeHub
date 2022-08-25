
library(raster)

# depois de remover os usos ESA 1992 da classe other natural uses que ainda estava agregada, faltava juntar o resultado aos rasters finais de land uses

# caminho dos land-uses

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses"

# listagem das pastas com os land uses

scen <-list.files(file.path(p),full.names = T,recursive = T)

# esses sao os cenarios que escolhemos pra comecar

scen_to_keep <- c("TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2",
                  "TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2")

# por enquanto so tem os land uses pro cenario 3 do vetor acima. Depois teria q # fazer isso cenario por cenario

i=4
scen_subset <- grep(pattern = scen_to_keep[i],x = scen,value = T )

# pastas com usos

uses_folder <- c("desert","grassland","other_natland_disaggregated","pasture","restored_dissaggregated","rock_ice","shrubland","wetland")


lu <- c("desert","grassland","rock_ice","shrubland","wetland")


usos_2merge <- grep(pattern = paste(uses_folder,collapse="|"),x =scen_subset ,value = T)

# merge

for(k in 1:length(lu)){
  # criando pasta
  dir.create(path = file.path(p,paste0(lu[k],"_final")))
  # susbet com os rasters pra somar
  r <- grep(pattern = lu[k],x = usos_2merge,value = T)
  # criando lista de rasters
  r_open <- lapply(X = r,FUN = stack)
  r_sum <- Reduce("+",r_open)
  p_save <- file.path(p,paste0(lu[k],"_final/",fname,scen_to_keep[i],"-24May2022_FinalMask_55x55km_",lu[k],".tif"))
  writeRaster(r_sum,p_save,overwrite=T)

}
