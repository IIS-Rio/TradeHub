# calcular as areas de expansão agricola em cada cenario, que vai servir pra plotar sobre os países

#---- pacotes ------------------------------------------------------------------

library(raster)

#-------------------------------------------------------------------------------


#--- gerando rasters de expansao agrícola -------------------------------------

# rasters area agricola 2050

p_land_2050 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050"

# cenarios rodados

scenarios_full <- grep(pattern = "SSP2",x = list.files(p_land_2050),value = T)

# cenarios so comercio

scenarios_trade <-  grep(pattern = "NOBIOD_NOTECH_NODEM_SPA0_SSP2",x =  scenarios_full,value = T)


# cenario BAU (usado pra comparacao de todas as metricas)

for(i in 1:length(scenarios_trade)){
  baseline_2020 <- raster(file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use","agriculture.tif"))
  
  
  scen_2050 <- raster(file.path(p_land_2050,scenarios_trade[i],"agriculture.tif"))
  
  # essa eh a diferenca; soh oq for maior que zero eh expansao. Teve tb reducao em alguns locais.
  
  dif <- scen_2050 - baseline_2020
  
  # SO EXPANSAO
  
  expan <-  reclassify(dif, cbind(-1, 0, 0))
  
  writeRaster(x = expan,filename = file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agricultural expansion",paste0(scenarios_trade[i],"_agricultural_expansion.tif")) ,overwrite=T)
}
