# comparando pixeis de expansao agricola entre cenarios, de forma binaria: teve
# ou nao expansao. fazer esse calculo em relacao ao baseline 2050. Sempre a diferenca entre um dado cenario e o baseline.


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

scenarios_conservation <- grep(pattern = "NOBIOD_NOTECH_NODEM_SPA0_SSP2",x =  scenarios_full,value = T,invert = T)

# cenario BAU (usado pra comparacao de todas as metricas)

baseline_2050 <- raster(file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050","TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2","agriculture.tif"))

# selecionar apenas pixels > 30%
# fica ruim assim, praticamente nao aparecem diferenças

# m <- c(0, 0.3, 0,  0.3, 1, 1)
# 
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# 
# # reclassificando
# 
# baseline_2050_rc <- reclassify(baseline_2050,rclmat)

agri_exp <- function(lista_scen){
  
  

for(i in 1:length(lista_scen)){
  
  
  
  scen_2050 <- raster(file.path(p_land_2050,lista_scen[i],"agriculture.tif"))
  
  # selecionar apenas pixels > 30%
  
  #scen_2050_rc <-  reclassify(scen_2050,rclmat)
  
  
  # essa eh a diferenca; soh oq for maior que zero eh expansao. Teve tb reducao em alguns locais.
  
  dif <- scen_2050 - baseline_2050
  
  # SO EXPANSAO
  
  expan <-  reclassify(dif, cbind(-1.1, 0.9, NA))
  
  writeRaster(x = expan,filename = file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agriculture_expansion_baseline_2050",paste0(lista_scen[i],"_agricultural_expansion_baseline_2050.tif")) ,overwrite=T)
}
  
}  


# calculando pra trade

agri_exp(lista_scen = scenarios_trade)


# calculando pra conservation

agri_exp(lista_scen = scenarios_conservation)


# ------------------------------------------------------------------------------

# calculando expansao de pastagem

# cenario BAU (usado pra comparacao de todas as metricas)

baseline_2050 <- raster(file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050","TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2","pasture.tif"))

pasture_expansion <- function(lista_scen){

for(i in 1:length(lista_scen)){
  
  scen_2050 <- raster(file.path(p_land_2050,lista_scen[i],"pasture.tif"))
  
  # essa eh a diferenca; soh oq for maior que zero eh expansao. Teve tb reducao em alguns locais.
  
  dif <- scen_2050 - baseline_2050
  
  # SO EXPANSAO
  
  expan <-  reclassify(dif, cbind(-1, 0, 0))
  
  writeRaster(x = expan,filename = file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agriculture_expansion_baseline_2050",paste0(lista_scen[i],"_pasture_expansion_baseline_2050.tif")) ,overwrite=F)

  }
}

pasture_expansion(lista_scen = scenarios_conservation)
