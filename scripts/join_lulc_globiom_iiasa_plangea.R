# Simplificando LULC TradeHub
library(terra)

lu_orig = list.files(path = "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses_2050/TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2/", pattern = ".tif$", full.names = T)

#sum(rast(lu_orig))

lulc_classes = c("agriculture","desert","forest","grassland","ice","ignored","other_natland","km_other_restored","pasture", "shrubland", "wetland", "urban")

for (class in lulc_classes) {
  lulc_rasters = grep(x = lu_orig, pattern = class, value = T)
  #print(class)
  #print(lulc_rasters)
  sum_class = sum(rast(lulc_rasters))
  terra::writeRaster(sum_class, paste0("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/", class, ".tif"), overwrite = T)
  #lu_orig = lu_orig[-lulc_rasters]
}

# Teste soma 1
sum_lu_novo = sum(rast(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/", full.names = T)))

