# Simplificando LULC TradeHub
library(terra)

# 2050 -------------------------------------------------------------------------

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

# 2020 -------------------------------------------------------------------------

lu_orig = list.files(path = "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses_2020/TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2/", pattern = ".tif$", full.names = T)

sum(rast(lu_orig))

lulc_classes = c("agriculture","desert","forest","grassland","ice","ignored","other_natland","km_other_restored","pasture", "shrubland", "wetland", "urban")

for (class in lulc_classes) {
  lulc_rasters = grep(x = lu_orig, pattern = class, value = T)
  #print(class)
  #print(lulc_rasters)
  sum_class = sum(rast(lulc_rasters))
  terra::writeRaster(sum_class, paste0("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use/", class, ".tif"), overwrite = T)
  #lu_orig = lu_orig[-lulc_rasters]
}

# Teste soma 1
sum_lu_novo = sum(rast(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use/", full.names = T)))

# 2015 -------------------------------------------------------------------------

lu_orig = rast(list.files(path = "/dados/rawdata-legacy/900m/current_LU/", pattern = "_2015_", full.names = T))

#lu_orig = lu_orig[[!grepl(pattern = "ESA", x = names(lu_orig))]]

base_ras = rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/agriculture.tif")

for (iter_ras in 1:nlyr(lu_orig)) {
  ras_orig = lu_orig[[iter_ras]]
  lu_res = terra::resample(x = ras_orig, y = base_ras, method = 'bilinear')
  writeRaster(lu_res, paste0("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use/",
                             names(ras_orig), "_resampled_50km.tif"))
}

# Teste soma um
lu_current = rast(list.files(path = "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use/", pattern = "_2015_", full.names = T))

sum(lu_current)

# Ecoregions -------------------------------------------------------------------
ec_orig = rast("/dados/bd_iis/ecoregions_esa_2017/ecoregions_2017_1000m_moll.tif")

ec_res = terra::resample(x = ec_orig, y = base_ras, method = 'near')

writeRaster(ec_res, paste0("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/",
                           names(ec_orig), "_resampled_50km.tif"))

# CB e OC ----------------------------------------------------------------------
cb = rast("/dados/projetos_andamento/CBD-draft/rawdata/variables/CBD-carbon_layer_updated.tif")
oc = rast("/dados/projetos_andamento/CBD-draft/rawdata/variables/CBD-opportunity_cost.tif")

# Resampling
cb = terra::resample(x = cb, y = base_ras, method = 'bilinear', filename = "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/CBD-carbon_layer_updated_reproj.tif")
oc = terra::resample(x = oc, y = base_ras, method = 'bilinear', filename = "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/CBD-opportunity_cost_reproj.tif")
