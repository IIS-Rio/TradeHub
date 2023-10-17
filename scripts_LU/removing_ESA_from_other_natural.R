# Desambiguando 'other natural lands'

library(raster)

other_nat_ssp2 = stack("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/other_natural_land/TradeHubTrack1Prelim-LCproj-GLOBIOM-TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2-24May2022_FinalMask_55x55km_other_natural_land.tif")

raster::plot(other_nat_ssp2)

# ESA 1992
esa_raster_file_paths = list.files("/dados/rawdata/land-use/past/", full.names = T)
esa_rasters = lapply(esa_raster_file_paths, raster)

# Teste soma um
#esa_sum = Reduce('+', esa_rasters)

# ESA naturais
esa_nat = esa_rasters[-c(1, 2, 7, 8)]

# Reamostrando ESA natural
esa_nat_res = lapply(esa_nat, function(r){raster::resample(x = r, y = other_nat_ssp2)})

# Removendo negativos
for (r in 1:length(esa_nat_res)) {
  esa_nat_res[[r]][esa_nat_res[[r]] < 0] = 0
}
#esa_nat_res[[6]][esa_nat_res[[6]] < 0] = 0

# Multiplicação do other por cada classe natural
other_nat_ssp2_mult = lapply(esa_nat_res, function(r){r * other_nat_ssp2})

# Removendo negativos
for (r in 1:length(other_nat_ssp2_mult)) {
  other_nat_ssp2_mult[[r]][other_nat_ssp2_mult[[r]] < 0] = 0
}

# Ler rasters de land-use do TradeHub
lu_trade = list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/", pattern = ".tif$", full.names = T, recursive = T)
lu_trade = lu_trade[-c(5, 7:13)]
lu_trade_ras = lapply(lu_trade, stack)

# Testando soma um após multiplicação
other_2050_mult_sum = Reduce('+', other_nat_ssp2_mult)
lu_trade_ras_sum = Reduce('+', lu_trade_ras)

test_sum_1 = other_2050_mult_sum + lu_trade_ras_sum

other_nat_new = 1 - test_sum_1

files = gsub("(ESA_landuse_300m_1992_)(.*)(_media.*$)", "\\2", unlist(lapply(esa_nat, names)))
files[6] =  "grassland"


dir = "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/other_natland_disaggregated//"

for(r in 1:length(other_nat_ssp2_mult)){
writeRaster(other_nat_ssp2_mult[[r]], paste0(dir, "TradeHubTrack1Prelim-LCproj-GLOBIOM-TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2-24May2022_FinalMask_55x55km_", files[r], ".tif"), overwrite = T)
}

writeRaster(other_nat_new, paste0(dir, "TradeHubTrack1Prelim-LCproj-GLOBIOM-TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2-24May2022_FinalMask_55x55km_", "other_natland.tif" ))
