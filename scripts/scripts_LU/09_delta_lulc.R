# Obter os deltas de LULC a partir do cenário BAU 2050 em relação a todos os outros cenários 2050 (x_vals)

# os deltas ja foram criados pra calcular balanco carbono. Precisa ser feito de forma regional???

#

regions <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/subregions/world_11_regions.tif")

region_code <- read.csv("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/subregions/region_code.csv")

# deltas

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas"

lulc_clss <- list.files(p,full.names = F)
scenarios <- gsub(pattern = "delta_agri_",replacement = "",list.files('/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas/delta_agri'))
#scenarios <- gsub(".tif","",scenarios)

# criar pastas pra salvar os recortes regionis

for(lu in seq_along(lulc_clss)){
  dir.create(path = paste0("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas/delta_regional/",lulc_clss[lu]))
}


for(lu in seq_along(lulc_clss)){
  rs <- list.files(file.path(p,lulc_clss[lu]),full.names = T)
  # abrir raster
  for(fl in seq_along(rs)){
    r <- rast(rs[fl])
    # cruzar com mascara regional
    for(region in 1:nrow(region_code)){
      id <- region_code$raster_id[region]
      reg=regions
      reg[reg!=id] <- NA
      mskd_r <- r*reg
      # criar pasta da regiao
      reg_nm <- region_code$region[region]
      dest <- paste0("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas/delta_regional/",lulc_clss[lu],"/",reg_nm)
      #
      dir.create(path =dest )
      # salvar
      writeRaster(mskd_r,filename =paste0(dest,"/",lulc_clss[lu],"_",scenarios[fl]),overwrite=T )
    }
  }
}
