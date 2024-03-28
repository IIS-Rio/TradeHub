scens <- list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050")

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/bd_future_per_clim_env"

for(i in seq_along(scens)){
  
  ls <- list.files(p,pattern = scens[i],full.names = T)
  # rasteriza
  ls_r <- lapply(ls,rast)
  # calcula Media
  r_m <- Reduce(mean,ls_r)
  writeRaster(r_m,paste0("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/future_biodiv_index_rasters/","bd_future_globiom_iiasa_",scens[i],".tif"))
}
