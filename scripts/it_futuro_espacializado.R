devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")

# fu <- aux_load("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global/globiom_iiasa_baseline/results/solver_runs",'solver_future_land_use')


base_ras <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/ecoregions_2017_1000m_moll_resampled_50km.tif")

cfg = jsonlite::fromJSON("/dados/pessoal/francisco/TradeHub/json/globiom_iiasa.json")


scens <- list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global/")

scens_base <- grep(pattern = paste(c("_NOBIOD_","baseline"),collapse="|"), scens,value = T)

HFI <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/hfi_2050_ssp2_resampled.tif")


# funcoes necessarias

source("/dados/pessoal/francisco/TradeHub/scripts/multi_calc_it_mod.R")
source("/dados/pessoal/francisco/TradeHub/scripts/multi_it_aggregate_modified.R")



for(i in seq_along(scens)){

  cfg$io$base_path <- paste0("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global/",scens[i],"/")
  it_res = aux_load(paste0(cfg$io$base_path, cfg$io$processed_relative_path, 'harmonize_it'))
  lu_res = aux_load(paste0(cfg$io$base_path, cfg$io$processed_relative_path, 'harmonize_lu'))
  x_vals = aux_load(paste0(cfg$io$base_path, "results/solver_runs/", 'solver_future_land_use'))
  px_area = aux_load(paste0(cfg$io$base_path, cfg$io$processed_relative_path, 'mi_aux'))$px_area
  # master index tem as posicoes!
  m_i <- aux_load(paste0(cfg$io$base_path, cfg$io$processed_relative_path, 'master_index'))
  
  it <- multi_it_aggregate_modified(cfg = cfg,x_vals = x_vals, )
  
  writeRaster(it,filename = paste0("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/future_biodiv_index_rasters/","it_future_",scens[i],".tif"),overwrite=T)
  
  }

