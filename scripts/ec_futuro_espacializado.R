devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")

fu <- aux_load("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global/globiom_iiasa_baseline/results/solver_runs",'solver_future_land_use')

# testando com a funcao/ nao consegui!
# o hab now ta no presente, tem q ser com futuro pra ter o ec pra kda cenario!
# nao sei onde tem os valores de futuro. mas acho q daria pra fazer com os hab_pot!
#ec_fut <- multi_calc_ec(cfg)

raster_base <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/ecoregions_2017_1000m_moll_resampled_50km.tif")


cfg = jsonlite::fromJSON("/dados/pessoal/francisco/TradeHub/json/globiom_iiasa.json")

z=0.25

scens <- list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global/")

scens_base <- grep(pattern = paste(c("_NOBIOD_","baseline"),collapse="|"), scens,value = T)

# guardar resultados

#ec_futuro <- list()

# resolvi salvar!

for(i in seq_along(scens)){

  cfg$io$base_path <- paste0("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global/",scens[i],"/")
  ec_res = aux_load(paste0(cfg$io$base_path, cfg$io$processed_relative_path, 'harmonize_ec'))
  lu_res = aux_load(paste0(cfg$io$base_path, cfg$io$processed_relative_path, 'harmonize_lu'))
  x_vals = aux_load(paste0(cfg$io$base_path, "results/solver_runs/", 'solver_future_land_use'))
  px_area = aux_load(paste0(cfg$io$base_path, cfg$io$processed_relative_path, 'mi_aux'))$px_area
  # master index tem as posicoes!
  m_i <- aux_load(paste0(cfg$io$base_path, cfg$io$processed_relative_path, 'master_index'))
  
  
  for (j in 1:length(ec_res)) {assign(names(ec_res)[j], ec_res[[j]])}
  
  # COMPUTING THE EC LAYER -----------------------------------------------------
  # current and potential proportion of natural areas
  pnat_now = Reduce('+', lu_vals[lu_class_types == "N"])
  pnat_pot = Reduce('+', lu_vals[lu_class_types %in% c("N", "A")])
  pnat_res = pnat_now + x_vals
  
  # if(!is.null(x) & (length(x) == length(pnat_now))){
  #   if ("C" %in% cfg$scenarios$problem_type) {
  #    pnat_now = x
  #   } else {
  #     pnat_now = pnat_now + x
  #   }
  # }
  
  # total current and potential natural areas of each ecosystem in eco_vals
  
  eco_areas_now = lapply(unique(eco_vals), function(x){sum(pnat_now[eco_vals == x])}) 
  
  names(eco_areas_now) = unique(eco_vals)
  
  eco_areas_pot = lapply(unique(eco_vals), function(x){sum(pnat_pot[eco_vals == x])})
  
  names(eco_areas_pot) = unique(eco_vals)
  
  
  eco_areas_res = lapply(unique(eco_vals), function(x){sum(pnat_res[eco_vals == x])})
  
  names(eco_areas_res) = unique(eco_vals)
  
  
  
  
  # Derivative of ecoregions risk of collapse
  delta_risk = .multi_extinction_slope(A = eco_areas_now, Amax = eco_areas_pot, z = z )  
  
  
  # delta_risk_future
  
  risk1=multi_extinction_risk(A = eco_areas_now, Amax = eco_areas_pot, z = z)
  risk2=multi_extinction_risk(A = eco_areas_res, Amax = eco_areas_pot, z = z)
  delta_crisk_fut = mapply('-', risk1, risk2, SIMPLIFY = FALSE)
  # delta_crisk_fut = unlist(multi_extinction_risk(A = eco_areas_now, Amax = eco_areas_pot, z = z), use.names = F) -
  #   unlist(multi_extinction_risk(A = eco_areas_res, Amax = eco_areas_pot, z = z), use.names = F)
  
  # EC layer present
  res = rep(NA, length(eco_vals))
  
  # Filling EC layer
  for (eco in unique(eco_vals)) {
    res[eco_vals == eco] = delta_risk[[as.character(eco)]]
  }
  
  if (any(is.na(res))) {
    stop("NA's found in EC layer! (refer to multi_calc_ec module)")
  }
  
  # EC layer present
  res = rep(NA, length(eco_vals))
  
  # Filling EC layer
  for (eco in unique(eco_vals)) {
    res[eco_vals == eco] = delta_risk[[as.character(eco)]]
  }
  
  if (any(is.na(res))) {
    stop("NA's found in EC layer! (refer to multi_calc_ec module)")
  }
  
  # EC layer future
  res_fut = rep(NA, length(eco_vals))
  
  # Filling EC layer
  for (eco in unique(eco_vals)) {
    res_fut[eco_vals == eco] = delta_crisk_fut[[as.character(eco)]]
  }
  
  if (any(is.na(res_fut))) {
    stop("NA's found in EC layer! (refer to multi_calc_ec module)")
  }


# transformar res em raster novamente. Acho q o master index serve

  raster_resultado = 0* raster_base
  raster_resultado[m_i] = res
  
  raster_resultado_fu = 0* raster_base
  raster_resultado_fu[m_i] = res_fut
  
  raster_resultado_fu[raster_resultado_fu>0] <- NA
  
  #ec_futuro[[i]] <- raster_resultado

  writeRaster(raster_resultado_fu,filename = paste0("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/future_biodiv_index_rasters/","ec_future_",scens_base[i],".tif"))
  
  }

