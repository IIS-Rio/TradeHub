#- OBS -------------------------------------------------------------------------

# adaptacao script CBD-draft: scripts/analyses_bd/multiple_gcms/with_intersection/04_run_plangea_multiple_gcms.R 

#-------------------------------------------------------------------------------



#running plangea with multiple GCMs

rm(list = ls())
gc()


#SE TRUE vc está fazendo para o SSP3 (Chico: isso aqui eh em relacao a Land use, nao clima!)
#SE FALSE vc está fazendo para os inputs normais do plangea
ssp3 = FALSE

# modificar, checar versao! fazer via PLANGEA ou sandbox??

#require(devtools)
#load_all(path = "/dados/pessoal/gabi/plangea-pkg/")

diretorios = c("bc","ca", "cm", "cn", "gf", "ip", "mi", "mr", "ms")

#ssp3 = "_SSP3_bd" essa linha nao faz sentido


for (i in 1:length(diretorios)){
  
  # Chico: aqui roda o plangea comparando lu passado com lu presente
  cfg = aux_read_cfg("/dados/projetos_andamento/CBD-draft/json/running_different_bds_multiple_gcms/02_cfg_CBD_multiple_gcms_calc_bds.json")
  cfg$io$base_path = paste0("/dados/projetos_andamento/CBD-draft/problem_objects/different_bds_multiple_gcms/", diretorios[i],"/")
  
  # parametros do json que precisam ser aproveitados para o sandbox:
    #"base_path": "/dados/projetos_andamento/CBD-draft/problem_objects/different_bds_multiple_gcms/test/",  
    # "lu_relative_path": "land-use/current_lulc_without-urban2050/",
    #"past_lu_relative_path": "land-use/past/",
    #"future_lu_relative_path": "land-use/future/",
    #"species_relative_path": "climate_envelopes/bien_intersections/bc/",
    #**********************************************************
    # Chico: esse de cima nao entendo. bc eh o parametro normal do plangea????
    # no nosso caso, eu uso o current aqui, q vai cruzar com os gcms futuros pra
    # calcular os bds
    #**********************************************************
  
  # Chico: aqui roda o plangea comparando lu passado com lu futuro
  if(ssp3){
    cfg = aux_read_cfg("/dados/projetos_andamento/CBD-draft/json/running_different_bds_multiple_gcms/03_cfg_CBD_multiple_gcms_SSP3_bd.json")
    cfg$io$base_path = paste0("/dados/projetos_andamento/CBD-draft/problem_objects/different_bds_multiple_gcms/", diretorios[i],"_SSP3_bd","/")
  }
  
  # Chico: eu peguei as spp pra reamostrar dentro de rawdata/climate_envelops/bien_intersection etc e não do problem_objects!!! Qual ta certo??
  
  cfg$io$species_relative_path = paste0("climate_envelopes/bien_intersections/", diretorios[i], "/")
  
  # parametros do json que precisam ser aproveitados para o sandbox:
    # "base_path": "/dados/projetos_andamento/CBD-draft/problem_objects/different_bds_multiple_gcms/test/global_SSP3_bd/",
    # "rawdata_path": "/dados/projetos_andamento/CBD-draft/rawdata/",
    # "lu_relative_path": "land-use/future/",
    # "past_lu_relative_path": "land-use/past/",
    #"future_lu_relative_path": "land-use/future/",
    #"species_relative_path": "climate_envelopes/bien_intersections/bc/",
  
  cfg$post_process$run_postprocess = FALSE
  
  plangea(cfg, force_comp = FALSE)
}


# JSON 04_cfg_CBD_multiple_gcms_SSP3.json parece ser mais "correto na verdade, apesar de nao estar no run! preciso ver se tem algum run com ela!!