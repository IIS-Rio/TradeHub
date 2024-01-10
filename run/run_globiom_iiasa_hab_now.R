# rodar plangea para calcular hab_pot

# calculado a partir do LULC 2050 BAU em relacao a todos os demais, mas usando
# os envelopes climaticos das spp. tem q rodar 1x pra cada gcm

# tem q copiar o diretorio pra dentro do rawdata, se nao nao funciona!

# pacotes ----------------------------------------------------------------------
devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")
library(jsonlite)
library(foreach)
library(doParallel)

# ------------------------------------------------------------------------------

# caminho pra salvar as analises por regiao
# precisa mudar o nome pra ficar claro q o new results eh hab_pot!

# p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/regional_analysis/"
#  
# # uma pasta por regiao
regioes <- read.csv("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/subregions/region_code.csv")

scen <- list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050")

reg <- regioes$region

# envelopes climaticos

rel_path_spp <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3"

gcms <- grep(pattern = ".csv",x = list.files(rel_path_spp,full.names = F),value = T,invert = T)



dest <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/hab_now"


# criando objeto com indices do JSON

cfg = jsonlite::fromJSON("/dados/pessoal/francisco/TradeHub/json/globiom_iiasa_regions_hab_now.json")

run_plangea <- function(reg, scen, gcms, dest, cfg) {
  
  # local de destino onde os resultados são salvos
  cfg$io$base_path <- paste0(dest, "/",gcms , "/",reg,"/" ,scen, "/")
        
  # tem que adicionar a regiao!
  cfg$io$lu_relative_path <- paste0("land-use-regional_2050/", reg, "/")
        
  # tem que adicionar o cenario, porque para cada regiao eh para rodar o cenario
  cfg$io$future_lu_relative_path <- paste0("land-use-2050/", scen, "/")
        
  # caminho das spp tb varia
  cfg$io$species_relative_path <- paste0("species_climate/SSP3", "/", gcms,"/")
        
  plangea(cfg = cfg)
  
}
 
num_clusters <- 20

cl <- makeCluster(num_clusters)

tasks <- expand.grid(reg,scen,gcms)
names(tasks) <- c("reg","scen","gcms")

# Run tasks in parallel
foreach(i = 1:nrow(tasks), .combine = 'c') %dopar% {
  run_plangea(reg = tasks$reg[i], scen = tasks$scen[i],tasks$gcms[i] ,dest, cfg)
}

# Stop the parallel cluster
stopCluster(cl)
