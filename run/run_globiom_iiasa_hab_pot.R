# rodar plangea para calcular hab_pot

# calculado a partir do LULC 2050 BAU em relacao a todos os demais


# pacotes ----------------------------------------------------------------------
devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")
library(jsonlite)
#library(parallel)
library(foreach)
library(doParallel)
# ------------------------------------------------------------------------------

# caminho pra salvar as analises por regiao
# precisa mudar o nome pra ficar claro q o new results eh hab_pot!

# p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/regional_analysis/"
#  
# # uma pasta por regiao
regioes <- read.csv("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/subregions/region_code.csv")
# # f <- function(x)dir.create(file.path(p,x))
# # lapply(regioes$region,f)
# configurando o JSON pra rodar pra varias regioes
# cfg <- aux_read_cfg(file.path("/dados/pessoal/francisco/TradeHub/json/globiom_iiasa.json"))
# updates cfg with plangea_path

scen <- list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050")

reg <- regioes$region

#reg <- c("EAS" , "EUR", "LAC" ,"MNA" ,"OCE" ,"SAS", "SEA", "SSA", "USA")

dest <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/hab_pot"


# cfg$io$plangea_path = plangea_path

# criando objeto com indices do JSON

cfg = jsonlite::fromJSON("/dados/pessoal/francisco/TradeHub/json/globiom_iiasa_regions_hab_pot.json")


# for(i in 1:length(reg)){
#   for(j in 1:length(scen)){
#     # local de destino onde os resultados sao salvos
#     cfg$io$base_path <- paste0(dest,"/",reg[i],"/",scen[j],"/")
# 
#     # tem q adicionar a regiao!
#     cfg$io$lu_relative_path <- paste0("land-use-regional_2050/",reg[i],"/")
# 
# 
#     # tem q adiconar o cenario, pq pra cada regiao eh pra rodar o cenario
#     cfg$io$future_lu_relative_path <- paste0("land-use-2050/",scen[j],"/")
# 
#     plangea(cfg = cfg)
#     
#     }
# 
#   }

# Your function
run_plangea <- function(reg, scen, dest, cfg) {
  # local de destino onde os resultados sao salvos
  cfg$io$base_path <- paste0(dest, "/", reg, "/", scen, "/")

  # tem q adicionar a regiao!
  cfg$io$lu_relative_path <- paste0("land-use-regional_2050/", reg, "/")

  # tem q adiconar o cenario, pq pra cada regiao eh pra rodar o cenario
  cfg$io$future_lu_relative_path <- paste0("land-use-2050/", scen, "/")

  plangea(cfg = cfg)

}


# Create a list of tasks

tasks <- expand.grid(reg = reg, scen = scen)

# Run plangea in parallel for each region

num_clusters <- length(reg)

cl <- makeCluster(num_clusters)

# Run tasks in parallel
foreach(i = 1:nrow(tasks), .combine = 'c') %dopar% {
  run_plangea(reg = tasks$reg[i], scen = tasks$scen[i], dest, cfg)
}




# Stop the parallel cluster
stopCluster(cl)