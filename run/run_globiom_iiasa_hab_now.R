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

# uma pasta por regiao

regioes <- read.csv("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/subregions/region_code.csv")

scen <- list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050")

# houve problema nos seguintes cenatios

reg <- regioes$region

# ouve problema nas seguintes regioes

# envelopes climaticos

rel_path_spp <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3"

gcms <- grep(pattern = ".csv",x = list.files(rel_path_spp,full.names = F),value = T,invert = T)

dest <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/hab_now2"


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
 
tasks <- expand.grid(reg,scen,gcms)
names(tasks) <- c("reg","scen","gcms")
# tasks <- data.frame( reg = c("LAC","OCE"),
#                      scen = c("TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
#                                "TH_TFELIM_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2"),
#                      gcms = c("bc","ca"))


# Setting up the progress bar
iterations = nrow(tasks)

# Progress bar object
pb_l = progress::progress_bar$new(
  format = "Loading scenario [:bar] :percent in :elapsed",
  total = iterations, clear = FALSE, width = 70)

progress_number = 1:iterations
progress = function(n) {pb_l$tick(tokens = list(sp = progress_number[n]))}
opts = list(progress = progress)

# run in parallel

num_clusters <- 7

cl <- makeCluster(num_clusters)
doSNOW::registerDoSNOW(cl)



#names(tasks) <- c("reg","scen","gcms")


# Run tasks in parallel
foreach(i = 1:nrow(tasks), .combine = 'c',.packages = c('devtools', 'progress'),
        .options.snow = opts,
        .errorhandling = "remove") %dopar% {

          suppressWarnings(suppressMessages(devtools::load_all("/dados/pessoal/francisco/plangea-pkg/", quiet = TRUE)))
          run_plangea(reg = tasks$reg[i], scen = tasks$scen[i],tasks$gcms[i] ,dest, cfg)

        }

# Stop the parallel cluster
stopCluster(cl)

# for(i in  1:nrow(tasks)){
#   
#   run_plangea(reg = tasks$reg[i], scen = tasks$scen[i],tasks$gcms[i] ,dest, cfg)
#   
# }


# extent das spp ta diferente
