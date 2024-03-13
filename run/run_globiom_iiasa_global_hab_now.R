# pacotes ----------------------------------------------------------------------

devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")
library(jsonlite)
library(foreach)
library(doParallel)

# ------------------------------------------------------------------------------

# envelopes climaticos

rel_path_spp <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3"

gcms <- grep(pattern = ".csv",x = list.files(rel_path_spp,full.names = F),value = T,invert = T)

dest <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global_climate_hab_now"

# cenarios

scen <- list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050")

# funcao para rodar plangea

run_plangea <- function( scen, gcms, dest, cfg) {
  
  # local de destino onde os resultados são salvos
  cfg$io$base_path <- paste0(dest, "/",gcms , "/",scen, "/")
  
  # tem que adicionar a regiao!
  cfg$io$lu_relative_path <- paste0("land-use-2050/", scen, "/")
  
  # tem que adicionar o cenario, porque para cada regiao eh para rodar o cenario
  cfg$io$future_lu_relative_path <- paste0("land-use-2050/", scen, "/")
  
  # caminho das spp tb varia
  cfg$io$species_relative_path <- paste0("species_climate/SSP3", "/", gcms,"/")
  
  plangea(cfg = cfg)
  
}


# tarefas

tasks <- expand.grid(scen,gcms)
names(tasks) <- c("scen","gcms")


#tasks <- tasks[1,]

cfg = jsonlite::fromJSON("/dados/pessoal/francisco/TradeHub/json/globiom_iiasa_global_climate_habnow.json")


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

cl <- makeCluster(num_clusters,outfile="")
doSNOW::registerDoSNOW(cl)


# Run tasks in parallel
foreach(i = 1:nrow(tasks), .combine = 'c',.packages = c('devtools', 'progress'),
        .options.snow = opts,
        .errorhandling = "remove") %dopar% {
          
          suppressWarnings(suppressMessages(devtools::load_all("/dados/pessoal/francisco/plangea-pkg/", quiet = TRUE)))
          run_plangea(scen = tasks$scen[i],tasks$gcms[i] ,dest, cfg)
          
        }

# Stop the parallel cluster
stopCluster(cl)

