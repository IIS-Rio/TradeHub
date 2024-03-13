# rodar plangea para calcular hab_pot

# calculado a partir do LULC 2050 BAU em relacao a todos os demais


# pacotes ----------------------------------------------------------------------
devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")
library(jsonlite)
#library(parallel)
library(foreach)
library(doParallel)
# ------------------------------------------------------------------------------

scen <- list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050")
                   
dest <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global_climate_hab_pot"

cfg = jsonlite::fromJSON("/dados/pessoal/francisco/TradeHub/json/globiom_iiasa_global_climate_habpot.json")


# function to run 
run_plangea <- function(scen, dest, cfg) {
  # local de destino onde os resultados sao salvos
  cfg$io$base_path <- paste0(dest, "/", scen, "/")

  # tem q adicionar a regiao!
  cfg$io$lu_relative_path <- paste0("land-use-2050/", scen, "/")

  # tem q adiconar o cenario, pq pra cada regiao eh pra rodar o cenario
  cfg$io$future_lu_relative_path <- paste0("land-use-2050/", scen, "/")

  plangea(cfg = cfg)

}


# Create a list of tasks

tasks <- data.frame( scen = scen)

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
          run_plangea( scen = tasks$scen[i], dest, cfg)
          
        }


# Stop the parallel cluster
stopCluster(cl)
