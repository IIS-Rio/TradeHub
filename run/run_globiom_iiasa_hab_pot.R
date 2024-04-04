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


# function to run 
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

# task gorpe pra consertar cagadas

tasks <- data.frame(reg=c("MNA","OCE"),
                    scen=c("TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                            "TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2"))


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

num_clusters <- 5

cl <- makeCluster(num_clusters)
doSNOW::registerDoSNOW(cl)

# Run tasks in parallel
foreach(i = 1:nrow(tasks), .combine = 'c',.packages = c('devtools', 'progress'),
        .options.snow = opts,
        .errorhandling = "remove") %dopar% {
          
          suppressWarnings(suppressMessages(devtools::load_all("/dados/pessoal/francisco/plangea-pkg/", quiet = TRUE)))
          run_plangea(reg = tasks$reg[i], scen = tasks$scen[i], dest, cfg)
          
        }


# Stop the parallel cluster
stopCluster(cl)