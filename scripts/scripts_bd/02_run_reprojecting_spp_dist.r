
#- pacotes ---------------------------------------------------------------------

library(terra)
library(foreach)
library(doParallel)
library(snow)
#-------------------------------------------------------------------------------
# lista de gcms
gcm_list = list.dirs(path = "/dados/bd_iis/climate_envelopes_fromCI/ppm_gcm", full.names = FALSE, recursive = FALSE) 

files_to_future <- paste0("/dados/projetos_andamento/CBD-draft/rawdata/climate_envelopes/bien_intersections/")

file_to_present <- "/dados/projetos_andamento/CBD-draft/rawdata/climate_envelopes/bien_intersections/bien_current/"


output_path <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3")


groups <- list.dirs(file_to_present,recursive = F,full.names = F)

r <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-regional_2050/CAN/agriculture.tif")


# listando raster names per group

lst_rst_group <- list()
for(g in seq_along(groups)){
  lst_r <- basename(list.files(file.path(files_to_future,gcm_list[1],groups[g]), pattern = ".tif", full.names = TRUE))
  nm <- groups[g]
  df <- expand.grid(group=nm,raster=lst_r)
  lst_rst_group[[g]] <- df
  
}


files <- do.call(rbind,lst_rst_group)

tasks <- merge(files,gcm_list)

names(tasks)[3] <- "gcms"

tasks <- tasks[1:10,]


process_raster <- function(group,gcm,raster,input_path,output_path,current_or_SSP3) {
  
  # Load the raster package within the worker
  library(terra)
  
  input_file <-(file.path(input_path,gcm,group,raster))
  
  # Read the input raster
  
  r2 <- rast(input_file)
  
  # Reproject the raster (replace the projection parameters with your target projection)
  #target_projection <- crs
  
  r2 <- terra::project(r2, r,"near")
  
  # Define the output file name
  output_file <- file.path(output_path,gcm,group,raster )
  
  
  
  # Save the reprojected raster
  writeRaster(r2, filename = output_file, gdal=c("COMPRESS=DEFLATE"),overwrite=T)
  
}

# Setting up the progress bar
iterations = nrow(tasks)
# Progress bar object
pb_l = progress::progress_bar$new(
  format = "Loading scenario [:bar] :percent in :elapsed",
  total = iterations, clear = FALSE, width = 70)

progress_number = 1:iterations
progress = function(n) {pb_l$tick(tokens = list(sp = progress_number[n]))}
opts = list(progress = progress)

# Set the number of cores to use
num_cores <- 25
cl <- makeCluster(num_cores, outfile="")
# start parallel processing
doSNOW::registerDoSNOW(cl)

# Run tasks in parallel
foreach(i = 1:nrow(tasks),.packages = c('terra'),
        .options.snow = opts,
        .errorhandling = "remove") %dopar% {
          
          suppressWarnings(suppressMessages(devtools::load_all("/dados/pessoal/francisco/plangea-pkg/", quiet = TRUE)))
          process_raster(group = tasks$group[i],gcm = tasks$gcms[i],raster = tasks$raster[i],input_path = files_to_future,output_path = output_path,current_or_SSP3 = "SSP3" )
          
        }

# Stop the parallel cluster
stopCluster(cl)


#Error in unserialize(socklist[[n]]) : erro ao ler a partir de conexão


for(i in 1:nrow(tasks)){
  
  process_raster(group = tasks$group[i],gcm = tasks$gcms[i],raster = tasks$raster[i],input_path = files_to_future,output_path = output_path)
  
}
