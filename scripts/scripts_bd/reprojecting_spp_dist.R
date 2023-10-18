
#- Observacoes -----------------------------------------------------------------

# usar o cruzamento da distribuição de spp com os envelopes climaticos gerados pelo artigo da CBD. 

# A distribuicao com o clima atual e 9 gcms futuros foram cruzadas com os limites da IUCN.

#-------------------------------------------------------------------------------

#- pacotes ---------------------------------------------------------------------

library(raster)
library(foreach)
library(doParallel)

#-------------------------------------------------------------------------------
# caminho distribuicao spp presente

file_to_present <- "/dados/projetos_andamento/CBD-draft/rawdata/climate_envelopes/bien_intersections/bien_current/"

# caminho distribuicao futuro

# lista de gcms
gcm_list = list.dirs(path = "/dados/bd_iis/climate_envelopes_fromCI/ppm_gcm", full.names = FALSE, recursive = FALSE) 

files_to_future <- paste0("/dados/projetos_andamento/CBD-draft/rawdata/climate_envelopes/bien_intersections/",gcm_list)

# base raster with projection

r <- raster("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use/agriculture.tif")

crs <- crs(r)

# Set the number of cores to use
num_cores <- 25
cl <- makeCluster(num_cores)

process_raster <- function(input_file, output_folder) {
  
  # Load the raster package within the worker
  library(raster)
  # Read the input raster
  r2 <- raster(input_file)
  
  # Reproject the raster (replace the projection parameters with your target projection)
  target_projection <- crs
  
  r2 <- projectRaster(r, crs = target_projection)
  
  # Define the output file name
  output_file <- file.path(output_folder, basename(input_file))
  
  # Save the reprojected raster
  writeRaster(r2, filename = output_file, format = "GTiff",options = "COMPRESS=DEFLATE",overwrite=T)
}

input_folder <- list.dirs(file_to_present,recursive = F,full.names = F)



#create current directory

dir_output <- (file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/species_climate","current"))

dir.create(dir_output)

output_folder_present <- dir_output 

# for loop with vertebrates folder (current)

# start parallel processing
registerDoParallel(cl)

for(vert_group in input_folder){
  
  
  dir.path <- paste0(output_folder_present,"/",vert_group)
  dir.create(dir.path)
  # List all raster files in the input folder 
  input_files <- list.files(paste0(file_to_present,vert_group), pattern = ".tif", full.names = TRUE)
  
  # Process the rasters in parallel
  foreach(file = input_files) %dopar% {
    process_raster(file,dir.path )
  }
  
  
  
}

# Stop the parallel cluster
stopCluster(cl)
