library(terra)
#library(doParallel)
# caminho
p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3/"

# base raster

baser <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-regional_2050/CAN/agriculture.tif")

# listando rasters

ls_spp <- list.files(p,pattern = ".tif",recursive = T,full.names = T)

# Output folder for processed rasters
output_folder <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3_corrigido"

# Create a list of output paths based on the input raster paths

output_path <- file.path(output_folder, basename(ls_spp))

#ls_spp_teste <- ls_spp[1:4]

# Function to read, project, and write raster
reproj <- function(spp_rast_path) {
  spp_rast <- rast(spp_rast_path)
  spp_rastpj <- project(spp_rast, baser) 
  
  # Add your writing logic here
  # For example, if you want to write as GeoTIFF
  writeRaster(spp_rastpj, filename=spp_rast_path, overwrite=TRUE)
  
  # Return a message or any other result if needed
  return(paste("Raster processed and written:", spp_rast_path))
}

# Set up parallel backend
# num_clusters <- 5
# cl <- makeCluster(num_clusters)
# registerDoParallel(cl)

# Run tasks in parallel
for(i in 1:length(ls_spp)){
                    reproj(spp_rast_path = ls_spp[i])
                  }

# # Stop parallel backend
# stopCluster(cl)
