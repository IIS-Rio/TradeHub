
#- Observacoes -----------------------------------------------------------------

# usar o cruzamento da distribuição de spp com os envelopes climaticos gerados pelo artigo da CBD. 

# A distribuicao com o clima atual e 9 gcms futuros foram cruzadas com os limites da IUCN.

# pra rodar como background job, usar o scritp "run_reprojecting_spp_dist.r"

#-------------------------------------------------------------------------------
# tem q rodar essa porra de novo!!olhar com calma!!
#- pacotes ---------------------------------------------------------------------

library(raster)
library(foreach)
library(doParallel)

#-------------------------------------------------------------------------------
# caminho distribuicao spp presente

# file_to_present <- "/dados/projetos_andamento/CBD-draft/rawdata/climate_envelopes/bien_intersections/bien_current/"
# 
# # caminho distribuicao futuro
# 
# # lista de gcms
# gcm_list = list.dirs(path = "/dados/bd_iis/climate_envelopes_fromCI/ppm_gcm", full.names = FALSE, recursive = FALSE) 
# 
# files_to_future <- paste0("/dados/projetos_andamento/CBD-draft/rawdata/climate_envelopes/bien_intersections/")
# 
# # base raster with projection
# 
# r <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use/agriculture.tif")
# 
# crs <- crs(r)
# 


process_raster <- function(input_file, output_folder,current_or_SSP3) {
  
  # Load the raster package within the worker
  library(terra)
  
  # condition to run the ressampling
  
  # Read the input raster
    
    r2 <- rast(input_file)
    
    # Reproject the raster (replace the projection parameters with your target projection)
    target_projection <- crs
    
    r2 <- terra::project(r2, r)
    
    # Define the output file name
    output_file <- file.path(output_folder, basename(input_file))
    
    # Save the reprojected raster
    writeRaster(r2, filename = output_file, format = "GTiff",options = "COMPRESS=DEFLATE",overwrite=T)
  
}



