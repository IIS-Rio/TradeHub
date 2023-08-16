# Building the raster of lulc change -------------------------------------------

# Required libraries -----------------------------------------------------------
library(terra)

# Input data -------------------------------------------------------------------

# Listing current LULC rasters
current_lulc = list.files(path = "../iis_data/tradehub/land-use/",
                          pattern = ".tif$",
                          full.names = T)

# Natural classes patterns
nat_classes = c("forest", "other", "desert", "grassland", "shrubland", "wetland", "km_other")

# Natural current LC
nat_raster_names = grep(pattern = paste0(nat_classes, collapse = "|"), x = current_lulc, value = T)
nat_current_rasters = rast(nat_raster_names)
nat_current_lc = app(nat_current_rasters, 'sum')

# Anthropic current LU
ant_raster_names = grep(pattern = paste0(nat_classes, collapse = "|"), x = current_lulc, value = T, invert = T)
ant_current_rasters = rast(ant_raster_names)
ant_current_lc = app(nat_current_rasters, 'sum')

# Listing future scenarios
scens = list.dirs(path = "../iis_data/tradehub/land-use-2050/", recursive = F, full.names = F)

# Listing future LULC rasters
future_lulc = list.files(path = "../iis_data/tradehub/land-use-2050/",
                         pattern = ".tif$",
                         full.names = T, recursive = T)

# Looping over scenarios
for (scen in scens) {
  
  # Natural future LC
  nat_future_raster_names = grep(pattern = paste0(scen, '/', nat_classes, collapse = "|"), x = future_lulc, value = T)
  nat_future_rasters = rast(nat_future_raster_names)
  nat_future_lc = app(nat_future_rasters, 'sum')
  
  # Creating raster of subtraction of natural LCs (future - present)
  x = nat_future_lc - nat_current_lc
  
  # In which pixels is there a net restoration?
  net_restore_pixels = which(values(x, mat = F, dataframe = F) > 0)
  
  # Testing which natural cover had an increase
  #plot((nat_future_rasters - nat_current_rasters) * (x > 0))
  
  # Saving results
  writeRaster(x = x, filename = paste0("../iis_data/tradehub/Deltas/delta_", scen, ".tif"))
}