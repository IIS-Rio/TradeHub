# Building the raster of lulc change -------------------------------------------

# Required libraries -----------------------------------------------------------
library(terra)

# Input data -------------------------------------------------------------------

# Listing current LULC rasters
current_lulc = list.files(path = "../data_carbono/land-use/",
                          pattern = ".tif$",
                          full.names = T)

# Natural classes patterns
nat_classes = c("forest", "other", "desert", "grassland", "shrubland", "wetland", "km_other")

# Natural current LC
nat_raster_names = grep(pattern = paste0(nat_classes, collapse = "|"), x = current_lulc, value = T)
nat_current_rasters = rast(nat_raster_names)
nat_current_lc = app(nat_current_rasters, 'sum')

# Anthropic current LU
ant_raster_names = grep(pattern = paste0(ant_classes, collapse = "|"), x = current_lulc, value = T, invert = F)
ant_current_rasters = rast(ant_raster_names)
#ant_current_lc = app(nat_current_rasters, 'sum')

# Listing future scenarios
scens = list.dirs(path = "../data_carbono/land-use-2050/", recursive = F, full.names = F)

# Listing future LULC rasters
future_lulc = list.files(path = "../iis_data/tradehub/land-use-2050/",
                         pattern = ".tif$",
                         full.names = T, recursive = T)

# Looping over scenarios
for (scen in scens) {
  
  # Natural future LC
  # nat_future_raster_names = grep(pattern = paste0(scen, '/', nat_classes, collapse = "|"), x = future_lulc, value = T)
  # nat_future_rasters = rast(nat_future_raster_names)
  # nat_future_lc = app(nat_future_rasters, 'sum')
  
  # Creating raster of subtraction of natural LCs (future - present)
  # x = nat_future_lc - nat_current_lc
  
  # In which pixels is there a net restoration?
  # net_restore_pixels = which(values(x, mat = F, dataframe = F) > 0)
  
  # Testing which natural cover had an increase
  #plot((nat_future_rasters - nat_current_rasters) * (x > 0))
  
  # antropic future LC
  
  ant_future_raster_names = grep(pattern = paste0(scen, '/', ant_classes, collapse = "|"), x = future_lulc, value = T)
  # delta agri
    ant_future_agri = rast(grep(pattern = "agri",x = ant_future_raster_names,value = T))
      
    ant_current_agri = ant_current_rasters[[1]]
  
    # Creating raster of subtraction of natural LCs (future - present)
    x2 = ant_future_agri - ant_current_agri
  
  # Saving results
  
 #writeRaster(x = x, filename = paste0("../iis_data/tradehub/Deltas/delta_", scen, ".tif"))
  
  writeRaster(x = x2, filename = paste0("../data_carbono/Deltas/delta_agri/delta_agri_", scen, ".tif"),overwrite=T)
  
  # delta pastagem
  ant_future_past = rast(grep(pattern = "past",x = ant_future_raster_names,value = T))
  
  ant_current_past = ant_current_rasters[[2]]
  
  # Creating raster of subtraction of natural LCs (future - present)
  
  x3 = ant_future_past - ant_current_past
  
  # Saving results
  #writeRaster(x = x, filename = paste0("../iis_data/tradehub/Deltas/delta_", scen, ".tif"))
  
  writeRaster(x = x3, filename = paste0("../data_carbono/Deltas/delta_past/delta_past_", scen, ".tif"))
         
  
}
