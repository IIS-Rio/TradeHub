# Building a data frame with emissions/stock of Carbon  ------------------------
# with respect to IPCC Climatic Zones, GeoEcological Zones, Continental Division
# and 11 World Regions

# Required libraries -----------------------------------------------------------
library(terra)
library(dplyr)

# Reading original zonings -----------------------------------------------------
#
ipcc_climatic = rast("../iis_data/tradehub/ClimateZones.tif")
cont_div = rast("../iis_data/tradehub/ContinentalDiv.tif")
geo_eco = rast("../iis_data/tradehub/GlobalEcoZones.tif")
world_regions = rast("../iis_data/tradehub/world_11_regions.tif")
base_ras = 0 * world_regions

# Input data -------------------------------------------------------------------

# Listing current LULC rasters
# current_lulc = list.files(path = "../data_carbono/land-use/",
#                           pattern = ".tif$",
#                           full.names = T)
# 
# # Natural classes patterns
# nat_classes = c("forest", "other", "desert", "grassland", "shrubland", "wetland", "km_other")
# ant_classes = c("agriculture","pasture")
# 
# # Anthropic current LU
# ant_raster_names = grep(pattern = paste0(ant_classes, collapse = "|"), x = current_lulc, value = T, invert = F)
# 
# ant_current_rasters = rast(ant_raster_names)
# 
# 
# # Listing future LULC rasters
# future_lulc = list.files(path = "../data_carbono/land-use-2050/",
#                          pattern = ".tif$",
#                          full.names = T, recursive = T)

# Algoritmo:
# 1) Criar matriz de transição: a partir dos mapas de pastagem e agricultura do presente e futuro
# tomar o valor da diminuição dessas áreas

# Listing future scenarios
scens = list.dirs(path = "../data_carbono/land-use-2050/", recursive = F, full.names = F)

# criar df com id dos pixels, mudança pra agri, mudança pra past. e mudança pra rest
# 

deltas <- list.files("../data_carbono/Deltas",recursive = F)

# for loop

# delta agri
r_agri <- rast(file.path("../data_carbono/Deltas","delta_agri",paste0("delta_agri_",scen,".tif")))
# delta past
r_past <- rast(file.path("../data_carbono/Deltas","delta_past",paste0("delta_past_",scen,".tif")))
# delta resto
r_rest <- rast(file.path("../data_carbono/Deltas","delta_rest",paste0("delta_",scen,".tif")))

data_df <- data.frame(
   x = xyFromCell(r_agri, 1:ncell(r_agri))[,"x"],
   y = xyFromCell(r_agri, 1:ncell(r_agri))[,"y"],
  agri_value = extract(r_agri, 1:ncell(r_agri)),
  past_value = extract(r_past, 1:ncell(r_past)),
  rest_value = extract(r_rest, 1:ncell(r_rest)))

names(data_df)[3:5] <- c("delta_agri","delta_past","delta_rest")

# cleaning data

adjusted_df <- data_df %>%
  # eliminando NAs
  filter(!is.na(delta_agri) & !is.na(delta_past) & !is.na(delta_rest)) %>%
  # arredondando
  mutate(
    delta_agri = round(delta_agri, 5),   
    delta_past = round(delta_past, 5),   
    delta_rest = round(delta_rest, 5)    
  ) %>%
  # selecionando apenas celulas que tiveram restauracao  
  filter(delta_rest > 0) %>%
  mutate(pasture_agri = delta_agri + delta_past) %>%
  # qndo teve expansao de agri ou pasture, substituir por 0
  mutate(
    delta_past = if_else(delta_past > 0, 0, delta_past),
    delta_agri = if_else(delta_agri > 0, 0, delta_agri)
  ) %>%
  # Calculate the scaling factor based on the ratio
  mutate(
    scaling_factor = if_else(delta_past == 0 | delta_agri == 0, 1,
                             if_else(delta_agri < delta_past, delta_agri / delta_past, delta_past / delta_agri))
  ) %>%
  # Calculate adjusted values and cap adjusted_sum at delta_rest
  mutate(
    adjusted_agri = delta_agri * scaling_factor,
    adjusted_past = delta_past * scaling_factor,
    adjusted_sum = if_else(abs(adjusted_agri + adjusted_past) > delta_rest, delta_rest, adjusted_agri + adjusted_past)
  )

# ta quase, tem ainda ajustes pra fazer!
# rodar e conferir pq as vezes da merda! eh qndo past ou agri=0 e tb qndo os 2 sao 0
plot(adjusted_df$delta_rest,adjusted_df$adjusted_sum)

# 2) É possível (por conta de diminuições de outras áreas naturais) que a diminuição de área antrópica não seja igual ao aumento da floresta
# 3) nesse caso, usar regra de 3 para re-escalar as transições pasto-floresta e agicultura-floresta
# 4) Criar dataframe reunindo infos de zonings (acima), emissions factors (abaixo) e transições
# 5) Fazer a soma total para obter o aumento de carbono sequestrado em 2050 (para conservação usar o do Plangea)
# 6) Fazer o zonal para as 11 regiões mundiais
 
# Reading emissions factors tables
ef_forest = read.csv(file = "../iis_data/tradehub/Restoration_transition_matrix_Forest_EF_ER_Values_20210823_shared.csv")
