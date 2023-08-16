# Building a data frame with emissions/stock of Carbon  ------------------------
# with respect to IPCC Climatic Zones, GeoEcological Zones, Continental Division
# and 11 World Regions

# Required libraries -----------------------------------------------------------
library(terra)

# Reading original zonings
ipcc_climatic = rast("../iis_data/tradehub/ClimateZones.tif")
cont_div = rast("../iis_data/tradehub/ContinentalDiv.tif")
geo_eco = rast("../iis_data/tradehub/GlobalEcoZones.tif")
world_regions = rast("../iis_data/tradehub/world_11_regions.tif")
base_ras = 0 * world_regions

# Resampling rasters

# Algoritmo:
# 1) Criar matriz de transição: a partir dos mapas de pastagem e agricultura do presente e futuro
# tomar o valor da diminuição dessas áreas
# 2) É possível (por conta de diminuições de outras áreas naturais) que a diminuição de área antrópica
# não seja igual ao aumento da floresta
# 3) nesse caso, usar regra de 3 para re-escalar as transições pasto-floresta e agicultura-floresta
# 4) Criar dataframe reunindo infos de zonings (acima), emissions factors (abaixo) e transições
# 5) Fazer a soma total para obter o aumento de carbono sequestrado em 2050 (para conservação usar o do Plangea)
# 6) Fazer o zonal para as 11 regiões mundiais
 
# Reading emissions factors tables
ef_forest = read.csv(file = "../iis_data/tradehub/Restoration_transition_matrix_Forest_EF_ER_Values_20210823_shared.csv")