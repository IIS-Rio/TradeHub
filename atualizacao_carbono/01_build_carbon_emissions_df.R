# Building a data frame with emissions/stock of Carbon  ------------------------
# with respect to IPCC Climatic Zones, GeoEcological Zones, Continental Division
# and 11 World Regions

# Required libraries -----------------------------------------------------------

library(terra)
library(dplyr)
library(tidyverse)

# Reading original zonings -----------------------------------------------------
#
ipcc_climatic = rast("../data_carbono/ClimateZones.tif")
cont_div = rast("../data_carbono/ContinentalDiv.tif")
geo_eco = rast("../data_carbono/GlobalEcoZones.tif")
world_regions = rast("../data_carbono/world_11_regions.tif")
base_ras = 0 * world_regions

# tabelas

gez <- read.csv("../data_carbono/gez_2010_wgs84.csv",sep=";")
gez$gez_code <-as.double( gsub(pattern = ",",replacement = ".",x = gez$gez_code))
cont_div_df <- read.csv("../data_carbono/continents_20210725.csv",sep=";")

meta_CLI = data.frame(name = c("Tropical Montane",
                               "Tropical Wet",
                               "Tropical Moist",
                               "Tropical Dry",
                               "Warm Temperate Moist",
                               "Warm Temperate Dry",
                               "Cool Temperate Moist",
                               "Cool Temperate Dry",
                               "Boreal Moist",
                               "Boreal Dry",
                               "Polar Moist",
                               "Polar Dry"),
                      code = c(1:12))

meta_cont_division = data.frame(code = as.numeric(as.factor(cont_div_df$region2)), region = cont_div_df$region2)

# reprojetando

ipcc_climatic_pj <- project(ipcc_climatic,base_ras,method="near")
# sao rasters com numeros, precisa converter pra nome das classes!
cont_div_pj <- terra::project(cont_div,base_ras,method="near")
geo_eco_pj <- project(geo_eco,base_ras,method="near")

# lendo df base

emission_factors <- read.csv("../data_carbono/Restoration_transition_matrix_Forest_EF_ER_Values_20210823_shared.csv")

# emission_factors2 <- read.csv("../data_carbono/Restoration_transition_matrix_NatHabitats_EF_ER_Values_20210823_shared.csv")
#  

class_transitions <- unique(emission_factors$Transition_type)

# script 17 nature map github
# preenche algumas lacunas

# Dictionary of GEZ
# Be aware! No intersection regions: Polar, Water, Tropical desert, Subtropical desert. 

# OBS Chico: nao ter essas interseccoes eh um problema, pois tem restauracao nessas areas!

dictionary = data.frame(df_gez = sort(unique(adjusted_df_l$GEZ_name)), 
                        trans_tbl_gez = c("Boreal coniferous ", 
                                          "Boreal mountain",
                                          "Boreal tundra woodland", 
                                          NA, 
                                          NA,
                                          "Subtropical dry forests", 
                                          "Sub-tropical humid forests",
                                          "Subtropical mountain system", 
                                          "Subtropical steppe",
                                          "Temperate continental",
                                          "Temperate desert",
                                          "Temperate mountain",
                                          "Temperate oceanic",
                                          "Temperate steppe", 
                                          NA,
                                          "Tropical dry forest",
                                          "Tropical moist deciduous forest",
                                          "Tropical mountain systems",
                                          "Tropical rainforest",
                                          "Tropical shrublands",
                                          NA))
sort(unique(emission_factors$GEZ_name))

emission_factors$Continent[emission_factors$Continent=="Americans"] = "Americas"

# Joining all dictionaries (one dic to rule them all)

emission_factors2 = left_join(emission_factors, dictionary,by = c("GEZ_name" = "trans_tbl_gez"))

#OK, mas vai ficar com NAs nas classes que nao tem valor
      

# pegando valor das transicoes
# isso ta estranho, tem polar e tropical no mesmo role
uniqueC <- unique(emission_factors2[,c(1:5,8)])
names(uniqueC)[2] <- "transition_type"
names(uniqueC)[5] <- "C_ton_ha" 

# Input data -------------------------------------------------------------------
 
# Listing future scenarios
scens = list.dirs(path = "../data_carbono/land-use-2050/", recursive = F, full.names = F)

# mudancas 2020-2050

deltas <- list.files("../data_carbono/Deltas",recursive = F)

# for loop

# delta agri
r_agri <- rast(file.path("../data_carbono/Deltas","delta_agri",paste0("delta_agri_",scen,".tif")))
# delta past
r_past <- rast(file.path("../data_carbono/Deltas","delta_past",paste0("delta_past_",scen,".tif")))
# delta resto
r_rest <- rast(file.path("../data_carbono/Deltas","delta_rest",paste0("delta_",scen,".tif")))

# extraindo valores dos rasters

data_df <- data.frame(
   x = xyFromCell(r_agri, 1:ncell(r_agri))[,"x"],
   y = xyFromCell(r_agri, 1:ncell(r_agri))[,"y"],
  agri_value = terra::extract(r_agri, 1:ncell(r_agri)),
  past_value = terra::extract(r_past, 1:ncell(r_past)),
  rest_value = terra::extract(r_rest, 1:ncell(r_rest)),
  ipcc_climatic =terra::extract(ipcc_climatic_pj, 1:ncell(r_rest)),
  cont_div =terra::extract(cont_div_pj, 1:ncell(r_rest)),
  geo_eco =terra::extract(geo_eco_pj, 1:ncell(r_rest)),
  world_regions=terra::extract(world_regions, 1:ncell(r_rest))
)
  
  
names(data_df)[3:5] <- c("delta_agri","delta_past","delta_rest")

# 2) É possível (por conta de diminuições de outras áreas naturais) que a diminuição de área antrópica não seja igual ao aumento da floresta
# 3) nesse caso, usar regra de 3 para re-escalar as transições pasto-floresta e agicultura-floresta

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
  # qndo teve expansao de agri ou pasture, substituir por 0
  mutate(
    delta_past = if_else(delta_past > 0, 0, delta_past),
    delta_agri = if_else(delta_agri > 0, 0, delta_agri),
    delta_past = if_else(delta_agri == 0&delta_past== 0, delta_rest, delta_past)
    ) %>%
  mutate(pasture_agri = abs(delta_agri) + abs(delta_past)) %>%
  # Calculate the scaling factor based on the ratio
  mutate(
  scaling_factor = if_else(abs(delta_rest)<abs(pasture_agri),abs(delta_rest)/abs(pasture_agri),abs(pasture_agri)/abs(delta_rest)),
    scaling_factor = if_else(delta_past==0&abs(delta_rest)>abs(delta_agri),abs(delta_rest)/abs(delta_agri),scaling_factor),
    scaling_factor = if_else(delta_agri==0&abs(delta_rest)>abs(delta_past),abs(delta_rest)/abs(delta_past),scaling_factor)
    ) %>%
  # Calculate adjusted values and cap adjusted_sum at delta_rest
  mutate(
    adjusted_agri = abs(delta_agri * scaling_factor),
    adjusted_past = abs(delta_past * scaling_factor),
    adjusted_sum = abs(adjusted_agri) + abs(adjusted_past),

)
# pega os q nao deram certo e corrige

df_check <- adjusted_df%>%
  filter(round(abs(delta_rest),5)!=round(abs(adjusted_sum),5))%>%
  mutate(
    agri_f=round(abs(delta_agri)/abs(delta_rest),3),
    past_f=1-agri_f,
    adjusted_agri=abs(delta_rest)*agri_f,
    adjusted_past=abs(delta_rest)*past_f,
    adjusted_sum=adjusted_agri+adjusted_past
    )%>%
  select(1:14)

adjusted_df_f <- adjusted_df%>%
  filter(!paste0(x,y)%in%paste0(df_check$x,df_check$y))


adjusted_df_c <- rbind(adjusted_df_f,df_check)

# 4) Criar dataframe reunindo infos de zonings (acima), emissions factors (abaixo) e transições ----------------------------------------------------------

# mudando formato de wide pra long

adjusted_df_l <- pivot_longer(data = adjusted_df_c,cols =c( 12:13))%>%
  select(c(1:2,6:9,13:14))%>%
  # adicionando classe tipo de transicao
  mutate(
    transition_type=if_else(name=="adjusted_agri",class_transitions[1],class_transitions[2])
         )%>%
  left_join(meta_CLI,by = join_by(ClimateZones == code))%>%
  rename(IPCC_climate_zone=name.y  ) %>%
  left_join(meta_cont_division,by = join_by(ContinentalDiv == code))%>%
  rename(Continent=region  ) %>%
  left_join(gez[,1:2],by = join_by(GlobalEcoZones == gez_code))%>%
  rename(GEZ_name=gez_name  )%>%
  # adicionando carbono (ta dando merda uma linha tem multiplas combinacoes)
  left_join(uniqueC,by = c("GEZ_name" = "df_gez","IPCC_climate_zone","transition_type","Continent") )%>%
  #rename_at(vars(13), ~paste0("C_ton_ha"))%>%
  #limpar NAS
  filter(!is.na(ClimateZones) & !is.na(ContinentalDiv) & !is.na(GlobalEcoZones)& !is.na(world_11_regions))%>%
  mutate(C_ton_ha=as.numeric(C_ton_ha))


summary(adjusted_df_l)# oq fazer com esses NAs!

# contar nas por categorias

miss_data <- adjusted_df_l%>%
  group_by(across(c(10,11,12)))%>%
  summarize(across(starts_with("C_"), ~ sum(is.na(.))))%>%
  # so os q tem NA
  filter(C_ton_ha!=0)


# OBS: ########################################################################
# nao tem todas as combinacoes de classes na tabela de ref. rever depois!
# tem q rever os NAs tb! 
# parece q algumas classes foram simplificadas, precisaria simplificar tb, mas precisa entender pq, pra justificar! 
################################################################################

# 5) Fazer a soma total para obter o aumento de carbono sequestrado em 2050 (para conservação usar o do Plangea)
# 6) Fazer o zonal para as 11 regiões mundiais



