# Building a data frame with emissions/stock of Carbon  ------------------------
# with respect to IPCC Climatic Zones, GeoEcological Zones, Continental Division
# and 11 World Regions

# Required libraries -----------------------------------------------------------

library(terra)
library(dplyr)
library(tidyverse)
library(sf)
library(raster)
library(fasterize)

# Reading original zonings -----------------------------------------------------

# essa seção só precisa ser feita uma vez. ir direto pra de baixo

ipcc_climatic = rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/ipcc_zones_2017.tif")
plot(ipcc_climatic)
# # adjust pj
# rbase <- raster("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use/agriculture.tif")
# rbase <- rbase/rbase
# # ajustando projecao
# ipcc_climatic_pj <- project(ipcc_climatic,rbase,method="near")

cont_div <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/continents_20210725.tif")

geo_eco <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/GlobalEcoZones.tif")

world_regions = rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/subregions/world_11_regions.tif")

#-------------------------------------------------------------------------------


base_ras = 0 * world_regions

# tabelas
# bdis?gez
gez <- read.csv("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/gez_2010.csv")
#gez$gez_code <-as.double( gsub(pattern = ",",replacement = ".",x = gez$gez_code))
cont_div_df <- read.csv("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/continents_20210725.csv")

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
# ipcc_climatic_pj <- project(ipcc_climatic,base_ras,method="near")
# # sao rasters com numeros, precisa converter pra nome das classes!
# cont_div_pj <- terra::project(cont_div,base_ras,method="near")
# geo_eco_pj <- project(geo_eco,base_ras,method="near")

# lendo df base. esse aqui podemos atualizar com os novos dados da lera!

emission_factors <- read.csv("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Restoration_transition_matrix_equations_20210820.csv")

# emission_factors2 <- read.csv("../data_carbono/Restoration_transition_matrix_NatHabitats_EF_ER_Values_20210823_shared.csv")
#  

class_transitions <- unique(paste(emission_factors$Previous_lu,emission_factors$Future_lu,sep="_"))

# criar valores de EF pras classes de GEZ:

#Polar, Subtropical desert, Tropical desert, e water (!!!)

# average de tudo, menos GEZ

emissions_avg <- emission_factors%>%
  group_by(IPCC_climate_zone,Previous_lu,Future_lu,Continent)%>%
  summarise(Final_Cstock_tonnes_ha=sum(Final_Cstock_tonnes_ha,na.rm = T),
            CO2_eq=sum(CO2_eq,na.rm=T))


# Polar

# definir IPCC classes que tem polar no GEZ:

# esse objeto so eh criado la na frente
#unique(adjusted_df_l_C_NA$IPCC_climate_zone[adjusted_df_l_C_NA$GEZ_name=="Polar"])

cls_polar <- c("Polar Dry","Polar Moist","Boreal Moist","Boreal Dry","Cool Temperate Moist")

emissions_polar <- emissions_avg%>%
  filter(IPCC_climate_zone %in% cls_polar)%>%
  mutate(GEZ="Polar",
         Basis_carbon.stock_EF_RF="Adapated considering avg IPPC classes overlapping this zone")

# tropical and subtropical deserts

# esse objeto so eh criado la na frente
#unique(adjusted_df_l_C_NA$IPCC_climate_zone[adjusted_df_l_C_NA$GEZ_name=="Subtropical desert"])

cls_subtrop_desert <- c("Warm Temperate Dry","Tropical Dry" ,"Tropical Montane")

emissions_subtrop_desert <- emissions_avg%>%
  filter(IPCC_climate_zone%in% cls_subtrop_desert)%>%
  mutate(GEZ="Subtropical desert",
         Basis_carbon.stock_EF_RF="Adapated considering avg IPPC classes overlapping this zone")

# Tropical desert

# esse objeto so eh criado la na frente
#unique(adjusted_df_l_C_NA$IPCC_climate_zone[adjusted_df_l_C_NA$GEZ_name=="Tropical desert"])

cls_trop_desert <- c("Tropical Dry","Tropical Montane","Warm Temperate Dry")

emissions_trop_desert <- emissions_avg%>%
  filter(IPCC_climate_zone%in% cls_trop_desert)%>%
  mutate(GEZ="Tropical desert",
         Basis_carbon.stock_EF_RF="Adapated considering avg IPPC classes overlapping this zone")

# WATER (!!!!!!!)

# Tropical desert

# esse objeto so eh criado la na frente
#unique(adjusted_df_l_C_NA$IPCC_climate_zone[adjusted_df_l_C_NA$GEZ_name=="Water"])

cls_water <- c("Boreal Moist","Polar Moist","Cool Temperate Moist","Cool Temperate Dry","Warm Temperate Dry","Warm Temperate Moist","Tropical Dry","Tropical Montane",     "Tropical Moist")

emissions_water <- emissions_avg%>%
  filter(IPCC_climate_zone%in% cls_water)%>%
  mutate(GEZ="Water",
         Basis_carbon.stock_EF_RF="Adapated considering avg IPPC classes overlapping this zone")


# compilando tudo num novo objeto emissions

emission_factors2 <- rbind(emission_factors,emissions_polar,emissions_subtrop_desert,emissions_trop_desert,emissions_water)


# Input data -------------------------------------------------------------------
 
# Listing future scenarios

scens = list.dirs(path = "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/", recursive = F, full.names = F)

# mudancas 2020-2050

deltas <- list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas",recursive = F)


# onde nao tinha interseccao eu criei! ajustar, nao ta batendo o n. linhas!  
# esse dicionairo cria correspondencia entre a tabela da lira e o raster de gez
# como nem todos os cenarios tem todos, eu fiz fora do loop

df_gez <- c("Boreal coniferous forest","Boreal mountain system","Boreal tundra woodland","Polar",      
 "Subtropical desert","Subtropical dry forest","Subtropical humid forest","Subtropical mountain system","Subtropical steppe","Temperate continental forest","Temperate desert","Temperate mountain system","Temperate oceanic forest","Temperate steppe","Tropical desert","Tropical dry forest","Tropical moist forest","Tropical mountain system","Tropical rainforest","Tropical shrubland",   "Water" )                      


dictionary = data.frame(df_gez = df_gez,
                        trans_tbl_gez = c("Boreal coniferous",
                                          "Boreal mountain",
                                          "Boreal tundra woodland",
                                          "Polar", # adicionei (era NA)
                                          "Subtropical desert", # adicionei (era NA)
                                          "Subtropical dry forests",
                                          "Sub-tropical humid forests",
                                          "Subtropical mountain system",
                                          "Subtropical steppe",
                                          "Temperate continental",
                                          "Temperate desert",
                                          "Temperate mountain",
                                          "Temperate oceanic",
                                          "Temperate steppe",
                                          "Tropical desert", # adicionei (era NA)
                                          "Tropical dry forest",
                                          "Tropical moist deciduous forest",
                                          "Tropical mountain systems",
                                          "Tropical rainforest",
                                          "Tropical shrublands",
                                          "Water"))



# for loop

for(scen in scens)  {
  
  # delta agri
  r_agri <- rast(file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas","delta_agri",paste0("delta_agri_",scen,".tif")))
  # delta past
  r_past <- rast(file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas","delta_past",paste0("delta_past_",scen,".tif")))
  # delta resto
  r_rest <- rast(file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas","delta_rest",paste0("delta_rest_",scen,".tif")))
  
  # extraindo valores dos rasters
  
  data_df <- data.frame(
     x = xyFromCell(r_agri, 1:ncell(r_agri))[,"x"],
     y = xyFromCell(r_agri, 1:ncell(r_agri))[,"y"],
    agri_value = terra::extract(r_agri, 1:ncell(r_agri)),
    past_value = terra::extract(r_past, 1:ncell(r_past)),
    rest_value = terra::extract(r_rest, 1:ncell(r_rest)),
    ipcc_climatic =terra::extract(ipcc_climatic, 1:ncell(r_rest)),
    cont_div =terra::extract(cont_div, 1:ncell(r_rest)),
    geo_eco =terra::extract(geo_eco, 1:ncell(r_rest)),
    world_regions=terra::extract(world_regions, 1:ncell(r_rest))
  )
    
    
  names(data_df)[3:5] <- c("delta_agri","delta_past","delta_rest")
  
  # 2) É possível (por conta de diminuições de outras áreas naturais) que a diminuição de área antrópica não seja igual ao aumento da floresta
  # 3) nesse caso, usar regra de 3 para re-escalar as transições pasto-floresta e agicultura-floresta
  
  adjusted_df <- data_df %>%
    # eliminando NAs
    filter(!is.na(delta_agri) & !is.na(delta_past) & !is.na(delta_rest)& !is.na(ipcc_zones_2017)& !is.na(continents_20210725)& !is.na(GlobalEcoZones)& !is.na(world_11_regions)) %>%
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
    dplyr::select(1:14)
  
  adjusted_df_f <- adjusted_df%>%
    filter(!paste0(x,y)%in%paste0(df_check$x,df_check$y))
  
  
  adjusted_df_c <- rbind(adjusted_df_f,df_check)
  
  adjusted_df_c <- as.data.frame(adjusted_df_c)
  
  # 4) Criar dataframe reunindo infos de zonings (acima), emissions factors (abaixo) e transições ----------------------------------------------------------
  
  # mudando formato de wide pra long
  
  adjusted_df_l <- pivot_longer(data = adjusted_df_c,cols =c( 12:13))%>%
    dplyr::select(c(1:2,6:9,13:14))%>%
    # adicionando classe tipo de transicao
    mutate(
      transition_type=if_else(name=="adjusted_agri",class_transitions[1],class_transitions[2])
           )%>%
    left_join(meta_CLI,by = join_by(ipcc_zones_2017 == code))%>%
    rename(IPCC_climate_zone=name.y  ) %>%
    left_join(meta_cont_division,by = join_by(continents_20210725 == code))%>%
    rename(Continent=region  ) %>%
    left_join(gez[,1:2],by = join_by(GlobalEcoZones == gez_code))%>%
    rename(GEZ_name=gez_name  )
    
  
  # onde nao tinha interseccao eu criei! ajustar, nao ta batendo o n. linhas!  
  # dictionary = data.frame(df_gez = sort(unique(adjusted_df_l$GEZ_name)),
  #                           trans_tbl_gez = c("Boreal coniferous",
  #                                             "Boreal mountain",
  #                                             "Boreal tundra woodland",
  #                                             "Polar", # adicionei (era NA)
  #                                             "Subtropical desert", # adicionei (era NA)
  #                                             "Subtropical dry forests",
  #                                             "Sub-tropical humid forests",
  #                                             "Subtropical mountain system",
  #                                             "Subtropical steppe",
  #                                             "Temperate continental",
  #                                             "Temperate desert",
  #                                             "Temperate mountain",
  #                                             "Temperate oceanic",
  #                                             "Temperate steppe",
  #                                             "Tropical desert", # adicionei (era NA)
  #                                             "Tropical dry forest",
  #                                             "Tropical moist deciduous forest",
  #                                             "Tropical mountain systems",
  #                                             "Tropical rainforest",
  #                                             "Tropical shrublands",
  #                                             "Water"))
  # 
emission_factors2$Continent[emission_factors2$Continent=="Americans"] = "Americas"
  
  # Joining all dictionaries (one dic to rule them all)
  
  emission_factors3 = left_join(emission_factors2, dictionary,by = c("GEZ" = "trans_tbl_gez"))
  
  emission_factors3$transition_type <- paste(emission_factors3$Previous_lu,emission_factors3$Future_lu,sep = "_")
  
  # pegando valor das transicoes
  
  uniqueC <- unique(emission_factors3[,c(1:7,9:10)])
  #names(uniqueC)[2] <- "transition_type"
  #names(uniqueC)[5] <- "C_ton_ha" 
  
    
    
  adjusted_df_l_C <- adjusted_df_l%>%
  # adicionando carbono (ta dando merda uma linha tem multiplas combinacoes)
    left_join(uniqueC,by = c("GEZ_name" = "df_gez","IPCC_climate_zone","transition_type","Continent") )#%>%
  #    left_join(uniqueC)%>%
    #rename_at(vars(13), ~paste0("C_ton_ha"))%>%
    #limpar NAS
    #filter(!is.na(ClimateZones) & !is.na(ContinentalDiv) & !is.na(GlobalEcoZones)& !is.na(world_11_regions))%>%
    #mutate(C_ton_ha=as.numeric(C_ton_ha))
  
  
  # metade das areas nao tem combinacao possivel segundo a tabela
  
  adjusted_df_l_C_noNA <- adjusted_df_l_C[complete.cases(adjusted_df_l_C),]
  adjusted_df_l_C_NA <- adjusted_df_l_C[!complete.cases(adjusted_df_l_C),]
  
  # varias combinacoes nao tem fator emissao associado
  
  relacoes_semfe <- as.data.frame(table(adjusted_df_l_C_NA$Continent,adjusted_df_l_C_NA$GEZ_name,adjusted_df_l_C_NA$transition_type))%>%
    filter(Freq!=0)
  
  names(relacoes_semfe) <- c("Continent","GEZ_name","Freq")
  
  # calcular medias de emissao pra essas classes tb!
  
  
  emissions_avg <- emission_factors3%>%
    group_by(IPCC_climate_zone,Previous_lu,Future_lu,df_gez,transition_type,GEZ)%>%
    summarise(Final_Cstock_tonnes_ha=sum(Final_Cstock_tonnes_ha,na.rm = T),
              CO2_eq=sum(CO2_eq,na.rm=T))
  
  
  # combinar com dados sem valor
  
  adjusted_df_l_C_NA_filled <- left_join(adjusted_df_l_C_NA[,-c(13:17)],emissions_avg,by=join_by("IPCC_climate_zone","transition_type","GEZ_name"=="df_gez"))
  
  # juntar
  
  adjusted_df_l_C_combined <- rbind(adjusted_df_l_C_noNA,adjusted_df_l_C_NA_filled)
  
  
  # calcular area restaurada!
  
  # value eh a proporcao do pixel restaurado
  
  area <- (50100*61800)/10^4
  
  adjusted_df_l_C_combined$Cstock_emmited <-( adjusted_df_l_C_combined$value* area)*adjusted_df_l_C_combined$Final_Cstock_tonnes_ha
  
  adjusted_df_l_C_combined$CO2_emmited <-( adjusted_df_l_C_combined$value* area)*adjusted_df_l_C_combined$CO2_eq
  
  # espacializar!!
  
  df_vect <- vect(adjusted_df_l_C_combined,geom=c("x", "y"))
  
  carbon_emissions_IPCC <- rasterize(df_vect,y = base_ras,field="Cstock_emmited")
  CO2eq_IPCC <- rasterize(df_vect,y = base_ras,field="CO2_eq")
  plot(CO2eq_IPCC)
  # salvando tabela e raster
  
  dest <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/carbon_balance_IPCC",scen)
  
  dir.create(dest)
  
  write.csv(adjusted_df_l_C_combined,file.path(dest,paste0(scen,"_","carbon_balance.csv")),row.names = F)
  
  writeRaster(carbon_emissions_IPCC,file.path(dest,paste0(scen,"_","carbon_balance.tif")),overwrite=T)
  
  writeRaster(CO2eq_IPCC,file.path(dest,paste0(scen,"_","CO2eq_balance.tif")),overwrite=T)
  
}


