# pacotes ----------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(sf)
#-------------------------------------------------------------------------------

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/carbon_balance_IPCC"

lst_tbls <- list.files(p,pattern = ".csv",recursive = T,full.names = T)
scen_nms <- list.files(p,recursive = F,full.names = F)

tbls <- lapply(lst_tbls,fread)
# adicionar cenarios
for(i in seq_along(scen_nms)){
  tbls[[i]]$scenario=scen_nms[i]
  
}

df <- do.call(rbind,tbls)

cb_agg <- df%>%
  group_by(world_11_regions,scenario)%>%
  summarise(Cstock_emmited=sum(Cstock_emmited),
            CO2_emmited=sum(CO2_emmited))

# adicionando nome regioes e cenarios

df_dictionary <- fread("/dados/pessoal/francisco/TradeHub/output_tables/resultados_cenarios_regional_analysis.csv")

#,df_dictionary$label_scen,df_dictionary$conservation
dictionary_scen <- unique(df_dictionary[,c(2,6,7)])

regions <- st_read("/dados/pessoal/francisco/TradeHub/country_boundaries/world_11regions_boundaries_pj.shp")%>%
  st_drop_geometry()%>%
  rename(world_11_regions=rastr_d)%>%
  mutate(world_11_regions=as.integer(world_11_regions))

cb_agg2 <- left_join(cb_agg,regions)%>%
  left_join(dictionary_scen)

# eh so carbono relacionado a restauracao. so vale pra area restaurada
# relembrar oq fazer com as areas convertidas
unique(df$Previous_lu)
unique(df$Future_lu)


write.csv(cb_agg2,"/dados/pessoal/francisco/TradeHub/output_tables/updated_results/carbon_sequetered_restoration_Naturemap.csv",row.names = F)

# pra emissao, usar os valores existentes!! ou seja, pegar o mapa de carbono e multiplicar por pastagem e agri!