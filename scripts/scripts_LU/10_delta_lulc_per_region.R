# pacotes ----------------------------------------------------------------------

library(terra)
library(data.table)
library(tidyverse)
#-------------------------------------------------------------------------------

# cruzar areas de conversao com layer de carbono

# caminho deltas

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas/delta_regional"

# usos

lus <- c("agri","past")

# regioes

regioes <- list.files(file.path(p,"delta_agri"))

# scenarios

scenarios <- gsub(pattern = "delta_agri_","",x = list.files(file.path(p,"delta_agri","CAN")))

area <-( 50100*61800)/10^4

# # layer carbono
# 
# cb <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/CBD-carbon_layer_updated_reproj.tif")

lst_df <- list()
c=1

for(l in lus){
  for(r in regioes){
    for(s in scenarios){
      lu <- rast(file.path(p,paste0("delta_",l),r,paste0("delta_",l,"_",s)))
      # selecionar apenas oq teve conversao
      conv=lu
      conv[conv<=0] <- 0
      # area restaurada
      rest=lu
      rest[rest>=0] <- 0
      # transformar em area
      conv_area <- conv*area
      rest_area <- rest*area
      sm_conv <- sum(conv_area[],na.rm=T)
      sm_rest <- sum(rest_area[],na.rm=T)
      scen <- gsub(".tif","",s)
      df <- data.frame(area_convertida=sm_conv,area_restaurada=sm_rest,scenario=scen,region=r,lu=l)  
      # guardar resultado
      lst_df[[c]] <- df
      c= c + 1
      
      
    }
    
  }
  
}

lulcc_df <- do.call(rbind,lst_df)%>%
  # agrupar pastagem e agricultura
  group_by(scenario,region)%>%
  summarise(area_convertida=sum(area_convertida,na.rm = T),
            area_restaurada=sum(area_restaurada,na.rm = T))


# completando tabela

cb_restored <- fread("/dados/pessoal/francisco/TradeHub/output_tables/updated_results/carbon_sequetered_restoration_Naturemap.csv")


names(lulcc_df)[2] <- "AggrgtR"

lulcc_df2 <- left_join(lulcc_df,cb_restored[,c(2,5,6,7)])


# pq tem metade das linhas???

write.csv(lulcc_df2,"/dados/pessoal/francisco/TradeHub/output_tables/updated_results/lulcc.csv")
