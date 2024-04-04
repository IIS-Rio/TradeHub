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

# layer carbono

cb <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/CBD-carbon_layer_updated_reproj.tif")

lst_df <- list()
c=1

for(l in lus){
  for(r in regioes){
    for(s in scenarios){
      lu <- rast(file.path(p,paste0("delta_",l),r,paste0("delta_",l,"_",s)))
      # selecionar apenas oq teve conversao
      lu[lu<=0] <- 0
      # transformar em area
      lu_area <- lu*area
      # multiplicar por carbono/ha
      cb_emssion <- cb*lu_area
      # eliminar valores negativos
      sm_cb <- sum(cb_emssion[],na.rm=T)
      scen <- gsub(".tif","",s)
      df <- data.frame(cb_emission=sm_cb,scenario=scen,region=r)  
      # guardar resultado
      lst_df[[c]] <- df
      c= c + 1
      
                      
      }
                  
    }
       
  }

cb_emssion_df <- do.call(rbind,lst_df)%>%
  # agrupar pastagem e agricultura
  group_by(scenario,region)%>%
  summarise(cb_emission=sum(cb_emission))


# completando tabela

cb_restored <- fread("/dados/pessoal/francisco/TradeHub/output_tables/updated_results/carbon_sequetered_restoration_Naturemap.csv")


names(cb_emssion_df)[2] <- "AggrgtR"

cb_emssion_df2 <- left_join(cb_emssion_df,cb_restored[,c(-3,-4)])


# pq tem metade das linhas???

write.csv(cb_emssion_df2,"/dados/pessoal/francisco/TradeHub/output_tables/updated_results/carbon_emmited_carbonlayerIIS.csv")
