library(terra)
library(data.table)
library(dplyr)
# cruzando area restaurada com oc


# custo oportunidade

oc <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/CBD-opportunity_cost_reproj.tif")

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas/delta_regional/delta_rest"

regions <- list.files(p)
area <- (50100*61800)/10^6
c=1
ls_df <- list()
for(reg in regions){

  # listando delta lu
  ls <- list.files(file.path(p,reg),full.names = T)
  
  for(i in seq_along(ls)){
    r <- rast(ls[[i]])
    # so area restaurada
    r[r<0] <- 0
    # convertendo em ha
    r_km <- r*area
    # cruzando valores
    CxR <- oc*r_km
    total <- sum(CxR[],na.rm=T)
    total_rest <- sum(r_km[],na.rm=T)
    scn_nm <- gsub(pattern = "delta_rest_",replacement = "",gsub(".tif","",x=basename(ls[[i]])))

    df <- data.frame(oc=total,region=reg,scenario=scn_nm,area_rest_km2=total_rest)
    ls_df[[c]] <- df

    c=c+1
  }
  
  
}

df_final <- do.call(rbind,ls_df)

# completando df

# adicionando nome regioes e cenarios

df_dictionary <- fread("/dados/pessoal/francisco/TradeHub/output_tables/resultados_cenarios_regional_analysis.csv")

#,df_dictionary$label_scen,df_dictionary$conservation
dictionary_scen <- unique(df_dictionary[,c(2,6,7)])

df_final <- left_join(df_final,dictionary_scen)

write.csv(df_final,"/dados/pessoal/francisco/TradeHub/output_tables/updated_results/restoration_oc.csv",row.names = F)
