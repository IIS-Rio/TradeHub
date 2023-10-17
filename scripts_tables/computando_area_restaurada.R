# calcular area restaurada em cada cenario a partir do raster original enviado

#---- pacotes ------------------------------------------------------------------

library(raster)

#-------------------------------------------------------------------------------

# caminho pros usos

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses"

cen <- list.dirs(p,recursive = F)

# pegar nome simples dos cenarios 

cen_names <- list.files(p,recursive = F)

# pra cada cenario, abrir a pasta restored, com areas restauradas

area_restaurada <- list() # lista vazia pra guardar resultados

for(i in 1:length(cen)){
  path_restored <- file.path(cen[[i]],"restored/")
  
  # abrindo stack (so quero 2050)
  
  r <- stack(list.files(path_restored,full.names = T))[[5]]
  
  # convertendo pra area
  
  area <- r* (50100*61800)/10^6 # calculando area em km2
  
  # somando area total restaurada no cenario
  
  total <- cellStats(area, sum)
  
  # extrair nome
  
  gsub(pattern = "TradeHubTrack1Prelim.LCproj.GLOBIOM.","",x = names(r))
  
  # criar tabela
  
  total_df <- data.frame(scen= cen_names[[i]],restored_km2=total)
  
  # guardar numa lista
  
  area_restaurada[[i]] <- total_df


}

# unlist

area_restaurada_df <- do.call(rbind,area_restaurada)

# inserindo nome do cenario

nome_legivel <- c("Fr+C",
                  "Fr+BTC-base",
                  "Trade-base+C",
                  "Trade-base+IAP",
                  "BAU",
                  "Tr+C",
                  "Tr+BTC-base",
                  "Ta+C",
                  "Ta+BTC-base",
                  "ETL+C",
                  "ETL+IAP",
                  "ETL+BTC-base"
                  )


area_restaurada_df$scen_name <- nome_legivel

# salvando


write.csv(x = area_restaurada_df,"output_tables/restored_area.csv",row.names=F)

