library(raster)
library(stringr)

# criando os rasters de lu pro plangea, pra 2050

# caminho land uses

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses"

# usos que tiveram que ser "juntados"

lu <- paste0(c("desert","grassland","rock_ice","shrubland","wetland","other_natland"),"_final")

# usos restantes

lu_2 <- c("agriculture","forest","pasture","urban")

lu_final <-c(lu,lu_2)

# listando pastas com caminho completo

l_r <- list.files(file.path(p,lu_final),recursive = T,full.names = T)

# listando raster so com nomes

l_r_names <- list.files(file.path(p,lu_final),recursive = T)

# listando so land uses

l_r_lu <- list.files(file.path(p),pattern = paste(lu_final,collapse = "|"))

# exportando ano 2050
i=1

p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea"

dir.create(file.path(p2,"land_uses_2050"))



for( i in 1:length(l_r)){
  # abrindo raster
  r <- raster(l_r[i],band=5)
  # criando diretorio
  dir.create(file.path(p2,"land_uses_2050",l_r_lu[i]))
  save_path <- file.path(p2,"land_uses_2050",l_r_lu[i],l_r_names[i])
  writeRaster(r,filename = save_path)
  
}





