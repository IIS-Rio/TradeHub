# pra rodar o plangea por regiao, vou recortar o land use atual em cada umas das regioes. 

#---- pacotes ------------------------------------------------------------------

library(sf)
library(raster)
library(dplyr)

#---- 1: criar a estrutura de pastas necessarias -------------------------------

# caminho pros land uses com mascara

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/"

dir.create(file.path(p,"land-use-regional"))

# uma pasta por regiao

regioes <- read.csv("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/subregions/region_code.csv")

f <- function(x)dir.create(file.path(p,x))

lapply(regioes$region,f)

#---- recortar land use usando mascara regional --------------------------------

# shape das regioes
regions <- st_read(file.path("/dados/pessoal/francisco/TradeHub/country_boundaries","world_11regions_boundaries_pj.shp"))


# listando land-uses

lu <- list.files(file.path(p,"land-use"))

# listando full paths

lu_f <- list.files(file.path(p,"land-use"),full.names = T)

# recortando por lu e regiao 

for(i in 1:length(lu)){
  # abrindo raster de lu
  r <- raster(lu_f[i])
  # subset shapefile com as regioes
  for(reg in regioes$region){
    regiao <- filter(.data = regions,AggrgtR==reg)
    # mask
    r_m <- mask(x = r,regiao)
    # salvando na pasta
    writeRaster(r_m,file.path(p,"land-use-regional",reg,lu[i]))
    }
}


