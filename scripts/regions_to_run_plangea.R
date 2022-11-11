# from Janssens et al

# We mainly present model results aggregated to 11 regions (Supplementary Table 4): USA, CAN, EUR, OCE, SEA, SAS, SSA, MNA, EAS, CSI and LAC.

#---- pacotes ------------------------------------------------------------------

library(giscoR)
library(countrycode)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
library(biscale) 
library(cowplot)
library(raster)
library(readxl)
#---- pacotes ------------------------------------------------------------------

world <- gisco_get_countries()

# Add the subregion

world$region <- countrycode(world$ISO3_CODE,
                            origin = "iso3c",
                            destination = "un.regionsub.name")

# regioes Janssens


paises_Regioes <- read_excel("input_tables/paises_Regioes.xlsx")


# atribuir iso code pros paises e regioes


paises_Regioes$ISO3_CODE <- countrycode(sourcevar = paises_Regioes$Country,
            origin =  'country.name' ,
            destination = c("iso3c","un.regionsub.name"))

# faltou arzebaijao

paises_Regioes$ISO3_CODE[is.na(paises_Regioes$ISO3_CODE)] <- "AZE"

world2 <- left_join(paises_Regioes,world)


# salvando limites paises


plot(st_geometry(world2))


st_write(obj = world2,"country_boundaries/country_boundaries.shp")


# agregando por regioes

world_regions <- world2%>%
  group_by(`Aggregate Region`)%>%
  summarise()


st_write(obj = world_regions,"country_boundaries/world_11regions_boundaries.shp")


# fix projection

r_ref <- raster("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2/agriculture.tif")



world_regions_pj <- st_transform(x = world_regions,crs = crs(r_ref))

# atribuindo codigos correspondentes ao raster

world_regions_pj$raster_id <- rownames(world_regions_pj)

st_write(obj = world_regions_pj,"country_boundaries/world_11regions_boundaries_pj.shp")


#convertendo em raster

world_regions_r <- rasterize(x =world_regions_pj, r_ref)

#salvando raster

writeRaster(x = world_regions_r,"country_boundaries/world_11_regions.tif")

# salvar tabela

st_geometry(world_regions_pj) <- NULL

write.csv(x = world_regions_pj,file = "country_boundaries/region_code.csv",row.names = F)
