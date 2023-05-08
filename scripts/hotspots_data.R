# layer biodiv hotspots

# https://zenodo.org/record/3261807#.ZFQc2nbMJPY


url <- "https://zenodo.org/record/3261807/files/hotspots_2016_1.zip?download=1"

download.file(url,destfile = "/dados/pessoal/francisco/TradeHub/external_data/hotspots.zip")

unzip("/dados/pessoal/francisco/TradeHub/external_data/hotspots.zip",exdir ="/dados/pessoal/francisco/TradeHub/external_data" )

library(sf)
hp <- st_read("/dados/pessoal/francisco/TradeHub/external_data/hotspots_2016_1.shp")

