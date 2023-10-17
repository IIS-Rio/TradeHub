# baixei os fatores de emissao pra cropland e grassland do site:
# 
# https://www.ipcc-nggip.iges.or.jp/EFDB/find_ef.php?ipcc_code=3.B.1.b.ii&ipcc_level=4

from_cropland <- read.csv("../data_carbono/IPCC/cropland_forest.csv",sep = ";")

# dados sao baguncados , dificil de mexer

# head(from_cropland)
# names(from_cropland)
# unique(from_cropland$)