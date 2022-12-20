
library(dplyr)

lu <- read.csv("output_tables/resultados_lu_change_cenarios.csv")

# agregando  net change

lu_agg <- lu %>%
  filter(name=="AGR"|name=="PAS")%>%
  group_by(label_scen)%>%
  summarise(net_lu_change_km=sum(value))
 
write.csv(lu_agg, "output_tables/total_lu_converted.csv",row.names = F)
