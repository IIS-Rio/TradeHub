# plot regional map

regions <- st_read(file.path("/dados/pessoal/francisco/TradeHub/country_boundaries","world_11regions_boundaries.shp"))

regions_points<- st_centroid(regions)
regions_points <- cbind(regions, st_coordinates(st_centroid(regions$geometry)))


countries <-  st_read(file.path("/dados/pessoal/francisco/TradeHub/country_boundaries","country_boundaries.shp"))


regi_map <- countries %>%
  mutate(name = factor(region, levels=rev(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))) %>%
  ggplot() +
    geom_sf(color = "black",aes(fill = AggrgtR))+
    theme_map()+
    #scale_fill_brewer(NULL,palette = 'RdYlGn', direction = -1)+
    scale_fill_brewer(palette = "Set3", name="")+
    #geom_text(data =regions_points ,aes(x=X, y=Y, label=AggrgtR))+
    geom_label(data = regions_points,aes(x = X, y = Y, label = AggrgtR),
               hjust = 0, nudge_x = -2, nudge_y = 0,
               size = 2, color = "black", fontface = "bold")+
    theme(legend.position = "none")


ggsave(filename = "figures_paper/region_map.jpeg",width = 22,height = 12,units = "cm",plot = regi_map,bg ="white")