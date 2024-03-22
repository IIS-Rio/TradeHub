library(terra)

r <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/ecoregions_2017_1000m_moll_resampled_50km.tif")

ec <- st_read("/dados/bd_iis/ecoregions_esa_2017/Ecoregions2017_moll.shp")%>%
  st_transform(crs(r))

ec_df <- st_drop_geometry(ec)%>%
  group_by(BIOME_NAME,BIOME_NUM)%>%
  summarise(area=sum(SHAPE_AREA))

ec_df$BIOME_NUM <- as.character(ec_df$BIOME_NUM)

ec_df <- ec_df[ec_df$BIOME_NAME!="N/A",]

# limites globo

data(wrld_simpl)

# excluding Antartica

wrld_simpl <- wrld_simpl[-which(wrld_simpl$NAME == "Antarctica"),]

# projetando

wrld_transf <- spTransform(wrld_simpl, crs(r))

# 11 regions

regions <- st_read(file.path("/dados/pessoal/francisco/TradeHub/country_boundaries","world_11regions_boundaries.shp"))

regions_pj <-st_transform(regions,crs = crs(r))
regions_pj <- st_simplify(regions_pj)


length(unique(terra::va(r)))

# df pra plotar
eco_df <- as.data.frame(r, xy=TRUE)
names(eco_df)[3] <- "ecoregions"
eco_df$ecoregions <- as.character(eco_df$ecoregions)

# juntando nomes

eco_df2 <- eco_df%>%
  filter(!is.na(ecoregions),
         ecoregions!=0)

eco_df2 <- left_join(eco_df2,ec_df,by=c("ecoregions"="BIOME_NUM"))


color_palette <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
                   "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030")


map <- eco_df2%>%
  ggplot()+
  geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
  geom_raster(aes(x = x,y = y,fill=BIOME_NAME))+
  #scale_fill_manual(values = terrain.colors(n = 16))+
  scale_fill_manual(values = color_palette) +
  # add hotspots limits
  geom_sf(data = regions_pj, fill = "transparent", color = "black") + # Add the hotspots as hollow polygons with dotted black lines
#  scale_fill_viridis(option="inferno","agriculture_expansion",direction = -1)+
  labs(
    title = "",
    x = "",
    y = "",
    fill=""
    
  )+
  geom_sf_text(data = regions_pj,aes(label = AggrgtR),color="white", size = 2.5,) +
  theme_map()+
#  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme( legend.position = "bottom",
         legend.text = element_text(size=4.5),
         legend.box = "horizontal",)+
  guides(fill = guide_legend(ncol = 3)) 





ggsave(plot = map,"/dados/pessoal/francisco/TradeHub/fig_sup/ecoregions.png",width = 16,height = 14,units = "cm",scale = 1)
