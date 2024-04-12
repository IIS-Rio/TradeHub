p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas/delta_rest"

# ETL e Ta estao trocados

scens <- rast(list.files(p,pattern = "_NOBIOD_",full.names = T))
scens_C <- rast(list.files(p,pattern = "_BIOD_",full.names = T))


scen_nms <- c("Fr","BAU","Tr","ETL","Ta")
scen_nms_C <- c("Fr","Trade-base","Tr","Ta","ETL")

r <- scens[[1]]

# limites globo

data(wrld_simpl)

# excluding Antartica

wrld_simpl <- wrld_simpl[-which(wrld_simpl$NAME == "Antarctica"),]

# projetando

wrld_transf <- spTransform(wrld_simpl, crs(r))


#terrain_pal <- (terrain.colors(20))

terrain_pal <- c( "#E7D217", "#E8C32E", "#E9B846", "#EBB25E",
"#ECB176", "#EDB48E" ,"#EEBCA7", "#F0C9C0", "#F1DBD9" ,"#F2F2F2")


# Define the start color, middle color (yellow), and end color
start_color <- "#F2F2F2"  # Light gray
middle_color <- "#FFFF00" # Yellow
end_color <- "#FF0000"    # Red

# Define the number of colors in the gradient palette
num_colors <- 10

# Generate the gradient palette function
gradient_palette <- colorRampPalette(c(start_color, middle_color, end_color))

# Generate the gradient palette with specified number of colors
gradient_colors <- gradient_palette(num_colors)

# Output the generated gradient colors

plot_list_base <- list()

for(i in seq_along(scen_nms)){
  r <- scens[[i]]
  r [r>0] <- 0
  r <- r*-1
  df_r <- as.data.frame(r,xy=T)%>%
    mutate(sum=round(sum,3))%>%
    filter(sum!=0,
           !is.na(sum))
  
  mapa <- ggplot(df_r)+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
    geom_raster(aes(x = x,y = y,fill=sum))+
    #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
    #scale_fill_gradient(low = "orange", high = "darkred",limits=c(0,1))+
    #scale_fill_viridis_c(option ="inferno",limits=c(0,1))+
    scale_fill_gradientn(colours = gradient_colors)+
    theme_map()+
    labs(
      title = scen_nms[i],
      x = "",
      y = "",
      fill="converted (%)")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5)) 
  
  plot_list_base[[i]] <- mapa
}


pannel_converted_areas_base <- ggarrange(plot_list_base[[4]],plot_list_base[[5]],plot_list_base[[3]],plot_list_base[[1]],plot_list_base[[2]],common.legend = T,nrow=3,ncol=2)

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/converted_areas_base.png",pannel_converted_areas_base,scale = 1,width = 16,height = 20,units = "cm")


# cenarios com C

plot_list_C <- list()

for(i in seq_along(scen_nms_C)){
  r <- scens_C[[i]]
  r [r>0] <- 0
  r <- r*-1
  df_r <- as.data.frame(r,xy=T)%>%
    mutate(sum=round(sum,3))%>%
    filter(sum!=0,
           !is.na(sum))
  
  mapa <- ggplot(df_r)+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
    geom_raster(aes(x = x,y = y,fill=sum))+
    #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
    #scale_fill_gradient(low = "orange", high = "darkred",limits=c(0,1))+
    #scale_fill_viridis_c(option ="inferno",limits=c(0,1))+
    scale_fill_gradientn(colours = gradient_colors)+
    theme_map()+
    labs(
      title = scen_nms_C[i],
      x = "",
      y = "",
      fill="converted (%)")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5)) 
  
  plot_list_C[[i]] <- mapa
}

pannel_converted_areas_C <- ggarrange(plot_list_C[[5]],plot_list_C[[4]],plot_list_C[[3]],plot_list_C[[1]],plot_list_C[[2]],common.legend = T,nrow=3,ncol=2)

# rever. ta estranho pq tem mto mais conversao no com conservacao, acho q ta invertido! negativo sendo conversao e positivo restauracao!

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/converted_areas_C.png",pannel_converted_areas_C,scale = 1,width = 16,height = 20,units = "cm")

