p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas/delta_rest"

# ETL e Ta estao trocados

cenarios_com_restauracao <- rast(list.files(p,pattern = "_BIOD_",full.names = T))

scen_nms <- c("Fr","Trade-base","Tr","Ta","ETL")

plot_list <- list()

for(i in seq_along(scen_nms)){
  r <- cenarios_com_restauracao[[i]]
  r [r>0] <- 0
  r <- r*-1
  df_r <- as.data.frame(r,xy=T)%>%
    filter(sum!=0,
           !is.na(sum))
  
  mapa <- ggplot(df_r)+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
    geom_raster(aes(x = x,y = y,fill=sum))+
    #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
    scale_fill_gradient(low = "lightgray", high = "darkgreen",limits=c(0,1))+
    theme_map()+
    labs(
      title = scen_nms[i],
      x = "",
      y = "",
      fill="restoration (%)")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5)) 

  plot_list[[i]] <- mapa
  }

pannel_restoration_areas <- ggarrange(plot_list[[5]],plot_list[[4]],plot_list[[3]],plot_list[[1]],plot_list[[2]],common.legend = T,nrow=3,ncol=2)

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/restored_areas_con_scen_2050.png",pannel_restoration_areas,scale = 1,width = 16,height = 20,units = "cm")

