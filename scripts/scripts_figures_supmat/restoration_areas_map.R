p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Deltas/delta_rest"

# fazer com dados originais!

# ETL e Ta estao trocados

cenarios_com_restauracao <- rast(list.files(p,pattern = "_BIOD_",full.names = T))

scen_nms <- c("Fr","Trade-base","Tr","Ta","ETL")

# limites globo

data(wrld_simpl)

# excluding Antartica

wrld_simpl <- wrld_simpl[-which(wrld_simpl$NAME == "Antarctica"),]

# projetando

wrld_transf <- spTransform(wrld_simpl, crs(cenarios_com_restauracao[[1]]))




plot_list <- list()

for(i in seq_along(scen_nms)){
  r <- cenarios_com_restauracao[[i]]
  r [r<0] <- 0
  #r <- r*-1
  df_r <- as.data.frame(r,xy=T)%>%
    mutate(sum=round(sum,3))%>%
    filter(sum!=0,
           !is.na(sum),
           sum>=0.001)
  
  mapa <- ggplot(df_r)+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+  
    geom_raster(aes(x = x,y = y,fill=sum))+
    #geom_sf(data = regions_pj, fill = "transparent", color = "black") +  
    scale_fill_gradient(low = "lightgreen", high = "darkgreen",limits=c(0,1))+
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



rest <- rast("/dados/projetos_andamento/TRADEhub/GLOBIOM/atualizacao/scen_desagregados/restored_6/TradeHubTrack1Prelim-LCproj-GLOBIOM-TH_TFELIM_TCREDU_BIOD_NOTECH_NODEM_SPA0_SSP2-24May2022_FinalMask_55x55km_restored_6.tif")

plot(rest[[5]])
plot(cenarios_com_restauracao[[4]]>0.01)
