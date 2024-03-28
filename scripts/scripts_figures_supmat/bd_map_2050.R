p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/future_biodiv_index_rasters"

scens <- list.files(p,pattern = "bd_",full.names = T)

# ETL e Ta estao trocados

scens_C <- rast(grep(pattern = "_BIOD_",x = scens,value = T))
scens_trade <- rast(grep(pattern = "_BIOD_",x = scens,value = T,invert = T))

scen_nms_C <- c("Fr","BAU","Tr","Ta","ETL")
scen_nms <- c("Fr","BAU","Tr","ETL","Ta")


r <- scens_trade[[1]]

# limites globo

data(wrld_simpl)

# excluding Antartica

wrld_simpl <- wrld_simpl[-which(wrld_simpl$NAME == "Antarctica"),]

# projetando

wrld_transf <- spTransform(wrld_simpl, crs(r))

plot_list_base <- list()

for(i in seq_along(scen_nms)){
  
  bd <- scens_trade[[i]]
  
  bd_df <- as.data.frame(bd,xy=TRUE)
  
  names(bd_df)[3] <- "bd"
  
  bd_df <- filter(bd_df,bd!=0,
                  !is.na(bd))
  
  bd_df$trans <- asinh(bd_df$bd)
  
  bd_map <- bd_df%>%
    filter(!is.na(bd))%>%
    mutate(scaled_bd = trans/abs(min(trans)))%>%
    ggplot()+
    geom_raster(aes(x = x,y = y,fill=scaled_bd))+
    scale_fill_viridis(option="turbo","bd")+
    #ggtecle(scen[i])+
    theme_map()+
    labs(
      title = scen_nms[i],
      x = "",
      y = "",
      fill="bd")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5)) 
  
  plot_list_base[[i]] <- bd_map

  
  }



pannel_converted_areas_base <- ggarrange(plot_list_base[[4]],plot_list_base[[5]],plot_list_base[[3]],plot_list_base[[1]],plot_list_base[[2]],common.legend = T,nrow=3,ncol=2)

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/converted_areas_base.png",pannel_converted_areas_base,scale = 1,width = 16,height = 20,units = "cm")


# cenarios com C

plot_list_C <- list()

for(i in seq_along(scen_nms_C)){
  bd <- scens_C[[i]]
  
  bd_df <- as.data.frame(bd,xy=TRUE)
  
  names(bd_df)[3] <- "bd"
  
  bd_df <- filter(bd_df,bd!=0,
                  !is.na(bd))
  
  bd_df$trans <- asinh(bd_df$bd)
  
  bd_map_C <- bd_df%>%
    filter(!is.na(bd))%>%
    mutate(scaled_bd = trans/abs(min(trans)))%>%
    ggplot()+
    geom_raster(aes(x = x,y = y,fill=scaled_bd))+
    scale_fill_viridis(option="turbo","bd")+
    #ggtecle(scen[i])+
    theme_map()+
    labs(
      title = scen_nms_C[i],
      x = "",
      y = "",
      fill="bd")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5)) 
  
  plot_list_C[[i]] <- bd_map_C
}

pannel_converted_areas_C <- ggarrange(plot_list_C[[5]],plot_list_C[[4]],plot_list_C[[3]],plot_list_C[[1]],plot_list_C[[2]],common.legend = T,nrow=3,ncol=2)

# rever. ta estranho pq tem mto mais conversao no com conservacao, acho q ta invertido! negativo sendo conversao e positivo restauracao!

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/converted_areas_C.png",pannel_converted_areas_C,scale = 1,width = 16,height = 20,units = "cm")

