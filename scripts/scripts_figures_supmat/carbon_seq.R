
p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/carbon_balance_IPCC"

scens <- list.files(p,pattern = "CO2eq",full.names = T,recursive = T)

# esses rasters de IT nao sao o delta, mas sim o It futuro. O delta seria o presente - o futuro

scens_C <- rast(grep(pattern = "_BIOD_",x = scens,value = T))
scens_trade <- rast(grep(pattern = "_BIOD_",x = scens,value = T,invert = T))

scen_nms_C <- c("Fr","Trade-base","Tr","Ta","ETL")
scen_nms <- c("Fr","BAU","Tr","Ta","ETL")


data(wrld_simpl)

# excluding Antartica

wrld_simpl <- wrld_simpl[-which(wrld_simpl$NAME == "Antarctica"),]

# projetando

wrld_transf <- spTransform(wrld_simpl, crs(scens_trade[[1]]))





# legenda

CO_squared <- expression(CO^2)

not1000 <- expression(10^3)


scientific_notation <- expression(CO_squared * tons * not1000)

pal <- terrain.colors(10)
plot_list_base <- list()

for(i in seq_along(scen_nms)){
  
  cb <- scens_trade[[i]]
  
  cb_df <- as.data.frame(cb,xy=TRUE)
  
  names(cb_df)[3] <- "cb"
  
  cb_df <- filter(cb_df,!is.na(cb))
  
  # dividingo por 1000
  
  cb_df$cb <- round(cb_df$cb/1000,2)
  
  cb_df$cb_cat <- discretize(cb_df$cb,method = "frequency",breaks =10 )
  
  cb_map <- cb_df%>%
    ggplot()+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
    geom_raster(aes(x = x,y = y,fill=cb_cat))+
    #scale_fill_viridis(option="turbo","it",direction =  -1,limits=c(-0.7,-0.5))+
    #scale_fill_viridis_d(option="turbo","bd",direction = -1)+
    scale_fill_manual(values=pal)+
    #ggtitle(scen[i])+
    theme_map()+
    labs(
      title = scen_nms[i],
      x = "",
      y = "",
      fill=expression(CO^2~tons~10^3))+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5))
  
  
  plot_list_base[[i]] <- cb_map
  
  
}

pannel_carbon_seq <- ggarrange(plot_list_base[[5]],plot_list_base[[4]],plot_list_base[[3]],plot_list_base[[1]],plot_list_base[[2]],nrow=3,ncol=2,common.legend = T)

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/carbon_seq_trade_scens.png",pannel_carbon_seq ,scale = 1,width = 18,height = 20,units = "cm")


# cenarios conservacao

plot_list_C <- list()

for(i in seq_along(scen_nms_C)){
  
  cb <- scens_C[[i]]
  
  cb_df <- as.data.frame(cb,xy=TRUE)
  
  names(cb_df)[3] <- "cb"
  
  cb_df <- filter(cb_df,!is.na(cb))
  
  # dividingo por 1000
  
  cb_df$cb <- round(cb_df$cb/1000,2)
  
  cb_df$cb_cat <- discretize(cb_df$cb,method = "frequency",breaks =10 )
  
  cb_map <- cb_df%>%
    ggplot()+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
    geom_raster(aes(x = x,y = y,fill=cb_cat))+
    #scale_fill_viridis(option="turbo","it",direction =  -1,limits=c(-0.7,-0.5))+
    #scale_fill_viridis_d(option="turbo","bd",direction = -1)+
    scale_fill_manual(values=pal)+
    #ggtitle(scen[i])+
    theme_map()+
    labs(
      title = scen_nms_C[i],
      x = "",
      y = "",
      fill=expression(CO^2~tons~10^3))+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5))
  
  
  plot_list_C [[i]] <- cb_map
  
  
}

pannel_carbon_seq_C <- ggarrange(plot_list_C[[5]],plot_list_C[[4]],plot_list_C[[3]],plot_list_C[[1]],plot_list_C[[2]],nrow=3,ncol=2,common.legend = T)

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/carbon_seq_C_scens.png",pannel_carbon_seq_C  ,scale = 1,width = 18,height = 20,units = "cm")
