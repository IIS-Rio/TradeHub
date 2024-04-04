library(arules)
library(terra)
library(maptools)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(viridis)
library(ggthemes)

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/future_biodiv_index_rasters"

scens <- list.files(p,pattern = "it_",full.names = T)

# esses rasters de IT nao sao o delta, mas sim o It futuro. O delta seria o presente - o futuro

scens_C <- rast(grep(pattern = "_BIOD_",x = scens,value = T))
scens_trade <- rast(grep(pattern = "_BIOD_",x = scens,value = T,invert = T))

scen_nms_C <- c("Fr","BAU","Tr","Ta","ETL")
scen_nms <- c("Trade-base","Fr","Tr","Ta","ETL")


r <- scens_trade[[4]]

mask <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/CBD-carbon_layer_updated_reproj.tif")
mask[mask==0] <- NA
mask[!is.na(mask)] <- 1 

r <- r*mask

it_presente <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global/globiom_iiasa_baseline/results/post_processed/input_variables/it_2022-10-05.tif")

# # exploracao de como plotar!!
# r2 <- r
# 
# summary(r2[])
# hist(r2[])
# plot(r<(-0.05))
# # limites globo

data(wrld_simpl)

# excluding Antartica

wrld_simpl <- wrld_simpl[-which(wrld_simpl$NAME == "Antarctica"),]

# projetando

wrld_transf <- spTransform(wrld_simpl, crs(r))

plot_list_base <- list()
plot_list_base_ratio2050 <- list()


delta_BAU_2050 <- scens_trade[[1]]
delta_BAU_2050 <- delta_BAU_2050*mask
delta_BAU_2050 <- delta_BAU_2050 - it_presente

for(i in seq_along(scen_nms)){
  
  it <- scens_trade[[i]]
  it <- it*mask
  delta <- it - it_presente
  #value-value[label_scen=="BAU"])/abs(value[label_scen=="BAU"])
  delta_bau <- (delta - delta_BAU_2050)/(abs(delta_BAU_2050))
 
  # achatando valores
  
  delta_bau[delta_bau<(-1)] <- -1
  delta_bau[delta_bau>(1)] <- 1
  
  it_df <- as.data.frame(delta,xy=TRUE)
  
  names(it_df)[3] <- "it"
  
  it_df <- filter(it_df,it!=0,
                  !is.na(it))
  
  it_map <- it_df%>%
    filter(!is.na(it))%>%
    mutate(scaled_it = it/abs(min(it)),
           #scaled_it=if_else(scaled_it>=-0.05,0,scaled_it)
           )%>%
    ggplot()+
    geom_raster(aes(x = x,y = y,fill=it))+
    scale_fill_viridis(option="turbo","it",direction =  -1,limits=c(-0.5,0.2))+
    #scale_fill_viridis_d(option="turbo","it",dirittion = -1)+
    #ggtitle(scen[i])+
    theme_map()+
    labs(
      title = scen_nms[i],
      x = "",
      y = "",
      fill="it")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5)) 
  
  plot_list_base[[i]] <- it_map

  
  # plotando delta
  
  it_df_delta <- as.data.frame(delta_bau,xy=TRUE)
  
  names(it_df_delta)[3] <- "it"
  
  it_df_delta <- filter(it_df_delta,
                  !is.na(it))
  
  if(i==1){plot_list_base_ratio2050[[i]] <- NULL}else{
  it_df_delta$it_cat <- discretize(it_df_delta$it,method = "interval",breaks = 10)
  
  it_map_delta <- it_df_delta%>%
    filter(!is.na(it))%>%
    mutate(scaled_it = it/abs(min(it)),
           #scaled_it=if_else(scaled_it>=-0.05,0,scaled_it)
    )%>%
    ggplot()+
    geom_raster(aes(x = x,y = y,fill=it_cat))+
    #scale_fill_viridis(option="turbo","it",direction =  -1,limits=c(-1,1))+
    scale_fill_viridis_d(option="turbo","it",direction = -1)+
    #ggtitle(scen[i])+
    theme_map()+
    labs(
      title = scen_nms[i],
      x = "",
      y = "",
      fill="it")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5)) 
  
  plot_list_base_ratio2050[[i]] <- it_map_delta
  
  }
  
  
  }


# painel com valor it
pannel_converted_areas_base <- ggarrange(plot_list_base[[5]],plot_list_base[[4]],plot_list_base[[3]],plot_list_base[[2]],plot_list_base[[1]],common.legend = T,nrow=3,ncol=2)


# painel delta 2050

pannel_base_2050 <- ggarrange(plot_list_base_ratio2050[[5]],plot_list_base_ratio2050[[4]],plot_list_base_ratio2050[[3]],plot_list_base_ratio2050[[2]],common.legend = T,nrow=2,ncol=2)


ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/delta_it_2050_trade_scens.png",pannel_converted_areas_base,scale = 1,width = 16,height = 20,units = "cm")

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/delta_it_ratio2050_trade_scens.png",pannel_base_2050,scale = 1,width = 16,height = 20,units = "cm")


# cenarios com C

plot_list_C <- list()
plot_list_C_Delta <- list()

for(i in seq_along(scen_nms_C)){
  
  it <- scens_C[[i]]
  delta <- it - it_presente
  #value-value[label_scen=="BAU"])/abs(value[label_scen=="BAU"])
  delta_bau <- (delta - delta_BAU_2050)/(abs(delta_BAU_2050))
  
  # achatando valores
  
  delta_bau[delta_bau<(-1)] <- -1
  delta_bau[delta_bau>(1)] <- 1
  
  it_df <- as.data.frame(delta,xy=TRUE)
  
  names(it_df)[3] <- "it"
  
  it_df <- filter(it_df,it!=0,
                  !is.na(it))
  
  it_map_C <- it_df%>%
    filter(!is.na(it))%>%
    mutate(scaled_it =  it/abs(min(it)))%>%
    ggplot()+
    geom_raster(aes(x = x,y = y,fill=it))+
    scale_fill_viridis(option="turbo","it",direction =  -1,limits=c(-0.5,0.2))+
    #ggtecle(scen[i])+
    theme_map()+
    labs(
      title = scen_nms_C[i],
      x = "",
      y = "",
      fill="it")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5)) 
  
  plot_list_C[[i]] <- it_map_C
  
  # plotando delta
  
  it_df_delta <- as.data.frame(delta_bau,xy=TRUE)
  
  names(it_df_delta)[3] <- "it"
  
  it_df_delta <- filter(it_df_delta,
                        !is.na(it))
  
  # if(i==1){plot_list_base_ratio2050[[i]] <- NULL}else{
    it_df_delta$it_cat <- discretize(it_df_delta$it,method = "interval",breaks = 10)
    
    it_map_delta <- it_df_delta%>%
      filter(!is.na(it))%>%
      mutate(scaled_it = it/abs(min(it)),
             #scaled_it=if_else(scaled_it>=-0.05,0,scaled_it)
      )%>%
      ggplot()+
      geom_raster(aes(x = x,y = y,fill=it_cat))+
      #scale_fill_viridis(option="turbo","it",direction =  -1,limits=c(-1,1))+
      scale_fill_viridis_d(option="turbo","it",direction = -1)+
      #ggtitle(scen[i])+
      theme_map()+
      labs(
        title = scen_nms[i],
        x = "",
        y = "",
        fill="it")+
      #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
      theme(  legend.text = element_text(size=4.5),
              legend.title = element_text(size=4.5)) 
    
    plot_list_C_Delta[[i]] <- it_map_delta
    
  # }
  
  
}

pannel_converted_areas_C <- ggarrange(plot_list_C[[5]],plot_list_C[[4]],plot_list_C[[3]],plot_list_C[[1]],plot_list_C[[2]],common.legend = T,nrow=3,ncol=2)

pannel_ratio2050_C <- ggarrange(plot_list_C_Delta[[5]],plot_list_C_Delta[[4]],plot_list_C_Delta[[3]],plot_list_C_Delta[[2]],plot_list_C_Delta[[1]],common.legend = T,nrow=3,ncol=2)



ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/delta_it_2050_con_scens.png",pannel_converted_areas_C,scale = 1,width = 16,height = 20,units = "cm")

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/delta_it_ratio2050_con_scens.png",pannel_ratio2050_C,scale = 1,width = 16,height = 20,units = "cm")
