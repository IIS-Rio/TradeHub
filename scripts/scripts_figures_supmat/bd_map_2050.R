# pacotes ------------------------------------------------------------------
library(arules)
library(terra)
library(maptools)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(viridis)
library(ggthemes)

#---------------------------------------------------------------------------

# quase ok, mas os cenarios nao batem com a figura do heatmap!

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/future_biodiv_index_rasters"

scens <- list.files(p,pattern = "bd_",full.names = T)

# esses rasters de IT nao sao o delta, mas sim o It futuro. O delta seria o presente - o futuro

scens_C <- rast(grep(pattern = "_BIOD_",x = scens,value = T))
scens_trade <- rast(grep(pattern = "_BIOD_",x = scens,value = T,invert = T))

scen_nms_C <- c("Fr","Trade-base","Tr","Ta","ETL")
scen_nms <- c("Fr","BAU","Tr","Ta","ETL")

# se o ETL ta trocado com o ta, como q fica??


r <- scens_trade[[4]]

mask <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/CBD-carbon_layer_updated_reproj.tif")
mask[mask==0] <- NA
mask[!is.na(mask)] <- 1 

r <- r*mask

# bd_presente <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global/globiom_iiasa_baseline/results/post_processed/input_variables/bd_2022-10-05.tif")

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




delta_BAU_2050 <- scens_trade[[2]]
delta_BAU_2050 <- delta_BAU_2050*mask

plot_list_base <- list()
plot_list_base_ratio2050 <- list()

for(i in seq_along(scen_nms)){
  
  bd <- scens_trade[[i]]
  bd <- bd*mask
  # delta <- bd - bd_presente
  #value-value[label_scen=="BAU"])/abs(value[label_scen=="BAU"])
  delta_bau <- (bd - delta_BAU_2050)/(abs(delta_BAU_2050))
  # achatando a distribuicao
  # parece q so o elt ta zuado
  #delta_bau[delta_bau>0] <- 0.01
  bd_df <- as.data.frame(bd,xy=TRUE)
  
  names(bd_df)[3] <- "bd"
  
  bd_df <- filter(bd_df,bd!=0,
                  !is.na(bd))
  
  bd_map <- bd_df%>%
    filter(!is.na(bd))%>%
    mutate(scaled_bd = bd/abs(min(bd)),
           #scaled_it=if_else(scaled_it>=-0.05,0,scaled_it)
           )%>%
    ggplot()+
    geom_raster(aes(x = x,y = y,fill=bd))+
    scale_fill_viridis(option="turbo","bd",direction =  -1)+
    #scale_fill_viridis_d(option="turbo","it",dirittion = -1)+
    #ggtitle(scen[i])+
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

  
  # plotando delta
  
  bd_df_delta <- as.data.frame(delta_bau,xy=TRUE)
  
  names(bd_df_delta)[3] <- "bd"
  
  bd_df_delta <- filter(bd_df_delta,
                  !is.na(bd))
  
  
  # tentando ajustar a distribuicao
  
  # escalar pelo minimo
  
  bd_df_delta$bd_sc <- bd_df_delta$bd/abs(min(bd_df_delta$bd[bd_df_delta$bd!=0]))
  
  # tentando suavizar a distribuicao
  
  # pseudo_log <- function(x) sign(x) * log(1+abs(x))
  # 
  # #hist( pseudo_log(bd_df_delta$bd_sc)) 
  # 
  # 
  # seq_neg_1 <- seq(min(bd_df_delta$bd_sc),-0.9,0.005)
  # seq_neg_2 <- seq(-0.9,0,0.1)
  # seq_pos <- c(0,0.1,0.2)
  
  if(i==2){plot_list_base_ratio2050[[i]] <- NULL}else{
  bd_df_delta$bd_cat <- discretize(bd_df_delta$bd_sc,method = "frequency",breaks =5 )
  
  bd_map_delta <- bd_df_delta%>%
    filter(!is.na(bd))%>%
    # mutate(scaled_bd = bd/abs(min(bd)),
    #        #scaled_it=if_else(scaled_it>=-0.05,0,scaled_it)
    # )%>%
    ggplot()+
    geom_raster(aes(x = x,y = y,fill=bd_cat))+
    #scale_fill_viridis(option="turbo","it",direction =  -1,limits=c(-0.7,-0.5))+
    scale_fill_viridis_d(option="turbo","bd",direction = -1)+
    #ggtitle(scen[i])+
    theme_map()+
    labs(
      title = scen_nms[i],
      x = "",
      y = "",
      fill="bd")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5),
            legend.position = "top")+
    guides(fill=guide_legend(nrow=2, byrow=TRUE))
  
  plot_list_base_ratio2050[[i]] <- bd_map_delta
  
  }
  
  
  }



pannel_converted_areas_base <- ggarrange(plot_list_base_ratio2050[[5]],plot_list_base_ratio2050[[4]],plot_list_base_ratio2050[[3]],plot_list_base_ratio2050[[1]],nrow=2,ncol=2,common.legend = F)


# # painel delta 2050
# 
# pannel_base_2050 <- ggarrange(plot_list_base_ratio2050[[5]],plot_list_base_ratio2050[[4]],plot_list_base_ratio2050[[3]],plot_list_base_ratio2050[[2]],common.legend = T,nrow=2,ncol=2)


ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/delta_bd_2050_trade_scens.png",pannel_converted_areas_base,scale = 1,width = 18,height = 20,units = "cm")

# ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/delta_it_ratio2050_trade_scens.png",pannel_base_2050,scale = 1,width = 16,height = 20,units = "cm")


# cenarios com C

plot_list_C <- list()
plot_list_C_Delta <- list()

for(i in seq_along(scen_nms_C)){
  
  bd <- scens_C[[i]]
  # delta <- it - it_presente
  #value-value[label_scen=="BAU"])/abs(value[label_scen=="BAU"])
  delta_bau <- (bd - delta_BAU_2050)/(abs(delta_BAU_2050))
  
  bd_df <- as.data.frame(bd,xy=TRUE)
  
  names(bd_df)[3] <- "bd"
  
  bd_df <- filter(bd_df,bd!=0,
                  !is.na(bd))
  
  bd_map_C <- bd_df%>%
    filter(!is.na(bd))%>%
    #mutate(scaled_bd =  it/abs(min(bd)))%>%
    ggplot()+
    geom_raster(aes(x = x,y = y,fill=bd))+
    # scale_fill_viridis(option="turbo","bd",direction =  -1,limits=c(-0.5,0.2))+
    #ggtecle(scen[i])+
    theme_map()+
    labs(
      title = scen_nms_C[i],
      x = "",
      y = "",
      fill="bd")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5))+
    guides(fill=guide_legend(nrow=2, byrow=TRUE)) 
  
  plot_list_C[[i]] <- bd_map_C
  
  # plotando delta
  
  bd_df_delta <- as.data.frame(delta_bau,xy=TRUE)
  
  names(bd_df_delta)[3] <- "bd"
  
  bd_df_delta <- filter(bd_df_delta,
                        !is.na(bd))
  
  
  # escalar pelo minimo
  
  bd_df_delta$bd_sc <- bd_df_delta$bd/abs(min(bd_df_delta$bd[bd_df_delta$bd!=0]))
    
    
  bd_df_delta$bd_cat <- discretize(bd_df_delta$bd_sc,method = "frequency",breaks =5 )
  
  bd_map_delta <- bd_df_delta%>%
      filter(!is.na(bd))%>%
      ggplot()+
      geom_raster(aes(x = x,y = y,fill=bd_cat))+
      #scale_fill_viridis(option="turbo","it",direction =  -1,limits=c(-1,1))+
      scale_fill_viridis_d(option="turbo","bd",direction = -1)+
      #ggtitle(scen[i])+
      theme_map()+
      labs(
        title = scen_nms_C[i],
        x = "",
        y = "",
        fill="bd")+
      #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
      theme(  legend.text = element_text(size=4.5),
              legend.title = element_text(size=4.5)) +
    guides(fill=guide_legend(nrow=2, byrow=TRUE)) 
    
    plot_list_C_Delta[[i]] <- bd_map_delta
    
  # }
  
  
}

pannel_converted_areas_C <- ggarrange(plot_list_C_Delta[[5]],plot_list_C_Delta[[4]],plot_list_C_Delta[[3]],plot_list_C_Delta[[1]],plot_list_C_Delta[[2]],common.legend = F,nrow=3,ncol=2,legend="top")

# pannel_ratio2050_C <- ggarrange(plot_list_C_Delta[[5]],plot_list_C_Delta[[4]],plot_list_C_Delta[[3]],plot_list_C_Delta[[2]],plot_list_C_Delta[[1]],common.legend = T,nrow=3,ncol=2)



ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/delta_bd_2050_C_scens.png",pannel_converted_areas_C ,scale = 1,width = 18,height = 20,units = "cm")
