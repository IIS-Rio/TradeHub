library(arules)
library(terra)
library(maptools)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(viridis)
library(ggthemes)

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/future_biodiv_index_rasters"

scens <- list.files(p,pattern = "ec_",full.names = T)

# ETL e Ta estao trocados

scens_C <- rast(grep(pattern = "_BIOD_",x = scens,value = T))
scens_trade <- rast(grep(pattern = "_BIOD_",x = scens,value = T,invert = T))

scen_nms_C <- c("Fr","BAU","Tr","Ta","ETL")
scen_nms <- c("BAU","Fr","Tr","Ta","ETL")


r <- scens_trade[[4]]

mask <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/variables/CBD-carbon_layer_updated_reproj.tif")
mask[mask==0] <- NA
mask[!is.na(mask)] <- 1 

r <- r*mask
ec_presente <- rast("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global/globiom_iiasa_baseline/results/post_processed/input_variables/ec_2022-10-05.tif")

# limites globo

data(wrld_simpl)

# excluding Antartica

wrld_simpl <- wrld_simpl[-which(wrld_simpl$NAME == "Antarctica"),]

# projetando

wrld_transf <- spTransform(wrld_simpl, crs(r))

delta_BAU_2050 <- scens_trade[[1]]
delta_BAU_2050 <- delta_BAU_2050*mask
delta_BAU_2050 <- delta_BAU_2050 - ec_presente

plot_list_base <- list()
plot_list_base_bau <- list()

for(i in seq_along(scen_nms)){
  
  ec <- scens_trade[[i]]
  ec <- ec*mask
  delta <- ec - ec_presente
  #value-value[label_scen=="BAU"])/abs(value[label_scen=="BAU"])
  delta_bau <- (delta - delta_BAU_2050)/(abs(delta_BAU_2050))
  
  # achatando valores
  
  #delta_bau[delta_bau<(-1)] <- -1
  #delta_bau[delta_bau>(1)] <- 1
  
  ec_df <- as.data.frame(ec,xy=TRUE)
  
  names(ec_df)[3] <- "ec"
  
  ec_df <- filter(ec_df,ec!=0,
                  !is.na(ec))
  
  
  # categorizar
  
  #hist(ec_df$ec)
  
  #summary(ec_df$ec)
  
  ec_df$ec_cat <- discretize((ec_df$ec/abs(min(ec_df$ec))),method = "cluster",breaks = 5,labels=c("-1,-0.8","-0.8,0.4","-0.4,-0.2","-0.2,-01","-0.1,0"))
  
  #lbs <- unique(ec_df$ec_cat)
  
  #ec_df$trans <- asinh(ec_df$ec)
  
  ec_map <- ec_df%>%
    filter(!is.na(ec))%>%
    mutate(scaled_ec = ec/abs(min(ec)),
           #scaled_ec=if_else(scaled_ec>=-0.05,0,scaled_ec)
           )%>%
    ggplot()+
    geom_raster(aes(x = x,y = y,fill=ec_cat))+
    #scale_fill_viridis(option="turbo","ec",direction =  -1)+
    scale_fill_viridis_d(option="turbo","ec",direction = -1)+
    #ggtecle(scen[i])+
    theme_map()+
    labs(
      title = scen_nms[i],
      x = "",
      y = "",
      fill="ec")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5)) 
  
  plot_list_base[[i]] <- ec_map
  
  
  # plotando delta
  
  ec_df_delta <- as.data.frame(delta_bau,xy=TRUE)
  
  names(ec_df_delta)[3] <- "ec"
  
  ec_df_delta <- filter(ec_df_delta,
                        !is.na(ec))
  
  if(i==1){plot_list_base_bau[[i]] <- NULL}else{
    ec_df_delta$ec_cat <- discretize(round(ec_df_delta$ec/abs(min(ec_df_delta$ec)),2),method = "fixed",breaks = (seq(-1,1.5,0.03)))
    
    ec_map_delta <- ec_df_delta%>%
      filter(!is.na(ec))%>%
      mutate(scaled_ec = ec/abs(min(ec)),
             #scaled_ec=if_else(scaled_ec>=-0.05,0,scaled_ec)
      )%>%
      ggplot()+
      geom_raster(aes(x = x,y = y,fill=ec_cat))+
      #scale_fill_viridis(option="turbo","ec",direction =  -1,limecs=c(-1,1))+
      scale_fill_viridis_d(option="turbo","ec",direction = -1)+
      #ggtecle(scen[i])+
      theme_map()+
      labs(
        title = scen_nms[i],
        x = "",
        y = "",
        fill="ec")+
      #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
      theme(  legend.text = element_text(size=4.5),
              legend.title = element_text(size=4.5))

    plot_list_base_bau[[i]] <- ec_map_delta
    
    }
  
  }



pannel_converted_areas_base <- ggarrange(plot_list_base[[5]],plot_list_base[[4]],plot_list_base[[3]],plot_list_base[[2]],plot_list_base[[1]],common.legend = T,nrow=3,ncol=2)

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/delta_ec_2050_trade_scens.png",pannel_converted_areas_base,scale = 1,width = 16,height = 20,unecs = "cm")


pannel_ratio_bau_base <- ggarrange(plot_list_base_bau[[5]],plot_list_base_bau[[4]],plot_list_base_bau[[3]],plot_list_base_bau[[2]],common.legend = T,nrow=2,ncol=2)

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/delta_ec_ratio2050_trade_scens.png",pannel_ratio_bau_base,scale = 1,width = 16,height = 20,units = "cm")


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

teste <-log10(scens_trade[[1]]*-1+0.000001)*-1
teste[teste==6] <- NA
hist(teste)
teste2 <- teste
teste2[teste2>2] <- 2
teste2[teste2]
color_range <- rgb(0, 0, seq(0, 255, length.out = 100), maxColorValue = 255)
plot(teste2,col=color_range)
?asinh
summary(asin(teste))
