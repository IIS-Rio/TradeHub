#----- pacotes -----------------------------------------------------------------

library(terra)
library(dplyr)
library(ggpubr)
library(viridis)
library(ggthemes)
library(forcats)
library(ggrepel)

#-------------------------------------------------------------------------------

# esses sao os valores presentes, dai so vale a pena fazer 1

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global"

#"globiom_iiasa_test2/results/post_processed/tables/global"

# listando pastas

scenarios <- list.files(p,pattern = "globiom_iiasa",recursive = F)

scen_base <- grep(pattern =paste(c("_NOBIOD_NOTECH_","baseline"),collapse = "|") ,scenarios,value = T)
scen_base_nms <- c("BAU","Fr","Tr","Ta","ETL")

scen_C <- grep(pattern =paste(c("_NOBIOD_NOTECH_","baseline"),collapse = "|") ,scenarios,value = T,invert = T)

scen_C_nms <- c("Trade-base","Fr","Tr","Ta","ETL")


# listando resultados

# resultados <- list.files(file.path(p,scenarios),pattern = ".csv",recursive = T,full.names = T)

maps_base <- list()

for (i in seq_along(scen_base_nms)){
    
    ec <- rast(grep(".tif",list.files(file.path(p,scen_base[i],"results/post_processed/input_variables"),pattern ="ec",full.names = T),value = T)[1])
  
  ec_df <- as.data.frame(ec,xy=TRUE)
  
  names(ec_df)[3] <- "ec"
  
  ec_df <- filter(ec_df,ec!=0,
                  !is.na(ec))
  
  
  ec_df$ec_log10 <- log10(ec_df$ec)
  
  # normalizando valores de 0 a 1
  
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  
  ec_map <- ec_df%>%
    filter(!is.na(ec))%>%
    mutate(scaled_ec = range01(ec_log10))%>%
    ggplot()+
    geom_raster(aes(x = x,y = y,fill=scaled_ec))+
    scale_fill_viridis(option="turbo","ec")+
    #ggtecle(scen[i])+
    theme_map()+
    labs(
      title = scen_base_nms[i],
      x = "",
      y = "",
      fill="ec")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5)) 
  
  maps_base[[i]] <- ec_map
  
  }


panel_base <- ggarrange(plotlist =rev(maps_base),ncol = 2,nrow = 3,common.legend = T )

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/ec_base_2050.png",panel_base,scale = 1,width = 16,height = 20,units = "cm")


maps_C <- list()

for (i in seq_along(scen_C_nms)){
  
  ec <- rast(grep(".tif",list.files(file.path(p,scen_C[i],"results/post_processed/input_variables"),pattern ="ec",full.names = T),value = T)[1])
  
  ec_df <- as.data.frame(ec,xy=TRUE)
  
  names(ec_df)[3] <- "ec"
  
  ec_df <- filter(ec_df,ec!=0,
                  !is.na(ec))
  
  
  ec_df$ec_log10 <- log10(ec_df$ec)
  
  # normalizando valores de 0 a 1
  
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  
  ec_map <- ec_df%>%
    filter(!is.na(ec))%>%
    mutate(scaled_ec = range01(ec_log10))%>%
    ggplot()+
    geom_raster(aes(x = x,y = y,fill=scaled_ec))+
    scale_fill_viridis(option="turbo","ec")+
    #ggtecle(scen[i])+
    theme_map()+
    labs(
      title = scen_base_nms[i],
      x = "",
      y = "",
      fill="ec")+
    #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
    theme(  legend.text = element_text(size=4.5),
            legend.title = element_text(size=4.5)) 
  
  maps_C[[i]] <- ec_map
  
}


panel_C <- ggarrange(plotlist =rev(maps_C),ncol = 2,nrow = 3,common.legend = T )

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/ec_C_2050.png",panel_C,scale = 1,width = 16,height = 20,units = "cm")
