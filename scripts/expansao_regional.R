# extracting area expanded per country

#---- pacotes ------------------------------------------------------------------

library(giscoR)
library(countrycode)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
library(biscale) 
library(cowplot)
library(raster)

#---- pacotes ------------------------------------------------------------------

world <- gisco_get_countries()

# Add the subregion

world$region <- countrycode(world$ISO3_CODE,
                            origin = "iso3c",
                            destination = "un.regionsub.name")

# agrupando por regiao

subworld <- world %>% 
  group_by(region) %>%
  # Mock the data field
  summarise(data=n())

ggplot(subworld) +
  geom_sf(aes(fill=region))

# abrindo rasters de expansao 

p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agricultural expansion"

agri_expan_files <-list.files(p2,"agri")
past_expan_files <-list.files(p2,"past")


scen <- c("frictions and reconfig.","BAU","transp. cost. red","tariff elim.","exacerb. lib")


# pensar em filtrar so pixeis com grandes mudancas!!

library(raster)

for(s in 1:length(agri_expan_files)){
  
  agri_expan_r <- raster(file.path(p2,agri_expan_files[s]))
  past_expan_r <- raster(file.path(p2,past_expan_files[s]))
  soma <- agri_expan_r + past_expan_r
  
  # calculando area
  
  pixel_area <- (50100 * 61800)/10^6
  
  # convertendo pra area
  
  agri_km2 <- soma*pixel_area
  
  area_per_region <- data.frame(scen=extract(x = agri_km2,y = subworld,fun= sum,na.rm=TRUE ))
  
  names(area_per_region) <- scen[s]
  
  
  subworld <- cbind(subworld,area_per_region)
  
}

subworld_df <- subworld


names(subworld_df)[3:7] <- scen

st_geometry(subworld_df) <- NULL

subworld_df_l <- subworld_df%>%
  pivot_longer(cols = 3:7)
  #mutate(name=paste0("globiom_iiasa_",name))%>%
  #rename(scenario_name=name)%>%
  #left_join(nomes_scen2)


l2 <-expression(paste("Agriculture expansion ("~km^2,"x1000 ) ",sep=""))


subworld_df_l$name <-factor( subworld_df_l$name,levels = rev(c("exacerb. lib","tariff elim.","transp. cost. red","frictions and reconfig.","BAU")))


expansao_agricola_regional <- subworld_df_l %>%
  #filter(name== "AGR")%>%
  # tirando cenarios de conservacao
  #filter(!scenario_name %in% conserv_sub2)%>%
  # tirando BAU
  #filter(label_scen != "BAU")%>%
  # filter na
  filter(!is.na(region))%>%
  # adaptando escala
  mutate(value=value/1000)%>%
  ggplot( aes(x=name, y=value,fill=name)) + 
  geom_bar(stat = "identity",position = position_dodge())+
  #scale_y_continuous(label=scientific_10 ) +
  theme_classic()+
  xlab("Scenarios")+
  ylab(l2)+
  scale_fill_brewer(palette = "Spectral",name="name")+
  #geom_hline(yintercept=1, linetype="dashed",color = "darkgray", size=1)+
  coord_flip()+
  theme(legend.position = 'bottom', legend.direction = "horizontal",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill=guide_legend(nrow=2,byrow=TRUE,title = ""))+
  facet_wrap("region")


write.csv(x = subworld_df_l,"output_tables/agri_expansion_global_regions.csv",row.names = F)


ggsave(filename = "figures/exploratory_Trade_agri_expansion_per_region.jpeg",width = 25.4,height = 14.288,units = "cm",plot = expansao_agricola_regional,bg ="white")

#-----------------------------------------------------------------------------

# 3 regioes de maior crescimento. pra essas, fazer um zoom pra ver oq acontece pras metricas de biodiv.!

# regioes!
k <- c(48,51,7)

# plotar bivariate de prop. agri vs metrica? no caso das ecorregioes eh estranho, pq sao ecorregioes!

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/"

# listando pastas

scenarios <- list.files(p,pattern = "globiom_iiasa",recursive = F)


# listando resultados

resultados <- list.files(file.path(p,scenarios),pattern = ".csv",recursive = T,full.names = T)

# abrindo raster bd. sao todos iguais
bd <- raster(file.path(p,scenarios[1],"results/post_processed/input_variables","bd_2022-10-05.tif"))



bd_df <- as.data.frame(bd,xy=TRUE)

bd_df$bd_log10 <- log10(bd_df$bd)

names(bd_df)[3] <- "bd"

# America latina e caribe!


biv_list <- list()

for(s in 1:length(agri_expan_files)){

  # combinar os 2 dfs - bd e agri
  
  agri_expan_r <- raster(file.path(p2,agri_expan_files[s]))
  past_expan_r <- raster(file.path(p2,past_expan_files[s]))
  soma <- agri_expan_r + past_expan_r
  soma_df <- as.data.frame(soma,xy=TRUE)
  
  
  
  agri_bd <- left_join(soma_df,bd_df)
  
  agri_bd <- bi_class(agri_bd, x = layer, y = bd_log10, style = "quantile",dim=4)%>%
    filter(!bi_class=="NA-NA")
    #%>%
    #filtro com valores acima de 5% de expansao
    #filter(layer>=0.05)
  
  
  # limite da regiao
  
  subworld_region <- subworld %>%
    filter(data==k[2]) # mudar!Aqui define regiao!
  
  
  # transform data
  
  subworld_region_pj <- st_transform(subworld_region, crs(soma))
  
  
  #Extract coordinates max and min
  bbox_list <- lapply(st_geometry(subworld_region_pj), st_bbox)
  
  #To df
  maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(subworld_region_pj)))
  
  xlim <- c(maxmin[1,1],maxmin[1,3])
  ylim <- c(maxmin[1,2],maxmin[1,4])
  
  
  map <- ggplot() +
    geom_raster(data = agri_bd , aes(x = x, y = y, fill = bi_class)) +
    bi_scale_fill(pal = "DkBlue2",dim = 4) +
    lims(x = xlim, y = ylim)+
    #coord_quickmap() +
    labs(
      title = "",
      x = "",
      y = "",
      caption = scen[s]
  
    ) +
    bi_theme(base_size = 16) +
    theme_map()+
    theme(legend.position="none")
  biv_list[[s]] <- map

}


legend <- bi_legend(pal = "DkBlue2",
                    xlab = "agriculture cover  ",
                    ylab = "bd ",
                    size = 8,dim = 4)


biv_list[[6]] <- legend

biv_list2 <-list(biv_list[[2]],biv_list[[1]],biv_list[[3]],biv_list[[4]],biv_list[[5]],biv_list[[6]])


panel_SA <- plot_grid(plotlist = biv_list2 )










# finalPlot <-  plot_grid(
#   map, legend,
#   rel_widths = c(1, .2),
#   nrow = 1
# )


ggsave(filename = "figures/exploratory_Trade_agri_expansion_per_region_bd_bivariate.jpeg",width = 25.4,height = 14.288,units = "cm",plot = panel_SA,bg ="white")


#---- calculando pra ec --------------------------------------------------------

################################################################################

# A. Latina

################################################################################

ec <- raster(file.path(p,scenarios[1],"results/post_processed/input_variables","ec_2022-10-05.tif"))

ec_df <- as.data.frame(ec,xy=TRUE)

names(ec_df)[3] <- "ec"

ec_df$ec_log10 <- log10(ec_df$ec)

biv_list_ec <- list()

for(s in 1:length(agri_expan_files)){
  
  # combinar os 2 dfs - bd e agri
  
  agri_expan_r <- raster(file.path(p2,agri_expan_files[s]))
  past_expan_r <- raster(file.path(p2,past_expan_files[s]))
  soma <- agri_expan_r + past_expan_r
  soma_df <- as.data.frame(soma,xy=TRUE)
  
  
  
  agri_ec <- left_join(soma_df,ec_df)
  
  agri_ec <- bi_class(agri_ec, x = layer, y = ec_log10, style = "quantile",dim=3)%>%
    filter(!bi_class=="NA-NA")
  #%>%
  #filtro com valores acima de 5% de expansao
  #filter(layer>=0.05)
  
  
  # limite da regiao
  
  subworld_region <- subworld %>%
    filter(data==k[1]) # mudar!Aqui define regiao!
  
  
  # transform data
  
  subworld_region_pj <- st_transform(subworld_region, crs(soma))
  
  
  #Extract coordinates max and min
  bbox_list <- lapply(st_geometry(subworld_region_pj), st_bbox)
  
  #To df
  maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(subworld_region_pj)))
  
  xlim <- c(maxmin[1,1],maxmin[1,3])
  ylim <- c(maxmin[1,2],maxmin[1,4])
  
  
  map <- ggplot() +
    geom_raster(data = agri_ec , aes(x = x, y = y, fill = bi_class)) +
    bi_scale_fill(pal = "DkBlue",dim = 3) +
    lims(x = xlim, y = ylim)+
    #coord_quickmap() +
    labs(
      title = "",
      x = "",
      y = "",
      caption = scen[s]
      
    ) +
    bi_theme(base_size = 16) +
    theme_map()+
    theme(legend.position="none")
  biv_list_ec[[s]] <- map
  
}

legend <- bi_legend(pal = "DkBlue",
                    xlab = "agriculture cover  ",
                    ylab = "ec ",
                    size = 8,dim = 3)


biv_list_ec[[6]] <- legend

biv_list_ec2 <-list(biv_list_ec[[2]],biv_list_ec[[1]],biv_list_ec[[3]],biv_list_ec[[4]],biv_list_ec[[5]],biv_list_ec[[6]])


panel_SA_ec <- plot_grid(plotlist = biv_list_ec2 )


ggsave(filename = "figures/exploratory_Trade_agri_expansion_per_region_ec_bivariate.jpeg",width = 25.4,height = 14.288,units = "cm",plot = panel_SA_ec,bg ="white")


subset <- plot_grid(plotlist = biv_list_ec2[c(2,3)])


finalPlot <-  plot_grid(
  subset, legend,
  rel_widths = c(1, .2),
  nrow = 1
)

ggsave(filename = "figures/exploratory_Trade_agri_expansion_per_region_ec_bivariate_frict_transp.jpeg",width = 25.4,height = 14.288,units = "cm",plot = finalPlot,bg ="white")

################################################################################

# Africa Sub-Sahariana

################################################################################

ec <- raster(file.path(p,scenarios[1],"results/post_processed/input_variables","ec_2022-10-05.tif"))

ec_df <- as.data.frame(ec,xy=TRUE)

names(ec_df)[3] <- "ec"

ec_df$ec_log10 <- log10(ec_df$ec)

biv_list_ec <- list()

for(s in 1:length(agri_expan_files)){
  
  # combinar os 2 dfs - bd e agri
  
  agri_expan_r <- raster(file.path(p2,agri_expan_files[s]))
  past_expan_r <- raster(file.path(p2,past_expan_files[s]))
  soma <- agri_expan_r + past_expan_r
  soma_df <- as.data.frame(soma,xy=TRUE)
  
  
  
  agri_ec <- left_join(soma_df,ec_df)
  
  agri_ec <- bi_class(agri_ec, x = layer, y = ec_log10, style = "quantile",dim=3)%>%
    filter(!bi_class=="NA-NA")
  #%>%
  #filtro com valores acima de 5% de expansao
  #filter(layer>=0.05)
  
  
  # limite da regiao
  
  subworld_region <- subworld %>%
    filter(data==k[2]) # mudar!Aqui define regiao!
  
  
  # transform data
  
  subworld_region_pj <- st_transform(subworld_region, crs(soma))
  
  
  #Extract coordinates max and min
  bbox_list <- lapply(st_geometry(subworld_region_pj), st_bbox)
  
  #To df
  maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(subworld_region_pj)))
  
  xlim <- c(maxmin[1,1],maxmin[1,3])
  ylim <- c(maxmin[1,2],maxmin[1,4])
  
  
  map <- ggplot() +
    geom_raster(data = agri_ec , aes(x = x, y = y, fill = bi_class)) +
    bi_scale_fill(pal = "DkBlue",dim = 3) +
    lims(x = xlim, y = ylim)+
    #coord_quickmap() +
    labs(
      title = "",
      x = "",
      y = "",
      caption = scen[s]
      
    ) +
    bi_theme(base_size = 16) +
    theme_map()+
    theme(legend.position="none")
  biv_list_ec[[s]] <- map
  
}

legend <- bi_legend(pal = "DkBlue",
                    xlab = "agriculture cover  ",
                    ylab = "ec ",
                    size = 8,dim = 3)


biv_list_ec[[6]] <- legend

biv_list_ec2 <-list(biv_list_ec[[2]],biv_list_ec[[1]],biv_list_ec[[3]],biv_list_ec[[4]],biv_list_ec[[5]],biv_list_ec[[6]])


panel_SA_ec <- plot_grid(plotlist = biv_list_ec2 )


# ggsave(filename = "figures/exploratory_Trade_agri_expansion_per_region_ec_bivariate.jpeg",width = 25.4,height = 14.288,units = "cm",plot = panel_SA_ec,bg ="white")


subset <- plot_grid(plotlist = biv_list_ec2[c(2,3)])


finalPlot <-  plot_grid(
  subset, legend,
  rel_widths = c(1, .2),
  nrow = 1
)

ggsave(filename = "figures/exploratory_Trade_agri_expansion_per_region_ec_bivariate_frict_transp_Africa.jpeg",width = 25.4,height = 14.288,units = "cm",plot = finalPlot,bg ="white")


# nada q chame atencao!

################################################################################

# Eastern Asia

################################################################################

ec <- raster(file.path(p,scenarios[1],"results/post_processed/input_variables","ec_2022-10-05.tif"))

ec_df <- as.data.frame(ec,xy=TRUE)

names(ec_df)[3] <- "ec"

ec_df$ec_log10 <- log10(ec_df$ec)

biv_list_ec <- list()

for(s in 1:length(agri_expan_files)){
  
  # combinar os 2 dfs - bd e agri
  
  agri_expan_r <- raster(file.path(p2,agri_expan_files[s]))
  past_expan_r <- raster(file.path(p2,past_expan_files[s]))
  soma <- agri_expan_r + past_expan_r
  soma_df <- as.data.frame(soma,xy=TRUE)
  
  
  
  agri_ec <- left_join(soma_df,ec_df)
  
  agri_ec <- bi_class(agri_ec, x = layer, y = ec_log10, style = "quantile",dim=3)%>%
    filter(!bi_class=="NA-NA")
  #%>%
  #filtro com valores acima de 5% de expansao
  #filter(layer>=0.05)
  
  
  # limite da regiao
  
  subworld_region <- subworld %>%
    filter(data==k[3]) # mudar!Aqui define regiao!
  
  
  # transform data
  
  subworld_region_pj <- st_transform(subworld_region, crs(soma))
  
  
  #Extract coordinates max and min
  bbox_list <- lapply(st_geometry(subworld_region_pj), st_bbox)
  
  #To df
  maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(subworld_region_pj)))
  
  xlim <- c(maxmin[1,1],maxmin[1,3])
  ylim <- c(maxmin[1,2],maxmin[1,4])
  
  
  map <- ggplot() +
    geom_raster(data = agri_ec , aes(x = x, y = y, fill = bi_class)) +
    bi_scale_fill(pal = "DkBlue",dim = 3) +
    lims(x = xlim, y = ylim)+
    #coord_quickmap() +
    labs(
      title = "",
      x = "",
      y = "",
      caption = scen[s]
      
    ) +
    bi_theme(base_size = 16) +
    theme_map()+
    theme(legend.position="none")
  biv_list_ec[[s]] <- map
  
}

legend <- bi_legend(pal = "DkBlue",
                    xlab = "agriculture cover  ",
                    ylab = "ec ",
                    size = 8,dim = 3)


biv_list_ec[[6]] <- legend

biv_list_ec2 <-list(biv_list_ec[[2]],biv_list_ec[[1]],biv_list_ec[[3]],biv_list_ec[[4]],biv_list_ec[[5]],biv_list_ec[[6]])


panel_SA_ec <- plot_grid(plotlist = biv_list_ec2 )


# ggsave(filename = "figures/exploratory_Trade_agri_expansion_per_region_ec_bivariate.jpeg",width = 25.4,height = 14.288,units = "cm",plot = panel_SA_ec,bg ="white")


subset <- plot_grid(plotlist = biv_list_ec2[c(2,3)])


finalPlot <-  plot_grid(
  subset, legend,
  rel_widths = c(1, .2),
  nrow = 1
)

ggsave(filename = "figures/exploratory_Trade_agri_expansion_per_region_ec_bivariate_frict_transp_Africa.jpeg",width = 25.4,height = 14.288,units = "cm",plot = finalPlot,bg ="white")
