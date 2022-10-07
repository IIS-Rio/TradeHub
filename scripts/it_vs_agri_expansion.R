
#----- pacotes -----------------------------------------------------------------

library(raster)
library(dplyr)
library(ggpubr)
library(viridis)
library(ggthemes)
library(forcats)
library(ggrepel)

#-------------------------------------------------------------------------------

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/"

#"globiom_iiasa_test2/results/post_processed/tables/global"

# listando pastas

scenarios <- list.files(p,pattern = "globiom_iiasa",recursive = F)


# listando resultados

resultados <- list.files(file.path(p,scenarios),pattern = ".csv",recursive = T,full.names = T)

it <- raster(file.path(p,scenarios[1],"results/post_processed/input_variables","it_2022-10-05.tif"))

it_df <- as.data.frame(it,xy=TRUE)

names(it_df)[3] <- "it"


it_df%>%
  # filter(it>0)%>%
  # filter(it<10^(-3))%>%
  ggplot()+
  geom_histogram(aes(x=it))

it_map <- it_df%>%
  filter(!is.na(it))%>%
  ggplot()+
  #geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
  geom_raster(aes(x = x,y = y,fill=it))+
  scale_fill_viridis(option="turbo","it")+
  #ggtitle(scen[i])+
  theme_map()

# cortar raster nas classes  acima

it_cut <- it %>%
  cut(10)/10

# teria q cruzar classe a classe!

p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agricultural expansion"

agri_expan_files <-list.files(p2)

scen <- c("frictions and reconfig.","BAU","transp. cost. red","tariff elim.","exacerb. lib")
# o indice aqui nao eh igua o do scenarios, pq soh tem os de comercio. 

classes <- seq(0.1,1,0.1)


expansao_it <- list()

counter <-1 

for(s in 1:length(agri_expan_files)){
  
  agri_expan_r <- raster(file.path(p2,agri_expan_files[s]))
  agri_expan_r <- (agri_expan_r*(50100*61800))/10^6
  
  for(c in classes){
    class <- it_cut
    class[class==c] <- 1
    class[class<1] <- NA
    
    area_class <- class*agri_expan_r
    
    sum_raster_cells <-cellStats(area_class, 'sum')
    
    output_df <- data.frame(area_agricola= sum_raster_cells,scen=scen[s],it_class=c)
    
    expansao_it[[counter]] <- output_df
    
    counter <- counter+1
    
  }
  
  
  
}

expansao_it_result <- do.call(rbind,expansao_it)


expansao_it_result$scen <-factor(expansao_it_result$scen,levels = rev(c("exacerb. lib","transp. cost. red","tariff elim.","BAU","frictions and reconfig.")))


expansao_it_result2 <- expansao_it_result%>%  
  mutate(area_agricola_1000 = area_agricola/1000)%>%
  group_by(scen,it_class) %>%
  summarise(area_agricola_1000 = sum(area_agricola_1000)) %>%
  mutate(freq = area_agricola_1000 / sum(area_agricola_1000))

# ta mega errado!! corrigir essa merda!  

l2 <-"Agriculture expansion (%)"

expansao_it_plot2 <- expansao_it_result2%>%  
  mutate(perc = paste0(round(freq,2)*100," ","%")) %>%
  ggplot( aes(fill=it_class, y=freq, x=scen,label = perc)) + 
  geom_bar(position="stack", stat="identity")+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  #geom_text_repel(position =position_stack(vjust = 0.5) )+
  scale_fill_viridis(option="turbo","it")+
  coord_flip()+
  theme_classic()+
  xlab("")+
  ylab(l2)+
  theme(legend.position="none")

final <- ggarrange(it_map,expansao_it_plot2,nrow = 2)

ggsave(filename = "figures/agri_expansion_it_Trade_map.jpeg",width = 25.4,height = 14.288,units = "cm",plot = final,bg ="white")
