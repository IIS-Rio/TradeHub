# levantar expansao agricola (pastagem + agricultura) e cruzar com mapa atual bd

#---- pacotes ------------------------------------------------------------------
library(raster)
library(dplyr)
library(ggpubr)
library(viridis)
library(ggthemes)
library(forcats)
library(ggrepel)

#-------------------------------------------------------------------------------

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


bd_df%>%
  # filter(bd>0)%>%
  # filter(bd<10^(-3))%>%
  filter(!is.na(bd_log10))%>%
  mutate(scaled_bd = range01(bd_log10))%>%
  ggplot()+
  geom_histogram(aes(x=scaled_bd))

# normalizando valores de 0 a 1

range01 <- function(x){(x-min(x))/(max(x)-min(x))}


# plotando bd em log, normalizado de 0 a 10!

bd_map <- bd_df%>%
  filter(!is.na(bd_log10))%>%
  #filter(!agriculture_expansion<0.05)%>%
  mutate(scaled_bd = range01(bd_log10))%>%
  ggplot()+
  #geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
  geom_raster(aes(x = x,y = y,fill=scaled_bd))+
  #scale_fill_viridis(option="turbo","bd")+
  scale_fill_viridis_c(limits = c(0, 0.5),option="turbo","bd")+
  #ggtitle(scen[i])+
  theme_map()

# somar qnto expansao se sobrepos a areas relevantes de bd!

# cortar raster nas classes de bd acima
# como calcular expansao em cada classe de bd?


bd_cut <- bd %>%
  log10()%>%
  cut(20)/20

# teria q cruzar classe a classe!

p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agricultural expansion"

agri_expan_files <-list.files(p2,"agri")
pasture_expan_files <-list.files(p2,"pasture")



scen <- c("frictions and reconfig.","BAU","transp. cost. red","tariff elim.","exacerb. lib")
# o indice aqui nao eh igua o do scenarios, pq soh tem os de comercio. 

classes <- seq(0.05,1,0.05)


expansao_bd <- list()

counter <-1 

for(s in 1:length(agri_expan_files)){
  
  agri_expan_r <- raster(file.path(p2,agri_expan_files[s]))
  past_expan_r <- raster(file.path(p2,pasture_expan_files[s]))
  soma <- agri_expan_r + past_expan_r
  expan_r <- (soma*(50100*61800))/10^6
  
  
  
  for(c in classes){
    class <- bd_cut
    class[class==c] <- 1
    class[class<1] <- NA
    
    area_class <- class*expan_r
    
    sum_raster_cells <-cellStats(area_class, 'sum')
    
    output_df <- data.frame(area_agricola= sum_raster_cells,scen=scen[s],bd_class=c)
    
    expansao_bd[[counter]] <- output_df
    
    counter <- counter+1
    
  }
  
  

}

expansao_bd_result <- do.call(rbind,expansao_bd)


expansao_bd_result$scen <-factor(expansao_bd_result$scen,levels = rev(c("exacerb. lib","tariff elim.","transp. cost. red","frictions and reconfig.","BAU")))

l <-expression(paste("Agriculture expansion ("~km^2,"x1000 ) ",sep=""))

# calcular proporcionalmente, ver se tem uma expansao desproporcional em alguma classe! vai dar pra comparar melhor!!

expansao_bd_result2 <- expansao_bd_result%>%  
  mutate(area_agricola_1000 = area_agricola/1000)%>%
  group_by(scen,bd_class) %>%
  summarise(area_agricola_1000 = sum(area_agricola_1000)) %>%
  mutate(freq = area_agricola_1000 / sum(area_agricola_1000))


# agrupando classes acima de 0.5

expansao_bd_result2$bd_class_rc <- expansao_bd_result2$bd_class
expansao_bd_result2$bd_class_rc[expansao_bd_result2$bd_class_rc>0.5] <- 0.5


expansao_bd_result_rc <- expansao_bd_result2%>%
  group_by(scen,bd_class_rc)%>%
  summarise(freq_rc = sum(freq),area_agricola_1000 = sum(area_agricola_1000)) 
  
l2 <-"Agriculture expansion (%)"

# sem reclassificacao de grupo

expansao_bd_plot2 <- expansao_bd_result2%>%  
  mutate(perc = paste0(round(freq,2)*100," ","%")) %>%
  ggplot( aes(fill=bd_class, y=freq, x=scen,label = perc)) + 
  geom_bar(position="stack", stat="identity")+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  #geom_text_repel(position =position_stack(vjust = 0.5) )+
  scale_fill_viridis(option="turbo","bd")+
  coord_flip()+
  theme_classic()+
  xlab("")+
  ylab(l2)+
  theme(legend.position="none")

final_2 <- ggarrange(bd_map,expansao_bd_plot2,nrow = 2)

ggsave(filename = "figures/agri_expansion_bd_Trade_map.jpeg",width = 25.4,height = 14.288,units = "cm",plot = final_2,bg ="white")


# com reclassificacao (fica melhor!!)

expansao_bd_plot_rc <- expansao_bd_result_rc%>%  
  mutate(perc = paste0(round(freq_rc,2)*100," ","%")) %>%
  ggplot( aes(fill=bd_class_rc, y=freq_rc, x=scen,label = perc)) + 
  geom_bar(position="stack", stat="identity")+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  #geom_text_repel(position =position_stack(vjust = 0.5) )+
  scale_fill_viridis(option="turbo","bd")+
  coord_flip()+
  theme_classic()+
  xlab("")+
  ylab(l2)+
  theme(legend.position="none")


final_rc <- ggarrange(bd_map,expansao_bd_plot_rc,nrow = 2)


ggsave(filename = "figures/agri_expansion_bd_Trade_map.jpeg",width = 25.4,height = 14.288,units = "cm",plot = final_rc,bg ="white")

#-------------------------------------------------------------------------------

# olhar pra area total de expansao da categoria alta!

# ainda nao faz sentido

expansao_bd_plot_rc_area <- expansao_bd_result_rc%>%  
  filter(bd_class_rc>0.45)%>%
  ggplot( aes( y=area_agricola_1000, x=scen)) + 
  geom_bar(position="stack", stat="identity",fill="gray")+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  #geom_text_repel(position =position_stack(vjust = 0.5) )+
  #scale_fill_viridis(option="turbo","bd")+
  #scale_fill_viridis_c(limits = c(0.3, 0.5),option="turbo","bd")+
  coord_flip()+
  theme_classic()+
  xlab("")+
  ylab(l)+
  theme(legend.position="none")

expansao_bd_plot_rc_freq <- expansao_bd_result_rc%>%  
  filter(bd_class_rc>0.4)%>%
  ggplot( aes(fill="lightgrey", y=freq_rc, x=scen)) + 
  geom_bar(position="stack", stat="identity")+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  #geom_text_repel(position =position_stack(vjust = 0.5) )+
  #scale_fill_viridis(option="turbo","bd")+
  #scale_fill_viridis_c(limits = c(0.5, 0.5),option="turbo","bd")+
  coord_flip()+
  theme_classic()+
  xlab("")+
  ylab(l2)+
  theme(legend.position="none")


#---- focar na America do Sul! -------------------------------------------------

