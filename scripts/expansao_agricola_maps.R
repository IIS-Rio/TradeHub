# plotar sobre os países areas de expansao agricola

# Inclui agora pastagem tb!!

#---- pacotes ------------------------------------------------------------------

library(ggplot2)
library(ggpubr)
library(ggthemes)
library(viridis)
library(maptools)


#---- plotando por cenário ----------------------------------------------------

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/agricultural expansion"

agri_expan_files <-list.files(p,pattern = "agricultural") 
pasture_expan_files <- list.files(p,pattern = "pasture")

scen <- c("frictions and reconfig.","BAU","transp. cost. red","tariff elim.","exacerb. lib")

# limites globo

data(wrld_simpl)

# prohetando
wrld_transf <- spTransform(wrld_simpl, crs(agri_expan_r))

# lista pra guardar mapas

maps <- list()
dfs <- list()
for (i in 1:length(agri_expan_files)){
  
  agri_expan_r <- raster(file.path(p,agri_expan_files[i]))
  past_expan_r <- raster(file.path(p,pasture_expan_files[i]))
  #omando os 2 usos
  soma <- agri_expan_r + past_expan_r
  expan_df <- as.data.frame(soma, xy=TRUE)

  names(expan_df)[3] <- "agriculture_expansion"
  expan_df$scen <- scen[i]
  dfs[[i]] <- expan_df
  map <- expan_df%>%
    filter(!is.na(agriculture_expansion))%>%
    filter(!agriculture_expansion<0.05)%>%
    ggplot()+
    geom_polygon(data=wrld_transf, aes(long,lat,group=group), fill="lightgray")+
    geom_raster(aes(x = x,y = y,fill=agriculture_expansion))+
    scale_fill_viridis(option="turbo","Agriculture expansion (%)")+
    ggtitle(scen[i])+
    theme_map()+
    theme( legend.direction = "horizontal")
  l<- get_legend(map, position = NULL)
    
  #map <- map  + theme(legend.position="none")
  
  maps[[i]] <- map

}

#-------------------------------------------------------------------------------
# adicionar figura da expansao agricola total no mapa! usando rasters (fica pior, melhor usar tabela plangea)
#-------------------------------------------------------------------------------



# lu_change_global <- do.call(rbind,dfs) %>%
#   mutate(agri_expansion= agriculture_expansion *(50100*61800)/10^6)%>%
#   group_by(scen)%>%
#   summarise(total_agri_expansion=sum(agri_expansion,na.rm = T))
# 
# 
# 
# 
# trade <- unique(lu_change_global$label_scen)[c(1,3,5,6,8)]
# 
# library(forcats)
# 
# expansao_agricola_total <- lu_change_global %>%
#   mutate(value=total_agri_expansion/1000)%>%
#   mutate(name = fct_reorder(scen, value)) %>%
#   ggplot( aes(x=name, y=value)) + 
#   geom_bar(stat = "identity",position = position_dodge(),fill="lightgray")+
#   #scale_y_continuous(label=scientific_10 ) +
#   theme_classic()+
#   xlab("")+
#   ylab(l)+
#   #scale_fill_brewer(palette = "Spectral",name="name")+
#   #geom_hline(yintercept=1, linetype="dashed",color = "darkgray", size=1)+
#   coord_flip()
# 
# # maps2 <- maps[1:5]
# # 
# maps[[6]] <- expansao_agricola_total
# 
# 
# # reordenando toscamente pra ficar em ordem de expansao
# 
# maps2 <- list(maps[[5]],maps[[3]],maps[[4]],maps[[2]],maps[[1]],maps[[6]])
# 
# global_maps2 <- ggarrange(plotlist =  maps2,common.legend = T,ncol=2,nrow=3)
# 
# ggsave(filename = "figures/agri_expansion_Trade_map_2.jpeg",width = 25.4,height = 14.288,units = "cm",plot = global_maps2,bg ="white")

# pq o impacto nas metricas nao eh diretamente proporcional à expansao agricola

# como interfere nas areas mais sensiveis do BD: agri+pastagem em locais sensíveis!

# olhar tb pro desmatamento nas ecorregioes!

#-------------------------------------------------------------------------------

# usando tabela plangea 

#-------------------------------------------------------------------------------

l <-expression(paste("Agriculture expansion ("~km^2,"x1000 ) ",sep=""))

lu_change <- read_csv("output_tables/resultados_lu_change_cenarios.csv")

lu_change$label_scen <-factor(lu_change$label_scen,levels = rev(c("exacerb. lib. + BTC baseline","tarif.elim.+BTC baseline","transp.cost. red + BTC baseline","frict.&reconfig. + BTC baseline","BAU")))

library(forcats)

expansao_agricola_total_plangea <- lu_change %>%
  filter(name=="AGR"|name=="PAS")%>%
  mutate(value = value/1000)%>%
  #mutate(name = fct_reorder(scen, value)) %>%
  ggplot( aes(x=label_scen, y=value,fill=name)) + 
  geom_bar(stat = "identity",position = "stack")+
  #scale_y_continuous(label=scientific_10 ) +
  scale_x_discrete(labels=c("exacerb. lib. + BTC baseline"="exc.lb.","tarif.elim.+BTC baseline"="tff.elmtn","transp.cost. red + BTC baseline"="trsp.cst.rd","frict.&reconfig. + BTC baseline"="frcts.rcfgrts","BAU"="BAU"))+
  scale_fill_manual(values = c("lightgray","gray",rep(NA,8)),name = "land-use")+
  theme_classic()+
  xlab("")+
  ylab(l)+
  #scale_fill_brewer(palette = "Spectral",name="name")+
  #geom_hline(yintercept=1, linetype="dashed",color = "darkgray", size=1)+
  coord_flip()


# scen <- c("frictions and reconfig.","BAU","transp. cost. red","tariff elim.","exacerb. lib")

maps[[6]] <- expansao_agricola_total_plangea

maps2 <- list(maps[[5]],maps[[4]],maps[[3]],maps[[1]],maps[[2]],maps[[6]])

global_maps2 <- ggarrange(plotlist =  maps2,common.legend = T,ncol=2,nrow=3)

ggsave(filename = "figures/agri_expansion_Trade_map_PLANGEA.jpeg",width = 25.4,height = 14.288,units = "cm",plot = global_maps2,bg ="white")
