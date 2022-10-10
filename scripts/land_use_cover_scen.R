# calcular a proporcao de cada land-use nos cenarios. primeiro sem considerar mudanças, apenas cobertura. Depois posso fazer as mudanças em relacao à 2020!

#---- pacotes ------------------------------------------------------------------

library(readr)


#-------------------------------------------------------------------------------

# cobertura total por lu

#-------------------------------------------------------------------------------



p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/"

#"globiom_iiasa_test2/results/post_processed/tables/global"

# listando pastas

lu_scenarios_2050 <- list.files(file.path(p,"rawdata","land-use-2050"),pattern = "TH",full.names = T,recursive = F)

# continuar!

scen_lab <- c("frictions and reconfigurations","BAU","transp. cost. red.","tariff elimination","exacerbated liberalization")

# listando usos do solo

lus <- list.files(lu_scenarios_2050[1],recursive = T)

lu_list <- list()

c <- 1

for(i in 1:length(lu_scenarios_2050)){
  
  for(uso in lus){
    
    #abrindo raster
    r <- raster(file.path(lu_scenarios_2050[i],uso))
    # calculando area
    r_area <- r* (50100* 61800)/10^6
    #soma area
    total <- cellStats(r_area, 'sum')
    # guardando num df
    df <- data.frame(total_area_km2=total,lu=uso,scen=scen_lab[i])
    lu_list[[c]] <- df
    c <- c + 1  
  
  }
}

lu_df <- do.call(rbind,lu_list)

# barplot com as categorias

# calculando percentualmente

lu_share <- lu_df%>%  
  mutate(lu_1000 = total_area_km2/1000)%>%
  group_by(scen,lu) %>%
  summarise(lu_1000 = sum(lu_1000)) %>%
  mutate(freq = lu_1000 / sum(lu_1000))


lu_share$lu <- gsub(pattern = ".tif",replacement = "",lu_share$lu)

# combine other

lu_share$lu2 <- lu_share$lu

lu_share$lu2 <- gsub(pattern = paste(c("km_other_restored","other_natland"),collapse = "|"),replacement = "other nat. land.",x = lu_share$lu2)

lu_share$lu2 <- factor(lu_share$lu2,levels = c("agriculture","urban","pasture","forest","grassland","shrubland","wetland","desert","other nat. land.","ice","ignored"))

# proportions

lu_proportions <- lu_share%>%  
  mutate(perc = paste0(round(freq,2)*100," ","%")) %>%
  ggplot( aes(fill=lu2, y=freq, x=scen,label = lu2)) + 
  geom_bar(position="stack", stat="identity")+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  #geom_text_repel(position =position_stack(vjust = 0.5) )+
  scale_fill_manual(values = c("chocolate1","bisque4","darkgoldenrod3","darkgreen","chartreuse2","chartreuse4","darkolivegreen1","burlywood","darkseagreen2","deepskyblue","black"),"lulc")+
  #coord_flip()+
  theme_classic()+
  xlab("")+
  ylab("")+
  theme(legend.position="right")


# valores absolutos

lu_abs <- lu_share%>%  
  #mutate(perc = paste0(round(freq,2)*100," ","%")) %>%
  ggplot( aes(fill=lu2, y=lu_1000, x=scen,label = lu2)) + 
  geom_bar(position="stack", stat="identity")+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  #geom_text_repel(position =position_stack(vjust = 0.5) )+
  scale_fill_manual(values = c("chocolate1","bisque4","darkgoldenrod3","darkgreen","chartreuse2","chartreuse4","darkolivegreen1","burlywood","darkseagreen2","deepskyblue","black"),"lulc")+
  #coord_flip()+
  theme_classic()+
  xlab("")+
  ylab("")+
  theme(legend.position="right")


# como pode, se teve expansao agricola, ter a mesma quantidade de area??
# uma coisa compensa a outra?? expande num lugar, decresce em outro??

# talvez plotar como curvas faca mais sentido

lu_share$scen <-factor(lu_share$scen,levels = rev(c("exacerbated liberalization","transp. cost. red.","tariff elimination","BAU","frictions and reconfigurations")))



lu_share%>%
  #filter(lu=="grassland")%>%
  #mutate(perc = paste0(round(freq,2)*100," ","%")) %>%
  ggplot( aes(y=lu_1000, x=scen,group = 1,colour=lu2)) +
    geom_line()+
    geom_point()+
    facet_wrap("lu2")


# tem q plotar diferenca entre os cenarios. oq eh foda! talvez diferenca em relacao a 2020! pensar se vale a pena.

# fazer loop, pela questao da escala

land_uses <- unique(lu_share$lu2)


usos_inuteis <- c("ice","urban","ignored","desert","other nat. land.")

land_uses <-land_uses[!land_uses %in% usos_inuteis]




l <-expression(paste(~km^2,"x1000 ",sep=""))


lu_plots <- list()

for (land_use in land_uses ){
  df <- lu_share%>%
    #excluir usos inuteis
    #filter(!lu %in% usos_inuteis)%>%
    filter(lu2==land_use)
  
  plot_df <- ggplot(df, aes(y=lu_1000, x=scen,group = 1)) +
    geom_line()+
    geom_point()+
    ylab(l)+
    xlab("")+
    ggtitle(land_use)+
    scale_x_discrete(labels=c("exacerbated liberalization"="exc.lb.","transp. cost. red."="trsp.cst.rd","tariff elimination"="tff.elmtn","BAU"="BAU","frictions and reconfigurations"="frcts.rcfgrts"))+
    rotate_x_text(angle = 15)
  lu_plots[[land_use]] <- plot_df

}


lu_final <- ggarrange(plotlist = lu_plots)

ggsave(filename = "figures/lu_cover_Trade_scen.jpeg",width = 25.4,height = 14.288,units = "cm",plot = lu_final,bg ="white")

#-------------------------------------------------------------------------------

# mudanca de cobertura, baseada no resultado do plangea!

#-------------------------------------------------------------------------------

lu_change <- read_csv("output_tables/resultados_lu_change_cenarios.csv")

lu_change$label_scen <-factor(lu_change$label_scen,levels = rev(c("exacerb. lib. + BTC baseline","transp.cost. red + BTC baseline","tarif.elim.+BTC baseline","BAU","frict.&reconfig. + BTC baseline")))


lu_change$name <- factor(lu_change$name,levels = c("AGR","PAS","FOR","NGR","SHR","WET","DES","OTN","OTR"))

land_uses2 <- unique(lu_change$name)


usos_inuteis2 <- c("DES","OTR","OTN","desert","other nat. land.")

land_uses2 <-land_uses2[!land_uses2 %in% usos_inuteis2]

#area change (from baseline 2020)

l <-expression(paste("area-change relative to baseline 2020("~km^2,"x1000 ) ",sep=""))


lu_plangea <- lu_change%>%
  filter(!name %in% usos_inuteis2)%>%
  mutate(value=value/1000)%>%
  ggplot(aes(y=value, x=label_scen,group = 1)) +
    geom_line()+
    geom_point()+
    ylab(l)+
    xlab("")+
    rotate_x_text(angle = 15)+
    scale_x_discrete(labels=c("exacerb. lib. + BTC baseline"="exc.lb.","transp.cost. red + BTC baseline"="trsp.cst.rd","tarif.elim.+BTC baseline"="tff.elmtn","BAU"="BAU","frict.&reconfig. + BTC baseline"="frcts.rcfgrts"))+
    #ggtitle(land_use)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
    facet_wrap("name")
  # scale_x_discrete(labels=c("exacerbated liberalization"="exc.lb.","transp. cost. red."="trsp.cst.rd","tariff elimination"="tff.elmtn","BAU"="BAU","frictions and reconfigurations"="frcts.rcfgrts"))+
  

ggsave(filename = "figures/lu_cover_PLANGEA_Trade_scen.jpeg",width = 25.4,height = 14.288,units = "cm",plot = lu_plangea,bg ="white")

#-------------------------------------------------------------------------------

# testar pastagem + agricultura (usando output plangea)

#-------------------------------------------------------------------------------

# calculando percentualmente pasto e agri
# da uma diferença importante, pastagem faz resultados fazerem sentido!!!


lu_share_agri_past <- lu_change%>%  
  filter(name=="AGR"|name=="PAS")%>%
  mutate(lu_1000 = value/1000)%>%
  group_by(label_scen,name) %>%
  summarise(lu_1000 = sum(lu_1000)) %>%
  mutate(freq = lu_1000 / sum(lu_1000))


agri_past <- lu_share_agri_past%>%  
  #mutate(perc = paste0(round(freq,2)*100," ","%")) %>%
 filter(name=="AGR")%>%
  ggplot( aes(fill=name, y=lu_1000, x=label_scen)) + 
  geom_bar(position="stack", stat="identity")+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  #geom_text_repel(position =position_stack(vjust = 0.5) )+
  #scale_fill_manual(values = c("chocolate1","bisque4","darkgoldenrod3","darkgreen","chartreuse2","chartreuse4","darkolivegreen1","burlywood","darkseagreen2","deepskyblue","black"),"lulc")+
  coord_flip()+
  theme_classic()+
  xlab("")+
  ylab("")+
  theme(legend.position="right")