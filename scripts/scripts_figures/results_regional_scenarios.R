#---- pacotes ------------------------------------------------------------------

library(dplyr)
library(ggpubr)
library(purrr)
library(scales)
library(sf)
library(ggmap)
library(ggthemes)
library(RColorBrewer)
library(viridis)
library(ggrepel)
library(scico)

#-------------------------------------------------------------------------------


# fazer tb figura com mudanca % nao apenas absoluta! pq isso  enfatiza diferencas

df <- read.csv("output_tables/resultados_cenarios_regional_analysis.csv")


################################################################################
## absolute changes
################################################################################


# figura com lulcc

lulcc <- df %>%
  filter(variable=="lulcc")%>%
  filter(!name=="area")

# tem q tirar o BAU

lulcc$label_scen[lulcc$label_scen=="BAU"] <- "Trade-base"


lulcc$label_scen <-factor(lulcc$label_scen,levels = (c("ETL","Ta","Tr","Fr","Trade-base")))


lulcc$name <- factor(lulcc$name,levels = c("AGR","PAS","FOR","NGR","SHR","WET","DES","OTN","OTR"))


#area change (from baseline 2020)

l <-expression(paste("area-change relative to baseline 2020("~km^2,"x1000 ) ",sep=""))


# ordenar por area de mudanca - definir manualmente depois de agregar total

mudanca_total <- lulcc%>%
  group_by(region)%>%
  filter(name=="AGR"|name=="PAS")%>%
  filter(label_scen=='ETL')%>%
  filter(conservation=="BTC-base")%>%
  summarise(total_change=sum(value))%>%
  arrange(total_change)%>%  
  mutate(region=factor(region, levels=region))

levels_regions <- (mudanca_total$region)

lulcc$region <- factor(lulcc$region,levels = levels_regions)

lulcc_plangea <- lulcc%>%
  #filter(name=="AGR")%>%
  filter(!name=="OTR")%>%
  mutate(value=value/1000)%>%
  ggplot(aes(y=value, x=region,fill=conservation)) +
  geom_bar(stat="identity",position = "dodge")+
  coord_flip()+
  #geom_point()+
  ylab(l)+
  xlab("")+
  #rotate_x_text(angle = 90)+
  #scale_x_discrete(labels=label)+
  #ggtitle(land_use)+
  scale_fill_brewer(palette = "Set1",labels=c("BTC-base","C"),name="")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  facet_grid(label_scen~name)+
  #theme_classic()+
  theme_bw()+
  theme(legend.position="top")+
  rotate_x_text(angle = 90)

#paletas possiveis: Greens, Spectral, Accent, Dark2

ggsave(filename = "figures_paper/lulcc_PLANGEA_Trade_scen_with_conservation_absolute.jpeg",width = 22,height = 20,units = "cm",plot = lulcc_plangea,bg ="white")

################################################################################
## relative changes
################################################################################

# precisa ter a area total desses usos pra 2020
# pro total ficar compativel com o resto, precisa dividir tudo pelo total global, nao pelo total da regiao!

p_2020 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use"

lu_names <- c("agriculture","desert","forest","grassland","ice","ignored","km_other_restored","other_natland","pasture","shrubland","urban","wetland")

lu_r <- list.files(p_2020,pattern = ".tif",full.names = T)

list_areas <- list()

for(i in 1:length(lu_names)){
  r <- raster(lu_r[i])
  area_r<- (50100* 61800)*r
  
  area <- cellStats(area_r, 'sum')
  
  df_area <- data.frame(total_area=area,lu=lu_names[i])
  list_areas[[i]] <- df_area
}

areas_2020 <- do.call(rbind,list_areas)

# compatibilizando com o resultado plangea

areas_2020$lu_PLANGEA <- NA
areas_2020$lu_PLANGEA[areas_2020$lu=='agriculture'] <- "AGR"
areas_2020$lu_PLANGEA[areas_2020$lu=='desert'] <- "DES"
areas_2020$lu_PLANGEA[areas_2020$lu=='forest'] <- "FOR"
areas_2020$lu_PLANGEA[areas_2020$lu=='grassland'] <- "NGR"
areas_2020$lu_PLANGEA[areas_2020$lu=='ice'] <- "OTR"
areas_2020$lu_PLANGEA[areas_2020$lu=='ignored'] <- "OTR"
areas_2020$lu_PLANGEA[areas_2020$lu=='km_other_restored'] <- "OTN"
areas_2020$lu_PLANGEA[areas_2020$lu=='other_natland'] <- "OTN"
areas_2020$lu_PLANGEA[areas_2020$lu=='pasture'] <- "PAS"
areas_2020$lu_PLANGEA[areas_2020$lu=='shrubland'] <- "SHR"
areas_2020$lu_PLANGEA[areas_2020$lu=='urban'] <- "OTR"
areas_2020$lu_PLANGEA[areas_2020$lu=='wetland'] <- "WET"

# juntando areas

areas_2020 <- areas_2020%>%
  group_by(lu_PLANGEA)%>%
  summarise(total_area=sum(total_area))


# calculando mudanca total por cenario

total_change <- lulcc %>%
  group_by(scenario,name,variable,label_scen,conservation)%>%
  summarise(value=sum(value))


total_change$region <- "Total"

# adicionando "regiao total" ao data.frame

lulcc <- rbind(lulcc,total_change)


# agregando ao df 

lulcc2 <- left_join(lulcc,areas_2020,by=c("name"="lu_PLANGEA"))



lulcc2 <- lulcc2%>%
  mutate(prop_change=value/total_area)

lulcc2$label_scen <-factor(lulcc2$label_scen,levels = (c("ETL","Ta","Tr","Fr","Trade-base")))


lulcc2$name <- factor(lulcc2$name,levels = c("AGR","PAS","FOR","NGR","SHR","WET","DES","OTN","OTR"))


l2 <-expression(paste("proportional change relative to baseline 2020 (% x"~10^-4," ) ",sep=""))



levels_regions <- c("Global","SSA","LAC","CSI","SEA","EAS","MNA","OCE","SAS","EUR","USA","CAN")


lulcc2$region <- factor(lulcc2$region,levels = rev(levels_regions))

lulcc_plangea_rel <- lulcc2%>%
  #filter(name=="AGR")%>%
  filter(!name=="OTR")%>%
  mutate(prop_change=prop_change*10^6)%>%
  ggplot(aes(y=prop_change, x=region,fill=conservation)) +
  geom_bar(stat="identity",position = "dodge")+
  #scale_y_continuous(labels = function(x) sprintf("%g", x))+
  coord_flip()+
  #geom_point()+
  ylab(l2)+
  xlab("")+
  #scale_x_discrete(labels=label)+
  #ggtitle(land_use)+
  scale_fill_brewer(palette = "Set1",labels=c("BTC-base","C"),name="")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  facet_grid(label_scen~name)+
  #theme_classic()+
  theme_bw()+
  rotate_x_text(angle = 90)+
  theme(legend.position="top")

ggsave(filename = "figures_paper/lulcc_PLANGEA_Trade_scen_with_conservation_relative.jpeg",width = 22,height = 20,units = "cm",plot = lulcc_plangea_rel,bg ="white")


# talvez valha a pena focar nos usos com maiores mudanças, e deixar todos como
# material suplementar. Nesse caco: agr,pas,for,NGR
# levels <- rev(c('LAC','SEA','EAS','CSI','MNA',"SAS",'OCE','EUR','CAN'))

lulcc_plangea_rel2 <- lulcc2%>%
  #group_by(region)%>%
  arrange(desc(prop_change),.by_group = T)%>%
  # mutate(region=factor(region, levels=levels)) %>%   # update the factor levels
  filter(name=="AGR"|name=="PAS"|name=="FOR"|name=="NGR")%>%
  #filter(!name=="OTR")%>%
  mutate(prop_change=prop_change*10^6)%>%
  ggplot(aes(y=prop_change, x=region,fill=conservation)) +
  geom_bar(stat="identity",position = "dodge")+
  #scale_y_continuous(labels = function(x) sprintf("%g", x))+
  coord_flip()+
  #geom_point()+
  ylab(l2)+
  xlab("")+
  #scale_x_discrete(labels=label)+
  #ggtitle(land_use)+
  scale_fill_brewer(palette = "Set1",labels=c("BTC-base","C"),name="")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  facet_grid(label_scen~name)+
  #theme_classic()+
  theme_bw()+
  rotate_x_text(angle = 90)+
  theme(legend.position="top")

ggsave(filename = "figures_paper/lulcc_PLANGEA_Trade_scen_with_conservation_relative_mainLU.jpeg",width = 22,height = 20,units = "cm",plot = lulcc_plangea_rel2,bg ="white")

################################################################################
## PLANGEA METRICS
################################################################################

metricas <- df%>%
  filter(!variable=="lulcc")%>%
  group_by(region,name)%>%
  filter(variable== unique(variable)) %>%
  # calculando prop. em relacao ao BAU em modulo
  mutate(relative_to_BAU_2050=value/abs(value[label_scen=="BAU"]))

# calculando metricas total

metricas_total <- df%>%
  filter(!variable=="lulcc")%>%
  group_by(variable,name,scenario,label_scen,conservation)%>%
  summarise(value=sum(value))
  
metricas_total  <- metricas_total%>%
  group_by(name)%>%
  filter(variable== unique(variable)) %>%
  # calculando prop. em relacao ao BAU em modulo
  mutate(relative_to_BAU_2050=value/abs(value[label_scen=="BAU"]))

metricas_total$region <- "Global"  


metricas <- rbind(metricas,metricas_total)


# tem q ser relativo ao BAU 2050, mas acho q tem q ser em modulo pra fazer sentido


#ordenar 

metricas$label_scen <-factor(metricas$label_scen,levels = (c("ETL","Ta","Tr","Fr","Trade-base","BAU")))



metricas$variable <- recode_factor(metricas$variable, 'Ecoregion vulnerability' = "Ecoregion vulnerability reduction")


metricas$variable <- factor(metricas$variable,levels = (c("Extinction debt reduction","Ecoregion vulnerability reduction","Ecossistem integrity reduction","Carbon","Land opportunity cost")))

# falta ordenar por regiao e entender melhor as metricas: qndo eh reducao, qndo eh aumento!

metricas$region <- factor(metricas$region,levels = rev(c(levels_regions)))

# reducao no risco de extincao
# reducao na vulnerabilidade das ecorregioes
# reducao na integridade dos ecossistemas
# todas as metricas sao qnto maior melhor! menos custo!

#plotando valores absolutos


metricas_plangea_absol <- metricas%>%
  #filter(conservation=="BTC-base")%>%
  #filter(region=="LAC")%>%
  filter(!label_scen=="BAU")%>%
  #filter(!label_scen=="Trade-base")%>%
  ggplot(aes(y=value, x=region,fill=conservation)) +
  geom_bar(stat="identity",position = "dodge")+
  #scale_y_continuous(labels = function(x) sprintf("%g", x))+
  coord_flip()+
  #geom_point()+
  ylab("relative variation (BAU 2020)")+
  xlab("")+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),breaks = c(-30,-100,-7,-2,0,2,7,30,100))+
  #ggtitle(land_use)+
  scale_fill_brewer(palette = "Set1",labels=c("BTC-base","C"),name="")+
  geom_hline(yintercept=0, linetype="dashed",color = "darkgray", size=1)+
  facet_grid(label_scen~variable,scales = "free_x")+
  #theme_classic()+
  theme_bw()+
  rotate_x_text(angle = 90)+
  theme(legend.position="top")+
  theme(strip.text.x = element_text(size = 7))


#criando df pra plotar pontos

metricas_Trade_base_C <- metricas%>%
  filter(!label_scen=="BAU")%>%
  # so esse cenario tem valor, resto eh NA
  mutate(relative_to_BAU_2050_ed= ifelse(scenario=="TH_TFBASE_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2",relative_to_BAU_2050,NA))
  
# plotando em relacao ao BAU 2050

metricas_plangea_rel <- metricas%>%
  #filter(conservation=="BTC-base")%>%
  # tirando Trade-base+C
  filter(!scenario=="TH_TFBASE_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2")%>%
  filter(!label_scen=="BAU")%>%
  #filter(!label_scen=="Trade-base")%>%
  ggplot(aes(y=relative_to_BAU_2050, x=region,fill=conservation)) +
  geom_bar(stat="identity",position = "dodge")+
  # geom_point(data = metricas_Trade_base_C[,c(1,3,4,5,7,9)], aes(x = region, y= relative_to_BAU_2050_ed),shape=3,show.legend = F)+
  #scale_y_continuous(labels = function(x) sprintf("%g", x))+
  coord_flip()+
  #geom_point()+
  ylab("relative variation (BAU 2050)")+
  xlab("")+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),breaks = c(-30,-100,-7,-2,0,2,7,30,100))+
  #ggtitle(land_use)+
  scale_fill_brewer(palette = "Set1",labels=c("BTC-base","C"),name="")+
  geom_hline(yintercept=0, linetype="dashed",color = "darkgray", size=1)+
  facet_grid(label_scen~variable,scales = "free_x")+
  #theme_classic()+
  theme_bw()+
  rotate_x_text(angle = 90)+
  theme(legend.position="top")+
  theme(strip.text.x = element_text(size = 7))

#tentativa de incluir cruz na legenda sem sobrepor

metricas_plangea_rel2 <- metricas_plangea_rel + geom_point(data = metricas_Trade_base_C[,c(1,3,4,5,7,9)], aes(x = region, y= relative_to_BAU_2050_ed),color="black",shape=3,show.legend = F)

ggsave(filename = "figures_paper/metrics_PLANGEA_Trade_scen_with_conservation_relative_BAU2050.jpeg",width = 24,height = 20,units = "cm",plot = metricas_plangea_rel2,bg ="white")


# oq da pra concluir é que na regiao mais afetada (LAC), ações de restauração e conservação são mto importantes e revertem o desempenho ruim da liberalização do comércio!


metricas_plangea_relnormal_sc <- metricas%>%
  #filter(conservation=="BTC-base")%>%
  #filter(region=="LAC")%>%
  filter(!label_scen=="BAU")%>%
  #filter(!label_scen=="Trade-base")%>%
  ggplot(aes(y=relative_to_BAU_2050, x=region,fill=conservation)) +
  geom_bar(stat="identity",position = "dodge")+
  #scale_y_continuous(labels = function(x) sprintf("%g", x))+
  coord_flip()+
  #geom_point()+
  ylab("relative variation (BAU 2050)")+
  xlab("")+
  # scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),breaks = c(-30,-100,-7,-2,0,2,7,30,100))+
  #ggtitle(land_use)+
  scale_fill_brewer(palette = "Set1",labels=c("BTC-base","C"),name="")+
  geom_hline(yintercept=0, linetype="dashed",color = "darkgray", size=1)+
  facet_grid(label_scen~variable,scales = "free_x")+
  #theme_classic()+
  theme_bw()+
  rotate_x_text(angle = 90)+
  theme(legend.position="top")+
  theme(strip.text.x = element_text(size = 7))

# ggsave(filename = "figures_paper/metrics_PLANGEA_Trade_scen_with_conservation_relative_BAU2050.jpeg",width = 24,height = 20,units = "cm",plot = metricas_plangea_rel,bg ="white")

# pensar em plotar separado

metricas$region <- factor(metricas$region,levels = rev(c(levels_regions)))
metricas$label_scen <-factor(metricas$label_scen,levels = rev(c("ETL","Ta","Tr","Fr","Trade-base","BAU")))

bd <- metricas%>%
  filter(name=="bd.val")%>%
  filter(!scenario=="TH_TFBASE_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2")%>%
  filter(!label_scen=="BAU")%>%
  #filter(!label_scen=="Trade-base")%>%
  ggplot(aes(y=relative_to_BAU_2050, x=label_scen,fill=conservation)) +
  geom_bar(stat="identity",position = "dodge")+
  #scale_y_continuous(labels = function(x) sprintf("%g", x))+
  coord_flip()+
  #geom_point()+
  ylab("relative variation (BAU 2050)")+
  xlab("")+
   scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),breaks = c(-30,-100,-7,-2,0,2,7,30,100))+
  #ggtitle(land_use)+
  scale_fill_brewer(palette = "Set1",labels=c("BTC-base","C"),name="")+
  geom_hline(yintercept=0, linetype="dashed",color = "darkgray", size=1)+
  facet_wrap(~region)+
  #theme_classic()+
  theme_bw()+
  rotate_x_text(angle = 90)+
  theme(legend.position="top")+
  theme(strip.text.x = element_text(size = 7))

#------------------------------------------------------------------------------

# plotando resultados como tiles, escalados em uma mesma escala de zero a um


# primeiro, escalar cada variavel separadamente, ente -1 e 1

pseudoLog10 <- function(x) { asinh(x/2)/log(10) }

pseudoLog10(metricas$value)

metricas2 <- metricas %>%
  # colocando bd em log (tem q ser pseudolog)
  mutate(relative_to_BAU_2050=if_else(condition = name=="bd.val",true = pseudoLog10(relative_to_BAU_2050),false = relative_to_BAU_2050))%>%
  # parte que ja funcionava antes
  group_by(across(5))%>%
  filter(variable==variable)%>%
  mutate(relative_to_BAU_2050_sc=relative_to_BAU_2050/max(relative_to_BAU_2050))

my_breaks <- c(-0.4,0,1,2)

metricas2$name <- gsub(pattern = ".val",replacement = "",x = metricas$name)

limit <- max(abs(metricas2$relative_to_BAU_2050_sc)) * c(-1, 1)

#ordenando siglas das variaveis


metricas2$name <- factor(metricas2$name,levels=c("bd","ec","it","cb","oc"))


metricas_grid_BTC_base <- metricas2%>%
  filter(!label_scen=="BAU")%>%
  filter(!conservation=="C")%>%
  #mutate(relative_to_BAU_2050=relative_to_BAU_2050/max(relative_to_BAU_2050))%>%
  ggplot(aes(y = region, x=name)) +
  geom_raster(aes(fill = relative_to_BAU_2050_sc))+
  #scale_fill_viridis(name = "Rel. BAU",breaks = my_breaks,trans=scales::pseudo_log_trans(sigma = 0.001))+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
  #scale_fill_viridis(limit = limit)+
  theme_bw()+
  #scale_fill_distiller(type = "div", limit = limit)+
  scale_fill_scico(palette = "roma", limit = limit)+ 
  facet_wrap(~label_scen)+
  rotate_x_text(angle = 90)+
  xlab("")+
  ylab("")+
  ggtitle("BTC-base") +
  theme(legend.position = "none")



metricas_grid_C <- metricas2%>%
  filter(!label_scen=="BAU")%>%
  filter(conservation=="C")%>%
  ggplot(aes(y = region, x=name)) +
  geom_raster(aes(fill = relative_to_BAU_2050_sc))+
  # scale_fill_viridis(name = "Rel. BAU",breaks = my_breaks,trans=scales::pseudo_log_trans(sigma = 0.001))+
  theme_bw()+
  scale_fill_scico(palette = "roma", limit = limit,name = "Rel. variation BAU")+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
  facet_wrap(~label_scen)+
  rotate_x_text(angle = 90)+
  xlab("")+
  ylab("")+
  ggtitle("C")+
  theme(legend.position = c(.85, .2))


#option="plasma"

tiles_plot <- ggarrange(metricas_grid_BTC_base,metricas_grid_C,widths = c(2,3))




# talves separar aqui apenas quem melhorou e quem piorou em termos relativos a 2050 pode funcionar legal tb! mas perde info.

ggsave(filename = "figures_paper/metrics_PLANGEA_Trade_scen_with_conservation_relative_BAU2050_heatmaps.jpeg",width = 23,height = 12,units = "cm",plot = tiles_plot,bg ="white")

#------------------------------------------------------------------------------

# plot regional map

regions <- st_read(file.path("/dados/pessoal/francisco/TradeHub/country_boundaries","world_11regions_boundaries.shp"))

regions_points<- st_centroid(regions)
regions_points <- cbind(regions, st_coordinates(st_centroid(regions$geometry)))


countries <-  st_read(file.path("/dados/pessoal/francisco/TradeHub/country_boundaries","country_boundaries.shp"))


regi_map <- ggplot(data = countries) +
  geom_sf(color = "black",aes(fill = AggrgtR),alpha=0.8)+
  theme_map()+
  scale_fill_brewer(NULL,palette = 'RdYlGn', direction = -1)+
  #scale_fill_viridis_d(option = "turbo")+
  #geom_text(data =regions_points ,aes(x=X, y=Y, label=AggrgtR))+
  geom_label(data = regions_points,aes(x = X, y = Y, label = AggrgtR),
                 hjust = 0, nudge_x = -2, nudge_y = 0,
                 size = 2, color = "black", fontface = "bold")+
  theme(legend.position = "none")


ggsave(filename = "figures_paper/region_map.jpeg",width = 22,height = 12,units = "cm",plot = regi_map,bg ="white")

################################################################################
# figura simplificada
################################################################################
l2 <-expression(paste("proportional change relative to baseline 2020 (% x"~10^-4," ) ",sep=""))
yl <- expression(paste("pasture and agriculture  change (km"^2~10^3,")"))
yl2 <- expression(paste("natural areas cover change (km"^2~10^3,")"))


# falta aqui ainda o elemento do cenario. Acho q vale somar agri.  e ter outro pra area natural!
# somando total de pastagem + agri
total_agri <- (1.625285e+13 + 1.800167e+13)/10^3


# Define a custom formatting function for the secondary y-axis labels

my_format <- function(x) {
  sapply(x, function(y) {
    if (is.na(y)) {
      # Return an empty string if the input value is missing
      ""
    } else if (y >= 1 || y <= -1) {
      # Format the label as usual if the value is greater than or equal to 1 or less than or equal to -1
      scales::number_format()(y)
    } else if (is.infinite(y) && y > 0) {
      # Return 0 if the input value is positive infinite
      "0"
    } else if (is.infinite(y) && y < 0) {
      # Return "-0" if the input value is negative infinite
      "-0"
    } else {
      # Convert the exponent to a string
      exponent <- format(log10(abs(y)), scientific = FALSE)
      # Construct the label as "10^exponent"
      label <- paste0("10 ", floor(as.numeric(exponent)))
      # Return the label
      label
    }
  })
}



agri_exp <- lulcc%>%
  filter(region!="Total")%>%
  mutate(region = factor(region, levels=(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN'))))%>%
  mutate(value=value/10^3)%>%
  filter(name %in% c("AGR","PAS"))%>%
  group_by_at(c(1,6,7))%>%
  summarise(value=sum(value))%>%
  ggplot() +
    aes(x = label_scen, fill = region, weight = value) +
    geom_bar() +
    scale_fill_brewer(palette = "Set3", name="")+
    geom_hline(yintercept = 0, linetype = "dotted", color = "red",linewidth=1.5)+
    #scale_fill_hue(direction = 1) +
    theme_bw()+
    facet_wrap(vars(conservation))+
    xlab("")+
    ylab(yl)+
    rotate_x_text(45)
    # add a second y-axis
    # scale_y_continuous(sec.axis = sec_axis(~ . /(total_agri*10^3), 
    #   name = "change relative to baseline (%)",  labels = my_format))
    
nat_veg_exp <- lulcc%>%
  filter(region!="Total")%>%
  mutate(region = factor(region, levels=(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN'))))%>%
  mutate(value=value/10^3)%>%
  filter(name %in% c("FOR","NGR","OTN","SHR","WET"))%>%
  group_by_at(c(1,6,7))%>%
  summarise(value=sum(value))%>%
  ggplot() +
  aes(x = label_scen, fill = region, weight = value) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3", name="")+
  geom_hline(yintercept = 0, linetype = "dotted", color = "red",linewidth=1.5)+
  #scale_fill_hue(direction = 1) +
  theme_bw()+
  facet_wrap(vars(conservation))+
  xlab("")+
  ylab(yl2)+
  rotate_x_text(45)

panel_lulc <- ggarrange(agri_exp,nat_veg_exp,common.legend = T)

ggsave(filename = "figures_paper/lulcc_tidy.jpeg",width = 25,height = 12,units = "cm",plot = panel_lulc,bg ="white")
