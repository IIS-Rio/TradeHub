#---- pacotes ------------------------------------------------------------------

library(dplyr)
library(ggpubr)
library(purrr)
library(scales)
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
  scale_fill_brewer(palette = "Greens",labels=c("BTC-base","C"),name="")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  facet_grid(label_scen~name)+
  #theme_classic()+
  theme_bw()+
  theme(legend.position="top")

ggsave(filename = "figures_paper/lulcc_PLANGEA_Trade_scen_with_conservation_absolute.jpeg",width = 22,height = 20,units = "cm",plot = lulcc_plangea,bg ="white")

################################################################################
## relative changes
################################################################################

# precisa ter a area total desses usos pra 2020

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

# agregando ao df 

lulcc2 <- left_join(lulcc,areas_2020,by=c("name"="lu_PLANGEA"))


lulcc2 <- lulcc2%>%
  mutate(prop_change=value/total_area)

lulcc2$label_scen <-factor(lulcc2$label_scen,levels = (c("ETL","Ta","Tr","Fr","Trade-base")))


lulcc2$name <- factor(lulcc2$name,levels = c("AGR","PAS","FOR","NGR","SHR","WET","DES","OTN","OTR"))


l2 <-expression(paste("proportional change relative to baseline 2020 (% x"~10^-4," ) ",sep=""))

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
  scale_fill_brewer(palette = "Greens",labels=c("BTC-base","C"),name="")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  facet_grid(label_scen~name)+
  #theme_classic()+
  theme_bw()+
  rotate_x_text(angle = 90)+
  theme(legend.position="top")

ggsave(filename = "figures_paper/lulcc_PLANGEA_Trade_scen_with_conservation_relative.jpeg",width = 22,height = 20,units = "cm",plot = lulcc_plangea_rel,bg ="white")


# talvez valha a pena focar nos usos com maiores mudanças, e deixar todos como
# material suplementar. Nesse caco: agr,pas,for,NGR

# ordenar por area de mudanca - definir manualmente depois de agregar total

mudanca_total <- lulcc2%>%
  group_by(region)%>%
  filter(name=="AGR"|name=="PAS")%>%
  filter(conservation=="BTC-base")%>%
  summarise(total_change=sum(value))

levels <- rev(c('LAC','EAS','CSI','MNA','EUR','OCE','CAN'))

lulcc_plangea_rel2 <- lulcc2%>%
  #group_by(region)%>%
  arrange(desc(prop_change),.by_group = T)%>%
  mutate(region=factor(region, levels=levels)) %>%   # update the factor levels
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
  scale_fill_brewer(palette = "Greens",labels=c("BTC-base","C"),name="")+
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
  # excluir OCE pq nao acabou de rodar
  filter(!region=='OCE')%>%
  group_by(region,name)%>%
  filter(variable== unique(variable)) %>%
  mutate(relative_to_BAU_2050=value/value[label_scen=="BAU"])

# tem q ser relativo ao BAU, logo a dif entre 1 eh o scenario eh a variacao  
# fazer sem assinalar o BAU 2050
#metricas$relative_to_BAU_2050 <- metricas$relative_to_BAU_2050 -1

#ordenar 

metricas$label_scen <-factor(metricas$label_scen,levels = (c("ETL","Ta","Tr","Fr","Trade-base","BAU")))

metricas$variable <- factor(metricas$variable,levels = (c("Extinction debt","Ecoregion vulnerability","Ecossistem integrity","Carbon","Land opportunity cost")))

# falta ordenar por regiao e entender melhor as metricas: qndo eh reducao, qndo eh aumento!

metricas$region <- factor(metricas$region,levels = rev(c('LAC','EAS','CSI','MNA','EUR','OCE','CAN')))

# reducao no risco de extincao
# vulnerabilidade das ecorregioes
# reducao na integridade dos ecossistemas

# a escala de mudancas eh mto distinta pra plotar junto. talvez separar por regiao seja melhor




metricas_plangea_rel <- metricas%>%
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
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),breaks = c(-30,-100,-7,-2,0,2,7,30,100))+
  #ggtitle(land_use)+
  scale_fill_brewer(palette = "Greens",labels=c("BTC-base","C"),name="")+
  geom_hline(yintercept=0, linetype="dashed",color = "darkgray", size=1)+
  facet_grid(label_scen~variable,scales = "free_x")+
  #theme_classic()+
  theme_bw()+
  rotate_x_text(angle = 90)+
  theme(legend.position="top")

ggsave(filename = "figures_paper/metrics_PLANGEA_Trade_scen_with_conservation_relative_BAU2050.jpeg",width = 22,height = 20,units = "cm",plot = metricas_plangea_rel,bg ="white")
