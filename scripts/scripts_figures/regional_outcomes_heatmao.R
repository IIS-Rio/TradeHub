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

levels_regions <- c("Global","SSA","LAC","CSI","SEA","EAS","MNA","OCE","SAS","EUR","USA","CAN")

################################################################################
## PLANGEA METRICS
################################################################################

# gambiarra, mudar depois

df2 <- df%>%
  mutate(value=if_else(label_scen=="BAU"&value>0,value*-1,value))

metricas <- df2%>%
  filter(!variable=="lulcc")%>%
  group_by(region,name)%>%
  filter(variable== unique(variable)) %>%
  # calculando prop. em relacao ao BAU em modulo
  #mutate(relative_to_BAU_2050=value/abs(value[label_scen=="BAU"]))
  # calculando prop. em relacao ao BAU em modulo e invertendo sinal
  # calculando prop. em relacao ao BAU em modulo e invertendo sinal
  mutate(
    relative_to_BAU_2050=((value-value[label_scen=="BAU"])/(value[label_scen=="BAU"])*100*-1)
    
    )
    


#ordenar 

metricas$label_scen <-factor(metricas$label_scen,levels = (c("ETL","Ta","Tr","Fr","Trade-base","BAU")))



metricas$variable <- recode_factor(metricas$variable, 'Ecoregion vulnerability' = "Ecoregion vulnerability reduction")


metricas$variable <- factor(metricas$variable,levels = (c("Extinction debt reduction","Ecoregion vulnerability reduction","Ecossistem integrity reduction","Carbon","Land opportunity cost")))

# falta ordenar por regiao e entender melhor as metricas: qndo eh reducao, qndo eh aumento!

metricas$region <- factor(metricas$region,levels = rev(c(levels_regions)))

#------------------------------------------------------------------------------

# plotando resultados como tiles, escalados em uma mesma escala de zero a um


# primeiro, escalar cada variavel separadamente, ente -1 e 1

pseudoLog10 <- function(x) { asinh(x/2)/log(10) }

pseudoLog10(metricas$value)

# aqui tem q arrumar, pq ta errado o scale!

metricas2 <- metricas %>%
  # colocando bd em log (tem q ser pseudolog)
   mutate(relative_to_BAU_2050=if_else(condition = name=="bd.val",true = pseudoLog10(relative_to_BAU_2050),false = relative_to_BAU_2050))%>%
  # # parte que ja funcionava antes
  group_by(across(5))%>%
  filter(variable==variable)%>%
  #mutate(relative_to_BAU_2050_sc=relative_to_BAU_2050/max(relative_to_BAU_2050))
  mutate(relative_to_BAU_2050_sc = scale(relative_to_BAU_2050, center = min(relative_to_BAU_2050), scale = max(relative_to_BAU_2050) - min(relative_to_BAU_2050)) * 2 - 1)
  
  
my_breaks <- c(-0.4,0,1,2)

metricas2$name <- gsub(pattern = ".val",replacement = "",x = metricas$name)

limit <- max(abs(metricas2$relative_to_BAU_2050_sc)) * c(-1, 1)

#ordenando siglas das variaveis


metricas2$name <- factor(metricas2$name,levels=c("bd","ec","it","cb","oc"))
names(metricas2)

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
  scale_fill_scico(palette = "roma")+ 
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
