library(data.table)
library(tidyverse)
library(ggplot2)
library(ggpubr)

# dados calculados pelo PLANGEA ------------------------------------------------
# df <- read.csv("output_tables/resultados_cenarios_regional_analysis.csv")
# 
# lulcc <- df %>%
#   filter(variable=="lulcc")%>%
#   filter(!name=="area")
# 
# # tem q tirar o BAU
# 
# lulcc$label_scen[lulcc$label_scen=="BAU"] <- "Trade-base"

# dados calculados por mim com delta lu ----------------------------------------

df <- fread("/dados/pessoal/francisco/TradeHub/output_tables/updated_results/lulcc.csv")[,-1]

lulcc <- pivot_longer(df,c(3,4))

# calculando net

lulcc2 <- lulcc%>%
  group_by_at(c(2,3,4))%>%
  summarise(value=sum(value))

lulcc2$label_scen[lulcc2$label_scen=="BAU"] <- "Trade-base"

lulcc2$label_scen <-factor(lulcc2$label_scen,levels = (c("ETL","Ta","Tr","Fr","Trade-base")))

# so usa nos dados plangea
# lulcc$name <- factor(lulcc$name,levels = c("AGR","PAS","FOR","NGR","SHR","WET","DES","OTN","OTR"))

yl <- expression(paste("pasture and agriculture  change (million km"^2,")"))
yl2 <- expression(paste("natural areas cover change (ha"^2~10^6,")"))


# falta aqui ainda o elemento do cenario. Acho q vale somar agri.  e ter outro pra area natural!
# somando total de pastagem + agri
#total_agri <- (1.625285e+13 + 1.800167e+13)/10^3


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


# plots usando plangea ---------------------------------------------------------
# agri_exp <- lulcc%>%
#   filter(region!="Total")%>%
#   mutate(region = factor(region, levels=(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN'))))%>%
#   mutate(value=value/10^3)%>%
#   filter(name %in% c("AGR","PAS"))%>%
#   group_by_at(c(1,6,7))%>%
#   summarise(value=sum(value))%>%
#   ggplot() +
#   aes(x = label_scen, fill = region, weight = value) +
#   geom_bar() +
#   scale_fill_brewer(palette = "Set3", name="")+
#   geom_hline(yintercept = 0, linetype = "dotted", color = "red",linewidth=1.5)+
#   #scale_fill_hue(direction = 1) +
#   theme_bw()+
#   facet_wrap(vars(conservation))+
#   xlab("")+
#   ylab(yl)+
#   rotate_x_text(45)
# 
# 
# nat_veg_exp <- lulcc%>%
#   filter(region!="Total")%>%
#   mutate(region = factor(region, levels=(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN'))))%>%
#   mutate(value=value/10^3)%>%
#   filter(name %in% c("FOR","NGR","OTN","SHR","WET"))%>%
#   group_by_at(c(1,6,7))%>%
#   summarise(value=sum(value))%>%
#   ggplot() +
#   aes(x = label_scen, fill = region, weight = value) +
#   geom_bar() +
#   scale_fill_brewer(palette = "Set3", name="")+
#   geom_hline(yintercept = 0, linetype = "dotted", color = "red",linewidth=1.5)+
#   theme_bw()+
#   facet_wrap(vars(conservation))+
#   xlab("")+
#   ylab(yl2)+
#   rotate_x_text(45)
# 
# panel_lulc <- ggarrange(agri_exp,nat_veg_exp,common.legend = T)
# 
# ggsave(filename = "figures_paper/lulcc_tidy.jpeg",width = 25,height = 12,units = "cm",plot = panel_lulc,bg ="white")

# fazer com os deltas de land use calculados -----------------------------------

# parece q ta certo o lulc, entao vale rever o carbono!!deve ser algo com o sinal!!


#Calculate the ratio between all other categories of label_scen and label_scen=="trade-base"

trade_base_value <- sum(lulcc2$value[lulcc2$label_scen=="Trade-base"&lulcc2$conservation=="BTC-base"])

trade_base_net <- lulcc2 %>%
  #filter(label_scen != "trade-base") %>%
  group_by(conservation,label_scen) %>%
  summarise(value=sum(value))%>%
  mutate(region="",
         value=value/100000000,
         ratio=value/18.0066413)

agri_exp <- lulcc2%>%
  # calculate percentages
  group_by(conservation,label_scen)%>%
  #filter(label_scen==label_scen,region==region,conservation==conservation)%>%
  mutate(prop=value/sum(abs(value)))%>%
  mutate(label=paste0(round(abs(prop),2)*100,"%"))%>%
  mutate(AggrgtR = factor(AggrgtR, levels=(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN'))),
 #        exclude=if_else(abs(prop)<=0.01,"exclude","include")
         )%>%
  mutate(label=ifelse(abs(prop)>0.03,label,""))%>%
  mutate(value=value/100000000)%>%
  mutate(ratio2 = value /trade_base_value)%>%
  dplyr::rename(region = AggrgtR)%>%
  ggplot() +
  aes(x = label_scen, fill = region,group=region, weight= value) +
  geom_bar() +
  #geom_text(data = trade_base_net[trade_base_net$conservation!="BTC-base",],aes(label = round(value,1),x = label_scen,y=value), position = position_stack(vjust = 1.5),size = 2,colour = 'black') +
  geom_text(
    #data = . %>% filter(exclude=="include" ),  # Filtered data for geom_text
    aes(label = label, x = label_scen, y = value,group=region),
    position = position_stack(vjust = 0.5),
    size = 2,
    colour = 'black'
  ) +
  scale_fill_brewer(palette = "Set3", name="",direction=-1)+
  geom_hline(yintercept = 0, linetype = "dotted", color = "red",linewidth=1.5)+
  #scale_fill_hue(direction = 1) +
  theme_bw()+
  facet_wrap(vars(conservation))+
  xlab("")+
  ylab(yl)+
  rotate_x_text(45)+
  # Add secondary y-axis representing the ratio between all other categories of label_scen and label_scen=="trade-base"
  scale_y_continuous(breaks=c(-4,0,4,8,12,16,20),
    sec.axis = sec_axis(~ ./(trade_base_value/100000000),breaks = c(-0.2221,0,0.2221,0.4442,0.666,0.888,1.110),labels =c(-0.2,0,0.2,0.4,0.6,0.8,1.1),"Ratio with BAU" ))


# Extract the unique colors from the original plot
original_colors <- unique(agri_exp$'')

# Define a color vector to use for geom_point
# Assuming you want to cycle through the original colors
point_colors <- rep(original_colors, length.out = nrow(trade_base_net))




agri_exp2 <- agri_exp + geom_point(data = trade_base_net[trade_base_net$conservation!="BTC-base",], aes(x = label_scen, y = value), shape = 8, size = 2, colour = "black",fill=NA,show.legend = FALSE)+
  scale_color_identity()  # Ensures that the color scale remains unchanged
  
  
 
ggsave(filename = "/dados/pessoal/francisco/TradeHub/figures_paper_new_versions/lulcc_barplot.jpeg",width = 16,height = 16,units = "cm",plot = agri_exp2)

# falta ajustar porcentagem/colocar net values tb! e diminuir % qndo eh menor 3%
