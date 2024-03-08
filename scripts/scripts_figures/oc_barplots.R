library(readr)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(scales)

resultados_cenarios_regional_analysis <- read_csv("output_tables/resultados_cenarios_regional_analysis.csv") %>%
  filter(name=="oc.val")


total_oc <- resultados_cenarios_regional_analysis %>%
  group_by(label_scen,conservation,variable)%>%
  summarise(total_oc=sum(value))%>%
  mutate(total_oc_1000=total_oc/10^9,
         total_oc_1000=total_oc_1000*-1 )


#write_csv(x = total_oc,file = "output_tables/global_oc.csv")

total_oc_region  <- resultados_cenarios_regional_analysis %>%
  group_by(label_scen,conservation,variable,region)%>%
  summarise(total_oc=sum(value))

#write_csv(x = total_oc_region,file = "output_tables/regional_oc.csv")


#-------------------------------------------------------------------------------




# testando stacked barplot com oc por regiao

total_oc_region$label_scen <- factor(total_oc_region$label_scen,levels = c("ETL","Ta","Tr","Fr","BAU","Trade-base"))

p_BTC_base <- total_oc_region %>%
  filter(conservation=="BTC-base")%>%
  # invert signal
  mutate(total_oc=total_oc*-1)%>%
  # calculate percentages
  group_by(conservation,label_scen)%>%
  #filter(label_scen==label_scen,region==region,conservation==conservation)%>%
  mutate(prop=total_oc/sum(abs(total_oc)))%>%
  mutate(label=paste0(round(abs(prop),2)*100,"%"))%>%
  mutate(total_oc_1000=total_oc/10^9)%>%
  # colocando em branco % < 1
  mutate(label=ifelse(abs(prop)>0.01,label,""))%>%
  arrange(prop) %>%
  mutate(name = factor(region, levels=rev(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))) %>%
  ggplot(aes(x=label_scen,y = total_oc_1000,fill=name),group=region)+
    geom_bar(position="stack", stat="identity")+
    geom_text(aes(label = label), position = position_stack(vjust = 0.5),size = 2    ,colour = 'black', seed = 1,segment.colour = "white")+
    geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgrey", size=1)+
    scale_fill_brewer(palette = "Set3", name="")+
    scale_y_continuous(labels = comma)+
    # incluindo net value
    geom_point(data = total_oc[total_oc$conservation=="BTC-base",],aes(y=total_oc_1000,x=label_scen),fill="black",colour="black",shape=8, position=position_nudge(x = -0.2))+
    #facet_grid(~conservation)+
    theme_bw()+
    ggtitle("BTC-base")+
    xlab("")+
    ylab("Billion USD")+ #land opportunity cost ( billion USD)
    ylim(-210,700)+
    guides(fill = guide_legend(reverse=TRUE))+
  annotate("text", x = 0.3, y = 50, label = "Profit", vjust = -0.5, hjust = 0, size = 4, angle = 90)+
  annotate("text", x = 0.3, y = -150, label = "Cost", vjust = -0.5, hjust = 0, size = 4, angle = 90)+
  coord_cartesian(xlim = c(0.9, 5),  clip = 'off') 

# p_BTC_base <- p_BTC_base +
#   geom_segment(aes(x = 0.21, xend = 0.21, y = -155, yend = -205), arrow = arrow(length = unit(0.3, "cm")), color = "black") +
#  geom_segment(aes(x =  0.21, xend =  0.21, y = 128, yend = 178), arrow = arrow(length = unit(0.3, "cm")), color = "black")


p_BTC_C <- total_oc_region %>%
  filter(conservation!="BTC-base")%>%
  # invert signal
  mutate(total_oc=total_oc*-1)%>%
  # calculate percentages
  group_by(conservation,label_scen)%>%
  #filter(label_scen==label_scen,region==region,conservation==conservation)%>%
  mutate(prop=total_oc/sum(abs(total_oc)))%>%
  mutate(label=paste0(round(abs(prop),2)*100,"%"))%>%
  mutate(total_oc_1000=total_oc/10^9)%>%
  arrange(prop) %>%
  # colocando em branco % < 1
  mutate(label=ifelse(abs(prop)>0.01,label,""))%>%
  mutate(name = factor(region, levels=rev(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))) %>%
  ggplot(aes(x=label_scen,y = total_oc_1000,fill=name))+
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),size = 2,colour = 'black', seed = 1,segment.colour = "white")+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=1)+
  scale_fill_brewer(palette = "Set3", name="")+
  scale_y_continuous(labels = comma)+
  # incluindo net value
  geom_point(data = total_oc[total_oc$conservation!="BTC-base",],aes(y=total_oc_1000,x=label_scen),fill="black",colour="black",shape=8, position=position_nudge(x = -0.3))+
  #facet_grid(~conservation)+
  theme_bw()+
  ggtitle("C")+
  xlab("")+
  ylab("")+
  ylim(-210,700)+
  guides(fill = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(0.9, 5),  clip = 'off') 


p_BTC_base <- ggpar(p = p_BTC_base,font.caption = c("plain",7,"black"),font.x = c("plain",7,"black"),font.y = c("plain",9,"black"),font.legend =c("plain",7,"black"),font.tickslab = c("plain",7,"black"),font.main = c("bold",9,"black") )

p_BTC_C  <- ggpar(p = p_BTC_C ,font.caption = c("plain",7,"black"),font.x = c("plain",7,"black"),font.y = c("plain",8,"black"),font.legend =c("plain",7,"black"),font.tickslab = c("plain",7,"black"),font.main = c("bold",9,"black") )


final <- ggarrange(p_BTC_base,p_BTC_C,common.legend = T)


ggsave(filename = "/dados/pessoal/francisco/TradeHub/figures_paper_new_versions/oc_barplot.jpeg",width = 16,height = 16,units = "cm",plot = final)


# get colors to use in other plots


colors <- unique(ggplot_build(p_BTC_base)$data[[1]]$fill)

colors <- c("#CCEBC5", "#BC80BD" ,"#FCCDE5","#B3DE69", "#D9D9D9", "#FDB462","#FB8072" ,"#80B1D3", "#BEBADA", "#FFFFB3", "#8DD3C7")

df <- data.frame(x=1:11, y=1, col=letters[1:11])

plot(df$x, df$y, col=colors, pch=20, cex=5)
