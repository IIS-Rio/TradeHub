library(readr)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(scales)

resultados_cenarios_regional_analysis <- read_csv("output_tables/resultados_cenarios_regional_analysis.csv") %>%
  filter(name=="oc.val")


total_oc <- resultados_cenarios_regional_analysis %>%
  group_by(label_scen,conservation,variable)%>%
  summarise(total_oc=sum(value))


write_csv(x = total_oc,file = "output_tables/global_oc.csv")

total_oc_region  <- resultados_cenarios_regional_analysis %>%
  group_by(label_scen,conservation,variable,region)%>%
  summarise(total_oc=sum(value))

write_csv(x = total_oc_region,file = "output_tables/regional_oc.csv")


#-------------------------------------------------------------------------------

# testando stacked barplot com oc por regiao

total_oc_region$label_scen <- factor(total_oc_region$label_scen,levels = c("ETL","Ta","Tr","Fr","BAU","Trade-base"))

p_BTC_base <- total_oc_region %>%
  filter(conservation=="BTC-base")%>%
  # calculate percentages
  group_by(conservation,label_scen)%>%
  #filter(label_scen==label_scen,region==region,conservation==conservation)%>%
  mutate(prop=total_oc/sum(abs(total_oc)))%>%
  mutate(label=paste0(round(abs(prop),2)*100,"%"))%>%
  mutate(total_oc_1000=total_oc/10^9)%>%
  arrange(prop) %>%
  mutate(name = factor(region, levels=rev(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))) %>%
  ggplot(aes(x=label_scen,y = total_oc_1000,fill=name))+
    geom_bar(position="stack", stat="identity")+
    geom_text(aes(label = label), position = position_stack(vjust = 0.5),size = 2,colour = 'black', seed = 1,segment.colour = "white")+
    geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgrey", size=1)+
    scale_fill_brewer(palette = "Set3", name="")+
    scale_y_continuous(labels = comma)+
    #facet_grid(~conservation)+
    theme_bw()+
    ggtitle("BTC-base")+
    xlab("")+
    ylab("land opportunity cost ( billion USD)")+
    ylim(-700,200)


p_BTC_C <- total_oc_region %>%
  filter(conservation!="BTC-base")%>%
  # calculate percentages
  group_by(conservation,label_scen)%>%
  #filter(label_scen==label_scen,region==region,conservation==conservation)%>%
  mutate(prop=total_oc/sum(abs(total_oc)))%>%
  mutate(label=paste0(round(abs(prop),2)*100,"%"))%>%
  mutate(total_oc_1000=total_oc/10^9)%>%
  arrange(prop) %>%
  mutate(name = factor(region, levels=rev(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))) %>%
  ggplot(aes(x=label_scen,y = total_oc_1000,fill=name))+
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),size = 2,colour = 'black', seed = 1,segment.colour = "white")+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=1)+
  scale_fill_brewer(palette = "Set3", name="")+
  scale_y_continuous(labels = comma)+
  #facet_grid(~conservation)+
  theme_bw()+
  ggtitle("C")+
  xlab("")+
  ylab("land opportunity cost ( billion USD)")+
  ylim(-700,200)


final <- ggarrange(p_BTC_base,p_BTC_C,common.legend = T)


ggsave(filename = "figures_paper/oc_barplot.jpeg",width = 16,height = 16,units = "cm",plot = final)


# get colors to use in other plots



colors <- unique(ggplot_build(p_BTC_base)$data[[1]]$fill)

colors <- c("#CCEBC5", "#BC80BD" ,"#FCCDE5","#B3DE69", "#D9D9D9", "#FDB462","#FB8072" ,"#80B1D3", "#BEBADA", "#FFFFB3", "#8DD3C7")

df <- data.frame(x=1:11, y=1, col=letters[1:11])

plot(df$x, df$y, col=colors, pch=20, cex=5)
