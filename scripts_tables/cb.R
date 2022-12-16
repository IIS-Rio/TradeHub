library(readr)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

resultados_cenarios_regional_analysis <- read_csv("output_tables/resultados_cenarios_regional_analysis.csv") %>%
  filter(name=="cb.val")


total_cb <- resultados_cenarios_regional_analysis %>%
  group_by(label_scen,conservation,variable)%>%
  summarise(total_cb=sum(value))


write_csv(x = total_cb,file = "output_tables/global_cb.csv")

total_cb_region  <- resultados_cenarios_regional_analysis %>%
  group_by(label_scen,conservation,variable,region)%>%
  summarise(total_cb=sum(value))

write_csv(x = total_cb_region,file = "output_tables/regional_cb.csv")

#-------------------------------------------------------------------------------

# testando stacked barplot com oc por regiao

total_cb_region$label_scen <- factor(total_cb_region$label_scen,levels = c("ETL","Ta","Tr","Fr","BAU","Trade-base"))

p_BTC_base <- total_cb_region %>%
  filter(conservation=="BTC-base")%>%
  # calculate percentages
  group_by(conservation,label_scen)%>%
  #filter(label_scen==label_scen,region==region,conservation==conservation)%>%
  mutate(prop=total_cb/sum(abs(total_cb)))%>%
  mutate(label=paste0(round(abs(prop),2)*100,"%"))%>%
  mutate(total_cb_1000=total_cb/10^9)%>%
  arrange(prop) %>%
  mutate(name = factor(region, levels=rev(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))) %>%
  ggplot(aes(x=label_scen,y = total_cb_1000,fill=name))+
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),size = 1.5,colour = 'black', seed = 1,segment.colour = "white")+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=1)+
  scale_fill_brewer(palette = "Set3", name="")+
  scale_y_continuous(labels = comma)+
  #facet_grid(~conservation)+
  theme_bw()+
  ggtitle("BTC-base")+
  xlab("")+
  ylab(expression(paste(CO[2], " balance ( billion tons) ", sep="")))+
  #ylab("CO2 balance ( billion tons)")+
  ylim(-25,12)


p_BTC_C <- total_cb_region %>%
  filter(conservation!="BTC-base")%>%
  # calculate percentages
  group_by(conservation,label_scen)%>%
  #filter(label_scen==label_scen,region==region,conservation==conservation)%>%
  mutate(prop=total_cb/sum(abs(total_cb)))%>%
  mutate(label=paste0(round(abs(prop),2)*100,"%"))%>%
  mutate(total_oc_1000=total_cb/10^9)%>%
  arrange(prop) %>%
  mutate(name = factor(region, levels=rev(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))) %>%
  ggplot(aes(x=label_scen,y = total_oc_1000,fill=name))+
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),size = 1.5,colour = 'black', seed = 1,segment.colour = "white")+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=1)+
  scale_fill_brewer(palette = "Set3", name="")+
  scale_y_continuous(labels = comma)+
  #facet_grid(~conservation)+
  theme_bw()+
  ggtitle("C")+
  xlab("")+
  ylab(expression(paste(CO[2], " balance ( billion tons) ", sep="")))+
  ylim(-25,12)


final <- ggarrange(p_BTC_base,p_BTC_C,common.legend = T)


ggsave(filename = "figures_paper/cb_barplot.jpeg",width = 16,height = 16,units = "cm",plot = final)
