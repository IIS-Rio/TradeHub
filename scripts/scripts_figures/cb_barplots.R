library(data.table)
library(tidyverse)
library(scales)
library(ggpubr)

# combining new carbon data (move to another script when possible)

emited <- fread("output_tables/updated_results/carbon_emmited_carbonlayerIIS.csv")

# convertendo em ha

emited$cb_emission <- emited$cb_emission/100

seq <- fread("output_tables/updated_results/carbon_sequetered_restoration_Naturemap.csv")




df <- left_join(emited,seq)[,c(2:4,6,7,9)]

names(df)[c(2,6)] <- c("region","cb_seq")

df$net <- df$cb_emission+df$cb_seq


df_l <- pivot_longer(df,c(3,6,7))

#-------------------------------------------------------------------------------

# testando stacked barplot com oc por regiao
# valor carbono emitido ta superestimado


df_l$label_scen <- factor(df_l$label_scen,levels = c("ETL","Ta","Tr","Fr","BAU","Trade-base"))


total_cb <- df_l %>%
  filter(name!="net")%>%
  group_by(label_scen,conservation)%>%
  summarise(total_cb=sum(value))%>%
  mutate(total_cb_1000=total_cb/10^9)





p_BTC_base <- df_l %>%
  filter(conservation=="BTC-base",
         name=="net")%>%
  # calculate percentages
  group_by(conservation,label_scen)%>%
  #filter(label_scen==label_scen,region==region,conservation==conservation)%>%
  mutate(prop=value/sum(abs(value)))%>%
  mutate(label=paste0(round(abs(prop),2)*100,"%"))%>%
  mutate(total_cb_1000=value/10^9)%>%
  # colocando em branco % < 1
  mutate(label=ifelse(abs(prop)>0.02,label,""))%>%
  arrange(prop) %>%
  mutate(name = factor(region, levels=rev(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))) %>%
  ggplot(aes(x=label_scen,y = total_cb_1000,fill=name))+
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),size = 2,colour = 'black', seed = 1,segment.colour = "white")+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=1)+
  scale_fill_brewer(palette = "Set3", name="")+
  scale_y_continuous(labels = comma)+
  # incluindo net value
#  geom_point(data = total_cb[total_cb$conservation=="BTC-base",],aes(y=total_cb_1000,x=label_scen),fill="black",colour="black",shape=8, position=position_nudge(x = -0.2))+
  #facet_grid(~conservation)+
  theme_bw()+
  ggtitle("BTC-base")+
  xlab("")+
  ylab(expression(paste(CO[2], " balance ( billion tons) ", sep="")))+
  #ylab("CO2 balance ( billion tons)")+
  ylim(-11,210)+
  guides(fill = guide_legend(reverse=TRUE))


p_BTC_C <- df_l %>%
  filter(conservation!="BTC-base",
         name=="net")%>%
  # calculate percentages
  group_by(conservation,label_scen)%>%
  #filter(label_scen==label_scen,region==region,conservation==conservation)%>%
  mutate(prop=value/sum(abs(value)))%>%
  mutate(label=paste0(round(abs(prop),2)*100,"%"))%>%
  mutate(total_oc_1000=value/10^9)%>%
  arrange(prop) %>%
  # colocando em branco % < 1
  mutate(label=ifelse(abs(prop)>0.02,label,""))%>%
  mutate(name = factor(region, levels=rev(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))) %>%
  ggplot(aes(x=label_scen,y = total_oc_1000,fill=name))+
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),size = 2,colour = 'black', seed = 1,segment.colour = "white")+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgrey", size=1)+
  scale_fill_brewer(palette = "Set3", name="")+
  scale_y_continuous(labels = comma)+
  # incluindo net value
#  geom_point(data = total_cb[total_cb$conservation!="BTC-base",],aes(y=total_cb_1000,x=label_scen),fill="black",colour="black",shape=8, position=position_nudge(x = -0.3))+
  #facet_grid(~conservation)+
  theme_bw()+
  ggtitle("C")+
  xlab("")+
  ylab(expression(paste(CO[2], " balance ( billion tons) ", sep="")))+
  ylim(-11,210)+
  guides(fill = guide_legend(reverse=TRUE))

p_BTC_base <- ggpar(p = p_BTC_base,font.caption = c("plain",7,"black"),font.x = c("plain",7,"black"),font.y = c("plain",9,"black"),font.legend =c("plain",7,"black"),font.tickslab = c("plain",7,"black"),font.main = c("bold",9,"black") )

p_BTC_C  <- ggpar(p = p_BTC_C ,font.caption = c("plain",7,"black"),font.x = c("plain",7,"black"),font.y = c("plain",8,"black"),font.legend =c("plain",7,"black"),font.tickslab = c("plain",7,"black"),font.main = c("bold",9,"black") )




final <- ggarrange(p_BTC_base,p_BTC_C,common.legend = T)


ggsave(filename = "figures_paper/cb_barplot.jpeg",width = 16,height = 16,units = "cm",plot = final)
