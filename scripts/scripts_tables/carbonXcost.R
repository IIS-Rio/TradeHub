# combinando 2 tabelas pra calcular custo minimo do carbono e custo em relacao ao
# presente

#---- pacotes ------------------------------------------------------------------

library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(tidyr)

#-------------------------------------------------------------------------------

cb <- read.csv("output_tables/regional_cb.csv")
oc <- read.csv("output_tables/regional_oc.csv")

cost_c <- left_join(cb[,c(-3)],oc[,c(-3)])

# filtrando apenas dados com sequestro de carbono e com oc positivo (de fato custo)


cost_c_f <- cost_c%>%
  # filtrando carbono
  filter(total_cb>0)%>%
  # filtrando co
  filter(total_oc>0)%>%
  mutate(cost_ton=total_oc/total_cb)


write.csv(cost_c_f,"output_tables/carbon_cost.csv",row.names = F)

cost_c_f$label_scen <- factor(cost_c_f$label_scen,levels = c("ETL","Ta","Tr","Fr","BAU","Trade-base"))

cost_c_f$region <- factor(cost_c_f$region, levels=(c('LAC','SAS','EUR','EAS','OCE','CAN')))

#===============================================================================
# plotando custo minimo da tonelada pra cobrir oc nos paises que tiveram sequestro de carbono e balanco positivo do oc
#===============================================================================


# extrai nome do grafico de carbono plotado no script cb!

colors <- c("#CCEBC5","#FDB462","#80B1D3","#FB8072","#FFFFB3","#8DD3C7")

minimum_cost <- ggplot(cost_c_f,aes(x=label_scen,y = cost_ton,col=region))+
  #scale_colour_brewer(palette = "Set3", name="")+
  scale_colour_manual(values = colors)+
  #geom_point(size=4 ,alpha=0.9)+
  geom_jitter(size=3,aes(colour = region))+
  # incluir linha de 20 dolares e 50 dolares
  geom_hline(yintercept=20, linetype="dashed", color = "darkgray")+
  geom_hline(yintercept=50, linetype="dashed", color = "darkgray")+
  geom_hline(yintercept=10, linetype="dashed", color = "darkgray")+
  scale_y_continuous(breaks = c(0,10,20,50,100,200,300))+
  theme_classic()+
  ylab(expression(paste("minimum cost of the ton of ", CO[2],"(USD)", sep=" "))
)+
  xlab("")+
  facet_grid(~conservation, drop = TRUE,scales="free")+
  theme(axis.title.y.right = element_blank(),                # 
        axis.text.y.right = element_blank(),                 # 
        axis.ticks.y = element_blank(),                      # 
        axis.text.y = element_text(margin = margin(r = 0)),  # 
        #panel.spacing = unit(0, "mm"),                      # 
        strip.background = element_blank())                  # hide facet outline


ggsave(filename = "figures_paper/minimum_carbon_cost.jpg",plot =minimum_cost,width = 15,height = 10,units = "cm" )

#===============================================================================
# plotando efeito de varios valores de carbono
#===============================================================================


#pra esse grafico, acho q o legal é usar os dados totais e ver como incluir carbono muda o balanco da figura

cost_c_variavel <- cost_c%>%
  mutate(cost_ton=ifelse(test = total_cb>0,yes = total_oc/total_cb,no = 0))%>%
  # net oc com carbono adicionado
  mutate(ton_5 = ifelse(total_cb>0,total_cb*5 + total_oc,total_oc))%>%
  mutate(ton_10 = ifelse(total_cb>0,total_cb*10 + total_oc,total_oc))%>%
  #mutate(ton_15 = ifelse(total_cb>0,total_cb*5 + total_oc),0)%>%
  mutate(ton_20 = ifelse(total_cb>0,total_cb*20 + total_oc,total_oc))%>%
  mutate(ton_30 = ifelse(total_cb>0,total_cb*30 + total_oc,total_oc))%>%
  mutate(ton_50 = ifelse(total_cb>0,total_cb*50 + total_oc,total_oc))

# criando coluna com valor de carbono


cost_c_variavel_p <- cost_c_variavel %>%
  pivot_longer(cols = 6:11)

?pivot_longer  

cost_c_variavel_p$label_scen <- factor(cost_c_variavel_p$label_scen,levels = c("ETL","Ta","Tr","Fr","BAU","Trade-base"))


p_BTC_base <- cost_c_variavel_p %>%
  filter(conservation=="BTC-base")%>%
  filter(name!="cost_ton")%>%
  # calculate percentages
  group_by(conservation,label_scen)%>%
  #filter(label_scen==label_scen,region==region,conservation==conservation)%>%
  mutate(prop=value/sum(abs(value)))%>%
  mutate(label=paste0(round(abs(prop),2)*100,"%"))%>%
  mutate(final_oc=value/10^9)%>%
  arrange(prop) %>%
  mutate(region = factor(region, levels=rev(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))) %>%
  ggplot(aes(x=label_scen,y = final_oc,fill=region))+
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
  #ylim(-700,200)+
  facet_grid(~name)+
  ylim(-700,650)

p_BTC_C <- cost_c_variavel_p %>%
  filter(conservation!="BTC-base")%>%
  filter(name!="cost_ton")%>%
  # calculate percentages
  group_by(conservation,label_scen)%>%
  #filter(label_scen==label_scen,region==region,conservation==conservation)%>%
  mutate(prop=value/sum(abs(value)))%>%
  mutate(label=paste0(round(abs(prop),2)*100,"%"))%>%
  mutate(final_oc=value/10^9)%>%
  arrange(prop) %>%
  mutate(region = factor(region, levels=rev(c('LAC', 'SSA', 'CSI','SEA','MNA','SAS','EUR','EAS','USA','OCE','CAN')))) %>%
  ggplot(aes(x=label_scen,y = final_oc,fill=region))+
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
  #ylim(-700,200)+
  facet_grid(~name)+
  ylim(-700,700)
