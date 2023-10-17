lu_change <- read_csv("output_tables/resultados_lu_change_cenarios.csv")


lu_change$label_scen <-factor(lu_change$label_scen,levels = rev(c("exacerb. lib. + BTC baseline","transp.cost. red + BTC baseline","tarif.elim.+ BTC baseline","frict.&reconfig. + BTC baseline","BAU","exacerb. lib. + C","transp.cost. red + C","tarif.elim.+ C","frict.&reconfig. + C","BAU + C")))


lu_change$name <- factor(lu_change$name,levels = c("AGR","PAS","FOR","NGR","SHR","WET","DES","OTN","OTR"))

land_uses2 <- unique(lu_change$name)


usos_inuteis2 <- c("DES","OTR","OTN","desert","other nat. land.")

land_uses2 <-land_uses2[!land_uses2 %in% usos_inuteis2]

#area change (from baseline 2020)

l <-expression(paste("area-change relative to baseline 2020("~km^2,"x1000 ) ",sep=""))


# adicionando coluna pra separar os cenarios

lu_change$scen_type <- NA

lu_change$scen_type[c(grep(pattern = "+ C",x = lu_change$label_scen))] <- "C"
lu_change$scen_type[c(grep(pattern = "+ C",x = lu_change$label_scen,invert = T))] <- "baseline"



lu_plangea <- lu_change%>%
  filter(!name %in% usos_inuteis2)%>%
  mutate(value=value/1000)%>%
  ggplot(aes(y=value, x=label_scen,group = 1,col=scen_type)) +
  geom_line()+
  geom_point()+
  ylab(l)+
  xlab("")+
  rotate_x_text(angle = 90)+
  scale_x_discrete(labels=c("exacerb. lib. + BTC baseline"="ETL","transp.cost. red + BTC baseline"="Tr","tarif.elim.+ BTC baseline"="Ta","BAU"="BAU","frict.&reconfig. + BTC baseline"="Fr","exacerb. lib. + C"="ETL+C","transp.cost. red + C"="Tr+C","tarif.elim.+ C"="Ta+C","BAU + C"="BAU + C","frict.&reconfig. + C"="Fr + C"))+
  #ggtitle(land_use)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  facet_wrap("name")
# scale_x_discrete(labels=c("exacerbated liberalization"="exc.lb.","transp. cost. red."="trsp.cst.rd","tariff elimination"="tff.elmtn","BAU"="BAU","frictions and reconfigurations"="frcts.rcfgrts"))+


ggsave(filename = "figures/lu_cover_PLANGEA_Trade_scen_with_conservation.jpeg",width = 25.4,height = 14.288,units = "cm",plot = lu_plangea,bg ="white")

#-------------------------------------------------------------------------------

# plotar como barplot

#-------------------------------------------------------------------------------


lu_plangea_bar <- lu_change%>%
  filter(!name %in% usos_inuteis2)%>%
  mutate(value=value/1000)%>%
  ggplot(aes(y=value, x=label_scen,fill=scen_type)) +
  geom_bar(stat="identity")+
  coord_flip()+
  #geom_point()+
  ylab(l)+
  xlab("")+
  #rotate_x_text(angle = 90)+
  scale_x_discrete(labels=c("exacerb. lib. + BTC baseline"="ETL","transp.cost. red + BTC baseline"="Tr","tarif.elim.+ BTC baseline"="Ta","BAU"="BAU","frict.&reconfig. + BTC baseline"="Fr","exacerb. lib. + C"="ETL+C","transp.cost. red + C"="Tr+C","tarif.elim.+ C"="Ta+C","BAU + C"="BAU + C","frict.&reconfig. + C"="Fr + C"))+
  #ggtitle(land_use)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  facet_wrap("name")


# stackeado

# criar coluna comum pros cenarios

library(stringr)



# f <- function(x)str_split(string = x,pattern = " ")
# 
# apply(X = lu_change[5],1,FUN = f)

lu_change2 <- lu_change%>%
  separate(col = 5,into = c("scen",'a'),sep = " ")

#continue

lu_change2$scen[lu_change2$scen=="BAU"] <- "Trade-base"


lu_change2$scen <- factor(lu_change2$scen,levels = rev(c("exacerb.","tarif.elim.+","transp.cost.","frict.&reconfig.","Trade-base")))



label <- rev(c("ETL","Ta","Tr","Fr","Trade-base"))

lu_plangea_bar2 <- lu_change2%>%
  filter(!name %in% usos_inuteis2)%>%
  mutate(value=value/1000)%>%
  ggplot(aes(y=value, x=scen,fill=scen_type)) +
  geom_bar(stat="identity",position = "dodge")+
  coord_flip()+
  #geom_point()+
  ylab(l)+
  xlab("")+
  #rotate_x_text(angle = 90)+
   scale_x_discrete(labels=label)+
  #ggtitle(land_use)+
  scale_fill_brewer(palette = "Greens",labels=c("BTC-base","C"),name="")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  facet_wrap("name")+
  theme_classic()+
  theme(legend.position="top")




#-------------------------------------------------------------------------------

# testar pastagem + agricultura (usando output plangea)

#-------------------------------------------------------------------------------

# calculando percentualmente pasto e agri
# da uma diferença importante, pastagem faz resultados fazerem sentido!!!


lu_share_agri_past <- lu_change2%>%  
  filter(name=="AGR"|name=="PAS")%>%
  filter(value>=0)%>%
  mutate(lu_1000 = value/1000)%>%
  group_by(scen,name,scen_type) %>%
  summarise(lu_1000 = sum(lu_1000)) %>%
  mutate(freq = lu_1000 / sum(lu_1000))

l2 <-expression(paste("land coversion relative to baseline 2020("~km^2,"x1000 ) ",sep=""))


lu_share_agri_past$scen <- factor(lu_share_agri_past$scen,levels = (c("exacerb.","tarif.elim.+","transp.cost.","frict.&reconfig.","Trade-base")))

agri_past <- lu_share_agri_past%>%  
  #mutate(perc = paste0(round(freq,2)*100," ","%")) %>%
  filter(name=="AGR"|name=="PAS")%>%
  ggplot( aes(fill=scen_type, y=lu_1000, x=scen)) + 
  geom_bar( stat="identity",position = "dodge")+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  #geom_text_repel(position =position_stack(vjust = 0.5) )+
  #scale_fill_manual(values = c("chocolate1","bisque4","darkgoldenrod3","darkgreen","chartreuse2","chartreuse4","darkolivegreen1","burlywood","darkseagreen2","deepskyblue","black"),"lulc")+
  scale_fill_brewer(palette = "Greens",labels=c("BTC-base","C"),name="")+
  #coord_flip()+
  theme_classic()+
  ylab(l2)+
  xlab("")+
  theme(legend.position="right")


###############################################################################
# carbon
###############################################################################

val_l <- read.csv("output_tables/resultados_metricas_cenarios.csv")%>%
  filter(name=="cb.val"|name=="oc.val")

val_l$scen_type <- NA


val_l$scen_type[c(grep(pattern = "+ C",x = val_l$label_scen))] <- "C"
val_l$scen_type[c(grep(pattern = "+ C",x = val_l$label_scen,invert = T))] <- "baseline"

val_l2 <- val_l%>%
  separate(col = 5,into = c("scen",'a'),sep = " ")


val_l2$scen[val_l2$scen=="BAU"] <- "Trade-base"

l3 <-expression(paste("Mt of"~CO[2],sep=""))

val_l2$scen <- factor(val_l2$scen,levels = (c("exacerb.","tarif.elim.+","transp.cost.","frict.&reconfig.","Trade-base")))

library(ggallin)

label2 <-(c("ETL","Ta","Tr","Fr","Trade-base"))

carbon <- val_l2%>% 
  filter(name=="cb.val")%>%
  mutate(value_1000 = value/10^6) %>%
  ggplot( aes(fill=scen_type, y=value_1000, x=scen)) + 
  #scale_y_continuous(trans=pseudolog10_trans)+
  geom_bar( stat="identity",position = "dodge")+
  scale_fill_brewer(palette = "Greens",labels=c("BTC-base","C"),name="")+
  #coord_flip()+
  scale_x_discrete(labels=label2)+
  theme_classic()+
  ylab(l3)+
  xlab("")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme(legend.position="none")

###############################################################################
# opportunity cost
###############################################################################

# o oportunity cost do PLANGEA é um balanco final. Como todos os cenarios tem mais area convertida doq protegida, o oc eh sempre negativo. Isso tem q ser pensado, pq no fundo é um sinal de ganho!

op <- val_l2%>% 
  filter(name=="oc.val")%>%
  mutate(value_1000 = value/10^9) %>%
  ggplot( aes(fill=scen_type, y=value_1000, x=scen)) + 
  #scale_y_continuous(trans=pseudolog10_trans)+
  geom_bar( stat="identity",position = "dodge")+
  scale_fill_brewer(palette = "Greens",labels=c("BTC-base","C"),name="")+
  #coord_flip()+
  scale_x_discrete(labels=label2)+
  theme_classic()+
  xlab("")+
  ylab("land opportunity cost (billion USD)")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme(legend.position="none")


lu_plangea_bar2 <- ggarrange(lu_plangea_bar2,labels = c("A"))

oc_op <- ggarrange(op,carbon,nrow = 2,labels = c("B","C"))

figura_final_lu <- ggarrange(lu_plangea_bar2,oc_op,widths = c(2,1))

ggsave(filename = "figures/lu_oc_cb_PLANGEA_Trade_scen_with_conservation.jpeg",width = 24,height = 19,units = "cm",plot = figura_final_lu,bg ="white")
