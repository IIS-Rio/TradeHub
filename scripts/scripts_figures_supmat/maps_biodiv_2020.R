# um mapa com cada uma das variaveis de biodiversidade em 2020 - q sao atualizadas pro futuro

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/global/globiom_iiasa_baseline/results/post_processed/input_variables"

# ec


ec <- rast(file.path(p,"ec_2022-11-09.tif"))


ec_df <- as.data.frame(ec,xy=TRUE)

names(ec_df)[3] <- "ec"

ec_df <- filter(ec_df,ec!=0,
                !is.na(ec))


ec_df$ec_log10 <- log10(ec_df$ec)

# normalizando valores de 0 a 1

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

ec_map <- ec_df%>%
  filter(!is.na(ec))%>%
  mutate(scaled_ec = range01(ec_log10))%>%
  ggplot()+
  geom_raster(aes(x = x,y = y,fill=scaled_ec))+
  scale_fill_viridis(option="turbo","ec")+
  #ggtecle(scen[i])+
  theme_map()+
  labs(
    title = "",
    x = "",
    y = "",
    fill="ec")+
  #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme(  legend.text = element_text(size=4.5),
          legend.title = element_text(size=4.5)) 


# bd

bd <- rast(file.path(p,"bd_2022-11-09.tif"))


bd_df <- as.data.frame(bd,xy=TRUE)

names(bd_df)[3] <- "bd"

bd_df <- filter(bd_df,bd!=0,
                !is.na(bd))



bd_df$bd_log10 <- log10(bd_df$bd)


bd_df%>%
  # filter(bd>0)%>%
  # filter(bd<10^(-3))%>%
  filter(!is.na(bd_log10))%>%
  mutate(scaled_bd = range01(bd_log10))%>%
  ggplot()+
  geom_histogram(aes(x=scaled_bd))


# normalizando valores de 0 a 1

bd_map <- bd_df%>%
  filter(!is.na(bd))%>%
  mutate(scaled_bd = range01(bd_log10))%>%
  ggplot()+
  geom_raster(aes(x = x,y = y,fill=scaled_bd))+
  scale_fill_viridis(option="turbo","bd",limits=c(0,0.5))+
  #ggtecle(scen[i])+
  theme_map()+
  labs(
    title = "",
    x = "",
    y = "",
    fill="bd")+
  #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme(  legend.text = element_text(size=4.5),
          legend.title = element_text(size=4.5)) 

# it


it <- rast(file.path(p,"it_2022-11-09.tif"))


it_df <- as.data.frame(it,xy=TRUE)

names(it_df)[3] <- "it"

it_df <- filter(it_df,it!=0,
                !is.na(it))


it_df$it_log10 <- log10(it_df$it)

# normalizando valores de 0 a 1

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

it_map <- it_df%>%
  filter(!is.na(it))%>%
  mutate(scaled_it = range01(it_log10))%>%
  ggplot()+
  geom_raster(aes(x = x,y = y,fill=scaled_it))+
  scale_fill_viridis(option="turbo","it")+
  #ggtitle(scen[i])+
  theme_map()+
  labs(
    title = "",
    x = "",
    y = "",
    fill="it")+
  #  theme(plot.margin = margin(-3, -2, -3, -3, "cm"))+
  theme(  legend.text = element_text(size=4.5),
          legend.title = element_text(size=4.5)) 


pannel_bio <- ggarrange(bd_map,ec_map,it_map,labels = "AUTO",nrow=3)

ggsave(filename = "/dados/pessoal/francisco/TradeHub/fig_sup/bio_variables_2020.png",pannel_bio,scale = 1,width = 16,height = 20,units = "cm")
