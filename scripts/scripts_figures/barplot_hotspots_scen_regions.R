
library(ggpubr)

df <- read.csv("/dados/pessoal/francisco/TradeHub/output_tables/updated_results/overlap_hotspots_11_regions_agriexp.csv")

unique(df$AggrgtR)

df$AggrgtR <- factor(df$AggrgtR,levels = c("LAC","MNA","SEA","SSA","SAS","USA","EAS","OCE","CSI"))

df$name <- factor(df$name,levels = c("ETL","Ta","Tr","Fr"))

color_palette <- terrain.colors(length(unique(df$NAME[df$total_area!=0])))

color_palette <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
"#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
"#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
"#8A7C64", "#599861","red")

color_scale <- scale_fill_viridis_d(option="inferno")

# criando coluna com legenda

x <- unique(df[,c(1,2)]) %>%
  group_by(NAME) %>%
  mutate(region_list = paste(AggrgtR, collapse = ", ")) %>%
  ungroup()%>%
  mutate(legend=paste0(NAME," (",region_list,")"))


# criando rotulo pro barplot

# Get unique NAME values
unique_names <- unique(df$NAME)

# Create a data frame with NAME and ID
id_df <- data.frame(
  NAME = unique_names,
  ID = seq_along(unique_names)
)

head(id_df)

# atualizando df

# df2 <- left_join(df,x)%>%
#   left_join(id_df)%>%
#   # adding to label
#   mutate(legend=paste0(ID," ",legend))

df2 <- df%>%
  rename(region=AggrgtR)%>%
  filter(total_area!=0,
         total_area>0.001)
df2$name <- factor(df2$name,levels = c("ETL","Ta","Tr","Fr"))

result <- df2%>%
  mutate(region = reorder(region, -total_area),
         total_area=total_area/10^6,
         ) %>%
  ggplot(aes(x = region,y = total_area, fill = legend)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(values = color_palette) +
  #scale_fill_brewer(palette = "Accent", name="",direction=-1)+
  # geom_text(aes(label = ID),
  #   position = position_stack(vjust = 0),
  #   size = 2,
  #   colour = 'black'
  # ) +
  facet_wrap(~name, ncol=4, scales = "free")+
  theme_bw()+
  rotate_x_text(angle=90)+
  ylab(expression(paste("Agricultural expansion area overlapping hotspots (", Km^2, " ", "x 10"^"-6", ")", sep = "")))+
  xlab("")+
  theme(legend.position = "bottom", legend.box.spacing = unit(0.01, "cm"))+
  guides(fill = guide_legend(ncol = 2))+
  coord_flip()#+
  #color_scale


result <- ggpar(result,font.main=c(8),font.legend = c(6),font.x = c(8),font.y=c(8),legend.title = "" )

ggsave(plot = result,filename = "/dados/pessoal/francisco/TradeHub/figures_paper_new_versions/barplot_regions_hotspots.png",scale = 1,width = 16,height = 21,units = "cm")

# separar por regiao! inverter x axis

plots <- list()

unique_regions <- unique(df2$AggrgtR[df$total_area!=0])

unique_NAME <-  unique(df2$NAME[df$total_area!=0])

for(i in 1:length(unique_regions)){
  r <- unique_regions[i]
  result <- df%>%
    rename(region=AggrgtR)%>%
    filter(total_area!=0,
           region==r)%>%
    mutate(region = reorder(region, -total_area),
           total_area=total_area/10^6) %>%
    ggplot(aes(x = name,y = total_area, fill = legend)) +
    geom_bar(stat = "identity")+
    scale_fill_brewer(palette = "Set3", name="",direction=-1)+
    theme_bw()+
    rotate_x_text(angle=90)+
    ylab("")+
    xlab("")+
    theme(legend.position = "top", legend.box.spacing = unit(0.01, "cm"))+
   # guides(fill = guide_legend(ncol = 2))+
    coord_flip()
  plots[[i]] <- result
  
}


teste <- ggarrange(plotlist =plots,nrow = 9,widths=rep(1,9),heights = rep(9,1))


library(egg)

teste <- ggarrange(plots = plots[1:4])


#teste <- ggarrange(plots = plots[1:8],nrow = 3)

teste <- ggpar(teste,font.main=c(8),font.legend = c(6),font.x = c(8),font.y=c(8),legend.title = "" )

plots[[3]]


ggsave(plot = teste,filename = "/dados/pessoal/francisco/TradeHub/figures_paper_new_versions/barplot_regions_hotspots2.png",scale = 1,width = 16,height = 21,units = "cm")
