library(readxl)

abas <- excel_sheets("projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Restoration_transition_matrix_equations_20210820.xlsx")

abas_select_forest <- grep("Forest_Values",x = abas,value = T)
abas_select_otn <- grep("Values",x = abas,value = T)
abas_select_otn <- grep("Forest",x = abas_select_otn,value = T,invert = T)
# listar abas e fundir num df unico

f <- function(x)read_excel("projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Restoration_transition_matrix_equations_20210820.xlsx",sheet = x)

# tabela transicoes florestais:
transitions_forests <- lapply(abas_select_forest,f)
transitions_forests_df <- do.call(rbind,transitions_forests)

nms <- c("IPCC_climate_zone",	"Previous_lu"	,"Future_lu",	"Continent",	"GEZ","Final_Cstock_tonnes_ha",	"CO2_eq","Basis_carbon stock_EF_RF"
)

names(transitions_forests_df) <- nms

# tabela transicoes nao florestais:
transitions_otn <- lapply(abas_select_otn,f)
transitions_otn_df <- do.call(rbind,transitions_otn)

nms_otn <- c("IPCC_climate_zone","apagar",	"Previous_lu"	,"Future_lu","Final_Cstock_tonnes_ha",	"CO2_eq","Basis_carbon stock_EF_RF"
)

head(transitions_otn_df)

names(transitions_otn_df) <- nms_otn

# adicionando continent e gez (aqui eh preciso criar as combinacoes, nao adianta ter all pq nao tem essa classe no raster!!)

Continent <- unique(transitions_forests_df$Continent)
GEZ <- unique(transitions_forests_df$GEZ)
IPCC_climate_zone <- unique(transitions_forests_df$IPCC_climate_zone)

df2add <- expand_grid(Continent,GEZ,IPCC_climate_zone)

transitions_otn_df2 <- left_join(transitions_otn_df,df2add)

# transitions_otn_df$Continent <- "All"
# transitions_otn_df$GEZ <- "All"

# juntando tudo

transitions <- rbind(transitions_forests_df,transitions_otn_df2[,-2])

# substituindo nas por NAs, transformar colunas numericas, substituir NAs!

transitions <- transitions %>%
  mutate_all(~na_if(., "na"))%>%
  mutate(Final_Cstock_tonnes_ha=as.numeric(Final_Cstock_tonnes_ha),
         CO2_eq=as.numeric(CO2_eq))

# filtrar celulas com na

transitions_NA <- transitions%>%
  filter_all(any_vars(is.na(.)))

# so tem NA em polar moist/dry pra outros usos

# ver quais classes preciso obter medias

#unique(paste0(transitions_NA$Previous_lu,"2",transitions_NA$Future_lu))


# freq <- as.data.frame(table(transitions$IPCC_climate_zone,transitions$Previous_lu,transitions$Future_lu,transitions$Continent,transitions$GEZ))%>%
#   # filtrando f==0
#   filter(!Freq==0)

#names(freq) <- c("IPCC_climate_zone","Previous_lu","Future_lu","Continent","GEZ","Freq")

# media pra qndo nao tem continente

# mean_all <- transitions%>%
#   group_by(IPCC_climate_zone,Previous_lu,Future_lu)%>%
#   summarise(Final_Cstock_tonnes_ha=mean(Final_Cstock_tonnes_ha,na.rm=T),
#             CO2_eq = mean(CO2_eq,na.rm=T))

# Polar dry e polar moist tem valor NA, precisa ter um nivel a menos!

mean_lus <- transitions%>%
  group_by(Previous_lu,Future_lu,GEZ)%>%
  summarise(Final_Cstock_tonnes_ha=mean(Final_Cstock_tonnes_ha,na.rm=T),
            CO2_eq = mean(CO2_eq,na.rm=T))


# pensar em como combinar essas medias! acho q uma sequencia de for serviria

# join

transitions_NA_join <- left_join(transitions_NA[,-c(6:8)],mean_lus)

transitions_NA_join$'Basis_carbon stock_EF_RF' <- "average value considering lus, Continent and GEZ"

transitions_noNA <- transitions[complete.cases(transitions),]

#nrow(transitions)==nrow(transitions_NA_join)+nrow(transitions_noNA)


# juntando novamente

transitions_completo <- rbind(transitions_NA_join,transitions_noNA)

transitions_completo <- transitions_completo[complete.cases(transitions_completo),]

# salvando

write.csv(transitions_completo,"/dados/projetos_andamento/TRADEhub/trade_hub_plangea/restoration_transitions/Restoration_transition_matrix_equations_20210820.csv",row.names = F)
