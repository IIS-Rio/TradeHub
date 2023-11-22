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

# adicionando continent e gez
transitions_otn_df$Continent <- "All"
transitions_otn_df$GEZ <- "All"

# juntando tudo

transitions <- rbind(transitions_forests_df,transitions_otn_df[,-2])

# substituindo nas por NAs, transformar colunas numericas, substituir NAs!

transitions <- transitions %>%
  mutate_all(~na_if(., "na"))%>%
  mutate(Final_Cstock_tonnes_ha=as.numeric(Final_Cstock_tonnes_ha),
         CO2_eq=as.numeric(CO2_eq))

# filtrar celulas com na

transitions_NA <- transitions%>%
  filter_all(any_vars(is.na(.)))

# ver quais classes preciso obter medias

freq <- as.data.frame(table(transitions$IPCC_climate_zone,transitions$Previous_lu,transitions$Future_lu,transitions$Continent,transitions$GEZ))%>%
  # filtrando f==0
  filter(!Freq==0)

names(freq) <- c("IPCC_climate_zone","Previous_lu","Future_lu","Continent","GEZ","Freq")

# media pra qndo nao tem continente

mean_all <- transitions%>%
  group_by(IPCC_climate_zone,Previous_lu,Future_lu)%>%
  summarise(Final_Cstock_tonnes_ha=mean(Final_Cstock_tonnes_ha,na.rm=T),
            CO2_eq = mean(CO2_eq,na.rm=T))

# Polar dry e polar moist tem valor NA, precisa ter um nivel a menos!

mean_lus <- transitions%>%
  group_by(Previous_lu,Future_lu)%>%
  summarise(Final_Cstock_tonnes_ha=mean(Final_Cstock_tonnes_ha,na.rm=T),
            CO2_eq = mean(CO2_eq,na.rm=T))


# pensar em como combinar essas medias! acho q uma sequencia de for serviria

# rodar por linha

for(i in seq_along(nrow(transitions_NA))){
  
  df <- transitions_NA[i,]
  mean_carbon <- mean_all[mean_all$IPCC_climate_zone==df$IPCC_climate_zone&mean_all$Previous_lu==df$Previous_lu&mean_all$Future_lu==df$Future_lu,]
  # condicional caso nao tenha valor na media acima  
  if(is.na(mean_carbon$Final_Cstock_tonnes_ha)){
    
      mean_carbon2 <- mean_all[mean_all$Previous_lu==df$Previous_lu&mean_all$Future_lu==df$Future_lu,]
      df$Final_Cstock_tonnes_ha <- mean(mean_carbon2$Final_Cstock_tonnes_ha,na.rm=T)
      df$CO2_eq <- mean(mean_carbon2$CO2_eq,na.rm=T)
      
      # continuar!
    
  }
  
}