# new results

bd_new <- read.csv("/dados/pessoal/francisco/TradeHub/output_tables/updated_results/agg_bd_climate_env_global.csv")

# adicionando nome regioes e cenarios

df_dictionary <- fread("/dados/pessoal/francisco/TradeHub/output_tables/resultados_cenarios_regional_analysis.csv")

#,df_dictionary$label_scen,df_dictionary$conservation
dictionary_scen <- unique(df_dictionary[,c(2,6,7)])


bd_new2 <- left_join(bd_new,dictionary_scen,by=c("scens"="scenario"))

# adicionando coluna

bd_new2 <- mutate(bd_new2,name="bd.val",variable="Extinction debt reduction")%>%
  rename(value=mean_bd)

value_BAU=-0.04402022

# calculando relacao com baseline 2050

bd_new2 <- mutate(bd_new2,relative_to_BAU_2050=(value-value_BAU)/abs(value_BAU))

# salvando

write.csv(bd_new2,"/dados/pessoal/francisco/TradeHub/output_tables/updated_results/agg_bd_climate_env_global_newcolumns.csv",row.names = F)


