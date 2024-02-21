# comparar resultados carbono gerados com base no plangea com os novos considerando nature map

cb_old <- fread("/dados/pessoal/francisco/TradeHub/output_tables/regional_cb.csv")
cb_new_lts <- list.files("/dados/pessoal/francisco/TradeHub/output_tables/updated_results/",full.names = T)
cb_new <- left_join(fread(cb_new_lts[[1]]),fread(cb_new_lts[[2]]))%>%
  mutate(total_cb_new=)
