
#- Observacoes -----------------------------------------------------------------

# usar o cruzamento da distribuição de spp com os envelopes climaticos gerados pelo artigo da CBD. Vamos usar o raster que considera a junção dos 9 gcms usados no paper, a partir desse script: 

# scripts/analyses_bd/multiple_gcms/with_intersection/01_intersection_SDM_bien_IUCN_rasters_multiple_gcm.R

# esse eh o resultado pro presente:

# #path of the place that we want to save the species intersection maps
# file_to_present <- paste0("/dados/projetos_andamento/CBD-draft/rawdata/climate_envelopes/bien_intersections/", gcm_code,"/", directory, "/", df_unique_names$taxonid[i], ".tif")

# tem esse do futuro aqui, gerado nesse scrip:

# scripts/analyses_bd/multiple_gcms/with_intersection/01_intersection_SDM_bien_IUCN_rasters_current_tocompare.R

# spp_raster_paths_future_done <- list.files("/dados/projetos_andamento/CBD-draft/rawdata/climate_envelopes/bien_intersections/bien_gcmagreement/", pattern = "*.tif", recursive = TRUE, full.names = FALSE)

# ate onde entendi, tem uma versao com baseline e um future agreement entre varios gcms; e tem tb uma versao da analise que foi feita usando multiplos gcms.

#-------------------------------------------------------------------------------

# caminho distribuicao spp presente

file_to_present <- paste0("/dados/projetos_andamento/CBD-draft/rawdata/climate_envelopes/bien_intersections/")

# length(list.files(file_to_present))



