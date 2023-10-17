
#- Observacoes -----------------------------------------------------------------

# usar o cruzamento da distribuição de spp com os envelopes climaticos gerados pelo artigo da CBD. 

# A distribuicao com o clima atual e 9 gcms futuros foram cruzadas com os limites da IUCN.

#-------------------------------------------------------------------------------

# caminho distribuicao spp presente

file_to_present <- "/dados/projetos_andamento/CBD-draft/rawdata/climate_envelopes/bien_intersections/bien_current/"

# caminho distribuicao futuro

# lista de gcms
gcm_list = list.dirs(path = "/dados/bd_iis/climate_envelopes_fromCI/ppm_gcm", full.names = FALSE, recursive = FALSE) 

files_to_future <- paste0("/dados/projetos_andamento/CBD-draft/rawdata/climate_envelopes/bien_intersections/",gcm_list)
