library(raster)

# defining path

p <- "/dados/projetos_andamento/TRADEhub/GLOBIOM/atualizacao/scen_desagregados"


# list of scenarios (20 in total)

scen <- gsub("_abn_cropland_2Gbioen_10.tif","",
             list.files(file.path(p,"abn_cropland_2Gbioen_10"),pattern = "55"))


# selecting 5 priority scenarios

## baseline_TRADE + baseline_BTC : TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2
## exacerbated trade liberalization + IAP_BTC: TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2
## frictions and reconfigurations + baseline_BTC: TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2
## frictions and reconfigurations + C_BTC: TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2
## baseline_TRADE + IAP_BTC: TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2

################################################################################

# *** checar se o vetor scen_to_keep e scen_subset estão na mesma ordem! ******

################################################################################


scen_to_keep <- c("TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2" ,
                  "TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2",
                  "TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",
                  "TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2")

# 5 scenarios

scen_subset <- grep(pattern =paste(scen_to_keep,collapse = "|"),x = scen,value = T )

# excluir o cenario piloto

scen_to_keep <- scen_to_keep[-1]

scen_subset <- scen_subset[-1]


# criando diretorio pra exportar usos finais

dir.create(file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea","land_uses_2050"))


# exportando lu finais


for (i in 1:length(scen_subset))  {
    
    
    p2 <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses",scen_to_keep[i])
  
  
  # listando pastas com usos finais (descarta usos agregados que foram desagregados; ex: restored e other)
  
  l_dir <- list.files(p2)[c(1:4,6,7,9,11:14)]
  
  l_r <- list.files(file.path(p2,l_dir),recursive = T,full.names = T)
  
  # listando raster so com nomes
  
  l_r_names <- tools::file_path_sans_ext(list.files(file.path(p2,l_dir),recursive = T))
  
  
  # exportando ano 2050
  
  p3 <- file.path("/dados/projetos_andamento/TRADEhub/trade_hub_plangea","land_uses_2050",scen_to_keep[i])
  
  # criando diretorio por cenario
  
  dir.create(p3)
  
  
  for( j in 1:length(l_r)){
    # abrindo raster
    r <- raster(l_r[j],band=5)
    # criando diretorio
    #dir.create(file.path(p2,"land_uses_2050",l_r_lu[i]))
    save_path <- file.path(p3,paste0(basename(l_r_names[j]),"_2050.tif"))
    writeRaster(r,filename = save_path)
    
  }
  
  
  # testando soma 1
  
  soma <- lapply(list.files(p3,full.names = T),raster)
  soma_reduced <- Reduce("+",soma)
  
  valor_max <- max(soma_reduced[],na.rm = T)
  
  if (valor_max >1.001){
    
    aviso <- paste("o cenario ",scen_to_keep[i],"tem soma >1")
    
  }else{
      
      aviso <- paste("o cenario ",scen_to_keep[i],"esta ok!")
      
    }
      
 
  
  #plot(soma_reduced)
  
  #summary(soma_reduced[])
  
  # salvando ultimo layer com fracoes que serao ignoradas
  
  ignored <- 1- soma_reduced
  
  
  # removendo valor negativo
  
  
  ignored[ignored<0] <- 0
  
  
  writeRaster(ignored,filename = file.path(p3,paste0(scen_subset[i],"_ignored_2050.tif")))
  
  print(aviso)

  
  }


################################################################################

# OBS:

# os cenarios -- com excecao do piloto, estao com soma>1, preciso rever

################################################################################





