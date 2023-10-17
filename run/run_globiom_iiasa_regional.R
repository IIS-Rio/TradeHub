devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")
library(jsonlite)

# caminho pra salvar as analises por regiao

# p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/regional_analysis/"
#  
# # uma pasta por regiao
regioes <- read.csv("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/subregions/region_code.csv")
# # f <- function(x)dir.create(file.path(p,x))
# # lapply(regioes$region,f)
# configurando o JSON pra rodar pra varias regioes
# cfg <- aux_read_cfg(file.path("/dados/pessoal/francisco/TradeHub/json/globiom_iiasa.json"))
# updates cfg with plangea_path


# i=1
# j=1

scen <- list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050")

reg <- regioes$region

dest <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/regional_analysis"


# cfg$io$plangea_path = plangea_path

# criando objeto com indices do JSON

cfg = jsonlite::fromJSON("/dados/pessoal/francisco/TradeHub/json/globiom_iiasa_regions.json")


# tem q adiconar o lugar pra salvar

for(i in 1:length(reg)){
  for(j in 1:length(scen)){
    # tem q adiconar o lugar pra salvar
    cfg$io$base_path <- paste0(dest,"/",reg[i],"/",scen[j],"/")
    
    # tem q adicionar a regiao!
    cfg$io$lu_relative_path <- paste0("land-use-regional/",reg[i],"/")
    
    
    # tem q adiconar o cenario, pq pra cada regiao eh pra rodar o cenario
    cfg$io$future_lu_relative_path <- paste0("land-use-2050/",scen[j],"/")
    
    plangea(cfg = cfg)    
  
    }

  }

