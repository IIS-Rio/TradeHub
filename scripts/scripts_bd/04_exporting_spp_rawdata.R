library(terra)

# exportar spp pra pasta rawdata

dirdest <- list.dirs("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3",recursive = F,full.names = F)

dirsource <- list.dirs("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/species_climate/SSP3",recursive = F,full.names = F)

dit2do <- dirsource[!dirsource %in% dirdest]

tax_groups <- list.dirs("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3/bc/",recursive = F,full.names = F)

source <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/species_climate/SSP3"
dest <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3"

# fazer so pros q faltam (incluir ip) ip falta mamiferos, repteis!!!
dit2do <- "mi"

for(dir in dit2do){
  for(fldr in tax_groups){

    rstrs2cpy <- list.files(file.path(source,dir,fldr),full.names = T)
    rsternme <- list.files(file.path(source,dir,fldr),full.names = F)
    c=1
    for(file in rstrs2cpy){
        r <- rast(file)
        dir.create(file.path(dest,dir))
        plce2save <- file.path(dest,dir,fldr)
        dir.create(plce2save)
        writeRaster(r,file.path(plce2save,paste0(rsternme[c])))
        c=c+1
    }

  }
}

