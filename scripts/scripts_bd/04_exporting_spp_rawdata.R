library(terra)

# exportar spp pra pasta rawdata

dirdest <- list.dirs("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3",recursive = F,full.names = F)

dirsource <- list.dirs("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/species_climate/SSP3",recursive = F,full.names = F)

dit2do <- dirsource[!dirsource %in% dirdest]

tax_groups <- list.dirs("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3/bc/",recursive = F,full.names = F)

source <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/species_climate/SSP3"
dest <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/species_climate/SSP3"

# fazer so pros q faltam (incluir ip) ip falta mamiferos, repteis!!!

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

# pra ip

# for(fldr in tax_groups[(3)]){
#     
#   rstrs2cpy <- list.files(file.path(source,"ip",fldr),full.names = T)
#   rsternme <- list.files(file.path(source,"ip",fldr),full.names = F)
#   c=1
#   for(file in rstrs2cpy){
#     r <- rast(file)
#     dir.create(file.path(dest,"ip"))
#     plce2save <- file.path(dest,"ip",fldr)
#     dir.create(plce2save)
#     writeRaster(r,file.path(plce2save,paste0(rsternme[c])))
#     c=c+1
#   
#   }
# }
  
# ip falta mammals
# mi so tem amphibians

# for(fldr in tax_groups[c(2:4)]){
#   
#   rstrs2cpy <- list.files(file.path(source,"mi",fldr),full.names = T)
#   rsternme <- list.files(file.path(source,"mi",fldr),full.names = F)
#   c=1
#   for(file in rstrs2cpy){
#     r <- rast(file)
#     dir.create(file.path(dest,"mi"))
#     plce2save <- file.path(dest,"mi",fldr)
#     dir.create(plce2save)
#     writeRaster(r,file.path(plce2save,paste0(rsternme[c])))
#     c=c+1
#     
#   }
# }

# mr so tem reptiles

# for(fldr in tax_groups[c(1:3)]){
#   
#   rstrs2cpy <- list.files(file.path(source,"mr",fldr),full.names = T)
#   rsternme <- list.files(file.path(source,"mr",fldr),full.names = F)
#   c=1
#   for(file in rstrs2cpy){
#     r <- rast(file)
#     dir.create(file.path(dest,"mr"))
#     plce2save <- file.path(dest,"mr",fldr)
#     dir.create(plce2save)
#     writeRaster(r,file.path(plce2save,paste0(rsternme[c])))
#     c=c+1
#     
#   }
# }
# ms so tem reptiles

# for(fldr in tax_groups[c(1:3)]){
#   
#   rstrs2cpy <- list.files(file.path(source,"ms",fldr),full.names = T)
#   rsternme <- list.files(file.path(source,"ms",fldr),full.names = F)
#   c=1
#   for(file in rstrs2cpy){
#     r <- rast(file)
#     dir.create(file.path(dest,"ms"))
#     plce2save <- file.path(dest,"ms",fldr)
#     dir.create(plce2save)
#     writeRaster(r,file.path(plce2save,paste0(rsternme[c])))
#     c=c+1
#     
#   }
# }