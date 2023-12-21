
forest1 <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2/forest",full.names = T))


forest2 <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2/forest",full.names = T))


agri1 <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2/agriculture",full.names = T))


agri2 <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2/agriculture",full.names = T))

agri3 <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2/agriculture",full.names = T))


plot(agri1[[5]])
plot(agri2[[5]])
plot(agri3[[5]])


plot(agri1[[5]] - agri2[[5]])
plot(agri1[[5]]-agri3[[5]])
summary(agri1[[5]][])
summary(agri2[[5]][])
summary(agri3[[5]][])



pasture1 <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2/pasture",full.names = T))


pasture2 <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2/pasture",full.names = T))


summary(pasture1[[5]][])
summary(pasture2[[5]][])
pasture1[[5]]- pasture2[[5]]

grassland1 <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2/grassland",full.names = T))


grassland2 <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses/TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2/grassland",full.names = T))

grassland1[[5]]- grassland2[[5]]


plot(grassland1[[5]])
plot(grassland2[[5]])

summary(grassland1[[5]][])
summary(grassland2[[5]][])

plot(grassland1[[5]]- grassland2[[5]])



#----pos exportacao 2050

agri1 <- raster(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses_2050/TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2",pattern = "agriculture",full.names = T))


agri2 <- raster(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses_2050/TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2",pattern = "agriculture",full.names = T))


plot(agri1 - agri2!=0)

plot(agri1)
plot(agri2)

# conferir agora rawdata ------------------------------------


agri1 <- raster(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2",pattern = "agriculture",full.names = T))


agri2 <- raster(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2",pattern = "agriculture",full.names = T))


#----- ver quais cenarios novos sao iguais aos velhos! nesse caso, nao precisa rodar o plangea de novo!

baseline <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/backup/baseline",full.names = T) )

new_baseline <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",full.names = T) )


baseline - new_baseline

plot(baseline)

# baseline ok igual

#TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2 # tem q refazer!

TFELIM_old <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/backup/TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2",full.names = T) )


TFELIM_new <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2",full.names = T) )


TFELIM_new- TFELIM_old

# TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2 (refazer!)

TH_TF2000_TCBASE <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/backup/TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2",full.names = T) )


TH_TF2000_TCBASE_new <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2",full.names = T) )

TH_TF2000_TCBASE_new - TH_TF2000_TCBASE

#TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2 # refazer


TH_TF2000_TCBASE_NOBIOD_NOTECH <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/backup/TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",full.names = T) )


TH_TF2000_TCBASE_NOBIOD_NOTECH_new <- stack(list.files("/dados/projetos_andamento/TRADEhub/trade_hub_plangea/rawdata/land-use-2050/TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2",full.names = T) )

TH_TF2000_TCBASE_NOBIOD_NOTECH_new - TH_TF2000_TCBASE_NOBIOD_NOTECH
