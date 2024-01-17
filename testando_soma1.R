
# cenario 1 a ser rodado

p <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses_2050/TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2"


rs <- list.files(p,full.names = T)


r <- lapply(rs,raster)


soma <- Reduce("+",r)

summary(soma[])

plot(soma)

plot(soma>1)


plot(soma>1.00000001)



# eh um problema de arredondamento,0k

#----  cenario 2 a ser rodado --------------------------------------------------

# esse tem problemas serios!



p2 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses_2050/TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2"


rs <- list.files(p2,full.names = T)


r <- lapply(rs,raster)


r <- lapply(r,function(x)round(x,3))

soma <- Reduce("+",r)

summary(soma[])

hist(soma[])

plot(soma)

plot(soma>1.01)

# puta merda, mtas areas com valores elevados, talvez arredondar resolva parte do probmea

plot(soma>1.00000001)

#----  cenario 3 a ser rodado --------------------------------------------------

# ok, um pouco melhor mais ainda com problemas

p3 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses_2050/TH_TFBASE_TCBASE_BIOD_TECH_DEM_SPA0_SSP2"


rs <- list.files(p3,full.names = T)


r <- lapply(rs,raster)

soma <- Reduce("+",r)

summary(soma[])

hist(soma[])

plot(soma)

plot(soma>1.01)

plot(soma>1.2)

plot(soma<0.99)


#----  cenario 4 a ser rodado --------------------------------------------------

# esse aqui valor maximo eh baixo, mas espalhado!

p4 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses_2050/TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2"


rs <- list.files(p4,full.names = T)


r <- lapply(rs,raster)

soma <- Reduce("+",r)

summary(soma[])

hist(soma[])

plot(soma)

plot(soma>1.00000001)

plot(soma>1.2)

plot(round(soma,2))

#----  cenario 5 a ser rodado --------------------------------------------------

# esse aqui ta bem ok mas ainda tem problemas 

p5 <- "/dados/projetos_andamento/TRADEhub/trade_hub_plangea/land_uses_2050/TH_TFELIM_TCREDU_BIOD_TECH_DEM_SPA0_SSP2"


rs <- list.files(p5,full.names = T)


r <- lapply(rs,raster)

soma <- Reduce("+",r)

summary(soma[])

hist(soma[])

plot(soma)

plot(soma>1.01)

plot(soma>1.2)

plot(soma<0.99)

#-------------------------------------------------------------------------------

# SOLUCOES

#-------------------------------------------------------------------------------

# 1.arredondar valores -- melhora, vale a pena
# 2. gerar novamente areas restauradas disagregadas!(continuar!)

