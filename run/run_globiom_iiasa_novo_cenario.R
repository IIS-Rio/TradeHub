# devtools::load_all("/dados/pessoal/luga/dev/plangea-pkg/")
# plangea(cfg = "/dados/pessoal/luga/dev/TradeHub/json/globiom_iiasa.json")

devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")


plangea(cfg = "/dados/pessoal/francisco/TradeHub/json/globiom_iiasa_regions.json")

# cenarios com baseline BTC

# 1: TH_TF2000_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2
# 2. TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2 
# 3. TH_TFBASE_TCREDU_NOBIOD_NOTECH_NODEM_SPA0_SSP2 
# 4. TH_TFELIM_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2
# 5. TH_TFELIM_TCREDU_NOBIOD_NOTECH_NODEM_SPA0_SSP2

# cenarios de conservacao!

# 1: TH_TF2000_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2
# 2: TH_TFBASE_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2
# 3: TH_TFELIM_TCREDU_BIOD_NOTECH_NODEM_SPA0_SSP2
# 4: TH_TFELIM_TCBASE_BIOD_NOTECH_NODEM_SPA0_SSP2
# 5. TH_TFBASE_TCREDU_BIOD_NOTECH_NODEM_SPA0_SSP2

# testando com subregioes

# TH_TFBASE_TCBASE_NOBIOD_NOTECH_NODEM_SPA0_SSP2 (BAU)