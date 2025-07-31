###############
###---PREP--###
###############

#clean environment
rm(list = ls())

#load libraries
library(tidyverse)

# setwd("")

#load data prepped environment
load("./leesextended_2023_dataprepped_20250430_mv57.RData") #VFTS-PT
#load("./leesextended_2023_dataprepped_20250505_mv59.RData") #VFTS-CQ
#load("./leesextended_2023_dataprepped_20250505_mv59.RData") #VFTS-MT
#load("./leesextended_2023_dataprepped_20250506_mv60b.RData") #VFTS-MT, low uniform tt
#load("./leesextended_2023_dataprepped_20250506_mv60c.RData") #VFTS-MT, hi uniform tt

###############
###---STAN--###
###############

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_data <-
  list(
    n = n,
    DO_obs_up = DO_obs_up,
    DO_obs_down = DO_obs_down,
    DO_sat_down = DO_sat_down,
    DO_sat_up = DO_sat_up,
    totlight = totlight,
    depth = depth,
    temp = temp,
    tt = tt,
    d = d
  )

params1 <- c("GPP","ER","sigma","k600")

#run VFTS-PT
ms_prior <- stan("./VFTS_20250716.stan",
                 data = stan_data, pars = params1,
                 chains = 4, cores = 4, iter = 4000, warmup = 3000)

#save workspace image
save.image(paste0("./VFTS-PT_modeloutput_", format(Sys.Date(), "%Y%m%d"), ".RData"))
#save.image(paste0("./VFTS-CQ_modeloutput_", format(Sys.Date(), "%Y%m%d"), ".RData"))
#save.image(paste0("./VFTS-MT_modeloutput_", format(Sys.Date(), "%Y%m%d"), ".RData"))
#save.image(paste0("./VFTS-MTlow_modeloutput_", format(Sys.Date(), "%Y%m%d"), ".RData"))
#save.image(paste0("./VFTS-MThi_modeloutput_", format(Sys.Date(), "%Y%m%d"), ".RData"))
