###############
###---PREP--###
###############

#clean environment
rm(list = ls())

#load libraries
library(tidyverse)

# setwd("")

#########################################################################
###---TAKE MODEL INPUTS FROM DATA RELEASE AND RESHAPE FOR MODEL RUN---###
#########################################################################

#load libraries
library(tidyverse)

#load table from data release
DR_inputs <- read_csv("../../manuscript/data_release/v2/tosubmit/2_VFTS_and_One-station_model_input.csv")

#convert data table for specific model run and variable to matrix that can be read by model
DO_obs_up_matrix_form <- DR_inputs %>% 
  filter(model_run=="VFTS-2") %>% #example here is VFTS-CQ
  select(datetime, upstream_DO) %>% #first up is upstream_DO
  mutate(date=as.Date(datetime), time=format(datetime, "%H:%M")) %>%
  select(date, time, upstream_DO) %>%
  pivot_wider(names_from=date, values_from=upstream_DO) %>%
  arrange(time) %>%
  select(-time) %>%
  as.matrix()
# remove dimnames
dimnames(DO_obs_up_matrix_form) <- NULL

#do again for other DR_inputs
DO_obs_down_matrix_form <- DR_inputs %>% 
  filter(model_run=="VFTS-2") %>% 
  select(datetime, downstream_DO) %>%
  mutate(date=as.Date(datetime), time=format(datetime, "%H:%M")) %>%
  select(date, time, downstream_DO) %>%
  pivot_wider(names_from=date, values_from=downstream_DO) %>%
  arrange(time) %>%
  select(-time) %>%
  as.matrix()
# remove dimnames
dimnames(DO_obs_down_matrix_form) <- NULL

DO_sat_up_matrix_form <- DR_inputs %>% 
  filter(model_run=="VFTS-2") %>% 
  select(datetime, upstream_DO_sat) %>%
  mutate(date=as.Date(datetime), time=format(datetime, "%H:%M")) %>%
  select(date, time, upstream_DO_sat) %>%
  pivot_wider(names_from=date, values_from=upstream_DO_sat) %>%
  arrange(time) %>%
  select(-time) %>%
  as.matrix()
# remove dimnames
dimnames(DO_sat_up_matrix_form) <- NULL

DO_sat_down_matrix_form <- DR_inputs %>% 
  filter(model_run=="VFTS-2") %>% 
  select(datetime, downstream_DO_sat) %>%
  mutate(date=as.Date(datetime), time=format(datetime, "%H:%M")) %>%
  select(date, time, downstream_DO_sat) %>%
  pivot_wider(names_from=date, values_from=downstream_DO_sat) %>%
  arrange(time) %>%
  select(-time) %>%
  as.matrix()
# remove dimnames
dimnames(DO_sat_down_matrix_form) <- NULL

totlight_matrix_form <- DR_inputs %>% 
  filter(model_run=="VFTS-2") %>% 
  select(datetime, light) %>%
  mutate(date=as.Date(datetime), time=format(datetime, "%H:%M")) %>%
  select(date, time, light) %>%
  pivot_wider(names_from=date, values_from=light) %>%
  arrange(time) %>%
  select(-time) %>%
  as.matrix()
# remove dimnames
dimnames(totlight_matrix_form) <- NULL

depth_matrix_form <- DR_inputs %>% 
  filter(model_run=="VFTS-2") %>% 
  select(datetime, reach_depth) %>%
  mutate(date=as.Date(datetime), time=format(datetime, "%H:%M")) %>%
  select(date, time, reach_depth) %>%
  pivot_wider(names_from=date, values_from=reach_depth) %>%
  arrange(time) %>%
  select(-time) %>%
  as.matrix()
# remove dimnames
dimnames(depth_matrix_form) <- NULL

temp_matrix_form <- DR_inputs %>% 
  filter(model_run=="VFTS-2") %>% 
  select(datetime, downstream_temp) %>%
  mutate(date=as.Date(datetime), time=format(datetime, "%H:%M")) %>%
  select(date, time, downstream_temp) %>%
  pivot_wider(names_from=date, values_from=downstream_temp) %>%
  arrange(time) %>%
  select(-time) %>%
  as.matrix()
# remove dimnames
dimnames(temp_matrix_form) <- NULL

tt_matrix_form <- DR_inputs %>% 
  filter(model_run=="VFTS-2") %>% 
  select(datetime, travel_time) %>%
  mutate(date=as.Date(datetime), time=format(datetime, "%H:%M")) %>%
  select(date, time, travel_time) %>%
  pivot_wider(names_from=date, values_from=travel_time) %>%
  arrange(time) %>%
  select(-time) %>%
  as.matrix()
# remove dimnames
dimnames(tt_matrix_form) <- NULL

lag_matrix_form <- DR_inputs %>% 
  filter(model_run=="VFTS-2") %>% 
  select(datetime, lag) %>%
  mutate(date=as.Date(datetime), time=format(datetime, "%H:%M")) %>%
  select(date, time, lag) %>%
  pivot_wider(names_from=date, values_from=lag) %>%
  arrange(time) %>%
  select(-time) %>%
  as.matrix()
# remove dimnames
dimnames(lag_matrix_form) <- NULL

#within day inveral count
n <- dim(lag)[1]
#days
d <- dim(lag)[2]


###############
###---STAN--###
###############

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#collect DR_inputs for model run
stan_data <-
  list(
    n = n,
    DO_obs_up = DO_obs_up_matrix_form,
    DO_obs_down = DO_obs_down_matrix_form,
    DO_sat_down = DO_sat_down_matrix_form,
    DO_sat_up = DO_sat_up_matrix_form,
    totlight = totlight_matrix_form,
    depth = depth_matrix_form,
    temp = temp_matrix_form,
    tt = tt_matrix_form,
    d = d
  )

#vector of parameters to estimate
params1 <- c("GPP","ER","sigma","k600")

#run VFTS-PT
ms_prior <- stan("./VFTS_20250716.stan",
                 data = stan_data, pars = params1,
                 chains = 4, cores = 4, iter = 4000, warmup = 3000)

#save workspace image
#save.image(paste0("./VFTS-PT_modeloutput_", format(Sys.Date(), "%Y%m%d"), ".RData"))
save.image(paste0("./VFTS-CQ_modeloutput_", format(Sys.Date(), "%Y%m%d"), ".RData"))
#save.image(paste0("./VFTS-MT_modeloutput_", format(Sys.Date(), "%Y%m%d"), ".RData"))
#save.image(paste0("./VFTS-MTlow_modeloutput_", format(Sys.Date(), "%Y%m%d"), ".RData"))
#save.image(paste0("./VFTS-MThi_modeloutput_", format(Sys.Date(), "%Y%m%d"), ".RData"))
