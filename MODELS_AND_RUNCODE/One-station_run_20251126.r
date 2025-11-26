#setwd
#setwd(...)

#load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(streamMetabolizer)
  library(doParallel)
  library(foreach)
})

#load table from data release
DR_inputs <- read_csv("2_VFTS_and_One-station_model_input.csv")

#isolate one-station inputs, rename a bit
dat <- DR_inputs %>% 
  filter(model_run=="OS") %>%
  filter(solar.time < ymd("2014-03-01")) %>%
  select(solar.time=datetime,
         DO.obs=downstream_DO,
         DO.sat=downstream_DO_sat,
         depth=reach_depth,
         temp.water=downstream_temp,
         light) %>%
  mutate(date=ymd(as.character(date(solar.time))))

#specify model
specs_daytime <- specs(
  "b_Kn_oipi_tr_plrckm.stan",
  K600_daily_meanlog_meanlog=log(3.46),
  K600_daily_meanlog_sdlog=0.16,
  K600_daily_sdlog_sigma=0.05,
  burnin_steps = 3000,
  saved_steps = 1000,
  n_cores=4,
  n_chains=4
)

#drop date column
dat <- dat %>% select(-date)

#specify core number
n.core = 4

#fit one-station model
fit <- metab(specs = specs_daytime, data = dat)

#save one-station model fit
save(fit, file = "./osfit_no_chunking.RData")
