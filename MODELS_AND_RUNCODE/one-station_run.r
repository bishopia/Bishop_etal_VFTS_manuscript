#setwd
#setwd(...)

#load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(streamMetabolizer)
  library(doParallel)
  library(foreach)
})

#output_name <- "mv51_payndates.lf"

#load model inputs prepped for VFTS-PT run
load("./leesextended_2023_dataprepped_20250430_mv57.RData")

#fix light column
dat <- one_station_input %>%
  rename(solar.time=datetime) %>%
  filter(solar.time < ymd("2014-03-01")) %>%
  arrange(solar.time)

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
