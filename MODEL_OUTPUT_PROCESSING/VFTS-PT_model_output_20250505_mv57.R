##############################
###---model output workup--###
##############################

#GOAL: Take model output and produce csvs of daily and subdaily output.

#clear workspace
rm(list = ls())

#setwd
setwd("C:/Users/ibishop/OneDrive - DOI/science/gc_metabolism/eda_twostation/mv57/")

#libraries
library(tidyverse)
library(patchwork)
library(padr)
library(dygraphs)
library(tidymodels)
library(bayesplot)
library(rstan)

#load model output Rdata file
load("../mv57/leesextended_2023_modeloutput_mv57_20250501_payndates.RData")

#choose and name model
model <- ms_prior
model_name <- "mv57_payndates_B1"


##############################################
###---Model Diagnostics and Output Table---###
##############################################

#total runtime
total_runtime <- round(mean(rowSums(get_elapsed_time(ms_prior)))/3600, 3)
print(paste0("For ", d, " days and 4000 iterations, runtime took a total of ", total_runtime, " hours" ))

#average runtime
mean_runtime <- round(mean(rowSums(get_elapsed_time(ms_prior)/d)),1)
print(paste0("On average, for ", d, " days and 4000 iterations, run time took ", mean_runtime, " seconds per day" ))

#chec: show caterpillars plots for first x days for each par
day_count <- 3
mcmc_trace(ms_prior, pars = c(paste0("GPP[", 1:day_count, "]"), paste0("ER[", 1:day_count, "]"), paste0("k600[", 1:day_count, "]")))


#get model output table
model_summary <- as.data.frame(summary(ms_prior)$summary)
model_summary_tbl <- model_summary %>%
  rownames_to_column(var = "parameter") %>%
  #extract the variable part, now allowing digits (e.g., k600, GPP, ER, sigma, lp__)
  mutate(
    variable = str_extract(parameter, "^[A-Za-z0-9_]+"),    # Allow digits in variable (e.g., k600)
    run_index = str_extract(parameter, "(?<=\\[)\\d+(?=\\])"),  # Extract the number inside [], if it exists
    run_index = as.numeric(run_index)                               # Convert index to numeric
  ) %>%
  select(-parameter) %>%
  dplyr::rename(parameter=variable) %>%
  as_tibble()

#finish model output table; you can now join with dates to df_daily later on to get Rhat values, neff values, etc.
model_summary_tbl <- tibble(run_index=1:d, date=mdy(dates_nonas)) %>% right_join(model_summary_tbl, by="run_index")
write_csv(model_summary_tbl, paste0("./", model_name, "_modeloutput.csv"))

#check convergence counts
model_summary_tbl %>% count(Rhat>1.05, parameter)

#plot Rhats
Rhat_hist <- model_summary_tbl %>% 
  filter(parameter %in% c("ER", "GPP", "k600")) %>%
  ggplot(aes(x=Rhat)) + 
  geom_histogram(col=1, fill="grey50") + theme_bw() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  facet_wrap(~parameter, ncol=1)


################################
###---EXTRACT MODEL OUTPUT---###
################################

datesofinterest <- dates_to_use

#extract posterior means and CrIs 
q975 <- function(x){quantile(x,.975)}
q025 <- function(x){quantile(x,.025)}

gpp <- rstan::extract(model,"GPP")[[1]]
er <- rstan::extract(model,"ER")[[1]]
k600 <- rstan::extract(model,"k600")[[1]]

gpp_mn <- apply(gpp,2,mean)
gpp_up <- apply(gpp,2,q975)
gpp_down <- apply(gpp,2,q025)
er_mn <- apply(er,2,mean)
er_up <- apply(er,2,q975)
er_down <- apply(er,2,q025)
k600_mn <- apply(k600,2,mean)
k600_up <- apply(k600,2,q975)
k600_down <- apply(k600,2,q025)
sigma <- rstan::extract(model, "sigma")[[1]]

#use posterior means to calculate predicted downstream DO
K600_mn <-matrix(NA,nrow=n,ncol=d)
K600_up <-matrix(NA,nrow=n,ncol=d)
K600_down <-matrix(NA,nrow=n,ncol=d)

KO2<-matrix(NA,nrow=n,ncol=d)
metab<-matrix(NA,nrow=n,ncol=d)

for (t in 1:d){
  K600_mn[,t] = (k600_mn[t] /depth[,t])
  K600_up[,t] = (k600_up[t] /depth[,t])
  K600_down[,t] = (k600_down[t] /depth[,t])
  
  KO2[,t] = K600_mn[,t] / ((600/(1800.6-(temp[,t] * 120.1)+(3.7818 * temp[,t]^2)-(0.047608 * temp[,t]^3)))^-0.5)
  metab[,t] = (DO_obs_up[,t] + gpp_mn[t] * totlight[,t] / depth[,t] + er_mn[t] * tt[,t] / depth[,t] + (KO2[,t]*tt[,t]*(DO_sat_up[,t]-DO_obs_up[,t]+DO_sat_down[,t])/2))/(1+ (KO2[,t] * tt[,t]) /2)
}


##########################################
###---CALCULATE QUARTERHOURLY OUTPUT---###
##########################################

#first, get input values into df with proper datetime. i think best approach is to collate together with fake datetime, then add 6 hours to all datetimes, then
#merge with covariates...
#96 times during day
time_vec <- LEES %>% mutate(time=chron::times(time/24)) %>% filter(jul==1) %>% pull(time)

#repeat times x times, corresponding to how many days you're keeping
# times <- rep(time_vec, length(dates_nonas[datesofinterest]))
times <- rep(time_vec, length(dates_to_use))

df_subdaily <-
  tibble(
    jul = rep(dates_to_use, each = 96),
    date = rep(mdy(dates_nonas), each = 96),
    time = times,
    predictedO2 = c(metab),
    DO_obs_down = c(DO_obs_down),
    DO_obs_up = c(DO_obs_up),
    DO_sat_up = c(DO_sat_up),
    DO_sat_down = c(DO_sat_down),
    KO2 = c(KO2),
    diff = DO_obs_down - predictedO2,
    percent_diff = (DO_obs_down - predictedO2) / DO_obs_down * 100,
    lag = c(lag),
    tt = c(tt),
    depth = c(depth),
    temp=c(temp),
    K600_mn = c(K600_mn),
    K600_up = c(K600_up),
    K600_down = c(K600_down)
  ) %>%
  mutate(datetime=ymd_hms(paste(date, time))) %>% 
  mutate(datetime=datetime+lubridate::hours(6)) %>% #offset because these estimates are for 6am-6pm
  mutate(time=as.character(hms::as_hms(datetime))) %>%
  mutate(time = as.numeric(hms(time), "hours")) %>%
  mutate(date=date(datetime)) %>%
  mutate(month=month(datetime)) %>%
  mutate(year=year(datetime))

#add season
df_subdaily <- df_subdaily %>%
  mutate(
    season = case_when(
      (month == 3 & day(date) >= 20) | (month > 3 & month < 6) | 
        (month == 6 & day(date) < 21) ~ "Spring",
      (month == 6 & day(date) >= 21) | (month > 6 & month < 9) | 
        (month == 9 & day(date) < 22) ~ "Summer",
      (month == 9 & day(date) >= 22) | (month > 9 & month < 12) | 
        (month == 12 & day(date) < 21) ~ "Autumn",
      (month == 12 & day(date) >= 21) | (month < 3) | 
        (month == 3 & day(date) < 20) ~ "Winter",
      TRUE ~ NA_character_ # For any cases that don't match
    )
  )

df_subdaily <- df_subdaily %>%
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")))

#add more valuable metrics
df_subdaily <- df_subdaily %>%
  mutate(DO_obs_updown_diff=DO_obs_up-DO_obs_down) %>%
  mutate(DO_satup_deficit=DO_sat_up-DO_obs_up) %>%
  mutate(DO_satdown_deficit=DO_sat_down-DO_obs_down)

##################################
###---CALCULATE DAILY OUTPUT---###
##################################

#Appling et al. R2
r2 <- 
  df_subdaily %>%
  group_by(jul) %>%
  summarise(
    r2 = 1 - (sum((predictedO2 - DO_obs_down)^2) / sum((DO_obs_down - mean(DO_obs_down))^2, na.rm = TRUE)), .groups = "drop") %>%
  pull(r2)

#check r2 histogram
hist(r2, breaks=100, xlab="daily DO predicted/observed r2", main="")

#combine daily in df
daily_out <- data.frame(jul=datesofinterest,
                  date=dates_nonas,
                  k600_mn=k600_mn,
                  k600_up=k600_up, 
                  k600_down=k600_down,
                  GPP_mn=gpp_mn,
                  GPP_up=gpp_up,
                  GPP_down=gpp_down,
                  ER_mn=er_mn,
                  ER_up=er_up,
                  ER_down=er_down,
                  sigma=sigma[datesofinterest],
                  r2)

#fix out, fix date, reorder variables
daily_out <- daily_out %>%
  as_tibble() %>%
  mutate(date=mdy(date)) %>%
  mutate(year=year(date),
         month=month(date),
         yday=yday(date)) %>%
  left_join(unique(df_subdaily[,c("date", "season")]), by="date") 

#add daily tt summaries
daily_out <- df_subdaily %>%
  group_by(date) %>%
  summarize(tt_daily_mean=mean(tt, na.rm=TRUE),
            tt_daily_sd=sd(tt, na.rm=TRUE)) %>%
  right_join(daily_out, by="date")

#add run metrics
run_metrics <- model_summary_tbl %>% filter(parameter %in% c("GPP", "ER", "k600")) %>% select(date, parameter, n_eff, Rhat) %>%
  pivot_wider(
    names_from = parameter, 
    values_from = c(n_eff, Rhat),
    names_glue = "{parameter}_{.value}"  # Create custom column names like GPP_n_eff, GPP_Rhat, etc.
  )

daily_out <- daily_out %>% 
  left_join(run_metrics, by="date") %>%
  select(date, year, season, month, yday, jul, starts_with("GPP"), starts_with("ER"), starts_with("K600"), everything())

#add big K600s
daily_out <- df_subdaily %>% select(datetime, date, K600_mn, K600_up, K600_down) %>%
  group_by(date) %>%
  summarize(K600_mn=mean(K600_mn, na.rm=TRUE),
            K600_up=mean(K600_up, na.rm=TRUE),
            K600_down=mean(K600_down, na.rm=TRUE)) %>%
  right_join(daily_out, by="date")

#write out df_subdaily
write_csv(df_subdaily, paste0("./", model_name, "_output_subdaily_", format(Sys.Date(), "%Y%m%d"), ".csv"))

#write out daily output
write_csv(daily_out, paste0("./", model_name, "_output_daily_", format(Sys.Date(), "%Y%m%d"), ".csv"))

#calculate turnover length
velos <- read_csv("../mv43/velocity_depth_trace_through_2022.csv")
turnover_lengths <- df_subdaily %>% 
  select(datetime, KO2) %>% 
  left_join(velos, by="datetime") %>% 
  mutate(date=date(datetime)) %>%
  group_by(date) %>%
  summarize(mean_daily_KO2=mean(KO2, na.rm = TRUE),
            mean_daily_velocity=mean(velocity, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(turnover_length = -log(1-0.8)*mean_daily_velocity/mean_daily_KO2*86400/1000)

velos <- read_csv("../mv43/velocity_trace_through_2022.csv") %>% mutate(date=date(datetime)) %>% group_by(date) %>%
  summarize(mean_daily_velocity=mean(velocity, na.rm=TRUE))
turnover_lengths <- daily_out %>% 
  select(date, K600_mn) %>% 
  left_join(velos, by="date") %>%
  mutate(turnover_length=1.6*mean_daily_velocity/K600_mn*86400/1000)
  
summary(turnover_lengths$turnover_length)
hist(turnover_lengths$turnover_length)
