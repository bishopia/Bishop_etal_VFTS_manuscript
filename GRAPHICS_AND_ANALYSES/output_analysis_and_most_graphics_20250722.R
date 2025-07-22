##############################
###---model output workup--###
##############################

#clear workspace
rm(list = ls())

#setwd
setwd("C:/Users/ibishop/OneDrive - DOI/science/gc_metabolism/manuscript/analysis3/")

#libraries
library(tidyverse)
library(patchwork)
library(padr)
library(dygraphs)
library(tidymodels)
library(bayesplot)
library(rstan)

#month ydays, names, and labels
#list of first day of month
first_days <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)

#month names vector
month_names<- tibble(month=c(1:12), MONTH=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month_labs <- setNames(month_names$MONTH, month_names$month)

#colors for plots
cols <- c("black", "#F2BB7F", "#D58936","#8A4310", "#009292")


####################################
###---IMPORT MODEL OUTPUT DATA---###
####################################

#VFTS-PT
B1_daily <- read_csv("../../eda_twostation/mv57/mv57_payndates_B1_output_daily_20250715.csv") %>%
  dplyr::select(date, year:ER_Rhat, K600_mn:K600_down, k600_mn:r2stepchange) %>%
  filter(ER_Rhat < 1.05) %>% filter(GPP_Rhat < 1.05) %>% filter(k600_Rhat < 1.05) %>%
  dplyr::select(-jul) %>%
  rename_with(.fn = ~ paste0("B1_", .), .cols = GPP_mn:r2stepchange)

#VFTS-CQ
B2_daily <- read_csv("../../eda_twostation/mv59/mv59_payndates_B2_output_daily_20250715.csv") %>% 
  dplyr::select(date, year:ER_Rhat, K600_mn:K600_down, k600_mn:r2stepchange) %>%
  filter(ER_Rhat < 1.05) %>% filter(GPP_Rhat < 1.05) %>% filter(k600_Rhat < 1.05) %>%
  dplyr::select(-jul) %>%
  dplyr::select(-c(year:yday)) %>%
  rename_with(.fn = ~ paste0("B2_", .), .cols = GPP_mn:r2stepchange)

#VFTS-MT
B3_daily <- read_csv("../../eda_twostation/mv60/mv60_payndates_B3_output_daily_20250715.csv") %>%
  dplyr::select(date, year:ER_Rhat, K600_mn:K600_down, k600_mn:r2stepchange) %>%
  filter(ER_Rhat < 1.05) %>% filter(GPP_Rhat < 1.05) %>% filter(k600_Rhat < 1.05) %>%
  dplyr::select(-jul) %>%
  dplyr::select(-c(year:yday)) %>%
  rename_with(.fn = ~ paste0("B3_", .), .cols = GPP_mn:r2stepchange)

#VFTS-MT, low uniform tt
B3b_daily <- read_csv("../../eda_twostation/mv60b/mv60b_payndates_B3_output_daily_20250506.csv") %>%
  dplyr::select(date, year:ER_Rhat, K600_mn:K600_down, k600_mn:r2) %>%
  filter(ER_Rhat < 1.05) %>% filter(GPP_Rhat < 1.05) %>% filter(k600_Rhat < 1.05) %>%
  dplyr::select(-jul) %>%
  dplyr::select(-c(year:yday)) %>%
  rename_with(.fn = ~ paste0("B3b_", .), .cols = GPP_mn:r2)

#VFTS-PT, high uniform tt
B3c_daily <- read_csv("../../eda_twostation/mv60c/mv60c_payndates_B3_output_daily_20250506.csv") %>%
  dplyr::select(date, year:ER_Rhat, K600_mn:K600_down, k600_mn:r2) %>%
  filter(ER_Rhat < 1.05) %>% filter(GPP_Rhat < 1.05) %>% filter(k600_Rhat < 1.05) %>%
  dplyr::select(-jul) %>%
  dplyr::select(-c(year:yday)) %>%
  rename_with(.fn = ~ paste0("B3c_", .), .cols = GPP_mn:r2)

#import depths, get mean_daily_depth
depths <- read_csv("../../eda_twostation/mv52/velocity_depth_trace_through_2022.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "America/Phoenix")) %>%
  mutate(date=date(datetime)) %>% 
  group_by(date) %>% 
  summarize(mean_daily_depth=mean(depth, na.rm=TRUE))

#load one-station data, rename several variables
os_daily <- read_csv("../../eda_twostation/mv58/osfit_no_chunking_3kiter-mv58_20250505.csv") %>%
  drop_na(GPP_mean) %>%
  dplyr::select(date, 
         os_GPP_mn=GPP_daily_mean, 
         os_ER_mn=ER_daily_mean, 
         os_K600_mn=K600_daily_mean,
         os_GPP_up=GPP_97.5pct, 
         os_ER_up=ER_97.5pct, 
         os_K600_up=K600_daily_97.5pct,
         os_GPP_down=GPP_2.5pct, 
         os_ER_down=ER_2.5pct, 
         os_K600_down=K600_daily_2.5pct,
         os_GPP_rhat=GPP_daily_Rhat, 
         os_ER_rhat=ER_daily_Rhat, 
         os_K600_rhat=K600_daily_Rhat,
         os_r2=DO_R2_mean)

#import subdaily predictions
# os_subdaily <- read_csv("../../eda_twostation/mv58/osfit_no_chunking_3kiter-mv58_20250715_DOpredictions.csv")

#what about getting R2 for the change in each time step
# filtered_dates <- os_subdaily %>%
#   filter(!is.na(DO.mod)) %>%
#   distinct(date)

# os_r2stepchange <- 
#   os_subdaily %>%
#   filter(!is.na(DO.mod)) %>%
#   mutate(
#     change_predictedO2 = DO.mod - lag(DO.mod),
#     change_DO_obs_down = DO.obs - lag(DO.obs)
#   ) %>%
#   nest(data=c(-date)) %>%
#   mutate(
#     fit = map(data, ~ lm(change_predictedO2 ~ change_DO_obs_down, data = .x)),
#     tidied = map(fit, tidy),
#     glanced = map(fit, glance),
#     augmented = map(fit, augment)
#   ) %>%
#   select(date, glanced) %>%
#   unnest(glanced) %>% 
#   filter(nobs==96) %>%
#   select(date, os_r2stepchange=r.squared)
# hist(os_r2stepchange$os_r2stepchange, breaks=100, xlab="daily DO predicted/observed r22", main="")
# 
# #add r2stepchange back to os_daily
# os_daily <- os_daily %>% left_join(os_r2stepchange)

#import more daily covariates; filter to study period
covs_daily <- read_csv("../../eda_twostation/mv57/full_daily_mean_ts_2008-2022_temp_light_q_clouds_windspeeds.csv") %>%
  filter(date<ymd("2014-03-01"))

#import subdaily data from VFTS-PT run
df_subdaily <- read_csv("../../eda_twostation/mv57/mv57_payndates_B1_output_subdaily_20250505.csv") %>%
  mutate(datetime=with_tz(datetime, tz="America/Phoenix"))

#CMH model output
payn_daily <- read_csv("../../provided_data/payn_model_output/summary.csv") %>%
  mutate(date=date(timeStamp)) %>%
  mutate(k600=k600*24, k600_post025=k600_post025*24, k600_post975=k600_post975*24) %>%
  dplyr::select(
    date,
    payn_GPP_mn = gpp,
    payn_ER_mn = er,
    payn_k600_mn = k600,
    payn_r2 = coeffdet,
    payn_GPP_down = gpp_post025,
    payn_GPP_up = gpp_post975,
    payn_ER_down = er_post025,
    payn_ER_up = er_post975,
    payn_k600_down = k600_post025,
    payn_k600_up = k600_post975
  )
payn_daily <- payn_daily %>% left_join(depths, by="date") %>%
  mutate(payn_K600_mn=payn_k600_mn/mean_daily_depth,
         payn_K600_up=payn_k600_up/mean_daily_depth,
         payn_K600_down=payn_k600_down/mean_daily_depth) %>%
  dplyr::select(-mean_daily_depth)

#add fixed sigma value stated in paper
payn_daily <- payn_daily %>% mutate(payn_sigma=0.05)

#add r2stepchange
# r2stepchange <- read_csv("../../provided_data/payn_model_output/r2stepchange.csv")
# payn_daily <- payn_daily %>% left_join(r2stepchange) %>% rename(payn_r2stepchange=r2stepchange)

#combine model runs and covariates
combo <- B1_daily %>%
  full_join(B2_daily, by="date") %>%
  full_join(B3_daily, by="date") %>%
  full_join(B3b_daily, by="date") %>%
  full_join(B3c_daily, by="date") %>%
  full_join(covs_daily, by="date") %>%
  full_join(os_daily, by="date") %>%
  right_join(payn_daily, by="date") %>%
  mutate(year=year(date),
         month=month(date),
         yday=yday(date)) %>%
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
  ) %>%
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) %>%
  dplyr::select(date, year, season, month, yday, everything()) %>%
  arrange(date)

#add mean daily velocities
vs <- read_csv("../../eda_twostation/mv57/v_daily_means.csv")
combo <- combo %>% left_join(vs, by="date")

#add mean_daily_reach_length
rl <- read_csv("../../eda_twostation/mv59/reachlength_daily_means.csv")
combo <- combo %>% left_join(rl, by="date")

#save with low R2 for any pre-filter analyses and graphics
combo_for_r2_plots <- combo %>% drop_na()

#determine dates that pass filters for all 5 models compared here
good_payn_dates_all_have_estimates <- combo %>% 
  filter(payn_r2>0.9) %>%
  filter(B1_r2>0.9) %>%
  filter(B2_r2>0.9) %>%
  filter(B3_r2>0.9) %>%
  filter(os_r2>0.9) %>%
  filter(payn_ER_mn<=0) %>% 
  filter(payn_GPP_mn>=0) %>%
  filter(B1_ER_mn<=0) %>% 
  filter(B1_GPP_mn>=0) %>%
  filter(B2_ER_mn<=0) %>% 
  filter(B2_GPP_mn>=0) %>%
  filter(B3_ER_mn<=0) %>% 
  filter(B3_GPP_mn>=0) %>%
  filter(os_ER_mn<=0) %>% 
  filter(os_GPP_mn>=0) %>%
  drop_na(payn_GPP_mn, B1_GPP_mn, B2_GPP_mn, B3_GPP_mn, os_GPP_mn) %>%
distinct(date)

#filter combine dataset to good dates
combo <- combo %>%
  filter(date %in% good_payn_dates_all_have_estimates$date)

#reshape combo to long form, adjusting names as you go
combo_long <- combo %>%
  pivot_longer(
    cols = starts_with("B1_") | starts_with("B2_") | starts_with("B3_") | starts_with("B3b_") | starts_with("B3c_") | starts_with("os_") | starts_with("payn_"),
    names_to = c("dataset", ".value"),
    names_pattern = "(B1|B2|B3|B3b|B3c|os|payn)_(.*)",   #extract dataset and the rest of the name
    values_drop_na = TRUE
  ) %>%
  mutate(NEP_mn=GPP_mn+ER_mn,
         NEP_down=GPP_down + ER_down,
         NEP_up=GPP_up + ER_up) %>%
  mutate(dataset = factor(dataset, levels = c("payn", "B1", "B2", "B3", "B3b", "B3c", "os"))) %>%
  arrange(dataset)

#export combo and combo_long
write_csv(combo, paste0("./combo_wide_", format(Sys.Date(), "%Y%m%d"), ".csv"))
write_csv(combo_long, paste0("./combo_long_", format(Sys.Date(), "%Y%m%d"), ".csv"))



####################################
###---BASIC SUMMARY STATISTICS---###
####################################

#median, mean and sd for GPP, ER, K600, and NEP by dataset
daily_summary <- combo_long %>% 
  group_by(dataset) %>% 
  summarize(GPP_mn_median=median(GPP_mn, na.rm=TRUE), 
            GPP_mn_mean=mean(GPP_mn, na.rm=TRUE), 
            GPP_mn_sd=sd(GPP_mn, na.rm=TRUE),
            ER_mn_median=median(ER_mn, na.rm=TRUE), 
            ER_mn_mean=mean(ER_mn, na.rm=TRUE), 
            ER_mn_sd=sd(ER_mn, na.rm=TRUE),
            K600_mn_median=median(K600_mn, na.rm=TRUE), 
            K600_mn_mean=mean(K600_mn, na.rm=TRUE), 
            K600_mn_sd=sd(K600_mn, na.rm=TRUE),
            NEP_mn_median=median(NEP_mn, na.rm=TRUE), 
            NEP_mn_mean=mean(NEP_mn, na.rm=TRUE), 
            NEP_mn_sd=sd(NEP_mn, na.rm=TRUE))

#median, mean, and sd for GPP, ER, K600 and NEP by year and dataset
annual_summary <- combo_long %>%
  group_by(dataset, year) %>%
  summarize(
    GPP_mn_median = median(GPP_mn, na.rm = TRUE),
    GPP_mn_mean = mean(GPP_mn, na.rm = TRUE),
    GPP_mn_sd = sd(GPP_mn, na.rm = TRUE),
    ER_mn_median = median(ER_mn, na.rm = TRUE),
    ER_mn_mean = mean(ER_mn, na.rm = TRUE),
    ER_mn_sd = sd(ER_mn, na.rm = TRUE),
    K600_mn_median = median(K600_mn, na.rm = TRUE),
    K600_mn_mean = mean(K600_mn, na.rm = TRUE),
    K600_mn_sd = sd(K600_mn, na.rm = TRUE),
    NEP_mn_median = median(NEP_mn, na.rm = TRUE),
    NEP_mn_mean = mean(NEP_mn, na.rm = TRUE),
    NEP_mn_sd = sd(NEP_mn, na.rm = TRUE),
    GPP_mn_max = max(GPP_mn, na.rm = TRUE),
    GPP_mn_min = min(GPP_mn, na.rm = TRUE),
    .groups = 'drop'
  )

#summarizing median and sd of annual values for 2009-2013 (period starting 3 months into 2008 and ended three months into 2014)
mean_annuals <- annual_summary %>%
  filter(year %in% 2009:2013) %>%
  group_by(dataset) %>%
  summarize(
    GPP_mn_median_avg = median(GPP_mn_median, na.rm = TRUE),
    GPP_mn_sd = sd(GPP_mn_median, na.rm = TRUE),
    ER_mn_median_avg = median(ER_mn_median, na.rm = TRUE),
    ER_mn_sd = sd(ER_mn_median, na.rm = TRUE),
    K600_mn_median_avg = median(K600_mn_median, na.rm = TRUE),
    K600_mn_sd = sd(K600_mn_median, na.rm = TRUE),
    NEP_mn_median_avg = median(NEP_mn_median, na.rm = TRUE),
    NEP_mn_sd = sd(NEP_mn_median, na.rm = TRUE),
    GPP_mn_max_avg = median(GPP_mn_max, na.rm = TRUE),
    GPP_mn_max_sd = sd(GPP_mn_max, na.rm = TRUE),
    GPP_mn_min_avg = median(GPP_mn_min, na.rm = TRUE),
    GPP_mn_min_sd = sd(GPP_mn_min, na.rm = TRUE),
    .groups = 'drop'
  )

#summarize percent difference in GPP between VFTS models and CMH
combo %>% mutate(gppdiffvfts=(payn_GPP_mn-B1_GPP_mn)/payn_GPP_mn*100) %>% select(gppdiffvfts) %>% summary()
combo %>% mutate(gppdiffvfts=(payn_GPP_mn-B2_GPP_mn)/payn_GPP_mn*100) %>% select(gppdiffvfts) %>% summary()
combo %>% mutate(gppdiffvfts=(payn_GPP_mn-B3_GPP_mn)/payn_GPP_mn*100) %>% select(gppdiffvfts) %>% summary()
combo %>% mutate(gppdiffvfts=(payn_GPP_mn-os_GPP_mn)/payn_GPP_mn*100) %>% select(gppdiffvfts) %>% summary()

#parameter overlaps with Payn 95% confidence interval 
combo %>%
  mutate(
    ranges_overlap_GPP = ifelse(B1_GPP_up >= payn_GPP_down & payn_GPP_up >= B1_GPP_down, "yes", "no"),
    ranges_overlap_ER = ifelse(B1_ER_up >= payn_ER_down & payn_ER_up >= B1_ER_down, "yes", "no"),
    ranges_overlap_K600 = ifelse(B1_K600_up >= payn_K600_down & payn_K600_up >= B1_K600_down, "yes", "no"),
  ) %>%
  count(ranges_overlap_K600) #change this last one to look at each parameter
  
#report median difference (and sd) between CMH estimates and alternative model estimates
combo_long %>%
  dplyr::select(dataset, date, GPP_mn, ER_mn, K600_mn, NEP_mn) %>%
  pivot_longer(cols = c(GPP_mn, ER_mn, K600_mn, NEP_mn), 
               names_to = "parameter", 
               values_to = "value") %>%
  pivot_wider(names_from = dataset, values_from = value) %>%
  drop_na() %>%
  mutate(diffB1 = payn - B1, 
         diffB2 = payn - B2, 
         diffB3 = payn - B3, 
         diffos = payn - os) %>%
  summarize(
    mdiffB1 = median(diffB1), 
    mdiffB2 = median(diffB2),
    mdiffB3 = median(diffB3),
    mdiffos = median(diffos),
    sddiffB1 = sd(diffB1), 
    sddiffB2 = sd(diffB2),
    sddiffB3 = sd(diffB3),
    sddiffos = sd(diffos),
    .by = parameter
  )


#################################
###---MANUSCRIPT MAIN PLOTS---###
#################################

#see separate script for FIGURE 1 map

#FIGURE 2: EXAMPLE OF DIEL DO AND FLOW
p1 <- df_subdaily %>% 
  filter(date %in% c(ymd("2012-02-02"), ymd("2012-08-02"))) %>%
  dplyr::select(date, DO_obs_down, DO_obs_up, DO_sat_down, DO_sat_up, time, lag) %>%
  mutate(perc_DO_down=DO_obs_down/DO_sat_down*100,
         perc_DO_up=DO_obs_up/DO_sat_up*100) %>% 
  select(date, time, starts_with("perc")) %>%
  pivot_longer(starts_with("perc"), names_to = "variable", values_to = "value") %>%
  ggplot() +
  geom_line(aes(x=time, y=value, group=interaction(date, variable), col=as.factor(date), linetype=variable, alpha=variable), size=2) +
  theme_bw(base_size=16) +
  guides(linetype="none", alpha="none") +
  xlab("") +
  ylab("Dissolved oxygen\n(% saturation)") +
  scale_color_manual(values=c("darkblue", "darkorange")) +
  scale_x_continuous(breaks=seq(0,24,3)) +
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),   
        legend.background = element_rect(fill = "white", color = "black", size = 0.5),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_blank()) + 
  labs(col="") +
  scale_alpha_manual(values=c(1, 0.6))

#load high frequency DO data
load("../../eda_twostation/mv48/leesextended_2023_prep1_mv48_20250328.RData")

#plot representative winter and summer days
p1 <- lees %>%
  mutate(date=date(datetime)) %>%
  mutate(time = hour(datetime) + minute(datetime) / 60) %>%
  filter(date %in% c(ymd("2012-02-02"), ymd("2012-08-02"))) %>%
  mutate(perc_DO_down=oxydown/oSatdown*100) %>%
  mutate(perc_DO_up=oxyup/oSatup*100) %>%
  select(date, time, starts_with("perc")) %>%
  pivot_longer(starts_with("perc"), names_to = "variable", values_to = "value") %>%
  ggplot() +
  geom_line(aes(x=time, y=value, group=interaction(date, variable), col=as.factor(date), linetype=variable, alpha=variable), size=2) +
  theme_bw(base_size=16) +
  guides(linetype="none", alpha="none") +
  xlab("") +
  ylab("Dissolved oxygen\n(% saturation)") +
  scale_color_manual(values=c("darkblue", "darkorange")) +
  scale_x_continuous(breaks=seq(0,24,3)) +
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),   
        legend.background = element_rect(fill = "white", color = "black", size = 0.5),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_blank()) + 
  labs(col="") +
  scale_alpha_manual(values=c(1, 0.6))
  
#load Q data
load("../../raw_data/lf_gage_discharge/lee_flow_gcgage_20080101-20241031.RData")
discharge <- lee.flow %>% 
  select(datetime, discharge=Discharge) %>%
  mutate(time = hour(datetime) + minute(datetime) / 60)

#plot discharge for the same representative winter and summer days, use as second subplot in manuscript graphic
p2 <- discharge %>% 
  mutate(date=date(datetime)) %>%
  filter(date %in% c(ymd("2012-02-02"), ymd("2012-08-02"))) %>%
  ggplot(aes(x=time, y=discharge*0.0283168)) + 
  geom_line(aes(group=date, col=as.factor(date)), size=2) +
  theme_bw(base_size=16) +
  guides(col="none") +
  xlab("Hour of day") +
  ylab("Discharge (cms)") +
  scale_x_continuous(breaks=seq(0,24,3)) +
  scale_y_continuous(breaks=seq(250,500,50)) +
  scale_color_manual(values=c("darkblue", "darkorange")) #+

#combine and export
p <- p1 / p2
p
ggsave("./graphics/FIG2_EXAMPLE_DIEL_DO_DISCHARGE_PLOT2.png", p, height=1.5*6, width=1.5*3.75, units="in")



#FIGURE 4: R2 scatterplots, using pre-filtered output, so it includes days where R2<0.9

#subplot1: VFTS-PT vs CMH
fit_1 <- lm(B1_r2 ~ payn_r2, data=combo_for_r2_plots) 
fit_1_slope <- coef(fit_1)[2]
fit_1_r2 <- summary(fit_1)$r.squared
#plot
p1 <- combo_for_r2_plots %>%
  ggplot(aes(x=payn_r2, y=B1_r2)) + 
  geom_point(size=0.5) + 
  geom_abline(intercept=0, slope=1, linetype="dashed", linewidth=1) +
  geom_abline(intercept = coef(fit_1)[1], slope = fit_1_slope, linewidth=0.75, col=1) +
  scale_x_continuous(limits=c(0.8,1)) +
  scale_y_continuous(limits=c(0.6,1)) +
  xlab("") +
  ylab(expression(R^2 * " (VFTS-PT)")) +  
  theme_bw(base_size = 12) +
  theme(plot.margin = margin(t = -20, r = 0, b = -20, l = 0)) +
  annotate("text",  x=0.8, y = 0.98, label = paste0("slope=", signif(fit_1_slope, 3)), size=3, hjust = 0) +
  annotate("text",  x=0.8, y = 0.94, label = bquote(R^2 == .(signif(fit_1_r2, 3))), size=3, hjust=0)
p1

#subplot2: VFTS-CQ vs CMH
fit_2 <- lm(B2_r2 ~ payn_r2, data=combo_for_r2_plots)
fit_2_slope <- coef(fit_2)[2]
fit_2_r2 <- summary(fit_2)$r.squared 
#plot
p2 <- combo_for_r2_plots %>%
  ggplot(aes(x=payn_r2, y=B2_r2)) + 
  geom_point(size=0.5) + 
  geom_abline(intercept=0, slope=1, linetype="dashed", linewidth=1) +
  geom_abline(intercept = coef(fit_2)[1], slope = fit_2_slope, linewidth=0.75, col=1) +
  scale_x_continuous(limits=c(0.8,1)) +
  scale_y_continuous(limits=c(0.6,1)) +
  xlab("") +
  ylab(expression(R^2 * " (VFTS-CQ)")) +  
  # stat_smooth(method="lm") +
  theme_bw(base_size = 12) +
  theme(plot.margin = margin(t = -20, r = 0, b = -20, l = 0)) +
  annotate("text",  x=0.8, y = 0.98, label = paste0("slope=", signif(fit_2_slope, 3)), size=3, hjust = 0) +
  annotate("text",  x=0.8, y = 0.94, label = bquote(R^2 == .(signif(fit_2_r2, 3))), size=3, hjust=0)
p2


#subplot3: VFTS-MT vs CMH
fit_3 <- lm(B3_r2 ~ payn_r2, data=combo_for_r2_plots)
fit_3_slope <- coef(fit_3)[2]
fit_3_r2 <- summary(fit_3)$r.squared
#plot
p3 <- combo_for_r2_plots %>%
  ggplot(aes(x=payn_r2, y=B3_r2)) + 
  geom_point(size=0.5) +
  geom_abline(intercept=0, slope=1, linetype="dashed", linewidth=1) +
  geom_abline(intercept = coef(fit_3)[1], slope = fit_3_slope, linewidth=0.75, col=1) +
  scale_x_continuous(limits=c(0.8,1)) +
  scale_y_continuous(limits=c(0.6,1)) +
  xlab("") +
  ylab(expression(R^2 * " (VFTS-MT)")) +  
  theme_bw(base_size = 12) +
  theme(plot.margin = margin(t = -20, r = 0, b = -20, l = 0)) +
  annotate("text",  x=0.8, y = 0.98, label = paste0("slope=", signif(fit_3_slope, 3)), size=3, hjust = 0) +
  annotate("text",  x=0.8, y = 0.94, label = bquote(R^2 == .(signif(fit_3_r2, 3))), size=3, hjust=0)
p3

#subplot4: OS vs CMH
fit_4 <- lm(os_r2 ~ payn_r2, data=combo_for_r2_plots)
fit_4_slope <- coef(fit_4)[2]
fit_4_r2 <- summary(fit_4)$r.squared
#plot
p4 <- combo_for_r2_plots %>%
  ggplot(aes(x=payn_r2, y=os_r2)) + 
  geom_point(size=0.5) + 
  geom_abline(intercept=0, slope=1, linetype="dashed", linewidth=1) +
  geom_abline(intercept = coef(fit_4)[1], slope = fit_4_slope, linewidth=0.75, col=1) +
  scale_x_continuous(limits=c(0.8,1)) +
  scale_y_continuous(limits=c(0.6,1)) +
  xlab(expression(R^2 * " (CMH)")) + 
  ylab(expression(R^2 * " (One-station)")) +  
  theme_bw(base_size = 12) +
  theme(plot.margin = margin(t = -20, r = 0, b = -20, l = 0)) +
  annotate("text",  x=0.8, y = 0.98, label = paste0("slope=", signif(fit_4_slope, 3)), size=3, hjust = 0) +
  annotate("text",  x=0.8, y = 0.94, label = bquote(R^2 == .(signif(fit_4_r2, 3))), size=3, hjust=0)
p4

#combine and export
p <- p1 + p2 + p3 + p4 + plot_layout(ncol=1)  # Optional: to add tags to subplots
p <- p1 + p2 + p3 + plot_layout(ncol=1)  # Optional: to add tags to subplots
ggsave("./graphics/FIG4_R2_plots_by_model_comparison_with_CMH2.png", p, width=3.25, height=8, units="in")



#FIGURE 5: Time series of all three parameters, showing CMH, VFTS-PT, and OS, filtered days only
#GPP subplot
p1 <- combo_long %>% 
  filter(dataset %in% c("payn", "B1", "os")) %>%
  mutate(dataset = recode(dataset,
                          "payn" = "Payn et al.",
                          "B1" = "Bishop et al.\n(Payn tt)",
                          "B2" = "Bishop et al.\n(Discharge-based tt)",
                          "B3" = "Bishop et al.\n(Mean Payn tt)",
                          "os" = "One-station")) %>%
  ggplot() + 
  geom_point(aes(x=date, y=GPP_mn, col=dataset), size=1.5, alpha=0.75) +
  geom_point(data = combo_long %>% filter(dataset == "payn"), 
             aes(x=date, y=GPP_mn), 
             color = "black", size = 1.5, alpha = 0.75) +
  scale_colour_manual(values=cols[c(1,3,5)]) +
  theme_bw(base_size = 14) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  xlab("") +
  ylab(expression(paste("GPP (g O"[2]*" m"^"-2"*"d"^"-1"*")"))) +
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c("left", "top"),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.title = element_blank()) +
  guides(col = "none")

#ER subplot
p2 <- combo_long %>% 
  filter(dataset %in% c("payn", "B1", "os")) %>%
  mutate(dataset = recode(dataset,
                          "payn" = "Payn et al.",
                          "B1" = "Bishop et al.\n(Payn tt)",
                          "B2" = "Bishop et al.\n(Discharge-based tt)",
                          "B3" = "Bishop et al.\n(Mean Payn tt)",
                          "os" = "One-station")) %>%
  ggplot() + 
  geom_point(aes(x=date, y=ER_mn, col=dataset), size=1.5, alpha=0.75) +
  geom_point(data = combo_long %>% filter(dataset == "payn"), 
             aes(x=date, y=ER_mn), 
             color = "black", size = 1.5, alpha = 0.75) +
  scale_colour_manual(values=cols[c(1,3,5)]) +
  theme_bw(base_size = 14) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  xlab("") +
  ylab(expression(paste("ER (g O"[2]*" m"^"-2"*"d"^"-1"*")"))) +
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c("left", "top"),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.title = element_blank()) +
  guides(col = "none") 

#K600 subplot
p3 <- combo_long %>% 
  filter(dataset %in% c("payn", "B1", "os")) %>%
  mutate(dataset = recode(dataset,
                          "payn" = "CMH",
                          "B1" = "VFTS-PT",
                          "B2" = "VFTS-CQ",
                          "B3" = "VFTS-MT",
                          "os" = "One-station")) %>%
  ggplot() + 
  geom_point(aes(x=date, y=K600_mn, col=dataset), size=1.5, alpha=0.75) +
  geom_point(data = combo_long %>% filter(dataset == "payn"), 
             aes(x=date, y=K600_mn), 
             color = "black", size = 1.5, alpha = 0.75) +
  scale_colour_manual(values=cols[c(1,3,5)]) +
  theme_bw(base_size = 14) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  xlab("") +
  ylab(expression(paste("K"[600]*"("*"d"^"-1"*")"))) +
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c("left", "top"),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.title = element_blank()) +
  guides(col = guide_legend(override.aes = list(size = 5))) 

#combine and export
p <- p1 / p2 /p3
ggsave("./graphics/FIG5_GEK_OVER_TIME_PAYN-B1-OS.png", p, width=1.5*6.5, height=1.5*6.5, units="in")

#FIGURE 6: 1:1 parameter comparison between CMH and alternative models
fit_1a <- lm(B1_GPP_mn ~ 0 + payn_GPP_mn, data=combo)
fit_1a_slope <- coef(fit_1a)[1]
fit_1a_r2 <- summary(fit_1a)$r.squared
p1a <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_GPP_mn, y = B1_GPP_mn, col = "B1 vs Payn"), fill = cols[c(2)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean GPP (CMH)") +
  ylab("Mean GPP (VFTS-PT)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(0,NA)) +
  scale_y_continuous(limits=c(0,25)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_1a_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=0, y = 25, label = paste0("slope=", signif(fit_1a_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=0, y = 23, label = paste0("R2=", signif(fit_1a_r2, 2)), size=5, hjust = 0)
p1a

fit_1b <- lm(B2_GPP_mn ~ 0 + payn_GPP_mn, data=combo)
fit_1b_slope <- coef(fit_1b)[1]
fit_1b_r2 <- summary(fit_1b)$r.squared
p1b <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_GPP_mn, y = B2_GPP_mn, col = "B2 vs Payn"), fill = cols[c(3)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean GPP (CMH)") +
  ylab("Mean GPP (VFTS-CQ)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(0,NA)) +
  scale_y_continuous(limits=c(0,25)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_1b_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=0, y = 25, label = paste0("slope=", signif(fit_1b_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=0, y = 23, label = paste0("R2=", signif(fit_1b_r2, 2)), size=5, hjust = 0)
p1b

fit_1c <- lm(B3_GPP_mn ~ 0 + payn_GPP_mn, data=combo)
fit_1c_slope <- coef(fit_1c)[1]
fit_1c_r2 <- summary(fit_1c)$r.squared
p1c <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_GPP_mn, y = B3_GPP_mn, col = "B3 vs Payn"), fill = cols[c(4)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean GPP (CMH)") +
  ylab("Mean GPP (VFTS-MT)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(0,NA)) +
  scale_y_continuous(limits=c(0,25)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_1c_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=0, y = 25, label = paste0("slope=", signif(fit_1c_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=0, y = 23, label = paste0("R2=", signif(fit_1c_r2, 2)), size=5, hjust = 0)
p1c

fit_1d <- lm(os_GPP_mn ~ 0 + payn_GPP_mn, data=combo)
fit_1d_slope <- coef(fit_1d)[1]
fit_1d_r2 <- summary(fit_1d)$r.squared
p1d <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_GPP_mn, y = os_GPP_mn, col = "OS vs Payn"), fill = cols[c(5)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean GPP (CMH)") +
  ylab("Mean GPP (One-station)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(0,NA)) +
  scale_y_continuous(limits=c(0,25)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_1d_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=0, y = 25, label = paste0("slope=", signif(fit_1d_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=0, y = 23, label = paste0("R2=", signif(fit_1d_r2, 2)), size=5, hjust = 0)
p1d


fit_2a <- lm(B1_ER_mn ~ 0 + payn_ER_mn, data=combo)
fit_2a_slope <- coef(fit_2a)[1]
fit_2a_r2 <- summary(fit_2a)$r.squared
p2a <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_ER_mn, y = B1_ER_mn, col = "B1 vs Payn"), fill = cols[c(2)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean ER (CMH)") +
  ylab("Mean ER (VFTS-PT)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(NA,0)) +
  scale_y_continuous(limits=c(-50,0)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_2a_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=-35, y = 0, label = paste0("slope=", signif(fit_2a_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=-35, y = -3.5, label = paste0("R2=", signif(fit_2a_r2, 2)), size=5, hjust = 0)
p2a

fit_2b <- lm(B2_ER_mn ~ 0 + payn_ER_mn, data=combo)
fit_2b_slope <- coef(fit_2b)[1]
fit_2b_r2 <- summary(fit_2b)$r.squared
p2b <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_ER_mn, y = B2_ER_mn, col = "B2 vs Payn"), fill = cols[c(3)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean ER (CMH)") +
  ylab("Mean ER (VFTS-CQ)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(NA,0)) +
  scale_y_continuous(limits=c(-50,0)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_2b_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=-35, y = 0, label = paste0("slope=", signif(fit_2b_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=-35, y = -3.5, label = paste0("R2=", signif(fit_2b_r2, 2)), size=5, hjust = 0)
p2b

fit_2c <- lm(B3_ER_mn ~ 0 + payn_ER_mn, data=combo)
fit_2c_slope <- coef(fit_2c)[1]
fit_2c_r2 <- summary(fit_2c)$r.squared
p2c <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_ER_mn, y = B3_ER_mn, col = "B3 vs Payn"), fill = cols[c(4)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean ER (CMH)") +
  ylab("Mean ER (VFTS-MT)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(NA,0)) +
  scale_y_continuous(limits=c(-50,0)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_2c_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=-35, y = 0, label = paste0("slope=", signif(fit_2c_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=-35, y = -3.5, label = paste0("R2=", signif(fit_2c_r2, 2)), size=5, hjust = 0)
p2c

fit_2d <- lm(os_ER_mn ~ 0 + payn_ER_mn, data=combo)
fit_2d_slope <- coef(fit_2d)[1]
fit_2d_r2 <- summary(fit_2d)$r.squared
p2d <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_ER_mn, y = os_ER_mn, col = "OS vs Payn"), fill = cols[c(5)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean ER (CMH)") +
  ylab("Mean ER (One-station)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(NA,0)) +
  scale_y_continuous(limits=c(-50,0)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_2d_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=-35, y = 0, label = paste0("slope=", signif(fit_2d_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=-35, y = -3.5, label = paste0("R2=", signif(fit_2d_r2, 2)), size=5, hjust = 0)
p2d

fit_3a <- lm(B1_K600_mn ~ 0 + payn_K600_mn, data=combo)
fit_3a_slope <- coef(fit_3a)[1]
fit_3a_r2 <- summary(fit_3a)$r.squared
p3a <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_K600_mn, y = B1_K600_mn, col = "B1 vs Payn"), fill = cols[c(2)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean K600 (CMH)") +
  ylab("Mean K600 (VFTS-PT)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(0,NA)) +
  scale_y_continuous(limits=c(0,2.4)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_3a_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=0, y = 2.4, label = paste0("slope=", signif(fit_3a_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=0, y = 2.2, label = paste0("R2=", signif(fit_3a_r2, 2)), size=5, hjust = 0)
p3a

fit_3b <- lm(B2_K600_mn ~ 0 + payn_K600_mn, data=combo)
fit_3b_slope <- coef(fit_3b)[1]
fit_3b_r2 <- summary(fit_3b)$r.squared
p3b <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_K600_mn, y = B2_K600_mn, col = "B2 vs Payn"), fill = cols[c(3)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean K600 (CMH)") +
  ylab("Mean K600 (VFTS-CQ)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(0,NA)) +
  scale_y_continuous(limits=c(0,2.4)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_3b_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=0, y = 2.4, label = paste0("slope=", signif(fit_3b_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=0, y = 2.2, label = paste0("R2=", signif(fit_3b_r2, 2)), size=5, hjust = 0)
p3b

fit_3c <- lm(B3_K600_mn ~ 0 + payn_K600_mn, data=combo)
fit_3c_slope <- coef(fit_3c)[1]
fit_3c_r2 <- summary(fit_3c)$r.squared
p3c <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_K600_mn, y = B3_K600_mn, col = "B3 vs Payn"), fill = cols[c(4)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean K600 (CMH)") +
  ylab("Mean K600 (VFTS-MT)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(0,NA)) +
  scale_y_continuous(limits=c(0,2.4)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_3c_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=0, y = 2.4, label = paste0("slope=", signif(fit_3c_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=0, y = 2.2, label = paste0("R2=", signif(fit_3c_r2, 2)), size=5, hjust = 0)
p3c

fit_3d <- lm(os_K600_mn ~ 0 + payn_K600_mn, data=combo)
fit_3d_slope <- coef(fit_3d)[1]
fit_3d_r2 <- summary(fit_3d)$r.squared
p3d <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_K600_mn, y = os_K600_mn, col = "OS vs Payn"), fill = cols[c(5)], shape=21, col="grey30", size = 2) +
  theme_bw(base_size = 16) +
  xlab("Mean K600 (CMH)") +
  ylab("Mean K600 (One-station)") +
  scale_color_manual(values = cols[c(2)]) +
  scale_x_continuous(limits=c(0,NA)) +
  scale_y_continuous(limits=c(0,2.4)) +
  theme(legend.title = element_blank()) +
  guides(fill="none", col="none", shape="none") +
  geom_abline(intercept = 0, slope=fit_3d_slope, col=1, linewidth=1.5) +
  geom_abline(intercept = 0, slope = 1, linewidth=1.5, linetype="dashed") +
  annotate("text",  x=0, y = 2.4, label = paste0("slope=", signif(fit_3d_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=0, y = 2.2, label = paste0("R2=", signif(fit_3d_r2, 2)), size=5, hjust = 0)
p3d

p <- p1a + p1b + p1c + p1d + p2a + p2b + p2c + p2d + p3a + p3b + p3c + p3d+ plot_layout(ncol=3, byrow = FALSE)
p
ggsave("./graphics/FIG6_main_1to1_parameter_scatterplots_model_faceted.png", p, height=2*8, width=2*6.5, units="in")



#FIGURE 7: Plot annual medians of daily median GPP, median ER, and median NEP
#GPP
p1 <- annual_summary %>% 
  filter(!(dataset %in% c("B3b", "B3c"))) %>%
  filter(year %in% 2009:2013) %>%
  mutate(dataset = recode(dataset,
                          "payn" = "Payn et al.",
                          "B1" = "VFTS (Payn tt)",
                          "B2" = "VFTS (Discharge-based tt)",
                          "B3" = "VFTS (Mean Payn tt)",
                          "os" = "One-station")) %>%
  ggplot(aes(x=dataset, y=GPP_mn_median, fill=dataset, shape=as.factor(year))) + 
  geom_jitter(width=0.2, col=1, size=3) +
  theme_bw(base_size = 14) + 
  xlab("") + 
  ylab(expression(atop("Median daily GPP", paste("(g O"[2]*" m"^"-2"*" d"^"-1"*")")))) +
  scale_fill_manual(values=cols) +
  scale_shape_manual(values=c(21:25)) +
  guides(fill="none", shape="none") +
  theme(axis.text.x = element_blank())

#ER
p2 <- annual_summary %>% 
  filter(!(dataset %in% c("B3b", "B3c"))) %>%
  filter(year %in% 2009:2013) %>%
  mutate(dataset = recode(dataset,
                          "payn" = "Payn et al.",
                          "B1" = "VFTS (Payn tt)",
                          "B2" = "VFTS (Discharge-based tt)",
                          "B3" = "VFTS (Mean Payn tt)",
                          "os" = "One-station")) %>%
  ggplot(aes(x=dataset, y=ER_mn_median, fill=dataset, shape=as.factor(year))) + 
  geom_jitter(width=0.2, col=1, size=3) +
  theme_bw(base_size = 14) + 
  xlab("") + 
  scale_shape_manual(values=c(21:25)) +
  ylab(expression(atop("Median daily ER", paste("(g O"[2]*" m"^"-2"*" d"^"-1"*")")))) +
  scale_fill_manual(values=cols) +
  guides(fill="none", shape="none") +
  theme(axis.text.x = element_blank())

#NEP
p3 <- annual_summary %>% 
  filter(!(dataset %in% c("B3b", "B3c"))) %>%
  filter(year %in% 2009:2013) %>%
  mutate(dataset = recode(dataset,
                          "payn" = "CMH",
                          "B1" = "VFTS-PT",
                          "B2" = "VFTS-CQ",
                          "B3" = "VFTS-MT",
                          "os" = "One-station")) %>%
  ggplot(aes(x=dataset, y=NEP_mn_median, fill=dataset, shape=as.factor(year))) + 
  geom_jitter(width=0.2, col=1, size=3) +
  theme_bw(base_size = 14) + 
  xlab("") + 
  ylab(expression(atop("Median daily NEP", paste("(g O"[2]*" m"^"-2"*" d"^"-1"*")")))) +
  scale_fill_manual(values=cols) +
  scale_shape_manual(values=c(21:25)) +
  guides(fill="none", shape="none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#one more for K600, not currently using
p4 <- annual_summary %>% 
  filter(!(dataset %in% c("B3b", "B3c"))) %>%
  filter(year %in% 2009:2013) %>%
  mutate(dataset = recode(dataset,
                          "payn" = "CMH",
                          "B1" = "VFTS-PT",
                          "B2" = "VFTS-CQ",
                          "B3" = "VFTS-MT",
                          "os" = "One-station")) %>%
  ggplot(aes(x=dataset, y=K600_mn_median, fill=dataset, shape=as.factor(year))) + 
  geom_jitter(width=0.2, col=1, size=3) +
  theme_bw(base_size = 14) + 
  xlab("") + 
  ylab(expression(atop("Median daily K600", paste("(d"^"-1"*")")))) +
  scale_fill_manual(values=cols) +
  scale_shape_manual(values=c(21:25)) +
  guides(fill="none", shape="none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#stats comparing means of above annual metrics:
tmp1 <- annual_summary %>% 
  filter(year %in% 2009:2013)
FSA::dunnTest(GPP_mn_median  ~ dataset, data = tmp1, method = "bh")
FSA::dunnTest(ER_mn_median ~ dataset, data = tmp1, method = "bh")
FSA::dunnTest(NEP_mn_median ~ dataset, data = tmp1, method = "bh")
FSA::dunnTest(K600_mn_median ~ dataset, data = tmp1, method = "bh")
rm(tmp1)

#combine and export plots
p <- p1 / p2 / p3
ggsave("./graphics/FIG7_ANNUAL_METRIC_COMPARISON_JITTERPLOT.png", p, width=3.25, height=8.5, units="in")


#for FIGURE 7, see separate script

#############################################
###---Supplementary tables and graphics---###
#############################################

#plot
p1 <- combo_for_r2_plots %>%
  ggplot(aes(x=payn_r2, y=B1_r2)) + 
  geom_point(size=0.5) + 
  geom_abline(intercept=0, slope=1, linetype="dashed", linewidth=1) +
  geom_abline(intercept = coef(fit_1)[1], slope = fit_1_slope, linewidth=0.75, col=1) +
  scale_x_continuous(limits=c(0.8,1)) +
  scale_y_continuous(limits=c(0.6,1)) +
  xlab("") +
  ylab(expression(R^2 * " (VFTS-PT)")) +  
  theme_bw(base_size = 12) +
  theme(plot.margin = margin(t = -20, r = 0, b = -20, l = 0)) +
  annotate("text",  x=0.8, y = 0.98, label = paste0("slope=", signif(fit_1_slope, 3)), size=3, hjust = 0) +
  annotate("text",  x=0.8, y = 0.94, label = bquote(R^2 == .(signif(fit_1_r2, 3))), size=3, hjust=0)
p1
## another supp, how parameter residual increases as chosen uniform tt deviates from PT tt
tmp1 <- combo %>% mutate(gppdiff=payn_GPP_mn-B3_GPP_mn, ttdiff=B1_tt_daily_mean*24-B3_tt_daily_mean*24)

fit_0 <- lm(gppdiff ~ ttdiff, data=tmp1) 
fit_0_slope <- coef(fit_0)[2]
fit_0_r2 <- summary(fit_0)$r.squared
p0 <- tmp1 %>% 
  ggplot(aes(x=ttdiff, y=gppdiff)) +
  geom_point() +
  geom_abline(intercept=0, slope=1, linetype="dashed", linewidth=1) +
  geom_abline(intercept = coef(fit_0)[1], slope = fit_0_slope, linewidth=0.75, col=1) +
  theme_bw(base_size=16) +
  xlab("Mean daily travel time difference (CMH - VFTS-MT; hours)") +
  ylab(expression(paste("GPP difference (CMH - VFTS-MT; g O"[2]*" m"^"-2"*"d"^"-1"*")"))) +
  annotate("text",  x=-2.3, y = 9, label = paste0("slope=", signif(fit_0_slope, 3)), size=3, hjust = 0) +
  annotate("text",  x=-2.3, y = 8.5, label = bquote(R^2 == .(signif(fit_0_r2, 3))), size=3, hjust=0)
p0
ggsave("./graphics/GPPresid~ttresid.png", p0, height=6.5, width=6.5, units="in")
  
  
#FIGURE S2: Parameter estimate difference histogram between CMH and alternative models, faceted by month
tmp1 <- combo_long %>% 
  select(date, dataset, GPP_mn, month) %>%
  pivot_wider(names_from = dataset, values_from=GPP_mn)
p1 <- tmp1 %>% 
  ggplot() +
  geom_vline(xintercept=0, linetype="dashed", linewidth=1.25, col="grey50") +
  geom_histogram(aes(x=(payn-B1)), fill=cols[2], col=1, alpha=1) +
  geom_histogram(aes(x=(payn-os)), fill=cols[5], col=1, alpha=1) +
  theme_bw(base_size=18) +
  facet_wrap(~month, scales="free_y", labeller = labeller(month = month_labs)) +
  xlab("Difference in mean daily GPP (CMH - Other)")
ggsave(paste0("./graphics/FIG_S2_GPP_difference_histogram_by_month.png"), p1, width=2*6.5, height=2*4.5, units="in")
rm(tmp1)

tmp1 <- combo_long %>% 
  select(date, dataset, ER_mn, month) %>%
  pivot_wider(names_from = dataset, values_from=ER_mn)
p2 <- tmp1 %>% 
  ggplot() +
  geom_vline(xintercept=0, linetype="dashed", linewidth=1.25, col="grey50") +
  geom_histogram(aes(x=(payn-B1)), fill=cols[2], col=1, alpha=1) +
  geom_histogram(aes(x=(payn-os)), fill=cols[5], col=1, alpha=1) +
  theme_bw(base_size=16) +
  facet_wrap(~month, scales="free_y", labeller = labeller(month = month_labs)) +
  xlab("Difference in mean daily ER (CMH - Other)")
ggsave(paste0("./graphics/FIG_SX_ER_difference_histogram_by_month.png"), p2, width=2*6.5, height=2*4.5, units="in")
rm(tmp1)

tmp1 <- combo_long %>% 
  select(date, dataset, K600_mn, month) %>%
  pivot_wider(names_from = dataset, values_from=K600_mn)
p3 <- tmp1 %>% 
  ggplot() +
  geom_vline(xintercept=0, linetype="dashed", linewidth=1.25, col="grey50") +
  geom_histogram(aes(x=(payn-B1)), fill=cols[2], col=1, alpha=1) +
  geom_histogram(aes(x=(payn-os)), fill=cols[5], col=1, alpha=1) +
  theme_bw(base_size=16) +
  facet_wrap(~month, scales="free_y", labeller = labeller(month = month_labs)) +
  xlab("Difference in mean daily K600 (CMH - Other)")
ggsave(paste0("./graphics/FIG_SX_K600_difference_histogram_by_month.png"), p3, width=2*6.5, height=2*4.5, units="in")
rm(tmp1)


#FIGURE S3: transition zone length
#summarize transition zone length in km (3*velocity/K600); use discharge-based estimates
p1 <- combo_long %>% 
  filter(dataset=="B1") %>% 
  select(date, v_daily_mean, K600_mn, reachlength_daily_mean, season) %>% 
  mutate(K600_mn_per_second = K600_mn / 86400) %>%
  mutate(Dk=reachlength_daily_mean*K600_mn_per_second/v_daily_mean) %>%
  ggplot(aes(x=Dk)) + 
  geom_histogram(col="black", fill="grey30") +
  theme_bw(base_size=16) +
  xlab("Damkohler number") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)), breaks=seq(0,0.7,0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) #+ 
  # facet_wrap(~season)

combo_long %>% 
  filter(dataset=="B1") %>% 
  select(date, v_daily_mean, K600_mn, reachlength_daily_mean, season) %>% 
  mutate(K600_mn_per_second = K600_mn / 86400) %>%
  mutate(Dk=reachlength_daily_mean*K600_mn_per_second/v_daily_mean) %>%
  summary()

p2 <- combo_long %>% 
  filter(dataset=="B1") %>% 
  select(date, v_daily_mean, K600_mn, reachlength_daily_mean, GPP_mn, GPP_down, GPP_up, season) %>%
  mutate(K600_mn_per_second = K600_mn / 86400) %>%
  mutate(Dk=reachlength_daily_mean*K600_mn_per_second/v_daily_mean) %>%
  mutate(stddev = (GPP_up - GPP_down) / (2 * 1.96)) %>%
  ggplot(aes(x=Dk, y=GPP_up - GPP_down)) + 
  geom_point(shape=21, col=1, fill="grey50") +
  theme_bw(base_size=16) +
  scale_x_continuous(breaks=seq(0,0.7,0.1)) +
  xlab("Damkohler number") + 
  ylab("GPP uncertainty\n(CI magnitude)")
  
combo_long %>% 
  filter(dataset=="B2") %>% 
  mutate(equilibrationlength=0.7*v_daily_mean/K600_mn*86400) %>% 
  select(date, equilibrationlength) %>% 
  summary()

#histogram of median daily transition zone length in km
p3 <- combo_long %>% 
  filter(dataset=="B2") %>% 
  ggplot(aes(x=v_daily_mean/K600_mn*86400/1000)) + 
  geom_histogram(col="black", fill="grey30") +
  theme_bw(base_size=16) +
  xlab("Mean daily transition zone length (km)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))



#fit linear model, use immediately below to annotation graphic
fit_4_data_subset <- combo_long %>% filter(dataset=="B2") %>% mutate(q=q_daily_mean*0.0283168) %>% mutate(t=3*v_daily_mean/K600_mn*86400/1000) %>%
  mutate(K600_mn_per_second = K600_mn / 86400) %>%
  mutate(Dk=reachlength_daily_mean*K600_mn_per_second/v_daily_mean) %>%
  select(q, t, Dk, date, K600_mn_per_second, dataset)
fit_4 <- lm(t~q, data=fit_4_data_subset)
fit_4_intercept <- coef(fit_4)[1]
fit_4_slope <- coef(fit_4)[2]
fit_4_r2 <- summary(fit_4)$r.squared

#now plot transition zone ~ Q
p4 <- combo_long %>%
  filter(dataset=="B2") %>% 
  ggplot(aes(x=q_daily_mean*0.0283168, y=3*v_daily_mean/K600_mn*86400/1000)) +
  geom_point(shape=21, col=1, fill="grey50") +
  theme_bw(base_size=16) +
  ylab("Mean daily transition zone length\n(km)") +
  xlab("Mean daily discharge (cms)") +
  # stat_smooth(method="lm", col=1) +
  geom_abline(intercept = fit_4_intercept, slope = fit_4_slope, linewidth=2, col=1) +
  annotate("text",  x=200, y = 360, label = paste0("slope=", signif(fit_4_slope, 3)), size=4, hjust = 0) +
  annotate("text",  x=200, y = 338, label = bquote(R^2 == .(signif(fit_4_r2, 3))), size=4, hjust = 0)


#now plot Dk~Q
fit_4_data_subset <- combo_long %>% filter(dataset=="B2") %>% mutate(q=q_daily_mean*0.0283168) %>% mutate(t=3*v_daily_mean/K600_mn*86400/1000) %>%
  mutate(K600_mn_per_second = K600_mn / 86400) %>%
  mutate(Dk=reachlength_daily_mean*K600_mn_per_second/v_daily_mean) %>%
  select(q, t, Dk, date, K600_mn_per_second, dataset)
fit_4 <- lm(Dk~q, data=fit_4_data_subset)
fit_4_intercept <- coef(fit_4)[1]
fit_4_slope <- coef(fit_4)[2]
fit_4_r2 <- summary(fit_4)$r.squared

p4 <- fit_4_data_subset %>% 
  filter(dataset=="B2") %>% 
  ggplot(aes(x=q, y=Dk)) + 
  geom_point(shape=21, col=1, fill="grey50") +
  theme_bw(base_size=16) +
  ylab("Damkohler number") +
  xlab("Mean daily discharge (cms)") +
  # stat_smooth(method="lm", col=1) +
  geom_abline(intercept = fit_4_intercept, slope = fit_4_slope, linewidth=2, col=1) +
annotate("text",  x=500, y = 0.60, label = paste0("slope=", signif(fit_4_slope, 3)), size=4, hjust = 0) +
annotate("text",  x=500, y = 0.55, label = bquote(R^2 == .(signif(fit_4_r2, 3))), size=4, hjust = 0)

#combine plots and export
p <- p1 / p2 / p4
ggsave("./graphics/FIG_S3_Damkohler_plots.png", p, height=1.5*6.5, width=1.5*3.5, units="in")


#FIGURE S4: Parameter 1:1 plots for all thre VFTS-MT runs (10th, 50th and 90th percentiles of Q)
fit_1a <- lm(B3_GPP_mn ~ 0 + payn_GPP_mn, data=combo)
fit_1a_slope <- coef(fit_1a)[1]
fit_1a_r2 <- summary(fit_1a)$r.squared
p1a <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_GPP_mn, y = B3_GPP_mn), color = "grey40", size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth=2, col=1, linetype="dashed") +
  geom_abline(intercept = 0, slope = fit_1a_slope, linewidth=2, col=1) +
  geom_abline(intercept = 0, slope = fit_1a_slope, linewidth=1.5, col=1) +
  theme_bw(base_size = 16) +
  xlab("GPP (CMH)") +
  ylab("GPP (VFTS-50)") +
  scale_x_continuous(limits=c(0,20)) +
  scale_y_continuous(limits=c(0,30)) +
  guides(fill="none", col="none", shape="none") +
  annotate("text",  x=1, y = 30, label = paste0("slope=", signif(fit_1a_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=1, y = 27.5, label = bquote(R^2 == .(signif(fit_1a_r2, 3))), size=5, hjust = 0)
p1a

fit_1b <- lm(B3b_GPP_mn ~ 0 + payn_GPP_mn, data=combo)
fit_1b_slope <- coef(fit_1b)[1]
fit_1b_r2 <- summary(fit_1b)$r.squared
p1b <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_GPP_mn, y = B3b_GPP_mn), color = "grey40", size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth=2, col=1, linetype="dashed") +
  geom_abline(intercept = 0, slope = fit_1b_slope, linewidth=2, col=1) +
  geom_abline(intercept = 0, slope = fit_1b_slope, linewidth=1.5, col=1) +
  theme_bw(base_size = 16) +
  xlab("GPP (CMH)") +
  ylab("GPP (VFTS-10)") +
  scale_x_continuous(limits=c(0,20)) +
  scale_y_continuous(limits=c(0,30)) +
  guides(fill="none", col="none", shape="none") +
  annotate("text",  x=1, y = 30, label = paste0("slope=", signif(fit_1b_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=1, y = 27.5, label = bquote(R^2 == .(signif(fit_1b_r2, 3))), size=5, hjust = 0)
p1b

fit_1c <- lm(B3c_GPP_mn ~ 0 + payn_GPP_mn, data=combo)
fit_1c_slope <- coef(fit_1c)[1]
fit_1c_r2 <- summary(fit_1c)$r.squared
p1c <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_GPP_mn, y = B3c_GPP_mn), color = "grey40", size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth=2, col=1, linetype="dashed") +
  geom_abline(intercept = 0, slope = fit_1c_slope, linewidth=2, col=1) +
  geom_abline(intercept = 0, slope = fit_1c_slope, linewidth=1.5, col=1) +
  theme_bw(base_size = 16) +
  xlab("GPP (CMH)") +
  ylab("GPP (VFTS-90)") +
  guides(fill="none", col="none", shape="none") +
  scale_x_continuous(limits=c(0,20)) +
  scale_y_continuous(limits=c(0,30)) +
  annotate("text",  x=1, y = 30, label = paste0("slope=", signif(fit_1c_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=1, y = 27.5, label = bquote(R^2 == .(signif(fit_1c_r2, 3))), size=5, hjust = 0)
p1c


fit_1d <- lm(B3_ER_mn ~ 0 + payn_ER_mn, data=combo)
fit_1d_slope <- coef(fit_1d)[1]
fit_1d_r2 <- summary(fit_1d)$r.squared
p1d <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_ER_mn, y = B3_ER_mn), color = "grey40", size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth=2, col=1, linetype="dashed") +
  geom_abline(intercept = 0, slope = fit_1d_slope, linewidth=2, col=1) +
  geom_abline(intercept = 0, slope = fit_1d_slope, linewidth=1.5, col=1) +
  theme_bw(base_size = 16) +
  xlab("ER (CMH)") +
  ylab("ER (VFTS-50)") +
  scale_x_continuous(limits=c(-40,0)) +
  scale_y_continuous(limits=c(-45,5)) +
  guides(fill="none", col="none", shape="none") +
  annotate("text",  x=-38, y = 5, label = paste0("slope=", signif(fit_1d_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=-38, y = 1, label = bquote(R^2 == .(signif(fit_1d_r2, 3))), size=5, hjust = 0)
p1d

fit_1e <- lm(B3b_ER_mn ~ 0 + payn_ER_mn, data=combo)
fit_1e_slope <- coef(fit_1e)[1]
fit_1e_r2 <- summary(fit_1e)$r.squared
p1e <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_ER_mn, y = B3b_ER_mn), color = "grey40", size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth=2, col=1, linetype="dashed") +
  geom_abline(intercept = 0, slope = fit_1e_slope, linewidth=2, col=1) +
  geom_abline(intercept = 0, slope = fit_1e_slope, linewidth=1.5, col=1) +
  theme_bw(base_size = 16) +
  xlab("ER (CMH)") +
  ylab("ER (VFTS-10)") +
  scale_x_continuous(limits=c(-40,0)) +
  scale_y_continuous(limits=c(-45,5)) +
  guides(fill="none", col="none", shape="none") +
  annotate("text",  x=-38, y = 5, label = paste0("slope=", signif(fit_1e_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=-38, y = 1, label = bquote(R^2 == .(signif(fit_1e_r2, 3))), size=5, hjust = 0)
p1e

fit_1f <- lm(B3c_ER_mn ~ 0 + payn_ER_mn, data=combo)
fit_1f_slope <- coef(fit_1f)[1]
fit_1f_r2 <- summary(fit_1f)$r.squared
p1f <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_ER_mn, y = B3c_ER_mn), color = "grey40", size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth=2, col=1, linetype="dashed") +
  geom_abline(intercept = 0, slope = fit_1f_slope, linewidth=2, col=1) +
  geom_abline(intercept = 0, slope = fit_1f_slope, linewidth=1.5, col=1) +
  theme_bw(base_size = 16) +
  xlab("ER (CMH)") +
  ylab("ER (VFTS-90)") +
  guides(fill="none", col="none", shape="none") +
  scale_x_continuous(limits=c(-40,0)) +
  scale_y_continuous(limits=c(-45,5)) +
  annotate("text",  x=-38, y = 5, label = paste0("slope=", signif(fit_1f_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=-38, y = 1, label = bquote(R^2 == .(signif(fit_1f_r2, 3))), size=5, hjust = 0)
p1f


fit_1g <- lm(B3_K600_mn ~ 0 + payn_K600_mn, data=combo)
fit_1g_slope <- coef(fit_1g)[1]
fit_1g_r2 <- summary(fit_1g)$r.squared
p1g <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_K600_mn, y = B3_K600_mn), color = "grey40", size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth=2, col=1, linetype="dashed") +
  geom_abline(intercept = 0, slope = fit_1g_slope, linewidth=2, col=1) +
  geom_abline(intercept = 0, slope = fit_1g_slope, linewidth=1.5, col=1) +
  theme_bw(base_size = 16) +
  xlab("K600 (CMH)") +
  ylab("K600 (VFTS-50)") +
  scale_x_continuous(limits=c(0,2.5)) +
  scale_y_continuous(limits=c(0,2.5)) +
  guides(fill="none", col="none", shape="none") +
  annotate("text",  x=0.1, y = 2.4, label = paste0("slope=", signif(fit_1g_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=0.1, y = 2.2, label = bquote(R^2 == .(signif(fit_1g_r2, 3))), size=5, hjust = 0)
p1g

fit_1h <- lm(B3b_K600_mn ~ 0 + payn_K600_mn, data=combo)
fit_1h_slope <- coef(fit_1h)[1]
fit_1h_r2 <- summary(fit_1h)$r.squared
p1h <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_K600_mn, y = B3b_K600_mn), color = "grey40", size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth=2, col=1, linetype="dashed") +
  geom_abline(intercept = 0, slope = fit_1h_slope, linewidth=2, col=1) +
  geom_abline(intercept = 0, slope = fit_1h_slope, linewidth=1.5, col=1) +
  theme_bw(base_size = 16) +
  xlab("K600 (CMH)") +
  ylab("K600 (VFTS-10)") +
  scale_x_continuous(limits=c(0,2.5)) +
  scale_y_continuous(limits=c(0,2.5)) +
  guides(fill="none", col="none", shape="none") +
  annotate("text",  x=0.1, y = 2.4, label = paste0("slope=", signif(fit_1h_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=0.1, y = 2.2, label = bquote(R^2 == .(signif(fit_1h_r2, 3))), size=5, hjust = 0)
p1h

fit_1i <- lm(B3c_K600_mn ~ 0 + payn_K600_mn, data=combo)
fit_1i_slope <- coef(fit_1i)[1]
fit_1i_r2 <- summary(fit_1i)$r.squared
p1i <- combo %>%
  ggplot() +
  geom_point(aes(x = payn_K600_mn, y = B3c_K600_mn), color = "grey40", size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth=2, col=1, linetype="dashed") +
  geom_abline(intercept = 0, slope = fit_1i_slope, linewidth=2, col=1) +
  geom_abline(intercept = 0, slope = fit_1i_slope, linewidth=1.5, col=1) +
  theme_bw(base_size = 16) +
  xlab("K600 (CMH)") +
  ylab("K600 (VFTS-90)") +
  guides(fill="none", col="none", shape="none") +
  scale_x_continuous(limits=c(0,2.5)) +
  scale_y_continuous(limits=c(0,2.5)) +
  annotate("text",  x=0.1, y = 2.4, label = paste0("slope=", signif(fit_1i_slope, 3)), size=5, hjust = 0) +
  annotate("text",  x=0.1, y = 2.2, label = bquote(R^2 == .(signif(fit_1i_r2, 3))), size=5, hjust = 0)
p1i

#combine and export
p <- (p1b + p1a + p1c) / (p1e + p1d + p1f) / (p1h + p1g + p1i)
p <- (p1b + p1e + p1h) / (p1a + p1d + p1i) / (p1c + p1f + p1i)
ggsave("./graphics/FIG_S4_VFts-MT_1to1_comparison.png", p, height=2*6.5, width=2*6.5, units="in")

