#clean the environment
rm(list = ls())

#setwd
setwd("C:/Users/ibishop/OneDrive - DOI/science/gc_metabolism/eda_twostation/tt_estimation/")

#load libraries
library(lubridate)
library(tidyverse)
library(zoo)
library(patchwork)
library(broom)

###########################################################
###---PREPROCESS DISCHARGE AND RELATE TO TRAVEL TIMES---###
###########################################################

#load up travel times
traveltimes <- read_csv("./traveltimes_20240410.csv")

#unit change
traveltimes <- traveltimes %>% mutate(q=q*0.0283168, moving_slope=moving_slope*0.0283168)

#add variable to designate whether discharge is on the rise or falling
traveltimes <- traveltimes %>% mutate(q_state=if_else(moving_slope>0, "rising", "falling"))

#drop all NAs
traveltimes <- traveltimes %>% 
  drop_na()

#fit log(tt)~log(q) + moving_slope
lm4 <- lm(log(tt)~log(q)+moving_slope, data = traveltimes)
summary(lm4)

#get mean slope
mean_moving_slope <- mean(traveltimes$moving_slope, na.rm = TRUE)

#make df for predictions
prediction_data <- data.frame(
  q = seq(min(traveltimes$q, na.rm = TRUE), max(traveltimes$q, na.rm = TRUE), length.out = 100),
  moving_slope = rep(mean_moving_slope, 100)
)

#add predicted values using the model but with mean moving_slope for all predictions
prediction_data$predicted_log_tt <- predict(lm4, newdata = prediction_data)
prediction_data$predicted_tt <- exp(prediction_data$predicted_log_tt)

#plot data and model
ggplot(traveltimes, aes(x = q, y = tt)) +
  geom_point(color = "black") +  # Actual data points
  geom_line(data = prediction_data, aes(x = q, y = predicted_tt), color = "darkred", size = 1) +
  labs(x = "Q", y = "tt") +
  theme_minimal()

#add predicted values to the data frame
df <- traveltimes %>%
  mutate(predicted_log_tt = predict(lm4, traveltimes),
         predicted_tt = exp(predicted_log_tt)) %>%
  mutate(residuals = log(tt) - predicted_log_tt)

#check residual~moving_slope, should be flat now
ggplot(df, aes(x = moving_slope, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Residuals vs. Moving Slope", x = "Moving Slope", y = "Residuals") +
  theme_minimal() +
  stat_smooth(method = "lm", color = "blue")

#summary
summary_lm4 <- summary(lm4)
r_squared <- summary_lm4$r.squared
sigma <- summary_lm4$sigma
aic <- AIC(lm4)
cat("R-squared:", r_squared, "\n",
    "Sigma (Residual Standard Error):", sigma)

#parameters
coef(lm4)


#fit estiamtes to be used for estimating travel time in DO time series
ttq_intercept <- 4.73797225
ttq_logq <- -0.49289591
ttq_movingslope <- -0.01908137

#plot data and model
p1 <- ggplot(traveltimes, aes(x = q, y = tt)) +
  geom_point(aes(col = moving_slope)) +  # Actual data points
  geom_line(data = prediction_data, aes(x = q, y = predicted_tt), color = 1, size = 1.5) +
  labs(x = "Discharge (cms)", y = "Travel time (hours)") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks=seq(100,700,100)) +
  labs(col="dQ/dt") +
  theme(legend.position = c(0.9, 0.9),  # Move legend inside the plot area
        legend.justification = c("right", "top"),
        legend.key = element_rect(fill = "white", color = "black"),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5))

tmp1 <- read_csv("../../provided_data/twostation_oxy_cleaned/Compiled_Bolt_20240606_cleanIWB_tentative.csv") %>%
  rename(datetime=`Date Time`) %>%
  mutate(date=date(datetime)) %>% 
  filter(date %in% c(ymd("2012-08-02"), ymd("2012-08-03")))  %>%
  select(datetime, boltSP=SpCond)
tmp2 <- read_csv("../../provided_data/twostation_oxy_cleaned/Compiled_Buoy_20240606_cleanIWB_tentative.csv") %>%
  rename(datetime=`Date Time`) %>%
  mutate(date=date(datetime)) %>% 
  filter(date %in% c(ymd("2012-08-02"), ymd("2012-08-03")))  %>%
  filter(date %in% c(ymd("2012-08-02"), ymd("2012-08-03")))  %>%
  select(datetime, buoySP=SpCond)
tmp3 <- tmp1 %>% left_join(tmp2, by="datetime")

p2 <- tmp3 %>%
  mutate(time = hour(datetime) + minute(datetime) / 60) %>%
  mutate(date=date(datetime)) %>%
  ggplot() + 
  geom_line(aes(x=datetime, y=boltSP, group=date), col="black", size=1.5, linetype="dashed") +
  geom_line(aes(x=datetime, y=buoySP, group=date), col="black", size=1.5) +
  theme_bw(base_size=16) +
  scale_x_datetime(date_labels="%H", date_breaks="6 hours", expand=c(0,0)) +
  ylab("Specific conductivity (uS)") + 
  xlab("Hour of day") +
  ylab(expression(paste("Specific conductivity (", mu, "S/cm)")))
p <-  p2 + p1

ggsave("./FIGURE2_tt~q.png", p, width=1.5*6.5, height=1.5*3.25, units="in")
 











# 
# p1 <- ggplot(traveltimes, aes(x = log(q), y = log(tt))) +
#   geom_point(color = "grey50") +  # Actual data points
#   geom_line(data = prediction_data, aes(x = log(q), y = log(predicted_tt)), color = 1, size = 1) +
#   labs(x = "discharge (cms)", y = "travel time (hours)") +
#   theme_bw(base_size = 16) +
#   scale_x_continuous(breaks=seq(100,700,100)) +
#   annotate("text",  x=600, y = 15, label = paste0("Intercept=", signif(ttq_intercept, 2)), size=5, hjust = 0) +
#   annotate("text",  x=600, y = 14.25, label = paste0("slope=", signif(ttq_logq, 2)), size=5, hjust = 0) +
#   annotate("text",  x=600, y = 13.5, label = paste0("R2=", signif(r_squared, 2)), size=5, hjust = 0)
# ggsave("./FIGURE2_tt~q.png", p1, width=6.5, height=4.5, units="in")
# 
# 
# 

# Generate the fitted travel time line data
line_data <- traveltimes %>% 
  mutate(predicted_tt = exp(ttq_intercept + (ttq_logq * log(q) + ttq_movingslope * moving_slope)))

# Plot with both points and the fitted line
traveltimes %>% 
  ggplot(aes(x = q, y = tt)) + 
  geom_point() + 
  geom_line(data = line_data, aes(y = predicted_tt), color = "blue", linewidth = 1) + 
  theme_bw(base_size = 16) + 
  xlab("Discharge (cfs)") + 
  ylab("Travel time (hours)")
  
  





lm_quad <- lm(log(tt) ~ log(q) + I(log(q)^2) + moving_slope, 
              data = traveltimes)
# Get mean slope
mean_moving_slope <- mean(traveltimes$moving_slope, na.rm = TRUE)

# Build a sequence of q from min to max
prediction_data <- data.frame(
  q = seq(
    min(traveltimes$q, na.rm = TRUE), 
    max(traveltimes$q, na.rm = TRUE), 
    length.out = 100
  ),
  moving_slope = rep(mean_moving_slope, 100)
)
prediction_data$predicted_log_tt <- predict(lm_quad, newdata = prediction_data)
prediction_data$predicted_tt <- exp(prediction_data$predicted_log_tt)

ggplot(traveltimes, aes(x = q, y = tt)) +
  geom_point(color = "black", alpha = 0.7) +  # scatter of actual data
  geom_line(
    data = prediction_data,
    aes(x = q, y = predicted_tt),
    color = "darkred", size = 1
  ) +
  labs(
    title = "Quadratic Model with Mean Moving Slope",
    x = "q",
    y = "Travel Time (tt)"
  ) +
  theme_minimal()

lm_quad_ortho <- lm(log(tt) ~ poly(log(q), 2) + moving_slope, data = traveltimes)
prediction_data$predicted_log_tt <- predict(lm_quad_ortho, newdata = prediction_data)
prediction_data$predicted_tt <- exp(prediction_data$predicted_log_tt)

ggplot(traveltimes, aes(x = q, y = tt)) +
  geom_point(color = "black", alpha = 0.7) +  # scatter of actual data
  geom_line(
    data = prediction_data,
    aes(x = q, y = predicted_tt),
    color = "darkred", size = 1
  ) +
  labs(
    title = "Quadratic Model with Mean Moving Slope",
    x = "q",
    y = "Travel Time (tt)"
  ) +
  theme_minimal()


lm_cubic <- lm(log(tt) ~ poly(log(q), 3) + moving_slope, data = traveltimes)
# Get mean slope
mean_moving_slope <- mean(traveltimes$moving_slope, na.rm = TRUE)

# Build a sequence of q from min to max
prediction_data <- data.frame(
  q = seq(
    min(traveltimes$q, na.rm = TRUE), 
    max(traveltimes$q, na.rm = TRUE), 
    length.out = 100
  ),
  moving_slope = rep(mean_moving_slope, 100)
)
prediction_data$predicted_log_tt <- predict(lm_cubic, newdata = prediction_data)
prediction_data$predicted_tt <- exp(prediction_data$predicted_log_tt)

ggplot(traveltimes, aes(x = q, y = tt)) +
  geom_point(color = "black", alpha = 0.7) +  # scatter of actual data
  geom_line(
    data = prediction_data,
    aes(x = q, y = predicted_tt),
    color = "darkred", size = 1
  ) +
  labs(
    x = "Q",
    y = "tt"
  ) +
  theme_minimal()



library(splines)
lm_spline <- lm(log(tt) ~ ns(log(q), df=4) + moving_slope, data = traveltimes)

# Get mean slope
mean_moving_slope <- mean(traveltimes$moving_slope, na.rm = TRUE)

# Build a sequence of q from min to max
prediction_data <- data.frame(
  q = seq(
    min(traveltimes$q, na.rm = TRUE), 
    max(traveltimes$q, na.rm = TRUE), 
    length.out = 100
  ),
  moving_slope = rep(mean_moving_slope, 100)
)
prediction_data$predicted_log_tt <- predict(lm_spline, newdata = prediction_data)
prediction_data$predicted_tt <- exp(prediction_data$predicted_log_tt)

ggplot(traveltimes, aes(x = q, y = tt)) +
  geom_point(color = "black", alpha = 0.7) +  # scatter of actual data
  geom_line(
    data = prediction_data,
    aes(x = q, y = predicted_tt),
    color = "darkred", size = 1
  ) +
  labs(
    title = "Spline Model with Mean Moving Slope",
    x = "q",
    y = "Travel Time (tt)"
  ) +
  theme_minimal()

coef(lm_cubic)


AIC(lm4)
AIC(lm_cubic)
AIC(lm_spline)
aic <- AIC(lm4)
aic <- AIC(lm4)


discharge <- read_csv("./discharge_20080301-20231231.csv") %>% select(datetime, q, moving_slope)

payn <- read_csv("../../provided_data/payn_model_output/summary_travel_times.csv") %>% 
  rename(datetime=time_MST, tt=traveltime) %>%
  left_join(discharge, by="datetime") %>% 
  drop_na()
  
  
lm_cubic <- lm(log(tt) ~ poly(log(q), 3), data = payn)
  # Get mean slope
mean_moving_slope <- mean(payn$moving_slope, na.rm = TRUE)
  
  # Build a sequence of q from min to max
  prediction_data <- data.frame(
    q = seq(
      min(payn$q, na.rm = TRUE), 
      max(payn$q, na.rm = TRUE), 
      length.out = 100
    ),
    moving_slope = rep(mean_moving_slope, 100)
  )
  prediction_data$predicted_log_tt <- predict(lm_cubic, newdata = prediction_data)
  prediction_data$predicted_tt <- exp(prediction_data$predicted_log_tt)
  
ggplot(payn, aes(x = q, y = tt)) +
  # geom_point()
  geom_point(color = "black", alpha = 0.7) +  # scatter of actual data
  geom_line(
    data = prediction_data,
    aes(x = q, y = predicted_tt),
    color = "darkred", size = 1
  ) +
  labs(
    x = "q",
    y = "Travel Time (tt)"
  ) +
  theme_minimal()


lm4 <- lm(log(tt)~log(q)+moving_slope, data = payn)
# Get mean slope
mean_moving_slope <- mean(payn$moving_slope, na.rm = TRUE)

# Build a sequence of q from min to max
prediction_data <- data.frame(
  q = seq(
    min(payn$q, na.rm = TRUE), 
    max(payn$q, na.rm = TRUE), 
    length.out = 100
  ),
  moving_slope = rep(mean_moving_slope, 100)
)
prediction_data$predicted_log_tt <- predict(lm4, newdata = prediction_data)
prediction_data$predicted_tt <- exp(prediction_data$predicted_log_tt)

ggplot(payn, aes(x = q, y = tt)) +
  # geom_point()
  geom_point(color = "black", alpha = 0.7) +  # scatter of actual data
  geom_line(
    data = prediction_data,
    aes(x = q, y = predicted_tt),
    color = "darkred", size = 1
  ) +
  labs(
    x = "q",
    y = "tt"
  ) +
  theme_minimal()

