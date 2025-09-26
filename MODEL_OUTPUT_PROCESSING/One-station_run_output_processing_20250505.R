#clear workspace
rm(list = ls())

#setwd
setwd(...)

#load fitted one-station object
load("./osfit_no_chunking.RData")

#some more libraries
library(streamMetabolizer)

#runtime?
streamMetabolizer::get_elapsed_time(fit)

#get fit predictions
predictions <- streamMetabolizer::predict_DO(fit)

#write to file
write_csv(predictions, "./osfit_no_chunking_3kiter-mv58_20250505.csv")
