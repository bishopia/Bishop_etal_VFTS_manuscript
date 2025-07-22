
data {
  int<lower=1> n;    // number of total do observations
  int<lower=1> d;    // number of days  
  vector [d] DO_obs_up [n];
  vector [d] DO_sat_up [n];
  vector [d] DO_obs_down [n];
  vector [d] DO_sat_down [n];
  vector [d] totlight [n];
  vector [d] depth [n];
  vector [d] temp [n];
  vector [d] tt [n];
}

parameters {
  vector <lower=0>[d] GPP;
  vector [d] ER;
  vector <lower=0>[d] k600;
  real <lower=0> sigma;
}

transformed parameters {
  vector [d] metab [n];
  vector [d] KO2 [n];
  for (i in 1:n){
    for (t in 1:d){
      KO2[i,t] = (k600[t] / depth[i,t]) / ((600 / (1800.6 - (temp[i,t] * 120.1) + (3.7818 * temp[i,t]^2) - (0.047608 * temp[i,t]^3)))^-0.5);
    }
    metab[i] = (DO_obs_up[i] + GPP .* totlight[i] ./ depth[i] + ER .* tt[i] ./ depth[i] + (KO2[i] .* tt[i] .* (DO_sat_up[i] - DO_obs_up[i] + DO_sat_down[i]) / 2)) ./ (1 + (KO2[i] .* tt[i]) / 2);
  }
}

model {
  for (i in 1:n){
    DO_obs_down[i] ~ normal(metab[i], sigma);
  }
  for (i in 1:d){
    k600[i] ~ normal(3.48, 0.552);
  }
}

