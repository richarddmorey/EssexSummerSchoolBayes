data { 
  int<lower=0> n; // length
  vector[n] neocortex_perc;
  vector[n] log_mass;	
  vector[n] kcal_per_g;
} 
parameters { 
  real a; // intercept
  real bn; // slope for neocortex percentage 
  real bm; // slope for mass
  real<lower=0> sigma; // error standard deviation
} 
model { 

  kcal_per_g ~ normal(a + bn * neocortex_perc, sigma);
//  kcal_per_g ~ normal(a + bm * log_mass, sigma);
//  kcal_per_g ~ normal(a + bn * neocortex_perc + bm * log_mass, sigma);

  a ~ normal(0, 100);
  bn ~ normal(0, 1);
  bm ~ normal(0, 1);
  sigma ~ uniform(0, 1);

}
