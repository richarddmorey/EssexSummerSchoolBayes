data { 
  int<lower=0> n; // length
  vector[n] x;
  vector[n] y;	
  real x22;
} 
parameters { 
  real beta1; // intercept  
  real beta2; // slope
  real<lower=0> sigma; // error standard deviation
  real y22;
} 
model { 

  y ~ normal(beta1 + beta2 * x, sigma);
  y22 ~ normal(beta1 + beta2 * x22, sigma);

  beta1 ~ normal(0, 100);
  beta2 ~ normal(0, 100);
  sigma ~ uniform(0, 100);

}
