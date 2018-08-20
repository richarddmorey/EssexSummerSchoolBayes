// ChaSaSoon Censored Data
data { 
  int<lower=0> nfails;
  int<lower=0> n;
  int<lower=0> z_observed;
} 
parameters { 
  real<lower=.25,upper=1> theta;  // Uniform Prior on Rate Theta
} 
model { 
  // Observed Data
  z_observed ~ binomial(n, theta); 
  
  // Unobserved Data
  target += nfails * log(binomial_cdf(25, n, theta) 
                                  - binomial_cdf(14, n, theta));
}
