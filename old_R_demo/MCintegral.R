######
# Monte Carlo integration method demo in R
# 2011, Richard D. Morey (richarddmorey@gmail.com)
# Please do not distribute without permission
######
# Instructions: Change settings to suit, save, then source() this file
# Estiamtes should converge to the horizontal gray line

# for inverse gamma function
library(MCMCpack)

# Function to integrate
intFunc = function(x)
  dinvgamma(x,1,1)

a = 0           # Lower limit of integration
b = 10          # Upper limit of integration
M = 10000       # number of MC iterations (more = more accurate)
ci.alpha = 0.05 # alpha for CI on integral (assuming estimate has variance....)

# Gaussian quadrature estimate of integral (highly accurate, for checking)
trueIntegral= integrate(intFunc,a,b)


u = runif(M,a,b)  # Sample uniform
cum.ests=(b-a)*cumsum(intFunc(u))/(1:M) # Cumulative means
all.est = (b-a)*mean(intFunc(u))        # Final estimate

# Estimate of error 
sderr=(b-a)*sd(intFunc(u))/sqrt(M)

# Plot estimate of integrate by iteration number (should converge to line)
plot(1:M,cum.ests,t='l',log="x",xlab="Iteration",ylab="Estimate of integral")
abline(h=trueIntegral[[1]],col="gray")

# Plot 95% error lines
CI = all.est + c(-1,1)*qnorm(1-ci.alpha/2)*sderr
abline(h=CI,col="blue",lty=2)

# Show proportional error on right axis
axis(4,at=seq(min(cum.ests),max(cum.ests),len=5),lab=round(seq(min(cum.ests),max(cum.ests),len=5)/trueIntegral[[1]],2))
