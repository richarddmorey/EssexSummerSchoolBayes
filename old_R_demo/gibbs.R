
######
# Gibbs sampler demo in R
# 2011, Richard D. Morey (richarddmorey@gmail.com)
# Please do not distribute without permission
######
# Instructions: Change settings to suit, save, then source() this file
# Shows bivariate posterior distribution as a countour plot
# overlays Gibbs samples over the contour
# light blue lines represent samples from full conditional
# red points represent resulting samples from joint

# Model
# p(y|\mu,\sigma^2) \sim Normal(\mu, \sigma^2)
# priors are:
# p(\mu) \propto 1 (flat)
# p(\sigma^2) \propto 1/\sigma^2 (flat on log sigma^2)


# Settings
##

# Should sampling from the conditional distributions be shown?
show.conditionals = TRUE

# Number of seconds to wait between steps
# if "key", wait for key press
wait.type = "key"

# Number of MCMC iterations
# Set this low (20 or so if using wait.type="key")
M = 20

# Random data setup
N = 100      # sample size
mu = 0       # normal mean
sig2 = 1     # normal variance


############
# You shouldn't have to change anything below this line
############
library(MCMCpack)

wait=function(type="key")
{
  if(type=="key" & interactive()){
      readline()
    }else if(is.numeric(type)){
      Sys.sleep(type)
    }else{
      stop("Invalid argument to wait().")
    }
invisible()
}

par(mfrow=c(1,1))

# sample data
y = rnorm(N,mu,sqrt(sig2))

# reserve vectors for MCMC chain
mu.chain = 1:M * 0
sig2.chain = 1:M * 0 + 1

# Statistics
ybar = mean(y)
s2 = var(y)
sderr = sd(y)/sqrt(N)
alpha = 0.01

# for estimating limits of plot and plotting the joint density
mus = seq(ybar-qt(1-alpha/2,N-1)*sderr,ybar+qt(1-alpha/2,N-1)*sderr,len=20)
sig2s = seq((N-1)*s2/qchisq(1-alpha/2,N-1),(N-1)*s2/qchisq(alpha/2,N-1),len=20)

# true joint posterior density (for contour plot)
log.joint.post = function(mu,sig2,y,N=length(y)){
  exp(sum(dnorm(y,mu,sqrt(sig2),log=TRUE)) - log(sig2))
}

log.joint.v = Vectorize(log.joint.post,c("mu","sig2"))

log.dens = outer(mus,sig2s,log.joint.v,y=y,N=N)
contour(mus,sig2s,log.dens,xlab="mu",ylab="sigma^2")
abline(v=ybar,col="gray")
abline(h=s2,col="gray")


# Start MCMC
for(m in 2:M)
{  
  
  wait(wait.type)
  contour(mus,sig2s,log.dens,xlab="mu",ylab="sigma^2")
  lines(mu.chain[1:(m-1)],sig2.chain[1:(m-1)],col=rgb(0,0,0,.5))
  points(mu.chain[1:(m-1)],sig2.chain[1:(m-1)],pch=21,bg=rgb(1,0,0,.3),col=rgb(1,1,1,0))
    
  abline(h=sig2.chain[m-1],col=rgb(0,0,1,.3))

  wait(wait.type)

  if(show.conditionals){
    cond.mus = seq(min(mus),max(mus),len=100)
    plot(cond.mus,dnorm(cond.mus,ybar,sqrt(sig2.chain[m-1]/N)),ty='l')
  }
  mu.chain[m] = rnorm(1,ybar,sqrt(sig2.chain[m-1]/N))  

  if(show.conditionals){
    wait(wait.type)
    abline(v=mu.chain[m],col=rgb(0,0,1,.3))
    wait(wait.type)
  }
  contour(mus,sig2s,log.dens,xlab="mu",ylab="sigma^2")
  lines(mu.chain[1:(m-1)],sig2.chain[1:(m-1)],col=rgb(0,0,0,.5))
  points(mu.chain[1:(m-1)],sig2.chain[1:(m-1)],pch=21,bg=rgb(1,0,0,.3),col=rgb(1,1,1,0))
  
  abline(v=mu.chain[m],col=rgb(0,0,1,.3))

  if(show.conditionals)
  {
    wait(wait.type)
    cond.sig2s = seq(min(sig2s),max(sig2s),len=100)
    plot(cond.sig2s,dinvgamma(cond.sig2s,N/2,.5*sum((y-mu.chain[m])^2)),ty='l')
  }
  sig2.chain[m] = rinvgamma(1,N/2,.5*sum((y-mu.chain[m])^2))  

  if(show.conditionals){
    wait(wait.type)
    abline(v=sig2.chain[m],col=rgb(0,0,1,.3))
  }
}

# Plot final results
contour(mus,sig2s,log.dens,xlab="mu",ylab="sigma^2")
lines(mu.chain[1:m],sig2.chain[1:m],col=rgb(0,0,0,.5))
points(mu.chain[1:m],sig2.chain[1:m],pch=21,bg=rgb(1,0,0,.3),col=rgb(1,1,1,0))

wait(wait.type)

par(mfrow=c(1,2))
plot(mu.chain,ty='l',main="Mean mu chain")
abline(h=ybar,col="gray")
plot(sig2.chain,ty='l', main="Variance sig2 chain")
abline(h=s2,col="gray")

#######################


