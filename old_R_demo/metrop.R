######
# Random walk Metropolis-Hastings method demo in R
# 2011, Richard D. Morey (richarddmorey@gmail.com)
# Please do not distribute without permission
######
# Instructions: Change settings to suit, save, then source() this file
# Solid curve is the PDF to sample from
# Blue sample is current sample
# Dashed red curve is candidate distribution
# Red sample is candidate
# Horizontal green line is uniform sample between 0 and height of blue dot
# If horizontal line is lower than red dot, sample is accepted
# Otherwise, current sample (blue) is retained.
# Red curve shows kernel density of samples (should converge to PDF)
# Acceptence rate is shown above (shoot for 30% or so)

# Settings
##

# Number of seconds to wait between steps
# if "key", wait for key press
wait.type = "key"

# Number of samples
# Set this low (20 or so if using wait.type="key")
M = 10

# logarithm of density to sample from
log.dens = function(x)
  dchisq(x,3,log=TRUE)

start = 1 # Algorithm starting value (first 'sample')
sdmet=5   # Standard deviation of normal candidate distribution

# plot x limits
limits = c(0,15)



# No need to change anything below here
#########################

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

samps = 1:M*NA
samps[1] = start

plotx = seq(limits[1],limits[2],len=200)
maxy = max(exp(log.dens(plotx)))
scalecand = 0.5*maxy/max(dnorm(plotx,0,sdmet))
scalehist = 1*maxy

# Begin MCMC
for(m in 2:M){
  
  plot(plotx,exp(log.dens(plotx)),ty='l',lwd=2)
  abline(h=0,col="gray")

  if(m>2){
    my.dens = density(samps[1:(m-1)])
    max.dens = max(my.dens$y)
    lines(my.dens$x,my.dens$y/max.dens*scalehist,col=rgb(1,0,0,1))
    acc.rate = mean(diff(samps[1:(m-1)])!=0)
    mtext(paste(m,": ",100*round(acc.rate,2),"%",sep=""),3,.1,adj=0,cex=1.5)
  }

  segments(samps[m-1],0,samps[m-1],exp(log.dens(samps[m-1])),col=rgb(0,0,1,1),lwd=2)
  points(samps[m-1],exp(log.dens(samps[m-1])),pch=21,bg=rgb(0,0,1,.3))

  wait(wait.type)

  lines(plotx,dnorm(plotx,samps[m-1],sdmet)*scalecand,lty=2,col=rgb(1,0,0,1))
  cand = rnorm(1,samps[m-1],sdmet)

  wait(wait.type)

  segments(cand,0,cand,exp(log.dens(cand)),col=rgb(1,0,0,1),lwd=2)
  points(cand,exp(log.dens(cand)),pch=21,bg=rgb(1,0,0,.3))

  wait(wait.type)

  b=runif(1,0,exp(log.dens(samps[m-1])))
  abline(h=b,col=rgb(0,1,0,.5),lwd=2)

  wait(wait.type)

  if(b<exp(log.dens(cand))){
    mtext("Accept Candidate",3,.1,adj=1,cex=1.5)
    samps[m] = cand
  }else{
    mtext("Reject Candidate",3,.1,adj=1,cex=1.5) 
    samps[m] = samps[m-1]
  }

  wait(wait.type)

}


# Final plot
my.dens = density(samps[1:m])
max.dens = max(my.dens$y)
lines(my.dens$x,my.dens$y/max.dens*scalehist,col=rgb(1,0,0,1))

wait(wait.type)

par(mfrow=c(1,2))
plot(samps,ty='l', main="MCMC chain",ylab="Sample",xlab="Iteration")
acf(samps,main="Sample autocorrelation")
