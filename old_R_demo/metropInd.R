######
# Independence Metropolis sampler method demo in R
# 2011, Richard D. Morey (richarddmorey@gmail.com)
# Please do not distribute without permission
######
# Instructions: Change settings to suit, save, then source() this file
# Solid curve is the PDF to sample from
# Blue sample is current sample
# Dashed red curve is candidate distribution
# Red sample is candidate
# Red curve shows kernel density of samples (should converge to PDF)
# Acceptence rate is shown above

# Settings
##


# Number of seconds to wait between steps
# if "key", wait for key press
wait.type = "key"

# Number of samples
# Set this low (20 or so if using wait.type="key")
M = 20

# logarithm of density to sample from
log.dens = function(x)
  dexp(x,1,log=TRUE)


# Make sure next two functions are consistent!
# PDF of candidate 
cand.dens=function(x)
  (x>0)*dnorm(x,.5,1) # truncated normal

# Function to sample from the candidate
rcand = function()
  qnorm(runif(1,pnorm(0,.5,1),1),.5,1) # Truncated normal using inverse CDF method


start = 1 # Algorithm starting value (first 'sample')

# plot x limits
limits = c(0,6)





# No need to change anything below this line!
#################
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
scalecand = 0.5*maxy/max(cand.dens(plotx))
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

  lines(plotx,cand.dens(plotx)*scalecand,lty=2,col=rgb(1,0,0,1))
  cand = rcand()

  wait(wait.type)

  segments(cand,0,cand,exp(log.dens(cand)),col=rgb(1,0,0,1),lwd=2)
  points(cand,exp(log.dens(cand)),pch=21,bg=rgb(1,0,0,.3))

  wait(wait.type)

  r = exp(log.dens(cand))/exp(log.dens(samps[m-1])) * cand.dens(samps[m-1])/cand.dens(cand)
  
  b=runif(1,0,1)

  if(b<r){
    mtext("Accept Candidate",3,.1,adj=1,cex=1.5)
    samps[m] = cand
  }else{
    mtext("Reject Candidate",3,.1,adj=1,cex=1.5) 
    samps[m] = samps[m-1]
  }

  wait(wait.type)

}

my.dens = density(samps[1:m])
max.dens = max(my.dens$y)
lines(my.dens$x,my.dens$y/max.dens*scalehist,col=rgb(1,0,0,1))

# Final plot

wait(wait.type)

par(mfrow=c(1,2))
plot(samps,ty='l', main="MCMC chain",ylab="Sample",xlab="Iteration")
acf(samps,main="Sample autocorrelation")

wait(wait.type)

# Compare to true distribution
par(mfrow=c(1,1))
# change the distribution dexp here if you change the target distribution
hist(samps,freq=FALSE)
lines(plotx,dexp(plotx),col="red")

