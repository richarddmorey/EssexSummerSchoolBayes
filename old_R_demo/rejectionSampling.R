######
# Rejection Sampling method demo in R
# 2011, Richard D. Morey (richarddmorey@gmail.com)
# Please do not distribute without permission
######
# Instructions: Change settings to suit, save, then source() this file
# Majorizing samples (blue) are transformed accepted/rejected
# Uniform is drawn from 0 to height blue dot
# Acceptance if horizontal line is less than PDF
# Dashed curve is PDF
# Red samples are corresponding samples from the PDF
# Red curve shows kernel density estimate based on samples (should converge to PDF)
# acceptance rate is shown above
# Settings
##

# Number of seconds to wait between steps
# if "key", wait for key press
wait.type = "key"

# Number of iterations
# Set this low (20 or so if using wait.type="key")
M = 20

# Function setup (make sure these are consistent!)
pdf = function(x) sin(pi*x)        # PDF to sample from
major = function(x) dunif(x)       # Majorizing function
majorRand = function() runif(1)    # Function to sample from majorizing function
const = 1                          # Majorizing constant

# (plot) limits to make plot nice 
xlims = c(0,1)
ylims = c(0,1.2)



# No need to change anything below this line
##################

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


xx = seq(xlims[1],xlims[2],len=200)
maxy = max(pdf(xx))




samps = NULL
cands = NULL

for(m in 1:M){

  
  nSamples = length(samps)
  
  plot(xx,pdf(xx),ty='l',lty=2,col="black",ylim=ylims)
  abline(h=0,col="gray")
  abline(h=ylims[2],col="gray")

  lines(xx,major(xx),lty=1,col="blue")

  if(m>1){
      points(cands,rep(ylims[2],m-1),bg=rgb(0,0,1,.3),col=rgb(0,0,0,0),pch=21)  
  }
  
  if(nSamples>0)
  {
    points(samps,rep(0,nSamples),bg=rgb(1,0,0,.3),col=rgb(0,0,0,0),pch=21)  
  }
  if(nSamples>1){
    my.dens = density(samps)
    maxdens = max(my.dens$y)
    lines(my.dens$x,my.dens$y/maxdens*maxy,col="red")
  }

  
  mtext(paste(nSamples,"/",m,"=",100*round(nSamples/m,2),"%"),3,.1,adj=0)

  wait(wait.type)
  
  cand = majorRand()
  cands = c(cands,cand)
  segments(cand,0,cand,major(cand),col="blue")
  points(cand,major(cand),pch=21,bg=rgb(0,0,1,.3),col=rgb(0,0,0,0))
  points(cand,pdf(cand),pch=21,bg=rgb(0,0,0,.3),col=rgb(0,0,0,0))
  
  
  wait(wait.type)

  
  u = runif(1,0,major(cand))
  abline(h=u,col="green")
  
  wait(wait.type)
  
  if(u<pdf(cand)){
    mtext("Accepted",3,.1,adj=1, cex=1.5)
    samps = c(samps,cand)
  }else{
    mtext("Rejected",3,.1,adj=1, cex=1.5)    
  }
  
    wait(wait.type)

}

  
  # Show one last time
  nSamples = length(samps)  

  plot(xx,pdf(xx),ty='l',lty=2,col="black",ylim=ylims)
  abline(h=0,col="gray")
  abline(h=ylims[2],col="gray")

  lines(xx,major(xx),lty=1,col="blue")

  points(cands,rep(ylims[2],M),bg=rgb(0,0,1,.3),col=rgb(0,0,0,0),pch=21)  
  
  if(nSamples>0)
  {
    points(samps,rep(0,nSamples),bg=rgb(1,0,0,.3),col=rgb(0,0,0,0),pch=21)  
  }
  if(nSamples>1){
    my.dens = density(samps)
    maxdens = max(my.dens$y)
    lines(my.dens$x,my.dens$y/maxdens*maxy,col="red")
  }

  
  mtext(paste(nSamples,"/",m,"=",100*round(nSamples/m,2),"%"),3,.1,adj=0)
