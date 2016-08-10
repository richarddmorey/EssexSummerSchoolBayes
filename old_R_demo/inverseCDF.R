######
# Inverse CDF method demo in R
# 2011, Richard D. Morey (richarddmorey@gmail.com)
# Please do not distribute without permission
######
# Instructions: Change settings to suit, save, then source() this file
# Shows samples from uniform distribution (y axis)
# Uniform samples (blue) are transformed by the inverse CDF
# Solid curve is the CDF
# Dashed curve is PDF
# Red samples are corresponding samples from the PDF
# Red curve shows kernel density estimate based on samples (should converge to PDF)

# Settings
##

# Number of seconds to wait between steps
# if "key", wait for key press
wait.type = "key"

# Number of samples
# Set this low (20 or so if using wait.type="key")
M = 20

# function definitions (make sure these are consistent!)
icdf = function(p) qchisq(p,3)  # inverse CDF
cdf = function(x) pchisq(x,3)   # CDF
pdf = function(x) dchisq(x,3)   # PDF


# (plot) x limits
xlims = c(0,15)

# (plot) scale the histogram to this proportion of plot hight
scalehist = .75

# No need to change anything below here
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
xx = seq(xlims[1],xlims[2],len=100)
maxy = max(pdf(xx)) 

samps = 1:M*NA

# Start samples
for(m in 1:M)
{
  plot(xx,cdf(xx),ty='l',lwd=2)
  lines(xx,pdf(xx)/maxy*scalehist,lty=2,col="black")
  abline(h=c(0,1),col="gray")

  abline(v=xlims[1],col="blue")
  
  # Sample from Uniform
  u = runif(1)
  # Transform by inverse CDF
  samps[m] = icdf(u)
  points(rep(xlims[1],m),cdf(samps[1:m]),pch=21,bg=rgb(0,0,1,.3),col=rgb(0,0,0,0))
  segments(xlims[1],u,icdf(u),u,col=rgb(0,0,1,1))

  wait(wait.type)

  points(samps[1:m],rep(0,m),pch=21,bg=rgb(1,0,0,.3),col=rgb(0,0,0,0))
  segments(icdf(u),u,icdf(u),0,col=rgb(1,0,0,1))

  wait(wait.type)

  if(m>1){
    my.dens = density(samps[1:m])
    max.dens = max(my.dens$y)
    lines(my.dens$x,my.dens$y/max.dens*scalehist,col=rgb(1,0,0,.6))
  }

wait(wait.type)

}

