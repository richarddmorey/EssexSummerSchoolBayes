######
# Adaptive rejection method envelope creation demo in R
# 2011, Richard D. Morey (richarddmorey@gmail.com)
# Please do not distribute without permission
######
# Instructions: Change settings to suit, save, then source() this file
# This demo file isn't done yet

log.pdf = function(x) log(sin(pi*x))

xlims = c(0.01,.99)
ylims = c(0,1.2)
log.ylims = c(-5,2)

startVals = seq(.1,.9,len=5)#c(.1,.25,.75,.9)
nPoints = length(startVals)

startLogDens = log.pdf(startVals)

findSlopeIntercept = function(x1,y1,x2,y2)
{
  slope = (y2-y1)/(x2-x1)
  intercept = y1 - slope*x1
  cbind(slope,intercept)
}

findIntersection = function(slope1,intercept1,slope2,intercept2)
{
x = (intercept2-intercept1)/(slope1-slope2)
y = slope1*x+intercept1
cbind(x,y)
}

ablineExp<-function(a,b,prec=100,...)
{
xx = seq(par()$usr[1],par()$usr[2],len=prec)
lines(xx,exp(a + b*xx),...)
}  


startLines = findSlopeIntercept(startVals[-nPoints], startLogDens[-nPoints], startVals[-1], startLogDens[-1])

#findIntersection(startLines[1,1],startLines[1,2],startLines[3,1],startLines[3,2])


xx = seq(xlims[1],xlims[2],len=100)

par(mfrow=c(1,2))

plot(xx,log.pdf(xx),ty='l',ylim=log.ylims)
points(startVals,startLogDens,pch=21,bg=rgb(0,0,0,.3))
apply(startLines,1,function(v) abline(a=v[2],b=v[1],col="blue"))
abline(v=xlims,col="gray")


plot(xx,exp(log.pdf(xx)),ty='l',ylim=ylims)
points(startVals,exp(startLogDens),pch=21,bg=rgb(0,0,0,.3))
apply(startLines,1,function(v) ablineExp(a=v[2],b=v[1],col="blue"))
abline(v=xlims,col="gray")

