#+ echo=FALSE, fig.width=4, fig.height=4

load("milk.Rda")

milk <- milk[ complete.cases(milk), ]

for(name in colnames(milk)){
	assign(name, milk[,name])
}

rm(milk)

log.mass <- log(mass)

# neocortex.perc <- neocortex.perc - mean(neocortex.perc)
# log.mass = log.mass <- mean(log.mass)

