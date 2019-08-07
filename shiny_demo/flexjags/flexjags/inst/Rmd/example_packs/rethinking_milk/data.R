#+ echo=FALSE, fig.width=4, fig.height=4

load("milk.Rda")

milk <- milk[ complete.cases(milk), ]

for(name in colnames(milk)){
	# Replace periods with underscores for stan
  name_new <- gsub(".", "_", name, fixed = TRUE)
  assign(name_new, milk[,name])
}

rm(milk, name_new)

log_mass <- log(mass)
n <- length(mass)

# neocortex_perc <- neocortex_perc - mean(neocortex_perc)
# log_mass = log_mass <- mean(log_mass)

