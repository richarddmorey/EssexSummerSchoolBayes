#+ echo=FALSE, fig.width=4, fig.height=4

stereogram = read.csv("stereograms.csv", header = TRUE)

for(name in colnames(stereogram)){
  assign(name, stereogram[,name])
}

rm(stereogram)

logFuseTime <- log(fuseTime)
n <- length(fuseTime)
condition_indicator <- (condition == "NV")-.5

boxplot(fuseTime ~ condition, xlab = "Condition",
        ylab = "Time to fuse stereogram")

