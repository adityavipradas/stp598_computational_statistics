#STP598 COMPUTATIONAL STATISTICS
#ASSIGNMENT 2
#PART 1a

#libraries
library(boot)

#create skewness function
skew.fn <- function(data, index){
  xvect = data[index]
  return (mean(((xvect-mean(xvect))/sd(xvect))^3))
}

getCI <- function(){
  
  # skewList <- rep(0,R)
  # 
  # for (i in 1:R){
  #   skewList[i] = skew.fn(X, sample(1:n, n, replace=TRUE))
  # }
  
  exp_boot = boot(data = X, statistic = skew.fn, R = RS)
  ci_boot = boot.ci(boot.out = exp_boot, conf = 0.95, type = c("basic","norm","bca","perc"))
  
  return(c(basic=ci_boot$basic[4:5],normal=ci_boot$normal[2:3],bca=ci_boot$bca[4:5],percentile=ci_boot$percent[4:5]))
}

#number of data points
n <- 50

#number of resamplings
RS <- 200

#repeat process
p <- 10

#create Y vector
Y <- rnorm(n, mean=0, sd=1)

#create X vector
X <- exp(Y)

#calculate original skewness
rskew = mean(((X-mean(X))/sd(X))^3)

#calulate confidence intervals
CIs = t(replicate(p,getCI()))
print(CIs)
print(rskew)

#coverage probabilities
basicCov = sum((CIs[,"basic1"] < rskew) & (CIs[,"basic2"] > rskew))/p
normalCov = sum((CIs[,"normal1"] < rskew) & (CIs[,"normal2"] > rskew))/p
bcaCov = sum((CIs[,"bca1"] < rskew) & (CIs[,"bca2"] > rskew))/p
percCov = sum((CIs[,"percentile1"] < rskew) & (CIs[,"percentile2"] > rskew))/p

print(c(basicCov, normalCov, bcaCov, percCov))

require(plotrix)
plotCI(1:p,rep(rskew,p), ui=CIs[,"basic2"],li=CIs[,"basic1"],col="red", barcol="green", lwd=3, xlab="Iterations", ylab="95% basic confidence intervals")
dev.copy(png,paste("basicCI.png"))
dev.off()

plotCI(1:p,rep(rskew,p), ui=CIs[,"normal2"],li=CIs[,"normal1"],col="red", barcol="green", lwd=3, xlab="Iterations", ylab="95% normal confidence intervals")
dev.copy(png,paste("normalCI.png"))
dev.off()

plotCI(1:p,rep(rskew,p), ui=CIs[,"bca2"],li=CIs[,"bca1"],col="red", barcol="green", lwd=3, xlab="Iterations", ylab="95% bca confidence intervals")
dev.copy(png,paste("bcaCI.png"))
dev.off()

plotCI(1:p,rep(rskew,p), ui=CIs[,"percentile2"],li=CIs[,"percentile1"],col="red", barcol="green", lwd=3, xlab="Iterations", ylab="95% percentile confidence intervals")
dev.copy(png,paste("percCI.png"))
dev.off()



