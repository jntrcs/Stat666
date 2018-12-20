#Simulation.R


  
generate_MAR<-function(consts, timeTrend, vars, Bmat, Xmat, armat, burnin=100){
  times<-1:nrow(Xmat)
  means<-Xmat%*%Bmat+
    matrix(rep(consts, nrow(Xmat)), nrow=nrow(Xmat), byrow = T)+
    times%*%t(timeTrend)
  means<-rbind(matrix(0, ncol=ncol(means), nrow=burnin), means)
  results<-matrix(0, ncol=ncol(means), nrow=nrow(means))
  error<-rnorm(3, 0, sqrt(vars))
  results[1,]<-means[1,]+error
  lastError=error
  for(i in 2:nrow(results)){
    #if(i==99)break
    #set.seed(1)
    error<-armat%*%lastError + rnorm(3,0, sqrt(vars))
    results[i,]<-means[i,]+error
    lastError = error
  }
  results[-(1:burnin),]
  # plot(results[,1])
  # Sys.sleep(1)
  # plot(results[,2])
  # Sys.sleep(1)
  # plot(results[,3])
  # Sys.sleep(1)
}


consts<-c(0,10,100)
vars<-c(6, 8, 1)
timeTrend=c(.05,-.06,0)
#pxlength(consts)
Bmat<-matrix(1, nrow=4, ncol=3)
Xmat<-matrix(runif(8000), nrow=2000, ncol=4)
armat=matrix(c(.5,  .2,  .10,
               .0, .8,  0,
                .4,  0, .5 ),nrow=3, byrow = T)
data=generate_MAR(consts,timeTrend, vars, Bmat, Xmat, armat)
plot(data[,1])
plot(data[,2])
plot(data[,3], ylab="Y")

Xmat<-cbind(1:nrow(Xmat), Xmat)
exog<-lm(data~Xmat)
resids<-exog$residuals

fitvar1=VAR(resids, p=1, type="none")
summary(fitvar1)

means<-Xmat%*%Bmat
drifts=data-means
plot(drifts[,2])
plot(drifts[,1])
plot(drifts[,3])
armat
acf(drifts[,3])
acf(drifts[,1])
acf(drifts[,2])

require(mAr)
?mAr.sim
mAr.est(data, p=1)


#What if Phi is set to the identity??
armat<-diag(3)
consts<-c(0,10,100)
vars<-c(6, 8, 1)
timeTrend=c(0,0,0)
#pxlength(consts)
Bmat<-matrix(1, nrow=4, ncol=3)
Xmat<-matrix(runif(10000), nrow=2500, ncol=4)

data=generate_MAR(consts,timeTrend, vars, Bmat, Xmat, armat)
plot(data[,1])
plot(data[,2], ylab="Y", xlab="Time")
plot(data[,3])

#What if Phi is set to the identity??
armat<-diag(c(1.1,1.1,1.1))
consts<-c(0,10,100)
vars<-c(6, 8, 1)
timeTrend=c(0,0,0)
#pxlength(consts)
Bmat<-matrix(1, nrow=4, ncol=3)
Xmat<-matrix(runif(1000), nrow=250, ncol=4)

data=generate_MAR(consts,timeTrend, vars, Bmat, Xmat, armat)
plot(data[,1])
plot(data[,2], ylab="Y", xlab="Time")
plot(data[,3])
