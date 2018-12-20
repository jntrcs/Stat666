##Baseline regressions



##Calculate MSE for the test set for each of the three models based on a simple
#linear regression of the actual outcome based on the last prediction
i=0
lapply(train, FUN=function(mod){
  i<<-i+1
  linmod<-lm(Result~Date93, data=mod)
  preds<-predict(linmod, newdata=test[[i]])
  MSE<-mean((preds-test[[i]]$Result)^2)
})

classicSimpleMod<-lm(Result~Date93, data=models$Classic)
summary(classicSimpleMod)
confint(classicSimpleMod)
plot(classicSimpleMod)

a=models$Classic$Date93+rnorm(432, 0, sqrt(3))


#Look at the covariance of the models
require(corrplot)
cors<-cor(models$Deluxe[,3:95])
require(reshape)
melted<-melt(cors)
melted$X<-rep(1:93, 93)
melted$Y<-rep(1:93, each=93)
head(melted)
ggplot(aes(x=X, y=93-Y, fill=value), data=melted)+geom_tile()+
  ylab("Days Before Election")+xlab("Days before Election")+
  scale_x_continuous(breaks=c(0, 25,50,75),
                   labels=c(93, 93-25, 93-50, 93-75))+ 
  guides(fill=guide_legend(title="Correlation"))+
  ggtitle("Correlation Matrix for 93 days of Predictions")



overTime<-lapply(models, FUN=function(lite){
  sapply(1:93, function(i){
    sqrt(sum(mean((lite[,i+2]-lite$Result)^2)))
  })
})

overTime=as.data.frame(overTime)
melted=melt(overTime)
names(melted)=c("Model", "RMSE")
melted$Time=rep(1:93,3)
ggplot(melted, aes(group=Model, color=Model, y=RMSE, x=Time))+geom_line(size=2)
ggplot(melted[melted$Model!="Lite",], aes(group=Model, color=Model, y=RMSE, x=Time))+geom_line(size=2)

demOverTime<-lapply(models, FUN=function(lite){
  sapply(1:93, function(i){
  mean(lite[,i+2]-lite$Result)
    })
})

demOverTime=as.data.frame(demOverTime)
demMelted=melt(demOverTime)
names(demMelted)=c("Model", "DemAverage")
demMelted$Time=rep(1:93,3)
ggplot(demMelted, aes(group=Model, color=Model, y=DemAverage, x=Time))+geom_line(size=2)#+

  varOverTime<-lapply(models, FUN=function(lite){
    sapply(1:93, function(i){
      sd(lite[,i+2]-lite$Result)
    })
  })
  
  varOverTime=as.data.frame(varOverTime)
  varMelted=melt(varOverTime)
  names(varMelted)=c("Model", "SD")
  varMelted$Time=rep(1:93,3)
  ggplot(varMelted, aes(group=Model, color=Model, y=SD, x=Time))+geom_line(size=2)
  ggplot(varMelted[varMelted$Model!="Lite",], aes(group=Model, color=Model, y=SD, x=Time))+geom_line(size=2)
  