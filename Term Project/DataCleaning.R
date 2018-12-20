require(reshape)
require(dplyr)
require(xlsx)
require(lubridate)

data=read.csv("house_district_forecast.csv",stringsAsFactors = F)


demRaces<-filter(data, party=='D', model=='deluxe')
demRaces$ID<-paste(demRaces$state, demRaces$district)
#demRaces$DaysBeforeElection<- mdy("11-6-18") - ymd(demRaces$forecastdate)

#data<-filter(data, party %in% c("R", "D"))
melted=melt(demRaces, id.vars = c(1:8, 13))
voteShare= melted[melted$variable=="voteshare",]
winProb<-melted[melted$variable=="win_probability",]

#The fun.aggregate function sums when the Democrats are running two candidates
#in one race, so the prediction of dem voter share is cand1+cand2
casted= cast(voteShare,formula =  ID ~forecastdate, fun.aggregate = sum )
#some kind of error made his vote total go to 0, interpolating between day before and after
casted[319, 96]<-77
castedWin= cast(winProb,formula =  ID ~forecastdate, fun.aggregate = sum )

which(apply(casted, 1, FUN=function(x)all(x[2:99]==100)))

Ys<-data.frame(DemShare=apply(casted[,-1], 2, mean))


names(casted)<-c("ID", paste0("Day", 1:98))
names(castedWin)<-c("ID", paste0("Day", 1:98))

actual<-read.xlsx("ActualResults.xlsx",1, head=T)
actual$DemShare<-actual$Dem..*100
head(actual)
abbrevs<-read.csv("Abbrevs.csv")
actual$Abbrev<-abbrevs$Abbreviation[match(actual$State, abbrevs$State)]
actual$CD[actual$CD=="AL"]<-1
actual$ID<-paste(actual$Abbrev, actual$CD)
#actual<-actual[actual$Dem.Votes>0| actual$GOP.Votes>0,]
#Get rid of the races where no democrat was running
actual<-actual[!actual$ID%in%setdiff(actual$ID, demRaces$ID),]
actual$DemWin = actual$Dem.Votes>actual$GOP.Votes

opponent<-casted$Day1<100 | casted$Day98<100
casted<-casted[opponent, ]
castedWin<-castedWin[opponent,]
ids<-casted$ID
actual<-actual[actual$ID%in%ids,]

castedWin$DemWon<-actual$DemWin[match(castedWin$ID, actual$ID)]
casted$DemShare<-actual$DemShare[match(casted$ID, actual$ID)]


calibration<-function(probs, results, nbins=20){
  bins=seq(0,1,length.out=nbins+1)
  total=table(cut(probs, bins))
  if (any(total==0))warning("Zero observed in bin, try increasing bin size")
  won=table(cut(probs[results], bins))
  midpoints=.5*bins[-1]+.5*bins[-(nbins+1)]
  expected = total*midpoints
  chi = sum((won-expected)^2/expected)
  chi
}

Ys$calibration=apply(castedWin[-c(1, ncol(castedWin))], 2, 
            FUN=function(x) calibration(x, castedWin$DemWon,11))


Ys$MSE<-apply(casted[,-c(1, ncol(casted))],2, FUN=function(preds)
  mean((preds-casted$DemShare)^2))


Ys$Time<-1:98
Ys$DaysBeforeElection = 5+(98-Ys$Time)

trends<-read.csv("TrendData.csv",skip = 2)
head(trends)
names(trends)<-c("Date", "Stocks", "Democrats", "Immigration", "Trump")
head(trends)
mtrends<-melt(trends, id.vars = "Date")
ggplot(mtrends, aes(x=Date, group=variable, color=variable, y=value))+geom_line(size=2)
trends$DaysBeforeElection<-mdy("11-6-18")-mdy(trends$Date)

##Since stock market searches are strongly influenced by days the market 
#is opened, we will account for whether it's a weekend or not and 
#record the residual as our unaccounted for interest in stock market
trends$DayOfWeek<-weekdays(mdy(trends$Date))
trends$Weekend<-as.numeric(trends$DayOfWeek%in%c("Saturday", "Sunday"))
trends$Stocks<-lm(Stocks~Weekend, data=trends)$residuals

trends<-mutate(trends, Trumplag1=lead(Trump, order_by = DaysBeforeElection))
trends<-mutate(trends, Immigrationlag1=lead(Immigration, order_by = DaysBeforeElection))
trends<-mutate(trends, Stockslag1=lead(Stocks, order_by = DaysBeforeElection))
trends<-mutate(trends, Democratslag1=lead(Democrats, order_by = DaysBeforeElection))
trends<-mutate(trends, Trumplag2=lead(Trump, n=2L, order_by = DaysBeforeElection))
trends<-mutate(trends, Immigrationlag2=lead(Immigration, n=2L,  order_by = DaysBeforeElection))
trends<-mutate(trends, Stockslag2=lead(Stocks, n=2L,  order_by = DaysBeforeElection))
trends<-mutate(trends, Democratslag2=lead(Democrats, n=2L,  order_by = DaysBeforeElection))
trends<-mutate(trends, Trumplag5=lead(Trump, n=5L,  order_by = DaysBeforeElection))
trends<-mutate(trends, Immigrationlag5=lead(Immigration, n=5L,  order_by = DaysBeforeElection))
trends<-mutate(trends, Stockslag5=lead(Stocks, n=5L,  order_by = DaysBeforeElection))
trends<-mutate(trends, Democratslag5=lead(Democrats, n=5L,  order_by = DaysBeforeElection))

head(trends)

ggplot(Ys, aes(x=Time, y=MSE))+geom_line()+
  ylab("Voter Share MSE")+
  scale_x_continuous("Days Until Election", c(0,25,50,75,100), 
                     labels=c(100, 75,50,25, 0))+ggsave("MSE.jpg")

ggplot(Ys, aes(x=Time, y=calibration))+geom_line()+
  ylab("Calibration")+
  scale_x_continuous("Days Until Election", c(0,25,50,75,100), 
                     labels=c(100, 75,50,25, 0))+ggsave("Calibration.jpg")

ggplot(Ys, aes(x=Time, y=DemShare))+geom_line()+
  ylab("Mean Democrat Vote Share")+
  scale_x_continuous("Days Until Election", c(0,25,50,75,100), 
                     labels=c(100, 75,50,25, 0))+ggsave("DemShare.jpg")

data<-merge(Ys,trends,by="DaysBeforeElection")
data$Date<-mdy(data$Date)
data<-arrange(data, Date)

pretty_print<-function(cos){
  print("AR Matrix")
  print(matrix(cos$B, nrow=3, ncol=3))
  print("Beta Matrix")
  print(matrix(cos$D, byrow = T, ncol=3))
  print("Error Matrix")
  print(diag(c(cos$Q)))
}
