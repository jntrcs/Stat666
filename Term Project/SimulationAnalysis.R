#SimulationAnalysis.R

load("Sim1Results.RData")
QIncluded
BIncluded
DIncluded

sum(BIncluded)/(length(BIncluded)*100)
sum(DIncluded[1:3])/300
sum(DIncluded[4:6])/300
sum(DIncluded[7:18])/1200

load("Sim2Results.RData")
QIncluded_RW
sum(BIncluded_RW)/(length(BIncluded)*100)
sum(DIncluded_RW[1:3])/300
sum(DIncluded_RW[4:6])/300
sum(DIncluded_RW[7:18])/1200

load("Sim3Results.RData")
QIncluded_NS
sum(BIncluded_NS)/(length(BIncluded)*100)
sum(DIncluded_NS[1:3])/300
sum(DIncluded_NS[4:6])/300
sum(DIncluded_NS[7:18])/1200
