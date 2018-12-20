##SimulationStudy.R

require(MARSS)
#AR matrix where the rows are less than 1
armat<-matrix(c(.6, 0, .1,
                .2, .5, .2,
                0, 0, .8), byrow = T, nrow=3, ncol=3)
Beta<-matrix(c(1, 0,0,
             .5, .5,.5,
             0,.1,-2,
             0,0,0), byrow = T, nrow=4)
consts<-c(0, 5, 10)
vars<-c(1, 2, 8)
timeTrend<-c(.1, 0, -.01)

Q = "diagonal and unequal"
B = "unconstrained"
U = x0 = "zero"
A="zero"
R = "identity"
D = "unconstrained"
QIncluded = rep(0, 3)
DIncluded<-rep(0, 18)
BIncluded<-rep(0, 9)

for (i in 1:100){
  Xmat<-matrix(runif(400, -2,2), ncol=4)
  y=generate_MAR(consts, timeTrend, vars, Beta, Xmat, armat)
  Xmat<-cbind(1, 1:nrow(Xmat), Xmat)
  model.list = list(B=B,U=U,Q=Q,A=A,R=R,D=D,d=t(Xmat),x0=x0)
  control.list = list(maxit=4000)
  kem = MARSS(t(y), model=model.list, control=control.list, method="BFGS")
  kem=MARSSparamCIs(kem)
  summ=summary(kem)
  lowers= summary(kem)$par.lowCI
  uppers= summary(kem)$par.upCI
  QIncluded = QIncluded+ as.numeric(lowers$Q <= vars & vars <= uppers$Q)
  BIncluded = BIncluded + as.numeric(lowers$B <= c(armat) & c(armat) <=uppers$B)
  beta = c(t(rbind(consts, timeTrend, Beta)))
  DIncluded = DIncluded + as.numeric(lowers$A <= beta & beta<=uppers$A)
  print(i)
}

save(QIncluded, BIncluded, DIncluded, kem, file="Sim1Results.RData")


##ARmatrix when it is diagonal (random walk)

armat<-diag(3)


QIncluded_RW = rep(0, 3)
DIncluded_RW<-rep(0, 18)
BIncluded_RW<-rep(0, 9)

for (i in 1:100){
  Xmat<-matrix(runif(400, -2,2), ncol=4)
  y=generate_MAR(consts, timeTrend, vars, Beta, Xmat, armat)
  Xmat<-cbind(1, 1:nrow(Xmat), Xmat)
  model.list = list(B=B,U=U,Q=Q,A=A,R=R,D=D,d=t(Xmat),x0=x0)
  control.list = list(maxit=4000)
  kem = MARSS(t(y), model=model.list, control=control.list, method="BFGS")
  kem=MARSSparamCIs(kem)
  summ=summary(kem)
  lowers= summary(kem)$par.lowCI
  uppers= summary(kem)$par.upCI
  QIncluded_RW = QIncluded_RW+ as.numeric(lowers$Q <= vars & vars <= uppers$Q)
  BIncluded_RW = BIncluded_RW + as.numeric(lowers$B <= c(armat) & c(armat) <=uppers$B)
  beta = c(t(rbind(consts, timeTrend, Beta)))
  DIncluded_RW = DIncluded_RW + as.numeric(lowers$A <= beta & beta<=uppers$A)
  print(i)
}

save(QIncluded_RW, DIncluded_RW, BIncluded_RW, file="Sim2Results.RData")

##Situation where AR matrix is non-stationary
armat<-matrix(c(1.01, 0,0,
                .05, .9, .06,
                0, .5,.51), byrow = T, nrow=3)


QIncluded_NS = rep(0, 3)
DIncluded_NS<-rep(0, 18)
BIncluded_NS<-rep(0, 9)

for (i in 1:100){
  Xmat<-matrix(runif(400, -2,2), ncol=4)
  y=generate_MAR(consts, timeTrend, vars, Beta, Xmat, armat)
  Xmat<-cbind(1, 1:nrow(Xmat), Xmat)
  model.list = list(B=B,U=U,Q=Q,A=A,R=R,D=D,d=t(Xmat),x0=x0)
  control.list = list(maxit=4000)
  kem = MARSS(t(y), model=model.list, control=control.list, method="BFGS")
  kem=MARSSparamCIs(kem)
  summ=summary(kem)
  lowers= summary(kem)$par.lowCI
  uppers= summary(kem)$par.upCI
  Qci=as.numeric(lowers$Q <= vars & vars <= uppers$Q)
  Qci[is.na(Qci)]<-0
  QIncluded_NS = QIncluded_NS+ Qci
  Bci<-as.numeric(lowers$B <= c(armat) & c(armat) <=uppers$B)
  Bci[is.na(Bci)]<-0
  BIncluded_NS = BIncluded_NS + Bci
  beta = c(t(rbind(consts, timeTrend, Beta)))
  Dci<-as.numeric(lowers$A <= beta & beta<=uppers$A)
  Dci[is.na(Dci)]<-0
  DIncluded_NS = DIncluded_NS + Dci
  print(i)
}

save(QIncluded_NS, DIncluded_NS, BIncluded_NS, file="Sim3Results.RData")
