require(MARSS)

Q = "diagonal and unequal"
B = "diagonal and unequal"
 U = x0 = "zero"
 A="zero"
 R = "identity"
D = "unconstrained"

Bmat<-matrix(c(1, 0, 0,
               0, 1, 0,
               0, 0, 1,
               1, 0, 0), nrow=4, ncol=3)
consts<-c(0,10,100)
vars<-c(4, 1, 1)
Xmat<-matrix(runif(800), nrow=200, ncol=4)
armat=matrix(c(.5,  0,  .0,
               .0, .2,  0,
               0,  0, .8 ),nrow=3, byrow = T)
data=generate_MAR(consts, vars, Bmat, Xmat, armat)
acf(data[,2])
#data<-scale(data, scale=F)
Xmat<-cbind(1:nrow(Xmat), Xmat)
Xmat<-cbind(1, Xmat)
#Xmat<-scale(Xmat, scale=F)

model.list = list(B=B,U=U,Q=Q,A=A,R=R,D=D,d=t(Xmat),x0=x0)
control.list = list(maxit=1500)
kem = MARSS(t(data), model=model.list, control=control.list, method="BFGS")
matrix(coef(kem)$D, nrow=6, ncol=3, byrow=T)
diag(c(coef(kem)$Q))
coef(kem)


Q = "diagonal and unequal"
B = "unconstrained"
U = x0 = "zero"
A="zero"
R = "identity"
D = "unconstrained"
Bmat<-matrix(c(1, 0, .5,
               0, -.5, 2,
               1, 1, 1), nrow=3, ncol=3, byrow=T)
consts<-c(0,-5,5)
vars<-c(4,4,4)
Xmat<-matrix(runif(600,0,1), nrow=200, ncol=3)
armat=matrix(c(.3,  .2,  .0,
               .0, .2,  .3,
               0,  0,  .6 ),nrow=3, byrow = T)
data=generate_MAR(consts, vars, Bmat, Xmat, armat)
acf(data[,3])
#data<-scale(data, scale=F)
#Xmat<-cbind(1:nrow(Xmat), Xmat)
Xmat<-cbind(1, Xmat)
#Xmat<-scale(Xmat, scale=F)

model.list = list(B=B,U=U,Q=Q,A=A,R=R,D=D,d=t(Xmat),x0=x0)
control.list = list(maxit=4000)
kem = MARSS(t(data), model=model.list, control=control.list, method="BFGS")
MARSSparamCIs(kem)

matrix(coef(kem)$D, nrow=4, ncol=3, byrow=T)
diag(c(coef(kem)$Q))
matrix(c(coef(kem)$B), nrow=3)
