#GARCH
library(EnvStats)


#GARCH-function with both X and sigma
GARCH_plot <- function(n,a0,a1,b1){
  X<-rep(NA,n)
  S<-rep(NA,n)
  S[1]<-0
  X[1]<-0
  Z<-0
  for (i in 1:(n-1)){
    S[i+1]<-a0+(a1*Z^2+b1)*S[i]
    Z<-rnorm(1)
    X[i+1]<- Z*sqrt(S[i+1])
  }
  M<-matrix(data=c(S,X), nrow=n, ncol=2)
  return(M)
}

#Plots of sigma and X
set.seed(2002)
D<- GARCH_plot(2000,0.2,0.5,0.5)[1001:2000,]
plot(D[,2], type='l', ylab='')
plot(D[,1], type='l', col='red', ylab='', lty=3)

#Simulations of quantile function
m<-10000000
quan_garch<- GARCH_plot(m,0.2,0.5,0.5)[-c(1:1000),1]
quantile(quan_garch, probs = c(0.99))




#Garch with only sigma
GARCH <- function(n,a0,a1,b1){
  s<-0
  for (i in 1:n){
    s<- a0+(a1*rnorm(1)^2+b1)*s
  }
  s1<- a0+(a1*rnorm(1)^2+b1)*s
  return(c(s,s1))
}

GARCH(1000,0.2,0.5,0.5)
m<-100000
SS<-rep(NA,m)

for (i in 1:m){
  SS[i]<-GARCH(1000,0.2,0.5,0.5)[1]
}

#Loglog plot of sigma
plot(log((10:1000)),log((1-sad((10:1000)))), xlab='x', ylab='P(sigma^2>x)')
abline(0.08,-1, col='red', lwd=2)


#Spectral histogram
m<-100000
x<-108
Spec1 <- rep(NA,1)
X<-rep(NA,2)
for (i in 1:m){
  X<-GARCH(1000,0.2,0.5,0.5)
  if (X[1]>x){
    Spec1<-append(Spec1,X[2]/X[1])
  }
}
hist(Spec1, breaks=50, main='', xlab = 'Theta_1')


#Comparing Sigma with Z^2
chi<-0.5*rchisq((length(Spec1[-1])),df=1)+0.5
qqplot(chi,Spec1[-1], xlab = '0.5Z^2+0.5', ylab = 'Theta_1')
abline(0,1, col='red')




#SIMULAION OF C_+
m<-10000000
a<-rep(NA,m)

for (i in 1:m){
  Z<-rnorm(1)
  a[i]<-((0.5*Z^2+0.5)*log(0.5*Z^2+0.5))
}
0.2/mean(a)
log(1.08)
