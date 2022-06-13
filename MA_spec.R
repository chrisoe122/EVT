library(EnvStats)

#MA-simulation
MA<-function(a1,a2,n){
  values <- rep(NA,(n-1))
  z <- rep(NA,(n+1))
  z<- rt((n+1),3)
  for (i in 1:(n-1)){
    values[i]<- z[i+2]+a1*z[i+1]+a2*z[i]
  }
  return(values)
}

#LOG LOG PLOT
m<-10000000

SX<-rep(NA,m)

for (i in 1:m){
  SX[i]<-MA(0.8,0.3,2)
}

plot(log(seq(1,300,0.2)),log((1-sad(seq(1,300,0.2)))), xlab='x', ylab='P(X>x)')
abline(log((1.2+0.8^3+0.3^3)),-3, col='red', lwd=2)






#Calculate quantile-function of MA(2)
m<-100000000

quan<-rep(NA,m)

for (i in 1:m){
  quan[i]<-MA(0.8,0.3,2)
}

prob<-c(0.9999,0.999,0.975, 0.025, 0.001, 0.0001)

quantile(quan, probs=prob)

# 99.99%/0.01%: 25.8/-25.8
# 99.9%/0.1%: 12.2/-12.2
#97.5%/2.5%: 4.3/-4.3




#Plot of MA(2)
plot(MA(0.5,0.2,1000), type='l', xlab='Time', ylab='')


########### Histrogram ########


loop<-function(t,a1,a2,x,n){
  t_v<- 1
  t_v2 <- 1
  t_v3 <- 1
  t_v4 <- 1
  for (i in 1:t){
    t_v1 <- MA(a1,a2,(n+1))[c((n-3),(n-2),(n-1),n)]
    if (abs(t_v1)[1]>x){
      t_v<-append(t_v,t_v1[1]) 
      t_v2 <- append(t_v2,t_v1[2])
      t_v3 <- append(t_v3, t_v1[3])
      t_v4 <- append(t_v4, t_v1[4])
    }
  }
  return(matrix(c(t_v[-1],t_v2[-1], t_v3[-1], t_v4[-1]),nrow=4, ncol=length(t_v[-1]), byrow=T))
}



#############x=4.3
set.seed(2022)
n<- 10000000
length(d1[1,])/n*100

d <- loop(n,0.8,0.3,4.3,4)
d1 <- t(t(d)/abs(d[1,]))

#t=0
h0_5 <- hist(d1[1,], breaks=100, plot=FALSE)
h0_5$counts=h0_5$counts/sum(h0_5$counts)
plot(h0_5, xlab = 'Theta_0', main='')


#t=1
h1_5 <- hist(d1[2,], breaks=300, plot=FALSE)
h1_5$counts=h1_5$counts/sum(h1_5$counts)
plot(h1_5, xlab = 'Theta_1', main='')

#t=2
h2_5 <- hist(d1[3,], breaks=300, plot=FALSE)
h2_5$counts=h2_5$counts/sum(h2_5$counts)
plot(h2_5, xlab = 'Theta_2', main='')

#t=3
h3_5 <- hist(d1[4,], breaks=300, plot=FALSE)
h3_5$counts=h3_5$counts/sum(h3_5$counts)
plot(h3_5, xlab = 'Theta_3', main='')



##########x=12.2
d_15 <- loop(n,0.8,0.3,12.2,4)
d3 <- t(t(d_15)/abs(d_15[1,]))

#t=0
h0_15 <- hist(d3[1,], breaks=20, plot=FALSE)
h0_15$counts=h0_15$counts/sum(h0_15$counts)
plot(h0_15, xlab = 'Theta_0', main='')
segments(x0=1,y0=0,y1=0.5,x1=1, col='red', lwd = 2, lty = 4)
segments(x0=-1,y0=0,y1=0.5,x1=-1, col='red', lwd = 2, lty = 4)



#t=1
h1_15 <- hist(d3[2,], breaks=50, plot=FALSE)
h1_15$counts=h1_15$counts/sum(h1_15$counts)
plot(h1_15, xlab = 'Theta_1', main='', ylim=c(0,0.35))
segments(x0=0.8,y0=0,y1=0.32,x1=0.8, col='red', lwd = 2, lty = 3)
segments(x0=-0.8,y0=0,y1=0.32,x1=-0.8, col='red', lwd = 2,lty = 3)
segments(x0=0.375,y0=0,y1=0.17,x1=0.375, col='red', lwd = 2,lty = 3)
segments(x0=-0.375,y0=0,y1=0.17,x1=-0.375, col='red', lwd = 2,lty = 3)
segments(x0=0,y0=0,y1=0.02,x1=0, col='red', lwd = 2,lty = 4)


#t=2
h2_15 <- hist(d3[3,], breaks=50, plot=FALSE)
h2_15$counts=h2_15$counts/sum(h2_15$counts)
plot(h2_15, xlab = 'Theta_2', main='', ylim=c(0,0.35))
segments(x0=0,y0=0,y1=0.34,x1=0, col='red', lwd = 2, lty=4)
segments(x0=0.3,y0=0,y1=0.32,x1=0.3, col='red', lwd = 2, lty=4)
segments(x0=-0.3,y0=0,y1=0.32,x1=-0.3, col='red', lwd = 2, lty=4)

#t=3
h3_15 <- hist(d3[4,], breaks=20, plot=FALSE)
h3_15$counts=h3_15$counts/sum(h3_15$counts)
plot(h3_15, xlab = 'Theta_3', main='', ylim=c(0,1))
segments(x0=0,y0=0,y1=1,x1=0, col='red', lwd = 2,lty = 4)












##################x=30
d2t <- loop(n,0.8,0.3,25.8,4)
d2<-t(t(d2t)/abs(d2t[1,])) #Nødt til at trans for at divide row-wise

#Første t=0
h0 <- hist(d2[1,], breaks=20, plot=FALSE)
h0$counts=h0$counts/sum(h0$counts)
plot(h0, xlab = 'Theta_0', main='')
segments(x0=1,y0=0,y1=0.5,x1=1, col='red', lwd = 2, lty = 3)
segments(x0=-1,y0=0,y1=0.5,x1=-1, col='red', lwd = 2, lty = 3)

#Anden t=1
h <- hist(d2[2,], breaks=20, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h, xlab = 'Theta_1', main='', ylim=c(0,0.3))
segments(x0=0.8,y0=0,y1=0.32,x1=0.8, col='red', lwd = 2, lty = 3)
segments(x0=-0.8,y0=0,y1=0.32,x1=-0.8, col='red', lwd = 2,lty = 3)
segments(x0=0.375,y0=0,y1=0.17,x1=0.375, col='red', lwd = 2,lty = 3)
segments(x0=-0.375,y0=0,y1=0.17,x1=-0.375, col='red', lwd = 2,lty = 3)
segments(x0=0,y0=0,y1=0.02,x1=0, col='red', lwd = 2,lty = 3)


#t=2
h2 <- hist(d2[3,], breaks=30, plot=FALSE)
h2$counts=h2$counts/sum(h2$counts)
plot(h2, xlab = 'Theta_2', main='', ylim=c(0,0.35))
segments(x0=0,y0=0,y1=0.34,x1=0, col='red', lwd = 2, lty = 3)
segments(x0=0.3,y0=0,y1=0.32,x1=0.3, col='red', lwd = 2, lty = 3)
segments(x0=-0.3,y0=0,y1=0.32,x1=-0.3, col='red', lwd = 2, lty = 3)



#t=3
h3 <- hist(d2[4,], breaks=20, plot=FALSE)
h3$counts=h3$counts/sum(h3$counts) 
plot(h3, xlab = 'Theta_3', main='', ylim=c(0,1))
segments(x0=0,y0=0,y1=1,x1=0, col='red', lwd = 2,lty = 4)
