#1-b)
mom_mse_compare<-function(n,theta,iter)
{
  data<-replicate(iter,runif(n,0,theta))
  if(NCOL(data)!=1)
  {
    MLE<-apply(data,2,max)
    MOM<-apply(data,2,mean)
  }
  else
  {
    MLE<-data
    MOM<-2*data
  }
  MLE_result<-sum((MLE-theta)^2)/iter
  MOM_result<-sum((MOM-theta)^2)/iter
  return(c(MLE_result,MOM_result))
}
#For combination (2,5) with n = 1000
mom_mse_compare(2,5,1000)


#1-c)
res1_1<-mom_mse_compare(1,1,1000)
res1_5<-mom_mse_compare(1,5,1000)
res1_50<-mom_mse_compare(1,50,1000)
res1_100<-mom_mse_compare(1,100,1000)

res2_1<-mom_mse_compare(2,1,1000)
res2_5<-mom_mse_compare(2,5,1000)
res2_50<-mom_mse_compare(2,50,1000)
res2_100<-mom_mse_compare(2,100,1000)

res3_1<-mom_mse_compare(3,1,1000)
res3_5<-mom_mse_compare(3,5,1000)
res3_50<-mom_mse_compare(3,50,1000)
res3_100<-mom_mse_compare(3,100,1000)

res5_1<-mom_mse_compare(5,1,1000)
res5_5<-mom_mse_compare(5,5,1000)
res5_50<-mom_mse_compare(5,50,1000)
res5_100<-mom_mse_compare(5,100,1000)

res10_1<-mom_mse_compare(10,1,1000)
res10_5<-mom_mse_compare(10,5,1000)
res10_50<-mom_mse_compare(10,50,1000)
res10_100<-mom_mse_compare(10,100,1000)

res30_1<-mom_mse_compare(30,1,1000)
res30_5<-mom_mse_compare(30,5,1000)
res30_50<-mom_mse_compare(30,50,1000)
res30_100<-mom_mse_compare(30,100,1000)

#Plotting the data by keeping the value of theta to a fixed value and a variable value of n
#n - variable, theta - fixed 
par(mfrow=c(2,2))
n<-c(1,2,3,5,20,30)

MLE_1<-c(res1_1[1],res2_1[1],res3_1[1],res5_1[1],res10_1[1],res30_1[1])
MLE_5<-c(res1_5[1],res2_5[1],res3_5[1],res5_5[1],res10_5[1],res30_5[1])
MLE_50<-c(res1_50[1],res2_50[1],res3_50[1],res5_50[1],res10_50[1],res30_50[1])
MLE_100<-c(res1_100[1],res2_100[1],res3_100[1],res5_100[1],res10_100[1],res30_100[1])

MOM_1<-c(res1_1[2],res2_1[2],res3_1[2],res5_1[2],res10_1[2],res30_1[2])
MOM_5<-c(res1_5[2],res2_5[2],res3_5[2],res5_5[2],res10_5[2],res30_5[2])
MOM_50<-c(res1_50[2],res2_50[2],res3_50[2],res5_50[2],res10_50[2],res30_50[2])
MOM_100<-c(res1_100[2],res2_100[2],res3_100[2],res5_100[2],res10_100[2],res30_100[2])

plot(n,MLE_1,xlab="Sample size", ylab="MSE",type="b",col='red', main="MSE vs
Sample(theta=1)" )
lines(n,MOM_1,type="b",col='green')
legend("topright",legend=c("MLE","MOM"),col=c('red','green'),text.col =
         c('black','black'),lty = 1,pch = 1,inset=0.01,ncol=1,cex=0.6,bty='n')

plot(n,MLE_5,type="b",xlab="Sample size", ylab="MSE",col='red', main="MSE vs
Sample(theta=5)" )
lines(n,MOM_5,type="b",col='green')
legend("topright",legend=c("MLE","MOM"),col=c('red','green'),text.col =
         c('black','black'),lty = 1,pch = 1,inset=0.01,ncol=1,cex=0.6,bty='n')

plot(n,MLE_50,type="b",xlab="Sample size", ylab="MSE",col='red', main="MSE vs
Sample(theta=50)" )
lines(n,MOM_50,type="b",col='green')
legend("topright",legend=c("MLE","MOM"),col=c('red','green'),text.col =
         c('black','black'),lty = 1,pch = 1,inset=0.01,ncol=1,cex=0.6,bty='n')

plot(n,MLE_100,type="b",xlab="Sample size", ylab="MSE",col='red', main="MSE vs
Sample(theta=100)" )
lines(n,MOM_100,type="b",col='green')
legend("topright",legend=c("MLE","MOM"),col=c('red','green'),text.col =
         c('black','black'),lty = 1,pch = 1,inset=0.01,ncol=1,cex=0.6,bty='n')


#n - fixed, theta - variable 
par(mfrow=c(2,2))
thetas<-c(1,5,50,100)
MLE_n1<-c(res1_1[1],res1_5[1],res1_50[1],res1_100[1])
MLE_n2<-c(res2_1[1],res2_5[1],res2_50[1],res2_100[1])
MLE_n3<-c(res3_1[1],res3_5[1],res3_50[1],res3_100[1])
MLE_n5<-c(res5_1[1],res5_5[1],res5_50[1],res5_100[1])
MLE_n10<-c(res10_1[1],res10_5[1],res10_50[1],res10_100[1])
MLE_n30<-c(res30_1[1],res30_5[1],res30_50[1],res30_100[1])

MOM_n1<-c(res1_1[2],res1_5[2],res1_50[2],res1_100[2])
MOM_n2<-c(res2_1[2],res2_5[2],res2_50[2],res2_100[2])
MOM_n3<-c(res3_1[2],res3_5[2],res3_50[2],res3_100[2])
MOM_n5<-c(res5_1[2],res5_5[2],res5_50[2],res5_100[2])
MOM_n10<-c(res10_1[2],res10_5[2],res10_50[2],res10_100[2])
MOM_n30<-c(res30_1[2],res30_5[2],res30_50[2],res30_100[2])

plot(thetas,MLE_n1,type="b",xlab="theta", ylab="MSE",col='red', main="MSE vs
Sample(n=1)" )
lines(thetas,MOM_n1,type="b",col='green')
legend("topright",legend=c("MLE","MOM"),col=c("red","green"),text.col = 
         c('black','black'),lty = 1,pch = 1,inset=0.01,ncol=1,cex=0.6,bty='n')

plot(thetas,MLE_n2,type="b",xlab="theta", ylab="MSE",col='red', main="MSE vs 
Sample(n=2)" )
lines(thetas,MOM_n2,type="b",col='green') 
legend("topright",legend=c("MLE","MOM"),col=c('red','green'),text.col = 
         c('black','black'),lty = 1,pch = 1,inset=0.01,ncol=1,cex=0.6,bty='n')

plot(thetas,MLE_n3,type="b",xlab="theta", ylab="MSE",col='red', main="MSE vs 
Sample(n=3)" )
lines(thetas,MOM_n3,type="b",col='green') 
legend("topright",legend=c("MLE","MOM"),col=c('red','green'),text.col = 
         c('black','black'),lty = 1,pch = 1,inset=0.01,ncol=1,cex=0.6,bty='n')

plot(thetas,MLE_n5,type="b",xlab="theta", ylab="MSE",col='red', main="MSE vs 
Sample(n=5)" )
lines(thetas,MOM_n5,type="b",col='green') 
legend("topright",legend=c("MLE","MOM"),col=c('red','green'),text.col = 
         c('black','black'),lty = 1,pch = 1,inset=0.01,ncol=1,cex=0.6,bty='n')

plot(thetas,MLE_n10,type="b",xlab="theta", ylab="MSE",col='red', main="MSE vs 
Sample(n=10)" )
lines(thetas,MOM_n10,type="b",col='green') 
legend("topright",legend=c("MLE","MOM"),col=c('red','green'),text.col = 
         c('black','black'),lty = 1,pch = 1,inset=0.01,ncol=1,cex=0.6,bty='n')

plot(thetas,MLE_n30,type="b",xlab="theta", ylab="MSE",col='red', main="MSE vs 
Sample(n=30)" )
lines(thetas,MOM_n30,type="b",col='green') 
legend("topright",legend=c("MLE","MOM"),col=c('red','green'),text.col = 
        c('black','black'),lty = 1,pch = 1,inset=0.01,ncol=1,cex=0.6,bty='n')










