
library(readr)
recovery <- read_csv("recovery.csv")


recovery
X<-recovery$blanket
Y<-recovery$minutes

cholesterol <- read_csv("cholesterol.csv")
X<-cholesterol$trt
Y<-cholesterol$response

Y1<-subset(Y,X=="1time")
#Y2<-subset(Y,X=="2times")


#Y1<-subset(Y,X=="2times")
Y2<-subset(Y,X=="4times")


mean(((sort(Y2)-sort(Y1)))^1)
mean(((sort(Y2)-sort(Y1))-((mean(sort(Y2))-mean(sort(Y1)))))^2)
mean(((sort(Y2)-sort(Y1))-((mean(sort(Y2))-mean(sort(Y1)))))^3)/((mean(((sort(Y2)-sort(Y1))-((mean(sort(Y2))-mean(sort(Y1)))))^2))^(3/2))

#Y1<-subset(Y,X=="2times")
#Y2<-subset(Y,X=="4times")

Boot<-100

a<-numeric(Boot)
al<-numeric(Boot)
au<-numeric(Boot)


Monte<-1000

for(r in 1:Boot){
  
  Y1s<-sample(Y1,length(Y1),TRUE)
  Y2s<-sample(Y2,length(Y2),TRUE)
  
  D0<-Y1s
  D1<-Y2s
  
  D1U<-numeric(length(Y1))
  D1L<-numeric(length(Y1)) 
  for(i in 1:length(Y1)){
    u<-runif(1,0,1)
    #u<-0
    D1U[i]<-quantile(Y2s,u*(mean(Y1s<=Y1s[i])-mean(Y1s<Y1s[i]))+mean(Y1s<Y1s[i]),type = 1)
    D1L[i]<-quantile(Y2s,1-(1-u)*(mean(Y1s<=Y1s[i])-mean(Y1s<Y1s[i]))-mean(Y1s<Y1s[i]),type = 1)
  }
  
  
  #a[r]<-mean(D1U/D0)
  #a[r]<-mean((D1U/D0-mean(D1U/D0))^2)
  a[r]<-mean((D1U/D0-mean(D1U/D0))^(3))/(sqrt(mean((D1U/D0-mean(D1U/D0))^(2)))^(3/2))
  #a[r]<-mean((D1U/D0-mean(D1U/D0))^(4))/(sqrt(mean((D1U/D0-mean(D1U/D0))^(2)))^(4/2))
  
  
  #mean
  mu<-mean(D1L/D0)
  ml<-mean(D1U/D0)
  #au[r]<-mean(D1L/D0)
  #al[r]<-mean(D1U/D0)
  
  #bound second
  su<-mean((D1L/D0)^2)
  sl<-mean((D1U/D0)^2)
  
  #bound third
  tu<-mean((D1L/D0)^3)
  tl<-mean((D1U/D0)^3)
  
  #bound fourth
  fu<-mean((D1L/D0)^4)
  fl<-mean((D1U/D0)^4)
  
  ###bound variance
  bl<-max(0,sl-mu^2)
  bu<-su-ml^2
  
  ###bound central third
  cl<-tl-3*su*mu+3*ml*ml^2-mu^3
  cu<-tu-3*sl*ml+3*mu*mu^2-ml^3
  
  
  ###bound central fourth
  dl<-fl-4*tu*mu+6*sl*ml^2-4*mu*mu^3+ml^4
  du<-fu-4*tl*ml+6*su*mu^2-4*ml*ml^3+mu^4
  
  #mean((D1U/D0)^3)-3*mean((D1U/D0)^2)*mean(D1U/D0)+3*mean((D1U/D0)^1)*mean(D1U/D0)^2-mean(D1U/D0)^3
  #tl^3-3*sl*ml+3*ml*ml^2-ml^3
  
  
  ###bound forth
  #dl<-max(mean(D1^4)-4*mean(D1U^3*D0)+6*mean(D1L^2*D0^2)-4*mean(D1U*D0^3)+mean(D0^4),0)
  #du<-mean(D1^4)-4*mean(D1L^3*D0)+6*mean(D1U^2*D0^2)-4*mean(D1L*D0^3)+mean(D0^4)
  
  
  
  ##var
  #au[r]<-bu
  #al[r]<-bl
  
  #skew
  au[r]<-(cu/(sqrt(bl+0.01)^3))*(as.numeric(cu>=0))+(cu/(sqrt(bu+0.01)^3))*(as.numeric(cu<0))
  al[r]<-(cl/(sqrt(bu+0.01)^3))*(as.numeric(cl>=0))+(cl/(sqrt(bl+0.01)^3))*(as.numeric(cl<0))
  
  #kurt
  #au[r]<-(du/(sqrt(bl+0.01)^3))*(as.numeric(du>=0))+(du/(sqrt(bu+0.01)^3))*(as.numeric(du<0))
  #al[r]<-max(0,(dl/(sqrt(bu+0.01)^3))*(as.numeric(dl>=0))+(dl/(sqrt(bl+0.01)^3))*(as.numeric(dl<0)))
  
}


quantile(au,0.025)
mean(au)
quantile(au,0.975)

quantile(a,0.025)
mean(a)
quantile(a,0.975)

quantile(al,0.025)
mean(al)
quantile(al,0.975)
