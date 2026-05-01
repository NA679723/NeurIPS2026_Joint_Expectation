
N<-20
#X<-rbinom(n = N, size = 1, prob = 0.5)
X<-c(rep(0,N/2),rep(1,N/2))
#U<-runif(N,0,1)
U<-sample(c(0,0.5,1),N,c(1/3,1/3,1/3),replace=TRUE)
Y<--(X+1)*(U)

#mean(((sort(subset(Y,X==1))-sort(subset(Y,X==0)))-mean(sort(subset(Y,X==1))-sort(subset(Y,X==0))))^1*as.numeric(((sort(subset(Y,X==1))-sort(subset(Y,X==0)))-mean(sort(subset(Y,X==1))-sort(subset(Y,X==0))))>0))
mean(((sort(subset(Y,X==1))-sort(subset(Y,X==0)))-mean(sort(subset(Y,X==1))-sort(subset(Y,X==0))))^2)
mean(as.numeric(((sort(subset(Y,X==1))-sort(subset(Y,X==0)))-mean(sort(subset(Y,X==1))-sort(subset(Y,X==0))))>0.9))




mean((-U)^(10))

mean((U-mean(U))^(2))
mean(as.numeric((-U-mean(-U))>0.9))
mean((U-mean(U))^(4))/(sqrt(mean((U-mean(U))^(2)))^(4/2))
mean(U*as.numeric(U>0))




Sim<-100
R<-numeric(Sim)
#Ite<-100
Ite<-1000

for(sim in 1:Sim){
  
  
  X<-rbinom(n = N, size = 1, prob = 0.5)
  #X<-c(rep(0,N/2),rep(1,N/2))
  #U<-runif(N,0,1)
  #Y<--X*(U+1)
  
  #U<-rnorm(N,0.5,0.1)
  U<-runif(N,-1,1)
  #U<-rbeta(N,5,5)
  #U<-sample(c(0,0.5,1),N,c(1/3,1/3,1/3),replace=TRUE)
  #Y<--(X+1)*U*as.numeric(X==1)-(X+1)*U*as.numeric(X==0)*as.numeric(U<0)
  
  Y<--(X+1)*U
  
  
  #Y<-2*(X+1)*U^2*as.numeric(X==1)-(X+1)*U^2*as.numeric(X==0)*as.numeric(U>0)
#  Y<--(X+1)*U*as.numeric(X*U>=0)
  #Y<--(X+1)*(U)
  #Y<-(X+1)*(U)
  #Y<-exp(X+1)*(U)
  
  
  a<-0
  b<-0
  
  for(r in 1:Ite){
    
    
    #y_1<-runif(1,-1,4)
    #y_2<-runif(1,-1,4)
    #y_3<-runif(1,-1,4)
    #y_4<-runif(1,-1,4)
    
    
    #y_1<-runif(1,-2,2)
    #y_2<-runif(1,-2,2)
    #y_3<-runif(1,-2,2)
    #y_4<-runif(1,-2,2)
    
    y_1<-runif(1,min(Y),max(Y))
    y_2<-runif(1,min(Y),max(Y))
    y_3<-runif(1,min(Y),max(Y))
    y_4<-runif(1,min(Y),max(Y))
    
    
    
    
    #y_1<-runif(1,-10,10)
    #y_2<-runif(1,-10,10)
    #y_3<-runif(1,-10,10)
    #y_4<-runif(1,-10,10)
    
    #y_1<-runif(1,-1.5,2.5)
    #y_2<-runif(1,-1.5,2.5)
    #y_3<-runif(1,-1.5,2.5)
    #y_4<-runif(1,-1.5,2.5)
    
    
    ##CACE
    #a<-a+max(mean(subset(as.numeric(Y<y_1),X==0))-mean(subset(as.numeric(Y<y_1),X==1)),0)-max(mean(subset(as.numeric(Y<y_1),X==1))-mean(subset(as.numeric(Y<y_1),X==0)),0)
    
    
    ###positive CACE
    #a<-a+max(mean(subset(as.numeric(Y<y_1),X==0))-mean(subset(as.numeric(Y<y_1),X==1)),0)
    ###negative CACE
    #a<-a+max(mean(subset(as.numeric(Y<y_1),X==1))-mean(subset(as.numeric(Y<y_1),X==0)),0)
    
    ###outlier positive CACE
    #theta<-0
    #a<-a+max(mean(subset(as.numeric(Y<y_1-theta),X==0))-mean(subset(as.numeric(Y<y_1),X==1)),0)
    
    ###outlier negative CACE
    #theta<-0.8
    #a<-a+max(mean(subset(as.numeric(Y<y_1-theta),X==1))-mean(subset(as.numeric(Y<y_1),X==0)),0)
    
    #second moment
    #a<-a+max(min(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)))-max(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1))),0)+max(min(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)))-max(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0))),0)
    
    
    #central variance
    #a<-a+max(min(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)))-max(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1))),0)+max(min(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)))-max(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0))),0)
    
    
    
    ###variance(lower)
    #a<-a+(max(sum(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)))-sum(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)))-1,0)+max(sum(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)))-sum(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)))-1,0))
    ###variance(upper)
    #a<-a+(min(min(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0))),1-max(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1))))+min(min(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1))),1-max(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)))))
    
    
    ###second momente(lower)
    #a<-a+(max(sum(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)))-sum(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)))-1,0)+max(sum(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)))-sum(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)))-1,0))
    ###second moment(upper)
    #a<-a+(min(min(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0))),1-max(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1))))+min(min(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1))),1-max(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)))))
    
    
    ###third moment
    #a<-a+max(min(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0)))-max(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1))),0)-max(min(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1)))-max(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0))),0)
    
    ##central third moment
    #b<-b+max(min(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0)))-max(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1))),0)-max(min(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1)))-max(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0))),0)
    
    
    ##central fourth moment
    #b<-b+max(min(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_4),X==0)))-max(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_4),X==1))),0)+max(min(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_4),X==1)))-max(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_4),X==0))),0)
    
    ##fourth moment
    a<-a+max(min(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0)),mean(subset(as.numeric(Y<y_4),X==0)))-max(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1)),mean(subset(as.numeric(Y<y_4),X==1))),0)+max(min(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1)),mean(subset(as.numeric(Y<y_4),X==1)))-max(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0)),mean(subset(as.numeric(Y<y_4),X==0))),0)
    
    
    
    
    ###skewness(lower)
    #b<-b+(max(sum(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0)))-sum(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1)))-2,0)-min(min(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1))),1-max(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0)))))
    ###skewness(Upper)
    #b<-b+(min(min(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0))),1-max(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1))))-max(sum(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1)))-sum(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0)))-2,0))
    
    
    ###third moment(lower)
    #a<-a+(max(sum(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0)))-sum(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1)))-2,0)-min(min(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1))),1-max(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0)))))
    ###third moment(Upper)
    #a<-a+(min(min(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0))),1-max(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1))))-max(sum(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1)))-sum(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0)))-2,0))
    
    
    ###kurtosis(lower)
    #b<-b+(max(sum(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_4),X==0)))-sum(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_4),X==1)))-3,0)+max(sum(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_4),X==1)))-sum(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_4),X==0)))-3,0))
    ###kurtosis(Upper)
    #b<-b+(min(min(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_4),X==0))),1-max(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_4),X==1))))+max(sum(mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_1),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_2),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_3),X==1)),mean(subset(as.numeric(Y-mean(subset(Y,X==1))<y_4),X==1)))-sum(mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_1),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_2),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_3),X==0)),mean(subset(as.numeric(Y-mean(subset(Y,X==0))<y_4),X==0)))-3,0))
    
    
    
    ###fourth moment(lower)
    #a<-a+(max(sum(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0)),mean(subset(as.numeric(Y<y_4),X==0)))-sum(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1)),mean(subset(as.numeric(Y<y_4),X==1)))-3,0)+max(sum(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1)),mean(subset(as.numeric(Y<y_4),X==1)))-sum(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0)),mean(subset(as.numeric(Y<y_4),X==0)))-3,0))
    ###fourth moment(Upper)
    #a<-a+(min(min(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0)),mean(subset(as.numeric(Y<y_4),X==0))),1-max(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1)),mean(subset(as.numeric(Y<y_4),X==1))))+min(min(mean(subset(as.numeric(Y<y_1),X==1)),mean(subset(as.numeric(Y<y_2),X==1)),mean(subset(as.numeric(Y<y_3),X==1)),mean(subset(as.numeric(Y<y_4),X==1))),1-max(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_2),X==0)),mean(subset(as.numeric(Y<y_3),X==0)),mean(subset(as.numeric(Y<y_4),X==0)))))
    
    
    #max
    #a<-a+(1-min(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_1),X==1))))
    #min
    #a<-a+(1-max(mean(subset(as.numeric(Y<y_1),X==0)),mean(subset(as.numeric(Y<y_1),X==1))))
    
  }
  
  
  #N0<-length(subset(Y,X==0))
  #N1<-length(subset(Y,X==1))
  #D<-numeric(N0)
  #D<-sort(Y1)-sort(Y0)
  #for(i in 1:N0){
  #  D[i]<-sort(subset(Y,X==1))[floor(N1*((i-1)/N0))+1]-sort(subset(Y,X==0))[i]
  #}
  
  
  
  ##SOS
  #a<-mean(((sort(subset(Y,X==1))-sort(subset(Y,X==0))))^2)
  #a<-mean(((sort(subset(Y,X==1))-sort(subset(Y,X==0))))^3)
  #a<-mean(((sort(subset(Y,X==1))-sort(subset(Y,X==0))))^4)
  #a<-mean(((sort(subset(Y,X==1))-sort(subset(Y,X==0)))-mean(sort(subset(Y,X==1))-sort(subset(Y,X==0))))^2)
  #a<-mean(((sort(subset(Y,X==1))-sort(subset(Y,X==0)))-mean(sort(subset(Y,X==1))-sort(subset(Y,X==0))))^3)/(mean(((sort(subset(Y,X==1))-sort(subset(Y,X==0)))-mean(sort(subset(Y,X==1))-sort(subset(Y,X==0))))^2)^(3/2))
  #a<-mean(((sort(subset(Y,X==1))-sort(subset(Y,X==0)))-mean(sort(subset(Y,X==1))-sort(subset(Y,X==0))))^4)/(mean(((sort(subset(Y,X==1))-sort(subset(Y,X==0)))-mean(sort(subset(Y,X==1))-sort(subset(Y,X==0))))^2)^(4/2))
  #a<-mean((sort(subset(Y,X==1))/sort(subset(Y,X==0)))^2)
  #R[sim]<-a
  
  #a<-mean(D^2)
  #a<-mean(D^3)
  #a<-mean(D^4)
  #R[sim]<-a
  
  #a/10000*(2^3)
  #R[sim]<-(a/Ite)*(4)
  #R[sim]<-a/Ite*(4^2)
  #R[sim]<-(b/Ite*(4^3))
  #R[sim]<-(b/Ite*(4^3))/((a/Ite*(4^2)+0.000000000000000001)^(3/2))
  #R[sim]<-(b/Ite*(4^4))/((a/Ite*(4^2)+0.0000000000000000001)^(4/2))
  #a/Ite*(4)
  
  
  #a/10000*(2^3)
  #R[sim]<-(a/Ite)*(4)
  #R[sim]<-a/Ite*(20^4)
  R[sim]<-a/Ite*((max(Y)-min(Y))^4)
  #R[sim]<-a/Ite*(3^3)
  #R[sim]<-a/Ite*(3^4)
  #R[sim]<-(b/Ite*(4^3))
  #R[sim]<-(b/Ite*(4^3))/((a/Ite*(4^2)+0.000000000000000001)^(3/2))
  #R[sim]<-(b/Ite*(4^4))/((a/Ite*(4^2)+0.0000000000000000001)^(4/2))
  #a/Ite*(4)
  
  #R[sim]<-(b/Ite*(4^3))/((a/Ite*(4^2)+0.01)^(3/2))
  #R[sim]<-(b/Ite*(4^3))/((a/Ite*(4^2)+0.01)^(4/2))
  
  
  #R[sim]<-(a/Ite)*(10)
  
  
  #R[sim]<-(a/Ite)*(4)
  #R[sim]<-a/Ite*(6^2)
  #R[sim]<-(b/Ite*(6^3))
  #R[sim]<-(b/Ite*(6^3))/((a/Ite*(6^2)+0.000000000000001)^(3/2))
  #R[sim]<-(b/Ite*(2.5^4))/((a/Ite*(2.5^2)+0.000000000000001)^(4/2))
  
  
  #R[sim]<-a
  
}



#R1<-R[R!=0]
#R<-R1

quantile(R,0.025)
mean(R)
quantile(R,0.975)




#U<-runif(100000,0,1)
U<-sample(c(0,0.5,1),100000,c(1/3,1/3,1/3),replace=TRUE)
Y1<--2*(U)
Y0<--1*(U)
U<-runif(1000000,-1,1)
Y1<--2*(U)*as.numeric(1*U>=0)+0*(U)*as.numeric(U<0)
Y0<--1*(U)*as.numeric(0*U>=0)+0*(U)*as.numeric(U<0)
mean((Y1-Y0)^2)
mean((Y1-Y0)^3)
mean((Y1-Y0)^4)
mean((Y1-Y0-mean(Y1)+mean(Y0))^2)
mean((Y1-Y0-mean(Y1)+mean(Y0))^3)
mean((Y1-Y0-mean(Y1)+mean(Y0))^3)/((mean((Y1-Y0-mean(Y1)+mean(Y0))^2))^(3/2))
mean((Y1-Y0-mean(Y1)+mean(Y0))^4)/((mean((Y1-Y0-mean(Y1)+mean(Y0))^2))^2)



mean(max(Y1-Y0-theta,0)*as.numeric(Y1>Y0+theta))
mean(max(Y0-Y1-theta,0)*as.numeric(Y0>Y1+theta))
mean(Y1/Y0)
mean((Y1/Y0)^2)



U<-runif(100000,0,1)
Y1<-exp(2)*(U)
Y0<-exp(1)*(U)
mean(apply(cbind(Y1,Y0),1,max))
mean(apply(cbind(Y1,Y0),1,min))
