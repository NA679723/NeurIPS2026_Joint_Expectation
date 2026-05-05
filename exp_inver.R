library(tictoc)

N<-20
Sim<-1
RU<-numeric(Sim)
RL<-numeric(Sim)

tic()

for(sim in 1:Sim){
  
  
  X<-rbinom(n = N, size = 1, prob = 0.5)
 # U<-runif(N,0,1)
  U<-sample(c(-1, 0, 1), N, replace = TRUE, prob = c(1/3, 1/3, 1/3))
  #E<-runif(N,-1,1)
  #U<-runif(N,0,1)
  #Y<--(X+1)*U*as.numeric(X==1)-(X+1)*U*as.numeric(X==0)*as.numeric(U<0)
  
  #Y<--(X+1)*U
  
  #Y<-2*(X+1)*U^2*as.numeric(X==1)-(X+1)*U^2*as.numeric(X==0)*as.numeric(U>0)
  #Y<--(X+1)*U^2*as.numeric(X*U>=0)
  #Y<-(5*X+1)*U*as.numeric(X==1)+(5*X+1)*U*as.numeric(X==0)*as.numeric(U>0.75)+2
  #Y<-(5*X+1)*U^2*as.numeric(X==1)+(5*X+1)*U^2*as.numeric(X==0)*as.numeric(U>0.75)+2+E-3*as.numeric(X==1)
  
  Y<-(X+1)+0*U
  
  
  
  a<-0
  b<-0
  
  N0<-length(subset(Y,X==0))
  N1<-length(subset(Y,X==1))
  D1U<-numeric(N0)
  D1L<-numeric(N0)
  
  
  D0<-subset(Y,X==0)
  D1<-subset(Y,X==1)
  
  for(i in 1:N0){
    #u<-runif(1,0,1)
    u<-0
    D1U[i]<-quantile(subset(Y,X==1),u*(mean(subset(Y,X==0)<=subset(Y,X==0)[i])-mean(subset(Y,X==0)<subset(Y,X==0)[i]))+mean(subset(Y,X==0)<subset(Y,X==0)[i]),type = 1)
    D1L[i]<-quantile(subset(Y,X==1),1-(1-u)*(mean(subset(Y,X==0)<=subset(Y,X==0)[i])-mean(subset(Y,X==0)<subset(Y,X==0)[i]))-mean(subset(Y,X==0)<subset(Y,X==0)[i]),type = 1)
  }
  
  mean((D1U<(0.5))&(D0<(0.03)))
  mean((D1L<(0.5))&(D0<(0.03)))
  
  plot(D0,D1U)
  plot(D0,D1L)
  
  
  #RU[sim]<-mean(apply(cbind(D1U,D0),1,max))
  #RL[sim]<-mean(apply(cbind(D1L,D0),1,max))
  
  RU[sim]<-mean((D1U-D0)^(2))
  RL[sim]<-mean((D1L-D0)^(2))
  
  #RU[sim]<-mean((D1U-D0+4)^(-3))
  #RL[sim]<-mean((D1L-D0+4)^(-3))
  
  #RU[sim]<-mean((D1U/D0)^2)
  #RL[sim]<-mean((D1L/D0)^2)
  
  #RU[sim]<-mean((D1U*D0^3))
  #RL[sim]<-mean((D1L*D0^3))
  
  #RU[sim]<-mean(D1^2)-2*mean(D1L*D0)+mean(D0^2)
  #RL[sim]<-mean(D1^2)-2*mean(D1U*D0)+mean(D0^2)
  
  #RU[sim]<-mean(D1^3)-3*mean(D1L^2*D0)+3*mean(D1U*D0^2)-mean(D0^3)
  #RL[sim]<-mean(D1^3)-3*mean(D1U^2*D0)+3*mean(D1L*D0^2)-mean(D0^3)
  
  
  #RU[sim]<-mean(D1^4)-4*mean(D1L^3*D0)+6*mean(D1U^2*D0^2)-4*mean(D1L*D0^3)+mean(D0^4)
  #RL[sim]<-max(mean(D1^4)-4*mean(D1U^3*D0)+6*mean(D1L^2*D0^2)-4*mean(D1U*D0^3)+mean(D0^4),0)
  
  
}

toc()

quantile(RU,0.025)
mean(RU)
quantile(RU,0.975)

quantile(RL,0.025)
mean(RL)
quantile(RL,0.975)



#U<-runif(100,-1,1)
U<-sample(c(-1, 0, 1), 100000, replace = TRUE, prob = c(1/3, 1/3, 1/3))
Y1<--(1+1)*U*as.numeric(1==1)-(1+1)*U*as.numeric(1==0)*as.numeric(U<0)
Y0<--(0+1)*U*as.numeric(0==1)-(0+1)*U*as.numeric(0==0)*as.numeric(U<0)
mean((Y1-Y0)^2)
mean(apply(cbind(Y1,Y0),1,max))


#U<-runif(100000,-1,1)
U<-sample(c(-1, 0, 1), 100000, replace = TRUE, prob = c(1/3, 1/3, 1/3))
Y1<--(1+1)*U*as.numeric(1==1)
Y0<--(0+1)*U*as.numeric(0==0)*as.numeric(U<0)
mean((Y1-Y0)^2)

U<-runif(1000,-1,1)
Y1<--(1+1)*U
Y0<--(0+1)*U
mean((Y1-Y0)^4)
mean(apply(cbind(Y1,Y0),1,max))


U<-runif(100,0,1)
Y1<-(5*1+1)*U*as.numeric(1==1)+(5*1+1)*U*as.numeric(1==0)*as.numeric(U>0.75)+2
Y0<-(5*0+1)*U*as.numeric(0==1)+(5*0+1)*U*as.numeric(0==0)*as.numeric(U>0.75)+2
mean((Y1/Y0)^2)




#U<-runif(100000,0,1)
#E<-runif(100000,-1,1)
U<-sample(c(-1, 0, 1), 100000, replace = TRUE, prob = c(1/3, 1/3, 1/3))
#Y1<-(5*1+1)*U^2*as.numeric(1==1)+(5*1+1)*U^2*as.numeric(1==0)*as.numeric(U>0.75)+2+E-3
#Y0<-(5*0+1)*U^2*as.numeric(0==1)+(5*0+1)*U^2*as.numeric(0==0)*as.numeric(U>0.75)+2+E
Y1<-(1+1)+U
Y0<-(0+1)+U
mean((Y1-Y0)^2)
mean((Y1/Y0)^3)
mean((Y1-Y0+4)^(-3))



U<-runif(100000,-1,1)
E<-runif(100000,-1,1)
Y1<-2*(1+1)*U^2*as.numeric(1==1)-(1+1)*U^2*as.numeric(1==0)*as.numeric(U>0)+E
Y0<-2*(0+1)*U^2*as.numeric(0==1)-(0+1)*U^2*as.numeric(0==0)*as.numeric(U>0)+E
#mean((Y1*Y0^2))
mean((Y1-Y0)^2) 
mean(apply(cbind(Y1,Y0),1,min))

