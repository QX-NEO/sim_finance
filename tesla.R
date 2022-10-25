library(dplyr)

setwd("C:/Users/neo qi xiang/Desktop/sim_finance/project")
tesla = read.csv("TSLA.csv")
tesla_filter <- subset(tesla, select = c('Date', 'Adj.Close'))
tesla_filter$Date <- as.Date(tesla_filter$Date)
tesla_filter$year <- format(tesla_filter$Date, format = "%Y")

tesla_train <- tesla_filter %>% filter(year <= 2021)
#tesla_test <- tesla_filter %>% filter(year > 2020)
tesla_train$lag_year <- lag(tesla_train$Adj.Close,252)
tesla_train$returns <- tesla_train$Adj.Close/tesla_train$lag_year

tesla_train <- tesla_train %>% filter(!is.na(returns))

tesla_train$ln_returns <- log(tesla_train$returns)

v = mean(tesla_train$ln_returns)/(1/252)
sig_2<- var(tesla_train$ln_returns)



Binomtreefit<-function(v,sigma,Deltat,approx=TRUE){
  if(approx){
    p=1/2+v*sqrt(Deltat)/sigma/2
    u=exp(sigma*sqrt(Deltat))
    d=1/u
  } else{
    p=1/2+v*Deltat/(2*sqrt((v*Deltat)^2+sigma^2*Deltat))
    u=exp(sqrt((v*Deltat)^2+sigma^2*Deltat))
    d=1/u
  }
  list(p=p,u=u,d=d)
}


sigma= sqrt(sig_2)
dt=1
ApproxResult<-Binomtreefit(v,sigma,dt)
ApproxResult



sigma=sqrt(sig_2)
dt=1
NonApproxResult <- Binomtreefit(v,sigma,dt)
NonApproxResult 



mu <- mean(tesla_train$returns)
sigma <- sqrt(var(tesla_train$returns))
s0 <- tail(tesla_train,1)$Adj.Close

SimGBMsde<-function(Nsim,S0,mu,sigma,Deltat,T){
  m=T/Deltat # number of periods
  S=matrix(S0,nrow=Nsim,ncol=m+1)
  for(i in 1:Nsim){
    Z<-rnorm(m)
    for(j in 2:(m+1)){
      S[i,j]=S[i,j-1]+S[i,j-1]*(mu*Deltat+sigma*sqrt(Deltat)*Z[j-1])
    }
  }
  S
}

Visualize<-function(S){
  # endindex=ncol(S)
  minS=min(S);maxS=max(S) # the y-limits of the plot
  noS<-nrow(S)
  cl<-rainbow(noS) # vector of rainbow colors
  plot(S[1,],type="l",ylim=c(minS,maxS),col=cl[1])
  if(noS>1){
    for(i in 2:noS){
      lines(S[i,],col=cl[i])
    }
  }
}

S0=s0; sigma=0.4; T=1; dt=1/252;
v=mu-sigma^2/2
set.seed(4518)
SimGBM<-SimGBMsde(100,S0,v,sigma,dt,T)

tesla_prices<-as.numeric(as.vector(tesla_train$Adj.Close))
tesla_prices<-tesla_prices[!is.na(tesla_prices)]
n0=length(tesla_prices)
St=tesla_prices[n0]
sim_tesla<-SimGBMsde(100,St,v,sigma,dt,1)
Histdata<-matrix(rep(tesla_prices,100),ncol=n0,byrow=T)
wholedata<-cbind(Histdata,sim_tesla)
par(mar=c(1,1,1,1))
Visualize(wholedata)

SimGBMexactAV<-function(Nsim,S0,v,sigma,Deltat,T,collate=FALSE){
  m=T/Deltat # number of periods
  S=matrix(S0,nrow=Nsim,ncol=m+1)
  Stilde=matrix(S0,nrow=Nsim,ncol=m+1)
  for(i in 1:Nsim){
    Z<-rnorm(m)
    for(j in 2:(m+1)){
      S[i,j]=S[i,j-1]*exp(v*Deltat+sigma*sqrt(Deltat)*Z[j-1])
      Stilde[i,j]=Stilde[i,j-1]*exp(v*Deltat+sigma*sqrt(Deltat)*(-Z[j-1]))
    }
  }
  if(collate){
    out=matrix(0,2*Nsim,ncol=m+1)
    for(i in 1:Nsim){
      out[(2*i-1),]=S[i,]
      out[2*i,]=Stilde[i,]
    }
    return(out)
  } else{return(rbind(S,Stilde))}
}

Nsim=100
SimTSLAAV<-SimGBMexactAV(Nsim,St,v,sigma,dt,1,collate=TRUE)
Visualize((SimTSLAAV))











dt=1/252

TESLAprices <-as.numeric(as.vector(tesla_train$Adj.Close))
TESLAprices <- TESLAprices[!is.na(TESLAprices)]
n0 =length(TESLAprices)

TESLAlogprices <-log(TESLAprices)

TESLAlogreturns <- TESLAlogprices[2:n0]- TESLAlogprices[1:(n0-1)]
v=mean(TESLAlogreturns)/dt
sigma=sd(TESLAlogreturns)/sqrt(dt)














