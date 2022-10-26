########### Preparation of tesla data
library(dplyr)
library(fGarch)

setwd("C:/Users/neo qi xiang/Desktop/sim_finance/project")
tesla = read.csv("TSLA.csv")
tesla_filter <- subset(tesla, select = c('Date', 'Adj.Close'))
tesla_filter$Date <- as.Date(tesla_filter$Date)
tesla_filter$year <- format(tesla_filter$Date, format = "%Y")

tesla_train <- tesla_filter %>% filter(year <= 2021)
tesla_test <- tesla_filter %>% filter(year > 2021)


dt=1/252
TESLAprices <-as.numeric(as.vector(tesla_train$Adj.Close))
TESLAprices <- TESLAprices[!is.na(TESLAprices)]
n0 =length(TESLAprices)

TESLAlogprices <-log(TESLAprices)

TESLAlogreturns <- TESLAlogprices[2:n0]- TESLAlogprices[1:(n0-1)]
v=mean(TESLAlogreturns)/dt
sigma=sd(TESLAlogreturns)/sqrt(dt)



####### Brownian motion #############

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


new_dt = 1/252
v=mean(TESLAlogreturns)/new_dt
sigma=sd(TESLAlogreturns)/sqrt(new_dt)


ApproxResult<-Binomtreefit(v,sigma,new_dt)
ApproxResult



NonApproxResult<-Binomtreefit(v,sigma,new_dt,FALSE)
NonApproxResult



p=ApproxResult$p; u=ApproxResult$u; d=ApproxResult$d;
SimBinomTree<-function(Nsim,S0,u,d,p,Deltat,T){
  m=T/Deltat # number of periods
  S=matrix(S0,nrow=Nsim,ncol=m+1)
  for(i in 1:Nsim){
    Z<-rbinom(m,1,p)*(u-d)+d
    S[i,2:(m+1)]=S0*cumprod(Z)
  }
  S
}

S0=100;T= 1
set.seed(4518)
binomial_tree<-SimBinomTree(100,S0,u,d,p,new_dt,T)



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
Visualize(binomial_tree)

v;sigma

# 1) to find out date dt, T how to simulate 3 month 6 months and 12 months

####### Geometric brownian motion

SimGBMexact<-function(Nsim,S0,v,sigma,Deltat,T){
  m=T/Deltat # number of periods
  S=matrix(S0,nrow=Nsim,ncol=m+1)
  for(i in 1:Nsim){
    Z<-rnorm(m)
    for(j in 2:(m+1)){
      S[i,j]=S[i,j-1]*exp(v*Deltat+sigma*sqrt(Deltat)*Z[j-1])
    }
  }
  S
}

dt =  1/252
v=mean(TESLAlogreturns)/dt
sigma=sd(TESLAlogreturns)/sqrt(dt)

n0 = length(tesla_train$Adj.Close)


St= tesla_train$Adj.Close[n0]
Nsim=10
set.seed(4518)
SimTesla<-SimGBMexact(Nsim,St,v,sigma,dt,1)
Histdata<-matrix(rep(tesla_train$Adj.Close,Nsim),ncol=n0,byrow=T)
wholedata<-cbind(Histdata,SimTesla)
Visualize(wholedata)


####### Antithetic variates 

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



SimTeslaAV<-SimGBMexactAV(Nsim,St,v,sigma,dt,1,collate=TRUE)
Histdata<-matrix(rep(tesla_train$Adj.Close,2*Nsim),ncol=n0,byrow=T)
wholedataAV <-cbind(Histdata,SimTeslaAV)
Visualize(wholedataAV)




####### stratified sampling

SimGBM1shootSS<-function(StratraNo,NsimS,S0,v,sigma,T){
  V=runif(NsimS)/StratraNo
  for(i in 2:StratraNo){
    V=c(V,(i-1+runif(NsimS))/StratraNo)
  }
  Z=qnorm(V)
  ST=S0*exp(v*T+sigma*sqrt(T)*Z)
  ST
}


SimGBM1shoot<-function(Nsim,S0,v,sigma,T){
  Z=rnorm(Nsim)
  ST=S0*exp(v*T+sigma*sqrt(T)*Z)
  ST
}

SimGBM1shoot(Nsim, S0,v, sigma, T)



####### Monte Carlo
# Parameter estimation
n=length(TESLAprices)-1
dt=1/n

TESLAstdFit<-stdFit(TESLAlogreturns)$par
v=TESLAstdFit["mean"]/dt; sigma=TESLAstdFit["sd"]/sqrt(dt); nu=TESLAstdFit["nu"]
TESLAstdLR<-(TESLAlogreturns-v*dt)/(sigma*sqrt(dt))

# Q-Q Plots (Benchmarking against Normal fitting)
par(mfrow=c(1,2))
qqplot(qt(ppoints(500),df=nu),TESLAstdLR,main="Q-Q plot for t(6.471)")
qqline(TESLAstdLR, distribution = function(p) qt(p,df=nu), probs=c(0.1, 0.6), col=2)
mtext("qqline(*, dist = qt(., df=6.471), prob = c(0.1, 0.6))")

qqnorm(TESLAlogreturns, pch = 1, frame = FALSE)
qqline(TESLAlogreturns, col = 2, lwd = 2)

# Recover Log-returns
R=v*dt+sigma*sqrt(dt)*TESLAlogreturns
matrixR=matrix(R,ncol=10) # Group ten daily log-returns in one row
R10=apply(matrixR,1,sum) # Calculate 10-day log-returns

# Compute VaR and CVaR
alpha=0.01
VaR=-quantile(R10,probs=alpha); VaR

CVaR=VaR+1/(alpha*length(R10))*sum((-R10-VaR)[-R10-VaR>0]); CVaR


#### Suppose that we collect two-year data of DJI. We again use a one-year window to estimate the parameter and 
### compute VaR for the next day. Then we use a rolling window approach to compute all the daily 99% VaR from Sep 20, 2021 
###to Sep 16, 2022. The first estimation window is Sep 18, 2020 - Sep 17, 2021 (Sep 18&19, 2021 are holidays).

par(mfrow=c(1,1))

tesla_prices <- tesla_filter$Adj.Close
n=length(tesla_prices)-1
tesla_lr <-log(tesla_prices[1:n]/tesla_prices[2:(n+1)])


# Define the first estimation window
trainind <-(1:n)[as.Date(tesla_filter[1:n,1],"%b %d, %Y") <= "2021-12-31"] ### All data before 2022
testind <- (1:n)[as.Date(tesla_filter[1:n,1],"%b %d, %Y") > "2021-12-31"]
ntrain=length(trainind) ## 1061 days of data roughly a 4 year
end = trainind[length(trainind)]
dt=1/ntrain # since we use one-year estimation window

alpha=0.01
VaR=rep(0,start-1)


Nsim=1000
set.seed(4518)
count = 1
for(i in 1:length(testind)){
  tesla_train_lr <-tesla_lr[trainind]
  teslastdFit<-stdFit(tesla_train_lr)$par
  v= teslastdFit["mean"]/dt; sigma=teslastdFit["sd"]/sqrt(dt); nu=teslastdFit["nu"]
  Z=rt(Nsim,df=nu)
  R=v*dt+sigma*sqrt(dt)*Z
  VaR[i]=-quantile(R,probs=alpha)
  trainind = seq(trainind[1]+1, testind[count])
  print(head(trainind))
  print(tail(trainind))
  print(length(trainind))
  count= count + 1
}

combinedVaR=c(VaR,rep(NA,ntrain))
plot(rev(tesla_lr), type="l", ylim=c(-max(VaR),max(tesla_lr)))
lines(-rev(combinedVaR), col=2)




