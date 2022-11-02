library(dplyr)


setwd("/home/nic/repos/group")
set.seed(4518)
########### Preparation of tesla data

tesla = read.csv("TSLA.csv")
tesla_filter <- subset(tesla, select = c('Date', 'Adj.Close'))
tesla_filter$Date <- as.Date(tesla_filter$Date)
tesla_filter$year <- format(tesla_filter$Date, format = "%Y")
tesla_filter = tesla_filter %>% filter(Date <= as.Date('2022-01-12'))
tesla_filter = tesla_filter %>% filter(Date >= as.Date('2021-01-12'))

tesla_filter$actual_close = tesla_filter$Adj.Close * 3

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

SimBinomTree<-function(N,S0,u,d,p,Deltat,T){
  m=T/Deltat # number of periods
  S=matrix(S0,nrow=N,ncol=m+1)
  for(i in 1:N){
    Z<-rbinom(m,1,p)*(u-d)+d
    S[i,2:(m+1)]=S0*cumprod(Z)
  }
  S
}

n = length(tesla_filter$actual_close) -1
tesla_lr <-  log(tesla_filter$actual_close[2:n+1]) - log(tesla_filter$actual_close[1:n])

dt=1/252
v=mean(tesla_lr)
sigma=sd(tesla_lr)
v; sigma

# approx
udp<-Binomtreefit(v,sigma,dt)

u=udp$u; d=udp$d; p=udp$p
T=1; dt=1/252
N=10000
S0=tesla_filter$actual_close[1]
tesla_sbt <-SimBinomTree(N,S0,u,d,p,dt,T)
tail(tesla_filter)
# as.Date('2022-10-13') - as.Date('2023-01-12')

# exact
udp<-Binomtreefit(v,sigma,dt,F)

u=udp$u; d=udp$d; p=udp$p
T=1; dt=1/252
N=10000
S0=1106.22 #tesla_filter$actual_close[1]
tesla_esbt <-SimBinomTree(N,S0,u,d,p,dt,T)

# gbm exact
SimGBMexact<-function(N,S0,v,sigma,Deltat,T){
  m=T/Deltat # number of periods
  S=matrix(S0,nrow=N,ncol=m+1)
  for(i in 1:N){
    Z<-rnorm(m)
    for(j in 2:(m+1)){
      S[i,j]=S[i,j-1]*exp(v*Deltat+sigma*sqrt(Deltat)*Z[j-1])
    }
  }
  S
}

tesla_egbm<-SimGBMexact(N,S0,v,sigma,dt,T)

visualize<-function(S){
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

n0=length(tesla_filter$actual_close)
histdata<-matrix(rep(tesla_filter,N),ncol=n0,byrow=T)
wholedata<-cbind(histdata,tesla_egbm)
visualize(wholedata)

visualize(tesla_egbm)
dim(tesla_egbm)
mean(tesla_egbm[,253])
sd(tesla_egbm[,253])
quantile(tesla_egbm[,253], .05)
quantile(tesla_egbm[,253], .95)

visualize(tesla_sbt)
dim(tesla_sbt)
mean(tesla_sbt[,253])
sd(tesla_sbt[,253])
quantile(tesla_sbt[,253], .05)
quantile(tesla_sbt[,253], .95)


visualize(tesla_esbt)
dim(tesla_esbt)
mean(tesla_esbt[,253])
sd(tesla_esbt[,253])
quantile(tesla_esbt[,253], .05)
quantile(tesla_esbt[,253], .95)

####### Antithetic variates 

SimGBMexactAV<-function(N,S0,v,sigma,Deltat,T,collate=FALSE){
  m=T/Deltat # number of periods
  S=matrix(S0,nrow=N,ncol=m+1)
  Stilde=matrix(S0,nrow=N,ncol=m+1)
  for(i in 1:N){
    Z<-rnorm(m)
    for(j in 2:(m+1)){
      S[i,j]=S[i,j-1]*exp(v*Deltat+sigma*sqrt(Deltat)*Z[j-1])
      Stilde[i,j]=Stilde[i,j-1]*exp(v*Deltat+sigma*sqrt(Deltat)*(-Z[j-1]))
    }
  }
  if(collate){
    out=matrix(0,2*N,ncol=m+1)
    for(i in 1:N){
      out[(2*i-1),]=S[i,]
      out[2*i,]=Stilde[i,]
    }
    return(out)
  } else{return(rbind(S,Stilde))}
}

tesla_eav<-SimGBMexactAV(N,S0,v,sigma,dt,1,collate=TRUE)
visualize(tesla_eav)

print_stats = function(mtx){
  m=mean(mtx[,253])
  s=sd(mtx[,253])
  q5=quantile(mtx[,253], .05)
  q95=quantile(mtx[,253], .95)
  return(c(m,s,q5,q95))
}

print_stats(tesla_eav)



par(mfrow=c(1,1))

# Define the first estimation window
trainind <-(1:n)[as.Date(tesla_filter[1:n,1],"%b %d, %Y") <= "2022-01-12"] ### All data before 2022
trainind <-(1:n)[as.Date(tesla_filter[1:n,1],"%b %d, %Y") > "2021-01-12"]
testind <- (1:n)[as.Date(tesla_filter[1:n,1],"%b %d, %Y") > "2022-01-12"]
ntrain=length(trainind) ## 1061 days of data roughly a 4 year
end = trainind[length(trainind)]
dt=1/ntrain # since we use one-year estimation window

start=trainind[1]
alpha=0.01
VaR=rep(0,start-1)



count = 1
for(i in 1:length(testind)){
  tesla_train_lr <-tesla_lr[trainind]
  teslastdFit<-stdFit(tesla_train_lr)$par
  v= teslastdFit["mean"]/dt; sigma=teslastdFit["sd"]/sqrt(dt); nu=teslastdFit["nu"]
  Z=rt(N,df=nu)
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

