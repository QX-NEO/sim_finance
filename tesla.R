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

v = mean(tesla_train$ln_returns)
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


sigma=sqrt(sig_2)
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





