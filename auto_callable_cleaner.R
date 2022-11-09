# 1) 2) 3) 5) follows todo.jpg
# load or save rds variables with
# saveRDS(sim_matrix, file = "Rds/sim_matrix-avregbm10k.Rds")
# sim_matrix <- readRDS(file = "Rds/sim_matrix-regbm10k.Rds")
# Rds/README.md explains naming convention

library(dplyr)
library(fGarch)
setwd("C:/Users/neo qi xiang/Desktop/sim_finance/sim_finance_project")

tesla <- read.csv("TSLA.csv")
df <- subset(tesla, select = c("Date", "Adj.Close"))
df$Date <- as.Date(df$Date)
df$year <- format(df$Date, format = "%Y")
df$actual_close <- df$Adj.Close * 3

# 253rd day is 2022-01-12 with actual_close 1106.22
df <- df %>% filter(Date > "2021-01-12")
df[253, ]

get_s0_lr <- function(df, start, window_size) {
    values <- c()
    end <- start + window_size - 1
    if (end > length(df$actual_close)) {
        print("window < 252")
        print(tail(window))
    }
    window <- df[start:end, ]$actual_close
    n <- length(window)
    s0 <- window[n]
    lr <- log(window[2:n]) - log(window[1:(n - 1)])
    values$s0 <- s0
    values$lr <- lr
    return(values)
}
get_s0_lr(df, 1, 3)

get_v_sigma <- function(lr) {
    values <- c()
    values$v <- mean(lr)
    values$sigma <- sd(lr)
    return(values)
}

get_gbm_s1 <- function(s0, v, sigma) {
    values <- c()
    delta_t <- 1
    z <- rnorm(1)
    values$s1 <- s0 * exp(v * delta_t + sigma * sqrt(delta_t) * z)
    values$s1_av <- s0 * exp(v * delta_t + sigma * sqrt(delta_t) * -z)
    return(values)
}

# window_size is 252
window_size <- length((df %>% filter(Date > "2021-01-12")
                          %>% filter(Date < "2022-01-12"))
                          $actual_close)
# on +192nd day, not enough real actual_close to have window_size 252
days_to_simulate <- length(df$actual_close) - window_size + 1
num_simulations <- 10000
initial_level <- 1106.22
barrier <- initial_level / 2
first_observation <- 252 / 4 * 2
second_observation <- 252 / 4 * 3
conversion_ratio <- 4.5199

# 3) rolling window gbm
sim_matrix <- c()
for (j in 1:num_simulations) { # N simulations
    print(paste("simulation", j))
    simulations <- c()
    for (i in 1:days_to_simulate) {
        values <- get_s0_lr(df, i, window_size)
        s0 <- values$s0
        lr <- values$lr
        values <- get_v_sigma(lr)
        v <- values$v
        sigma <- values$sigma
        s1 <- get_gbm_s1(s0, values$v, values$sigma)$s1
        simulations <- c(simulations, s1)
    }
    sim_matrix <- rbind(sim_matrix, simulations)
}




visualize <- function(simulations) {
    min_s <- min(simulations)
    max_s <- max(simulations)
    num_s <- nrow(simulations)
    cl <- rainbow(num_s) # vector of rainbow colors
    plot(simulations[1, ], type = "l", ylim = c(min_s, max_s), col = cl[1])
    if (num_s > 1) {
        for (i in 2:num_s) {
            print(paste("plotting", i))
            lines(simulations[i, ], col = cl[i])
    abline(h = barrier)
    abline(v = first_observation)
    abline(v = second_observation)
    abline(h = initial_level)
    }
  }
}
visualize(sim_matrix)



# 2) no rolling window gbm
sim_matrix <- c()
for (j in 1:num_simulations) { # N simulations
    print(paste("simulation", j))
    simulations <- c()
    for (i in 1:days_to_simulate) {
        values <- get_s0_lr(df, 1, window_size)
        s0 <- values$s0
        lr <- values$lr
        values <- get_v_sigma(lr)
        v <- values$v
        sigma <- values$sigma
        s1 <- get_gbm_s1(s0, values$v, values$sigma)$s1
        simulations <- c(simulations, s1)
    }
    sim_matrix <- rbind(sim_matrix, simulations)
}
visualize(sim_matrix)

# for 1 unit of product i.e 5000 denomination
get_early_redemption <- function(simulations, denomination) {
    redemption <- 0
    if (simulations[first_observation] >= initial_level) {
        # 100% denomination + coupon
        redemption <- denomination * (1 + .1325 / 4 * 2)
    }
    else if (simulations[second_observation] >= initial_level) {
        # 100% denomination + coupon
        redemption <- denomination * (1 + .1325 / 4 * 3)
    }
    return(redemption)
}

get_redemption <- function(simulations, denomination) {
    redemption <- 0
    break_barrier <- min(simulations) <= barrier
    # supposed to be 2023-01-17 but we only have data up to 2022-10-13
    # here i am taking 2022-10-13 to be final_fixing_date
    final_fixing_date <- length(simulations)

    # a)
    if (!(break_barrier) || simulations[final_fixing_date] >= initial_level) {
        redemption <- denomination * (1.1325)
    }
    # c)
    else if (simulations[final_fixing_date] == 0) {
        redemption <- denomination * (.1325)
    }
    # b) but not too sure on fractional shares
    else if (break_barrier && simulations[final_fixing_date] < initial_level) {
        redemption <- denomination * (.1325) + denomination / conversion_ratio
    }
    else {
        print("should not reach here")
    }
    return(redemption)
}

# 1) product study
# i only found 2 cases in simulated data
# a) and b)
for (i in 1:num_simulations) {
    redemption <- get_early_redemption(sim_matrix[i, ],denomination = 5000)
    if (redemption == 0) {
        redemption <- get_redemption(sim_matrix[i, ],denomination = 5000)
    }
    print(redemption)
    redemptions <- c(redemptions, redemption)
}



pay_off <- function(simulations, initial_level = 1106.22, 
                    denomination = 5000,
                    first_observation = (252/4)*2,
                    second_obeservation = (252/4)*3,
                    conversion_ratio = 4.5199
                    ){
  
  Notepay <- 0
  scenario <- 0
  break_barrier <- min(simulations) <= barrier
  final_fixing_date <- length(simulations)
  maturity = simulations[final_fixing_date]
  
  
  if(!break_barrier){
    
    if (simulations[first_observation] >= initial_level) {
      Notepay <- denomination * (1 + .1325 / 4 * 2)
      scenario <-1 
    }
    
    else if (simulations[second_observation] >= initial_level) {
      Notepay <- denomination * (1 + .1325 / 4 * 3)
      scenario <-2
    }
    else{
      Notepay <- denomination * (1 + .1325)
      scenario <-3
      
    }
    
  }
  
  else{
    
    if (simulations[final_fixing_date] < initial_level) {
      Notepay <- denomination * (.1325) + maturity * conversion_ratio
      scenario <- 4
    }
    else{
      
      Notepay <- denomination * (1 + .1325)
      scenario <-3
    }
    
  }
  
  #return(list(NotePay= Notepay, Scenario=scenario))
  return(Notepay)

  
}


# NotePayoff<-function(Spath,Initial=28523.35,Parti=1.57,Denom=1000){
#   T=length(Spath)
#   if(Spath[T]>=Initial){
#     NotePay=Denom*(1+Parti*(Spath[T]/Initial-1))
#     # Scenario=1
#   } else{
#     NotePay=Denom*Spath[T]/Initial
#     Scenario=2### 5.3. Payoff function
#   }
#   NotePay
#   # list(NotePay=NotePay, Scenario=Scenario)












# 5) antithetic gbm, show something?
sim_matrix <- c()
for (j in 1:num_simulations) { # N simulations
    print(paste("simulation", j))
    simulations <- c()
    for (i in 1:days_to_simulate) {
        values <- get_s0_lr(df, i, window_size)
        s0 <- values$s0
        lr <- values$lr
        values <- get_v_sigma(lr)
        v <- values$v
        sigma <- values$sigma
        values <- get_gbm_s1(s0, values$v, values$sigma)
        s1 <- values$s1
        s1_av <- values$s1_av
        simulations <- c(simulations, s1)
        simulations <- c(simulations, s1_av)
    }
    sim_matrix <- rbind(sim_matrix, simulations)
}
visualize(sim_matrix)



#### calculate mean of the paths and statics


gbm_pred_mean = colMeans(sim_matrix)
actual<- df %>% filter(Date >= as.Date("2022-01-11"))
error  <- actual$actual_close - gbm_pred_mean


plot(x = actual$Date, y = actual$actual_close, type ='l', col = 'blue')
lines(actual$Date, gbm_pred_mean, col="red", type = 'l')
title("Actual VS Predicted GBM")

plot(x = actual$Date, y = actual$actual_close - gbm_pred_mean, type ='l', col = 'blue')
#lines(actual$Date, gbm_pred_mean, col="red")
title("Error")


#### AV GBM 

sim_matrix_av <- readRDS(file = "Rds/sim_matrix-avregbm10k.Rds")
visualize(sim_matrix_av)

gbm_av_pred_mean <- colMeans(sim_matrix_av)


plot(x = actual$Date, y = actual$actual_close, type ='l', col = 'blue')
lines(actual$Date, gbm_av_pred_mean, col="red", type = 'l')
title("Actual VS Predicted GBM AV")

plot(x = actual$Date, y = actual$actual_close - gbm_av_pred_mean, type ='l', col = 'blue')
#lines(actual$Date, gbm_pred_mean, col="red")
title("Error")

error_av <- actual$actual_close - gbm_av_pred_mean



####### Value at risk


train_len = dim(df %>% filter(Date <= as.Date("2022-01-12")) %>% select(Date))[1]
n = length(df$Date)-1
trainind <-(1: train_len)
testind <- ((train_len+1):n)
ntrain= train_len
end = train_len
dt= 1/ntrain # since we use one-year estimation window

alpha=0.05
VaR=rep(0,length(testind))


tesla_lr <- get_s0_lr(df,start = 1,window_size =length(df$Date))$lr
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
plot(df$Date[-1],rev(tesla_lr), type="l", ylim=c(-max(VaR),max(tesla_lr)))
lines(df$Date[-1], -rev(combinedVaR), col=2)
title("Value at risk")






###### estimated probabilities under risk neutral

Nsim=1000
set.seed(4518)
St=HSCEprices[n0]
St <- 1106.22
dt = 1/252
v = mean(tesla_lr[1:252])/dt
sigma = sd(tesla_lr[1:252])/sqrt(dt)
mu=v+sigma^2/2
r=0.0466


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

SimTesla<-SimGBMexact(Nsim,St,r-sigma^2/2,sigma,dt,1)




#######m Delta ######






SimGBM1shootpmh<-function(Nsim,S0,v,sigma,Deltat,T,h){
  m=T/Deltat # number of periods
  Splush=matrix(S0+h,nrow=Nsim,ncol=m+1)
  S=matrix(S0,nrow=Nsim,ncol=m+1)
  Sminush=matrix(S0-h,nrow=Nsim,ncol=m+1)
  for(i in 1:Nsim){
    Z<-rnorm(m)
    for(j in 2:(m+1)){
      Splush[i,j]=Splush[i,j-1]*exp(v*Deltat+sigma*sqrt(Deltat)*Z[j-1])
      S[i,j]=S[i,j-1]*exp(v*Deltat+sigma*sqrt(Deltat)*Z[j-1])
      Sminush[i,j]=Sminush[i,j-1]*exp(v*Deltat+sigma*sqrt(Deltat)*Z[j-1])
    }
  }
  list(Splush=Splush,S=S,Sminush=Sminush)
}


tesla_lr <- get_s0_lr(df,start = 1,window_size =length(df$Date))$lr
Nsim=1000
set.seed(4518)
St = 1106.22
h = 0.01*St
dt = 1/252
v = mean(tesla_lr)
sigma = sd(tesla_lr)
Tminust = length(df$Date)/252
r = .04566


SimHSI<-SimGBM1shootpmh(Nsim,St,v,sigma,dt,Tminust,h)



SimHSISplush<-SimHSI$Splush
SimHSIS<-SimHSI$S
SimHSISminush<-SimHSI$Sminush



SimNotesplush<-apply(SimHSISplush,1,pay_off)
SimNotes<-apply(SimHSIS,1,pay_off)
SimNotesminush<-apply(SimHSISminush,1,pay_off)
NoteValueplush=exp(-r*Tminust)*mean(SimNotesplush)
NoteValue=exp(-r*Tminust)*mean(SimNotes)
NoteValueminush=exp(-r*Tminust)*mean(SimNotesminush)

NoteDelta=(NoteValueplush-NoteValueminush)/(2*h); NoteDelta

NoteDelta;h

NoteValueplush


all_red=c()
###### discounted price
for (i in 1:10000){
  all_red=c(all_red, pay_off(sim_matrix[i,]))
}

r=.0466
t=1

exp(-r*t)*mean(all_red)

r=.08
exp(-r*t)*mean(all_red)


