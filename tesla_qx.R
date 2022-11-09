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


# # window_size is 252
# window_size <- length((df %>% filter(Date > "2021-01-12")
#                        %>% filter(Date < "2022-01-12"))
#                       $actual_close)
# 
# 
# 
# # on +192nd day, not enough real actual_close to have window_size 252
# days_to_simulate <- length(df$actual_close) - window_size + 1
# num_simulations <- 10000
# 
# 
# # 3) rolling window gbm
# sim_matrix <- c()
# for (j in 1:num_simulations) { # N simulations
#   print(paste("simulation", j))
#   simulations <- c()
#   for (i in 1:days_to_simulate) {
#     values <- get_s0_lr(df, i, window_size)
#     s0 <- values$s0
#     lr <- values$lr
#     values <- get_v_sigma(lr)
#     v <- values$v
#     sigma <- values$sigma
#     s1 <- get_gbm_s1(s0, values$v, values$sigma)$s1
#     simulations <- c(simulations, s1)
#   }
#   sim_matrix <- rbind(sim_matrix, simulations)
# }

actual = df$actual_close[window_size:length(df$actual_close)]
error_calculator(sim_matrix,actual)


sim_matrix_av <- readRDS(sim_matrix, file = "Rds/sim_matrix-avregbm10k.Rds")




# window_size is 252
window_size <- length((df %>% filter(Date > "2021-01-12")
                       %>% filter(Date < "2022-01-12"))
                      $actual_close)

actual = df$actual_close[window_size:length(df$actual_close)]
error_calculator(sim_matrix_av,actual)

################# 6month rolling #################


df_6m <- df %>% filter(Date > "2021-07-12")
window_size <- length((df_6m %>% filter(Date > "2021-06-12")
                       %>% filter(Date < "2022-01-12"))
                      $actual_close)



# 128 days
days_to_simulate <- length(df_6m$actual_close) - window_size + 1


# 3) rolling window gbm
sim_matrix_6 <- c()
for (j in 1:num_simulations) { # N simulations
  print(paste("simulation", j))
  simulations <- c()
  for (i in 1:days_to_simulate) {
    values <- get_s0_lr(df_6m, i, window_size)
    s0 <- values$s0
    lr <- values$lr
    values <- get_v_sigma(lr)
    v <- values$v
    sigma <- values$sigma
    s1 <- get_gbm_s1(s0, values$v, values$sigma)$s1
    simulations <- c(simulations, s1)
  }
  sim_matrix_6 <- rbind(sim_matrix_6, simulations)
}


actual = df_6m$actual_close[window_size:length(df_6m$actual_close)]
error_calculator(sim_matrix_6,actual)



################# 3month rolling #################

# window_size is 126

df_3m <- df %>% filter(Date > "2021-10-12")
window_size <- length((df_3m %>% filter(Date > "2021-10-12")
                       %>% filter(Date < "2022-01-12"))
                      $actual_close)



# on +192nd day, not enough real actual_close to have window_size 252
days_to_simulate <- length(df_3m$actual_close) - window_size + 1
num_simulations <- 10000
print(days_to_simulate)

# 3) rolling window gbm
sim_matrix_3 <- c()
for (j in 1:num_simulations) { # N simulations
  print(paste("simulation", j))
  simulations <- c()
  for (i in 1:days_to_simulate) {
    values <- get_s0_lr(df_3m, i, window_size)
    s0 <- values$s0
    lr <- values$lr
    values <- get_v_sigma(lr)
    v <- values$v
    sigma <- values$sigma
    s1 <- get_gbm_s1(s0, values$v, values$sigma)$s1
    simulations <- c(simulations, s1)
  }
  sim_matrix_3 <- rbind(sim_matrix_3, simulations)
}

actual = df_3m$actual_close[window_size:length(df_3m$actual_close)]
error_calculator(sim_matrix_3,actual)


################### calculate rmse #############


error_calculator <- function(sim_matrix,actual){
  
  all_rmse <- c()
  all_mse <- c()
  values <- c()
  
  for(i in 1:dim(sim_matrix)[1]){
    
    rmse = sqrt(mean(actual-sim_matrix[i,])^2)
    all_rmse <- c(rmse, all_rmse)
    mse <- mean(actual-sim_matrix[i,])
    all_mse <- c(mse, all_mse)
    print(rmse)
    
  }
  
  values$mse <- mean(all_mse)
  values$rmse <- mean(all_rmse)
  
  return(values)
  
  
}

################ payoff ###################


initial_level <- 1106.22
barrier <- initial_level / 2
first_observation <- 252 / 4 * 2
second_observation <- 252 / 4 * 3
conversion_ratio <- 4.5199

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

############################################################

### risk neutral

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
v = mean(tesla_lr[1:252])
sigma = sd(tesla_lr[1:252])
Tminust = 1
r = .08


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
NoteGamma=(NoteValueplush-2*NoteValue+NoteValueminush)/(h^2); NoteGamma

NoteDelta; NoteGamma;h

#########################

adjust_pay_off <- function(simulations, initial_level = 1106.22, 
                    denomination = 5000,
                    #first_observation = (252/4)*2,
                    #second_obeservation = (252/4)*3,
                    conversion_ratio = 4.5199
){
  
  Notepay <- 0
  scenario <- 0
  break_barrier <- min(simulations) <= barrier
  final_fixing_date <- length(simulations)
  maturity = simulations[final_fixing_date]
  
  if(!break_barrier){
    
    if (simulations[first_obs] >= initial_level) {
      Notepay <- denomination * (1 + .1325 / 4 * 2)
      scenario <-1 
    }
    
    else if (simulations[second_obs] >= initial_level) {
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




#######################################################################

window_size <- 252
days_to_simulate <- 156
St = 1106.22
r = .04566
h = 0.05*St
dt = 1/252
Nsim=1000

delta_ts <- c()
gamma_ts <- c()


for (i in 1:125) {
  values <- get_s0_lr(df, i, window_size)
  s0 <- values$s0
  lr <- values$lr
  values <- get_v_sigma(lr)
  v <- values$v
  sigma <- values$sigma
  Tminust = (253-i)/252
  
  SimHSI<-SimGBM1shootpmh(Nsim,St,v,sigma,dt,Tminust,h)
  SimHSISplush<-SimHSI$Splush
  SimHSIS<-SimHSI$S
  SimHSISminush<-SimHSI$Sminush
  
  first_obs = (252/4)*2 - i +1
  second_obs = (252/4)*3 - i +1
  
  
  SimNotesplush<-apply(SimHSISplush,1,adjust_pay_off)
  SimNotes<-apply(SimHSIS,1,adjust_pay_off)
  SimNotesminush<-apply(SimHSISminush,1,adjust_pay_off)
  NoteValueplush=exp(-r*Tminust)*mean(SimNotesplush)
  NoteValue=exp(-r*Tminust)*mean(SimNotes)
  NoteValueminush=exp(-r*Tminust)*mean(SimNotesminush)
  
  
  NoteDelta=(NoteValueplush-NoteValueminush)/(2*h)
  NoteGamma=(NoteValueplush-2*NoteValue+NoteValueminush)/(h^2)
  
  delta_ts <- c(NoteDelta, delta_ts)
  gamma_ts <- c(NoteGamma, gamma_ts)
  
  print(paste0(i, ' ', NoteDelta," ", NoteGamma, ' ', first_obs))
}


############################


par(mfrow=c(1,2))
plot(rev(delta_ts), type ='l', col = 'blue')

title("Rolling Delta")


plot(rev(gamma_ts), type ='l', col = 'red')
title("Rolling Gamma")

