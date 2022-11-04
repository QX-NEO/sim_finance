########### Preparation of tesla data
library(dplyr)
library(fGarch)
setwd("/home/nic/repos/group")

tesla = read.csv("TSLA.csv")
df <- subset(tesla, select = c('Date', 'Adj.Close'))
df$Date <- as.Date(df$Date)
df$year <- format(df$Date, format = "%Y")
df$actual_close <- df$Adj.Close*3

df<- df %>% filter(Date > '2021-01-12') %>% filter(Date <= '2022-01-12')
n = length(df$actual_close)-1
lr <- log(df$actual_close[2:n+1]) - log(df$actual_close[1:n])

#### 

#update new log return
## calculate new v and sigma
## generate s1

update_lr <- function(log_s1, lr){
  log_s0 = lr[length(lr)]
  return(c(lr[2:length(lr)], log_s1 - log_s0))
}

generate_vsigma <-function(lr){
  values<- c()
  values$v = mean(lr)
  values$sigma = sd(lr)
  return(values)
}

gbm_s1 <- function(s0, v, sigma){ ## 252
  Deltat=1#/252
  z = rnorm(1)
  return(s0 *exp(v*Deltat+sigma*sqrt(Deltat)*z))
}

sim <- c()
rolling_lr <- lr
for(i in 1:252){
  debug=c()
  if(i==1){
    s0=df$actual_close[n+1]
  }
  debug$s0=s0
  values <- generate_vsigma(rolling_lr)
  debug$v=values$v; debug$sigma=values$sigma
  s1 <- gbm_s1(s0, values$v, values$sigma)
  s0=s1
  debug$s1=s1
  sim <- c(sim, s1)
  # rolling_lr <- update_lr(log(s1), rolling_lr)
  print("--------------------------------------------------------------------")
  print(debug$s0)
  #print(debug$v)
  #print(debug$sigma)
  print(debug$s1)
}
#sim
