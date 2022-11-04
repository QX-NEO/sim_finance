########### Preparation of tesla data
library(dplyr)
library(fGarch)
setwd("C:/Users/neo qi xiang/Desktop/sim_finance/sim_finance_project")

tesla = read.csv("TSLA.csv")
tesla_filter <- subset(tesla, select = c('Date', 'Adj.Close'))
tesla_filter$Date <- as.Date(tesla_filter$Date)
tesla_filter$year <- format(tesla_filter$Date, format = "%Y")
tesla_filter$actual_close <- tesla_filter$Adj.Close*3


tesla_filter<- tesla_filter %>% filter(Date >= '2021-01-12') %>% filter(Date <'2022-01-12')
n = length(tesla_filter$Adj.Close)-1
tesla_lr <-  log(tesla_filter$actual_close[2:n+1]) - log(tesla_filter$actual_close[1:n])




#### 

#update new log return
## calculate new v and sigma
## generate s1

updateLR <- function(newS0, old_LR){
  new_return <- log(news0) - old_LR[length(old_LR)]
  return(c(old_LR[2:length(old_LR)],new_return ))
}



generate_vsigma <-  function(logLR){
  values<- c()
  values$v = mean(logLR)
  values$sigma = sd(logLR)
  
  return(values)
}


gbm_s1 <- function(s0,v , sigma){ ## 252
  z = rnorm(1)
  return(s0 *exp(v+sigma*z))
}



sim <- c()
tesla_lr_gbm <- tesla_lr
for(i in 1:252){
  if(i ==1){
    news0 <- tesla_lr_gbm[n]
    val <- generate_vsigma(tesla_lr_gbm)
    news0 <- gbm_s1(news0,val$v, val$sigma)
    print(news0)
    sim <- c(sim, news0)
    tesla_lr_gbm <- updateLR(newS0,old_LR = tesla_lr_gbm)
    
  }
  else{
    val <- generate_vsigma(tesla_lr_gbm)
    news0 <- gbm_s1(news0,val$v, val$sigma)
    sim <- c(sim, news0)
    tesla_lr_gbm <- updateLR(newS0,old_LR = tesla_lr_gbm)
    print(news0)
    print(head(tesla_lr_gbm))
    print(tail(tesla_lr_gbm))

  }
  
}
print(sim)




#### early redeption case

check_early_redemption <- function(path,term1, term2){
  
  if(path[term1] > 1106.22){
    payout = (100+13.25/2)/100
  }
  else if(path[term2] > 1106.22){
    payout =  (100+13.25*3/4)/100
  }
  else{
    payout = 0
  }
  return(payout)
}


#### meets barrier

check_barrier <- function(path){
  checker = 0 
  for(i in path){
    if(i <= 1106.22/2){
      checker =1
      break
    }
  }
  if(checker == 1){
    if(path[length(path)] >= 1106.22){
      payout = 113.25/100
    }
    else{
      payout = (path[length(path)] - 1106.22)/1106.22 + 113.25/100
    }
  }
  else{
    payout = 113.25/100
    }
  return(payout)
}
  
final_payoff <- function(path, term1,term2){
  
  if (check_early_redemption(path, term1,term2) != 0){
    return(check_early_redemption(path, term1,term2))
  }
  else{
    return(check_barrier(path))
  }
    
}



final_payoff(tesla_filter$actual_close , 60, 90)



### exactGBM






