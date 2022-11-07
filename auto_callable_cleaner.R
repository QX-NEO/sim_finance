# 1) 2) 3) 5) follows todo.jpg
# load or save rds variables with
# saveRDS(sim_matrix, file = "Rds/sim_matrix-avregbm10k.Rds")
# sim_matrix <- readRDS(file = "Rds/sim_matrix-regbm10k.Rds")
# Rds/README.md explains naming convention

library(dplyr)
library(fGarch)
setwd("/home/nic/repos/y4s1/sim_finance")

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
get_early_redemption <- function(simulations) {
    redemption <- 0
    if (simulations[first_observation] >= initial_level) {
        # 100% denomination + coupon
        redemption <- 5000 * (1 + .1325 / 4 * 2)
    }
    else if (simulations[second_observation] >= initial_level) {
        # 100% denomination + coupon
        redemption <- 5000 * (1 + .1325 / 4 * 3)
    }
    return(redemption)
}

get_redemption <- function(simulations) {
    redemption <- 0
    break_barrier <- min(simulations) <= barrier
    # supposed to be 2023-01-17 but we only have data up to 2022-10-13
    # here i am taking 2022-10-13 to be final_fixing_date
    final_fixing_date <- length(simulations)

    # a)
    if (!(break_barrier) || simulations[final_fixing_date] >= initial_level) {
        redemption <- 5000 * (1.1325)
    }
    # c)
    else if (simulations[final_fixing_date] == 0) {
        redemption <- 5000 * (.1325)
    }
    # b) but not too sure on fractional shares
    else if (break_barrier && simulations[final_fixing_date] < initial_level) {
        redemption <- 5000 * (.1325) + 5000 / conversion_ratio
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
    redemption <- get_early_redemption(sim_matrix[i, ])
    if (redemption == 0) {
        redemption <- get_redemption(sim_matrix[i, ])
    }
    print(redemption)
    redemptions <- c(redemptions, redemption)
}

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
