VDF_fitting = function(data, weavetype, time, source, bound_a, bound_e, taft_params, fn) { 
  library(dplyr)
    # time needs to be two elements, am pm, or amop pmop
  # bound_a: 2 elements, representing lower and upper, lowerbound_a = 0; upperbound_a = seq(3,10)
  # bound_e: 2 elements, representing lower and upper, lowerbound_e = -Inf; upperbound_e = 0
  # taft_params: 2 elements, representing a and e
  
  # data = sf_2022_npmrds_2025_vdf
  ####################################### PK VDF data selection #######################################
  row_select = which(data$weavetype == weavetype & 
                                 data$source == source & 
                                 (data$time == time[1] | data$time == time[2]))
  ########################################################## VDF fitting ###############################################################
  data_selected = data.frame(x = data$x[row_select], 
                                    t = data$t[row_select],
                                    t0 = data$t0[row_select],
                                    spd = data$spd[row_select],
                                    ffspd = data$ffspd[row_select],
                                    length = data$length[row_select],
                                    cap = data$cap[row_select])
  
  data_selected$t_hat_init = VDF(taft_params[1], taft_params[2], 
                                        data$t0[row_select],
                                        data$x[row_select])
  data_selected$v_hat_init = as.numeric(data_selected$length)/data_selected$t_hat_init
  
  data_selected$t_hat_uniform = 0
  data_selected$t_hat_tested = 0
  data_selected$v_hat_uniform = 0
  # data_selected$v_hat_init = 0
  
  ######################### 10-folder cross validation VDF PK. for uniform calibration ###########################
  set.seed(200)
  params_mle_agg = matrix(numeric(0))
  metrics_agg = data.frame(rmse = 1000, percent_error = 1000, r_squared = 0)
  
  n <- length(data_selected$x)
  folds <- sample(rep(1:10, length.out = n))
  
  x =  data_selected$x
  T0 = data_selected$t0
  T_obs = data_selected$t
  
  cv_results <- data.frame(
    fold = 1:10,
    a = NA, e = NA, sigma = NA,
    negLL = NA
  )
  
  ## store the best fit in all upperbound_a
  params_mle_ffspd = matrix(numeric(0))
  metrics_ffspd = matrix(numeric(0))
  
  for (i in 1:(length(bound_a)-1)) {
    for (k in 1:10) {
      # Split into training and validation
      train_idx <- which(folds != k)
      valid_idx <- which(folds == k)
      
      x_train <- x[train_idx]
      T0_train <- T0[train_idx]
      T_obs_train <- T_obs[train_idx]
      
      x_valid <- x[valid_idx]
      T0_valid <- T0[valid_idx]
      T_obs_valid <- T_obs[valid_idx]
      
      # Fit model on training fold
      fit <- optim(
        par = c(10, -0.15, 1), 
        fn = nll_vdf,
        x = x_train,
        t0 = T0_train,
        t_obs = T_obs_train,
        #method = 'Nelder-Mead',
        # method = 'SANN'
        method = "L-BFGS-B",
        lower = c(bound_a[1], bound_e[1], 1e-6),
        upper = c(bound_a[i+1], bound_e[2], Inf)
      )
      
      # Evaluate on validation fold
      est_params <- fit$par
      nll_valid <- nll_vdf(est_params, x_valid, T0_valid, T_obs_valid)
      
      # Store results
      cv_results[k, c("a", "e", "sigma")] <- est_params
      cv_results[k, "negLL"] <- nll_valid
    }
    
    params_mle <- cv_results %>%
      summarise(across(c(a, e, sigma, negLL), list(mean = mean, sd = sd)))
    
    # params_mle_agg = rbind(params_mle_agg, params_mle)
    data_selected$t_hat_tested = VDF(params_mle$a_mean,
                                             params_mle$e_mean,
                                            data_selected$t0,
                                            data_selected$x)
    
    metrics = calculate_metrics(data_selected$t, data_selected$t_hat_tested)
    
    # store all
    metrics_ffspd = rbind(metrics_ffspd, data.frame(metrics))
    params_mle_ffspd = rbind(params_mle_ffspd, data.frame(params_mle))
    
    if (metrics$r_squared > metrics_agg$r_squared) {
      metrics_agg = metrics
      params_mle_agg = c('a' = bound_a[i+1], params_mle)
      
      # calculate estimated travel time t_hat
      data_selected$t_hat_uniform = VDF(params_mle$a_mean,
                               params_mle$e_mean,
                               data_selected$t0,
                               data_selected$x)
      
      # calculate speed
      data_selected$v_hat_uniform = as.numeric(data_selected$length)/data_selected$t_hat_uniform
    } 

  }
  
  return(list('fitted_value' = data_selected, 
              'params_best' = data.frame(params_mle_agg), 
              'metrics_best' = data.frame(metrics_agg),
              'params_metrics_all' = cbind(params_mle_ffspd,metrics_ffspd)))
}




################################## Function: VDF #################################
### VDF: T = T0 + Cd + Sd + Ud, T0: ff travel time, Cd: conical congestion delay, Sd: signalized delay, Ud: un-signalized delay
## CdbyT0 = (1/V-1/V0)/(1/V0), which is a function of a, b, e, h, x
##           as: 1+sqrt(a^2 * (1- V/C + e)^2 + b^2) - a*(1 - V/C + e) - b -(1 + sqrt(a^2 * (1 - e)^2 + b^2) - a * (1 - e) - b)

VDF = function(a, e, t0, x) {
  
  b = (2*a-1)/(2*a-2)
  h = 1+sqrt(a^2*(1+e)^2 + b^2) - a*(1+e) - b
  
  return(t0 + t0 * (1+sqrt(a^2 * (1-x+e)^2 + b^2) - a * (1-x+e) - b - h))
}

################################## Function: MLE VDF ###################################

norm <- function(x) (x - min(x)) / (max(x) - min(x))

## MLE assume standard normal distribution error
nll_vdf = function(params, x, t0, t_obs) {
  a <- params[1]
  e <- params[2]
  sigma <- params[3]
  
  # Constrain sigma to be positive
  if (sigma <= 0) return(Inf)
  
  # calculate b and h from a and e
  b = (2*a-1)/(2*a-2)
  h = 1+sqrt(a^2*(1+e)^2 + b^2) - a*(1+e) - b
  
  t_pred = t0 + t0 * (1+sqrt(a^2 * (1-x+e)^2 + b^2) - a * (1-x+e) - b - h)
  
  residuals <- t_obs - t_pred
  n <- length(t_obs)
  
  
  ll_negative <- 0.5 * n * log(2 * pi * sigma^2) + 0.5 * sum(residuals^2) / sigma^2
  return(ll_negative)
}

################################## Function: BPR #################################
### BPR: T = T0 * (1 + a * (V/C) ^ b), T0: ff travel time, x = V/C, y = T/T0 = vff/v
## 
BPR_1 = function(a, b, t0, x) {
  return(t0*(1 + a * (x)^b))
}


################################## Function: MLE BPR ###################################

## MLE assume standard normal distribution error
nll_bpr = function(params, x, t0, t_obs) {
  a <- params[1]
  b <- params[2]
  sigma <- params[3]
  
  # Constrain sigma to be positive
  if (sigma <= 0) return(Inf)
  
  t_pred = t0*(1 + a * (x) ^ b)
  
  residuals <- t_obs - t_pred
  n <- length(t_obs)
  
  
  ll_negative <- 0.5 * n * log(2 * pi * sigma^2) + 0.5 * sum(residuals^2) / sigma^2
  return(ll_negative)
}

