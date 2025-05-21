
nll_parab = function(params, x, y) {
  a <- params[1]
  b <- params[2]
  sigma <- params[3]
  
  # Constrain sigma to be positive
  if (sigma <= 0) return(Inf)
  
  # calculate b and h from a and e
  y_hat = b * (a - x) * x
  
  residuals <- y - y_hat
  n <- length(y)
  
  
  ll_negative <- 0.5 * n * log(2 * pi * sigma^2) + 0.5 * sum(residuals^2) / sigma^2
  return(ll_negative)
}

set.seed(200)

n <- length(sf_2022_npmrds_2025_plot$avgspd)
folds <- sample(rep(1:10, length.out = n))

x =  sf_2022_npmrds_2025_plot$avgspd
y = sf_2022_npmrds_2025_plot$avgvol/sf_2022_npmrds_2025_plot$lane

cv_results <- data.frame(
  fold = 1:10,
  a = NA, e = NA, sigma = NA,
  negLL = NA
)


for (k in 1:10) {
  # Split into training and validation
  train_idx <- which(folds != k)
  valid_idx <- which(folds == k)
  
  x_train <- x[train_idx]
  y_train <- y[train_idx]
  
  x_valid <- x[valid_idx]
  y_valid <- y[valid_idx]
  
  # Fit model on training fold
  fit <- optim(
    par = c(10, 1, 1), 
    fn = nll_parab,
    x = x_train,
    y = y_train,
    #method = 'Nelder-Mead',
    # method = 'SANN'
    method = "L-BFGS-B",
    lower = c(0, 1, 1e-6),
    upper = c(70, 100, Inf)
  )
  
  # Evaluate on validation fold
  est_params <- fit$par
  nll_valid <- nll_parab(est_params, x_valid, y_valid)
  
  # Store results
  cv_results[k, c("a", "b", "sigma")] <- est_params
  cv_results[k, "negLL"] <- nll_valid
}

params_mle <- cv_results %>%
  summarise(across(c(a, b, sigma, negLL), list(mean = mean, sd = sd)))

# Plot data
plot(x, y, main = "Fit of (x - a)x", col = "white", pch = 16, bg = 'black')
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)


# Define y range
y_vals <- seq(min(sf_2022_npmrds_2025_plot$avgspd), max(sf_2022_npmrds_2025_plot$avgspd), length.out = 100)

# Compute x values using x = (y - a)y
x_vals <- params_mle$b_mean * (params_mle$a_mean - y_vals) * y_vals
points(y_vals, x_vals, col = 'red')


# Calculate fitted values manually
sf_2022_npmrds_2025_plot <- sf_2022_npmrds_2025_plot %>%
  mutate(fitted = params_mle$b_mean * (params_mle$a_mean - avgspd) * avgspd)

row_select = which(sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC' &
                     sf_2022_npmrds_2025_plot$source == 'npm' &
                     sf_2022_npmrds_2025_plot$time != 'amop' &
                     sf_2022_npmrds_2025_plot$time != 'pmop')

# Keep only the points at or above the parabola
sf_filtered <- sf_2022_npmrds_2025_plot %>%
  filter(avgvol/lane >= fitted, avgspd > 0.5*params_mle$a_mean,
         weavetype == 'FRWY_BASIC', 
         source == 'npm',
         time != 'amop' , 
         time != 'pmop')


sf_2022_npmrds_2025_vdf = data.frame(x = sf_filtered$vdf_x, 
                                     t = sf_filtered$vdf_t,
                                     t0 = sf_filtered$vdf_t0,
                                     spd = sf_filtered$avgspd,
                                     ffspd = sf_filtered$ffspd,
                                     length = sf_filtered$length,
                                     cap = sf_filtered$hrcap/sf_filtered$lane,
                                     weavetype = sf_filtered$weavetype,
                                     source = sf_filtered$source,
                                     time = sf_filtered$time)


vdf_fitting_sf_npmrds_frwybasic = VDF_fitting(sf_2022_npmrds_2025_vdf, 
                                              'FRWY_BASIC',
                                              c('am','pm'),
                                              'npm',
                                              bound_a = c(0, seq(3,10)),
                                              bound_e = c(-Inf, 0),
                                              taft_params = c(8, -0.15),
                                              fn = nll_vdf)


y_vals <- seq(min(sf_2022_npmrds_2025_plot$avgspd), max(sf_2022_npmrds_2025_plot$avgspd), length.out = 100)
# Compute x values using x = (y - a)y
x_vals <- params_mle$b_mean * (params_mle$a_mean - y_vals) * y_vals

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_v_speed_filtered.png",
    width = 800, height = 600, bg = 'black')
vc_v_by_speed(sf_filtered[row_select,], 
              sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
              sf_2022_npmrds_2025_plot$avgspd[row_select],
              
              vdf_fitting_sf_npmrds_frwybasic$fitted_value$cap * vdf_fitting_sf_npmrds_frwybasic$fitted_value$x,
              vdf_fitting_sf_npmrds_frwybasic$fitted_value$v_hat_uniform, 
              
              vdf_fitting_sf_npmrds_frwybasic$fitted_value$ffspd, 
              sort(unique(vdf_fitting_sf_npmrds_frwybasic$fitted_value$ffspd)), 
              
              'Flow (veh/hr/ln)', 'Speed (mph)', 'Flow-Speed, calibrated', 
              c(vdf_fitting_sf_npmrds_frwybasic$params_best$a_mean, vdf_fitting_sf_npmrds_frwybasic$params_best$e_mean), 
              vdf_fitting_sf_npmrds_frwybasic$metrics_best)
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle$b_mean*35*(params_mle$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_v_speed_filtered_init.png",
    width = 800, height = 600, bg = 'black')
vc_v_by_speed(sf_filtered[row_select,], 
              sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
              sf_2022_npmrds_2025_plot$avgspd[row_select], 
              
              vdf_fitting_sf_npmrds_frwybasic$fitted_value$cap * vdf_fitting_sf_npmrds_frwybasic$fitted_value$x,
              vdf_fitting_sf_npmrds_frwybasic$fitted_value$v_hat_init, 
              
              vdf_fitting_sf_npmrds_frwybasic$fitted_value$ffspd, 
              sort(unique(vdf_fitting_sf_npmrds_frwybasic$fitted_value$ffspd)), 
              
              'Flow (veh/hr/ln)', 'Speed (mph)', 'Flow-Speed, TAFT', 
              c(8, -0.15), 
              calculate_metrics(vdf_fitting_sf_npmrds_frwybasic$fitted_value$t, vdf_fitting_sf_npmrds_frwybasic$fitted_value$t_hat_init))
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle$b_mean*35*(params_mle$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

write.csv(vdf_fitting_sf_npmrds_frwybasic$fitted_value, '05202025_filteredValue_1.0858x(70-x)_vdf.csv', row.names = F)




