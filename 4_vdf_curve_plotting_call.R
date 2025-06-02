## this file calls vdf_curve_plotting functions to plot 


data = vdf_fitting_sf_npmrds_A$fitted_value
vc = data$x
flow = data$x * data$cap

spd = data$spd
spd_hat = as.numeric(data$length)/data$t_hat_uniform
spd_hat_init = as.numeric(data$length)/data$t_hat_init



speed = data$ffspd
speed_uniq = sort(unique(speed))
params = c(vdf_fitting_sf_npmrds_A$params_best$a_mean, vdf_fitting_sf_npmrds_A$params_best$e_mean)
metrics = vdf_fitting_sf_npmrds_A$metrics_best

# vc ratio

## displayed text in figure
label_text_vdf <- paste0(
  "Calib params: ", paste(round(params,2), collapse = ', '), "\n",
  "\n",
  
  "No. Obs: ", nrow(data), "\n",
  
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,4), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)


x_y_bycategory(data, flow, spd, flow, spd_hat, speed, speed_uniq, 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated', label_text_vdf)
