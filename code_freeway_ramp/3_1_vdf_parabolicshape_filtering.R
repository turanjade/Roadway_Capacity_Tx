# this file add filters to each freeway type and then use filtered data to do the fitting (in call)

################################ FRWY_BASIC ##################################################################
row_select = which(sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC' &
                     sf_2022_npmrds_2025_plot$source == 'npm' &
                     sf_2022_npmrds_2025_plot$time != 'amop' &
                     sf_2022_npmrds_2025_plot$time != 'pmop')

params_mle_parab_frwybasic = k_folder(sf_2022_npmrds_2025_plot$avgspd[row_select], 
                                  sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
                                  nll_parab, initial = c(10, 1, 1), lower = c(0, 1, 1e-6), upper = c(70, 100, Inf))




# Calculate fitted values manually
sf_2022_npmrds_2025_plot <- sf_2022_npmrds_2025_plot %>%
  mutate(fitted = params_mle_parab_frwybasic$b_mean * (params_mle_parab_frwybasic$a_mean - avgspd) * avgspd)

# Keep only the points at or above the parabola
sf_filtered_frwybasic <- sf_2022_npmrds_2025_plot %>%
  filter(avgvol/lane >= fitted, 
         avgspd > 0.5*params_mle_parab_frwybasic$a_mean,
         sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC',
         sf_2022_npmrds_2025_plot$source == 'npm',
         sf_2022_npmrds_2025_plot$time != 'amop',
         sf_2022_npmrds_2025_plot$time != 'pmop')


sf_2022_npmrds_2025_vdf_frwybasic = data.frame(x = sf_filtered_frwybasic$vdf_x, 
                                           t = sf_filtered_frwybasic$vdf_t,
                                           t0 = sf_filtered_frwybasic$vdf_t0,
                                           spd = sf_filtered_frwybasic$avgspd,
                                           ffspd = sf_filtered_frwybasic$ffspd,
                                           length = sf_filtered_frwybasic$length,
                                           cap = sf_filtered_frwybasic$hrcap/sf_filtered_frwybasic$lane,
                                           weavetype = sf_filtered_frwybasic$weavetype,
                                           source = sf_filtered_frwybasic$source,
                                           time = sf_filtered_frwybasic$time)


# vdf_fitting_sf_npmrds_frwybasic = VDF_fitting(sf_2022_npmrds_2025_vdf_frwybasic, 
#                                          'FRWY_BASIC',
#                                          c('am','pm'),
#                                          'npm',
#                                          bound_a = c(0, seq(3,10)),
#                                          bound_e = c(-Inf, 0),
#                                          taft_params = c(8, -0.15),
#                                          fn = nll_vdf)


y_vals <- seq(min(sf_2022_npmrds_2025_plot$avgspd[row_select]), max(sf_2022_npmrds_2025_plot$avgspd[row_select]), length.out = 100)
# Compute x values using x = (y - a)y
x_vals <- params_mle_parab_frwybasic$b_mean * (params_mle_parab_frwybasic$a_mean - y_vals) * y_vals

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_frwybasic_PK_v_speed_filtered.png",
    width = 800, height = 600, bg = 'black')
label_text_vdf <- paste0(
  "Calib params: ", paste(round(vdf_fitting_sf_npmrds_frwybasic$params_best$a_mean,0),
                          round(vdf_fitting_sf_npmrds_frwybasic$params_best$e_mean,4),
                          sep = ', '), "\n",
  "\n",
  
  "No. Obs: ", nrow(data), "\n",
  
  "% Error: ", round(vdf_fitting_sf_npmrds_frwybasic$metrics_best$percent_error,2), '%', "\n",
  
  "RMSE: ", round(vdf_fitting_sf_npmrds_frwybasic$metrics_best$rmse,4), '\n',
  
  "% RSQ: ", round(vdf_fitting_sf_npmrds_frwybasic$metrics_best$r_squared * 100, 2), '%'
)

# x_y_bycategory = function(x, y, x_hat, y_hat, category, category_uniq, xlab, ylab, title, label, legend, legend.title) {
x_y_bycategory(sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
              sf_2022_npmrds_2025_plot$avgspd[row_select],
              
              vdf_fitting_sf_npmrds_frwybasic$fitted_value$cap * vdf_fitting_sf_npmrds_frwybasic$fitted_value$x,
              vdf_fitting_sf_npmrds_frwybasic$fitted_value$v_hat_uniform, 
              
              vdf_fitting_sf_npmrds_frwybasic$fitted_value$ffspd, 
              sort(unique(vdf_fitting_sf_npmrds_frwybasic$fitted_value$ffspd)), 
              
              'Flow (veh/hr/ln)', 'Speed (mph)', 'Flow-Speed, calibrated', 
              
              label_text_vdf, 0, '')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_frwybasic$b_mean*35*(params_mle_parab_frwybasic$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_v_speed_filtered_init.png",
    width = 800, height = 600, bg = 'black')
x_y_bycategory(sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
               sf_2022_npmrds_2025_plot$avgspd[row_select],
               
               vdf_fitting_sf_npmrds_frwybasic$fitted_value$cap * vdf_fitting_sf_npmrds_frwybasic$fitted_value$x,
               vdf_fitting_sf_npmrds_frwybasic$fitted_value$v_hat_init, 
               
               vdf_fitting_sf_npmrds_frwybasic$fitted_value$ffspd, 
               sort(unique(vdf_fitting_sf_npmrds_frwybasic$fitted_value$ffspd)), 
               
               'Flow (veh/hr/ln)', 'Speed (mph)', 'Flow-Speed, TAFT', 
               
               ' ', 0, '')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_frwybasic$b_mean*35*(params_mle_parab_frwybasic$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

# write.csv(vdf_fitting_sf_npmrds_frwybasic$fitted_value, '05202025_filteredValue_1.0858x(70-x)_vdf.csv', row.names = F)


################################ BASIC WEAVE ##################################################################
row_select = which(sf_2022_npmrds_2025_plot$weavetype == 'BASIC' &
                     sf_2022_npmrds_2025_plot$source == 'npm' &
                     sf_2022_npmrds_2025_plot$time != 'amop' &
                     sf_2022_npmrds_2025_plot$time != 'pmop')

# params_mle_parab_basic = k_folder(sf_2022_npmrds_2025_plot$avgspd[row_select], 
#                                  sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
#                                  nll_parab, initial = c(10, 1, 1), lower = c(0, 1, 1e-6), upper = c(70, 100, Inf))




# Calculate fitted values manually
sf_2022_npmrds_2025_plot <- sf_2022_npmrds_2025_plot %>%
  mutate(fitted = params_mle_parab_basic$b_mean * (params_mle_parab_basic$a_mean - avgspd) * avgspd)

# Keep only the points at or above the parabola
sf_filtered_basic <- sf_2022_npmrds_2025_plot %>%
  filter(avgvol/lane >= fitted, 
         avgspd > 0.5*params_mle_parab_basic$a_mean,
         sf_2022_npmrds_2025_plot$weavetype == 'BASIC',
           sf_2022_npmrds_2025_plot$source == 'npm',
           sf_2022_npmrds_2025_plot$time != 'amop',
           sf_2022_npmrds_2025_plot$time != 'pmop')


sf_2022_npmrds_2025_vdf_basic = data.frame(x = sf_filtered_basic$vdf_x, 
                                     t = sf_filtered_basic$vdf_t,
                                     t0 = sf_filtered_basic$vdf_t0,
                                     spd = sf_filtered_basic$avgspd,
                                     ffspd = sf_filtered_basic$ffspd,
                                     length = sf_filtered_basic$length,
                                     cap = sf_filtered_basic$hrcap/sf_filtered_basic$lane,
                                     weavetype = sf_filtered_basic$weavetype,
                                     source = sf_filtered_basic$source,
                                     time = sf_filtered_basic$time)


#vdf_fitting_sf_npmrds_basic = VDF_fitting(sf_2022_npmrds_2025_vdf_basic, 
#                                              'BASIC',
#                                              c('am','pm'),
#                                              'npm',
#                                              bound_a = c(0, seq(3,10)),
#                                              bound_e = c(-Inf, 0),
#                                              taft_params = c(8, -0.15),
#                                              fn = nll_vdf)


y_vals <- seq(min(sf_2022_npmrds_2025_plot$avgspd[row_select]), max(sf_2022_npmrds_2025_plot$avgspd[row_select]), length.out = 100)
# Compute x values using x = (y - a)y
x_vals <- params_mle_parab_basic$b_mean * (params_mle_parab_basic$a_mean - y_vals) * y_vals


png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_basic_PK_v_speed_filtered.png",
    width = 800, height = 600, bg = 'black')
#label_text_vdf <- paste0(
#  "Calib params: ", paste(round(vdf_fitting_sf_npmrds_basic$params_best$a_mean,0),
#                          round(vdf_fitting_sf_npmrds_basic$params_best$e_mean,4),
#                          sep = ', '), "\n",
#  "\n",
#  
#  "No. Obs: ", nrow(data), "\n",
#  
#  "% Error: ", round(vdf_fitting_sf_npmrds_basic$metrics_best$percent_error,2), '%', "\n",
#  
#  "RMSE: ", round(vdf_fitting_sf_npmrds_basic$metrics_best$rmse,4), '\n',
#  
#  "% RSQ: ", round(vdf_fitting_sf_npmrds_basic$metrics_best$r_squared * 100, 2), '%'
#)

# x_y_bycategory = function(x, y, x_hat, y_hat, category, category_uniq, xlab, ylab, title, label, legend, legend.title) {
x_y_bycategory(sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
               sf_2022_npmrds_2025_plot$avgspd[row_select],
               
               vdf_fitting_sf_npmrds_basic$fitted_value$cap * vdf_fitting_sf_npmrds_basic$fitted_value$x,
               vdf_fitting_sf_npmrds_basic$fitted_value$v_hat_uniform, 
               
               vdf_fitting_sf_npmrds_basic$fitted_value$ffspd, 
               sort(unique(vdf_fitting_sf_npmrds_basic$fitted_value$ffspd)), 
               
               'Flow (veh/hr/ln)', 'Speed (mph)', 'Flow-Speed, field data and calibrated, BASIC', 
               
               '', 1, 'Legend')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_basic$b_mean*35*(params_mle_parab_basic$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_BASIC_PK_v_speed_filtered_init.png",
    width = 800, height = 600, bg = 'black')
x_y_bycategory(sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
               sf_2022_npmrds_2025_plot$avgspd[row_select],
               
               vdf_fitting_sf_npmrds_basic$fitted_value$cap * vdf_fitting_sf_npmrds_basic$fitted_value$x,
               vdf_fitting_sf_npmrds_basic$fitted_value$v_hat_init, 
               
               vdf_fitting_sf_npmrds_basic$fitted_value$ffspd, 
               sort(unique(vdf_fitting_sf_npmrds_basic$fitted_value$ffspd)), 
               
               'Flow (veh/hr/ln)', 'Speed (mph)', 'Flow-Speed, TAFT', 
               
               ' ', 0, '')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_basic$b_mean*35*(params_mle_parab_basic$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()


################################ MD WEAVE ##################################################################
row_select = which(sf_2022_npmrds_2025_plot$weavetype == 'MD' &
                     sf_2022_npmrds_2025_plot$source == 'npm' &
                     sf_2022_npmrds_2025_plot$time != 'amop' &
                     sf_2022_npmrds_2025_plot$time != 'pmop')

# params_mle_parab_md = k_folder(sf_2022_npmrds_2025_plot$avgspd[row_select], 
#                                  sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
#                                  nll_parab, initial = c(10, 1, 1), lower = c(0, 1, 1e-6), upper = c(70, 100, Inf))




# Calculate fitted values manually
sf_2022_npmrds_2025_plot <- sf_2022_npmrds_2025_plot %>%
  mutate(fitted = params_mle_parab_md$b_mean * (params_mle_parab_md$a_mean - avgspd) * avgspd)

# Keep only the points at or above the parabola
sf_filtered_md <- sf_2022_npmrds_2025_plot %>%
  filter(avgvol/lane >= fitted, 
         avgspd > 0.5*params_mle_parab_md$a_mean,
         sf_2022_npmrds_2025_plot$weavetype == 'MD',
         sf_2022_npmrds_2025_plot$source == 'npm',
         sf_2022_npmrds_2025_plot$time != 'amop',
         sf_2022_npmrds_2025_plot$time != 'pmop')


sf_2022_npmrds_2025_vdf_md = data.frame(x = sf_filtered_md$vdf_x, 
                                           t = sf_filtered_md$vdf_t,
                                           t0 = sf_filtered_md$vdf_t0,
                                           spd = sf_filtered_md$avgspd,
                                           ffspd = sf_filtered_md$ffspd,
                                           length = sf_filtered_md$length,
                                           cap = sf_filtered_md$hrcap/sf_filtered_md$lane,
                                           weavetype = sf_filtered_md$weavetype,
                                           source = sf_filtered_md$source,
                                           time = sf_filtered_md$time)


# vdf_fitting_sf_npmrds_md = VDF_fitting(sf_2022_npmrds_2025_vdf_md, 
#                                          'MD',
#                                          c('am','pm'),
#                                          'npm',
#                                          bound_a = c(0, seq(3,10)),
#                                          bound_e = c(-Inf, 0),
#                                          taft_params = c(8, -0.15),
#                                          fn = nll_vdf)


y_vals <- seq(min(sf_2022_npmrds_2025_plot$avgspd[row_select]), max(sf_2022_npmrds_2025_plot$avgspd[row_select]), length.out = 100)
# Compute x values using x = (y - a)y
x_vals <- params_mle_parab_md$b_mean * (params_mle_parab_md$a_mean - y_vals) * y_vals


png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_md_PK_v_speed_filtered.png",
    width = 800, height = 600, bg = 'black')
# label_text_vdf <- paste0(
#  "Calib params: ", paste(round(vdf_fitting_sf_npmrds_md$params_best$a_mean,0),
#                          round(vdf_fitting_sf_npmrds_md$params_best$e_mean,4),
#                          sep = ', '), "\n",
#  "\n",
#  
#  "No. Obs: ", nrow(data), "\n",
#  
#  "% Error: ", round(vdf_fitting_sf_npmrds_md$metrics_best$percent_error,2), '%', "\n",
#  
#  "RMSE: ", round(vdf_fitting_sf_npmrds_md$metrics_best$rmse,4), '\n',
#  
#  "% RSQ: ", round(vdf_fitting_sf_npmrds_md$metrics_best$r_squared * 100, 2), '%'
# )

# x_y_bycategory = function(x, y, x_hat, y_hat, category, category_uniq, xlab, ylab, title, label, legend, legend.title) {
x_y_bycategory(sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
               sf_2022_npmrds_2025_plot$avgspd[row_select],
               
               vdf_fitting_sf_npmrds_md$fitted_value$cap * vdf_fitting_sf_npmrds_md$fitted_value$x,
               vdf_fitting_sf_npmrds_md$fitted_value$v_hat_uniform, 
               
               vdf_fitting_sf_npmrds_md$fitted_value$ffspd, 
               sort(unique(vdf_fitting_sf_npmrds_md$fitted_value$ffspd)), 
               
               'Flow (veh/hr/ln)', 'Speed (mph)', 'Flow-Speed, field data and calibrated, MD', 
               
               '', 1, 'Legend')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_md$b_mean*35*(params_mle_parab_md$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_MD_PK_v_speed_filtered_init.png",
    width = 800, height = 600, bg = 'black')
x_y_bycategory(sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
               sf_2022_npmrds_2025_plot$avgspd[row_select],
               
               vdf_fitting_sf_npmrds_md$fitted_value$cap * vdf_fitting_sf_npmrds_md$fitted_value$x,
               vdf_fitting_sf_npmrds_md$fitted_value$v_hat_init, 
               
               vdf_fitting_sf_npmrds_md$fitted_value$ffspd, 
               sort(unique(vdf_fitting_sf_npmrds_md$fitted_value$ffspd)), 
               
               'Flow (veh/hr/ln)', 'Speed (mph)', 'Flow-Speed, TAFT', 
               
               ' ', 0, '')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_md$b_mean*35*(params_mle_parab_md$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()


################################ Type A WEAVE ##################################################################
row_select = which(sf_2022_npmrds_2025_plot$weavetype == 'A' &
                     sf_2022_npmrds_2025_plot$source == 'npm' &
                     sf_2022_npmrds_2025_plot$time != 'amop' &
                     sf_2022_npmrds_2025_plot$time != 'pmop')

params_mle_parab_A = k_folder(sf_2022_npmrds_2025_plot$avgspd[row_select], 
                                  sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
                                  nll_parab, initial = c(10, 1, 1), lower = c(0, 1, 1e-6), upper = c(70, 100, Inf))




# Calculate fitted values manually
sf_2022_npmrds_2025_plot <- sf_2022_npmrds_2025_plot %>%
  mutate(fitted = params_mle_parab_A$b_mean * (params_mle_parab_A$a_mean - avgspd) * avgspd)

# Keep only the points at or above the parabola
sf_filtered_A <- sf_2022_npmrds_2025_plot %>%
  filter(avgvol/lane >= fitted, 
         avgspd > 0.5*params_mle_parab_A$a_mean,
         sf_2022_npmrds_2025_plot$weavetype == 'A',
         sf_2022_npmrds_2025_plot$source == 'npm',
         sf_2022_npmrds_2025_plot$time != 'amop',
         sf_2022_npmrds_2025_plot$time != 'pmop')


sf_2022_npmrds_2025_vdf_A = data.frame(x = sf_filtered_A$vdf_x, 
                                           t = sf_filtered_A$vdf_t,
                                           t0 = sf_filtered_A$vdf_t0,
                                           spd = sf_filtered_A$avgspd,
                                           ffspd = sf_filtered_A$ffspd,
                                           length = sf_filtered_A$length,
                                           cap = sf_filtered_A$hrcap/sf_filtered_A$lane,
                                           weavetype = sf_filtered_A$weavetype,
                                           source = sf_filtered_A$source,
                                           time = sf_filtered_A$time)


vdf_fitting_sf_npmrds_A = VDF_fitting(sf_2022_npmrds_2025_vdf_A, 
                                          'A',
                                          c('am','pm'),
                                          'npm',
                                          bound_a = c(0, seq(3,10)),
                                          bound_e = c(-Inf, 0),
                                          taft_params = c(8, -0.15),
                                          fn = nll_vdf)


y_vals <- seq(min(sf_2022_npmrds_2025_plot$avgspd[row_select]), max(sf_2022_npmrds_2025_plot$avgspd[row_select]), length.out = 100)
# Compute x values using x = (y - a)y
x_vals <- params_mle_parab_A$b_mean * (params_mle_parab_A$a_mean - y_vals) * y_vals


png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_A_PK_v_speed_filtered.png",
    width = 800, height = 600, bg = 'black')
label_text_vdf <- paste0(
  "Calib params: ", paste(round(vdf_fitting_sf_npmrds_A$params_best$a_mean,0),
                          round(vdf_fitting_sf_npmrds_A$params_best$e_mean,4),
                          sep = ', '), "\n",
  "\n",
  
  "No. Obs: ", nrow(data), "\n",
  
  "% Error: ", round(vdf_fitting_sf_npmrds_A$metrics_best$percent_error,2), '%', "\n",
  
  "RMSE: ", round(vdf_fitting_sf_npmrds_A$metrics_best$rmse,4), '\n',
  
  "% RSQ: ", round(vdf_fitting_sf_npmrds_A$metrics_best$r_squared * 100, 2), '%'
)

# x_y_bycategory = function(x, y, x_hat, y_hat, category, category_uniq, xlab, ylab, title, label, legend, legend.title) {
x_y_bycategory(sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
               sf_2022_npmrds_2025_plot$avgspd[row_select],
               
               vdf_fitting_sf_npmrds_A$fitted_value$cap * vdf_fitting_sf_npmrds_A$fitted_value$x,
               vdf_fitting_sf_npmrds_A$fitted_value$v_hat_uniform, 
               
               vdf_fitting_sf_npmrds_A$fitted_value$ffspd, 
               sort(unique(vdf_fitting_sf_npmrds_A$fitted_value$ffspd)), 
               
               'Flow (veh/hr/ln)', 'Speed (mph)', 'Flow-Speed, calibrated', 
               
               label_text_vdf, 0, '')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_A$b_mean*35*(params_mle_parab_A$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_A_PK_v_speed_filtered_init.png",
    width = 800, height = 600, bg = 'black')
x_y_bycategory(sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
               sf_2022_npmrds_2025_plot$avgspd[row_select],
               
               vdf_fitting_sf_npmrds_A$fitted_value$cap * vdf_fitting_sf_npmrds_A$fitted_value$x,
               vdf_fitting_sf_npmrds_A$fitted_value$v_hat_init, 
               
               vdf_fitting_sf_npmrds_A$fitted_value$ffspd, 
               sort(unique(vdf_fitting_sf_npmrds_A$fitted_value$ffspd)), 
               
               'Flow (veh/hr/ln)', 'Speed (mph)', 'Flow-Speed, TAFT', 
               
               ' ', 0, '')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_A$b_mean*35*(params_mle_parab_A$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()


rm(flow,flow_art,label_text_vdf,legend_x, legend_y, params, row_select, spd, spd_hat, v_hat_art, v_hat_init_art,
   spd_hat_init, data, metrics, speed, speed_uniq, t_hat_art, t_hat_init_art, vc, vc_artificial, x_vals, y_vals)
########################### comment out #############################

# Plot data
plot(sf_2022_npmrds_2025_plot$avgspd[row_select],
     sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
     main = "Fit of (x - a)x", col = "white", pch = 16, bg = 'black')
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)


# Define y range
y_vals <- seq(min(sf_2022_npmrds_2025_plot$avgspd[row_select]), max(sf_2022_npmrds_2025_plot$avgspd[row_select]), length.out = 100)

# Compute x values using x = (y - a)y
x_vals <- params_mle_parab_basic$b_mean * (params_mle_parab_basic$a_mean - y_vals) * y_vals
points(y_vals, x_vals, col = 'red')

