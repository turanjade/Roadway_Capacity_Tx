# supporting plot
setwd("C:/Users/rtu/OneDrive - The North Central Texas Council of Governments/Documents/0_ModelDataDevelopment/")

library(ggplot2)
library(viridis)  # for colorblind-friendly palettes
library(dplyr)
setwd("C:/Users/rtu/OneDrive - The North Central Texas Council of Governments/Documents/0_ModelDataDevelopment/")

library(ggplot2)
library(viridis)  # for colorblind-friendly palettes
library(dplyr)
library(ggforce)
library('see')
library(ggridges)


################################### for all types ###########################################################################
################################### plot violin distribution #################################################################
# Compute 95th percentiles per variable
hist_type = c('A', '65')

## summarize quantile for line marker
violin_quantiles_95 <- sidefire_vol_spd_2022_plot[which(sidefire_vol_spd_2022_plot$weavetype == hist_type[1] &
                                                          sidefire_vol_spd_2022_plot$ffspd == hist_type[2]),] %>%
  group_by(voltype) %>%
  summarise(q95 = quantile(as.numeric(volperlane), 0.95))

hrcap = as.numeric(unique(sidefire_vol_spd_2022_plot$hrcapperlane[which(sidefire_vol_spd_2022_plot$weavetype == hist_type[1] &
                                                                           sidefire_vol_spd_2022_plot$ffspd == hist_type[2])]))
wvhrcap = as.numeric(unique(sidefire_vol_spd_2022_plot$wvhrcapperlane[which(sidefire_vol_spd_2022_plot$weavetype == hist_type[1] &
                                                                           sidefire_vol_spd_2022_plot$ffspd == hist_type[2])]))
## violin plot
png(paste0("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFireOnly/Sidefire_voldist_",
           hist_type[1],'_',hist_type[2],'mph.png'), width = 800, height = 600)
ggplot(sidefire_vol_spd_2022_plot[which(sidefire_vol_spd_2022_plot$weavetype == hist_type[1] &
                                          sidefire_vol_spd_2022_plot$ffspd == hist_type[2]),], aes(x = factor(voltype), y = as.numeric(volperlane))) +
  geom_violinhalf(trim = FALSE, fill = "skyblue", color = 'grey', scale = "width") + 
  geom_hline(aes(yintercept = hrcap), color = "yellow", linetype = "dashed", linewidth = 2) + 
  annotate('text', 
           x = 0.5, y = hrcap + 50,  # 1 unit above the line
           label = as.character(hrcap), 
           fontface = "bold",          # Font style: plain, bold, italic, bold.italic
           size = 10,
           hjust = 0, vjust = 0, color = "yellow") +
  geom_hline(aes(yintercept = wvhrcap), color = "green", linetype = "dashed", linewidth = 2) + 
  annotate('text', 
           x = 0.5, y = wvhrcap + 50,  # 1 unit above the line
           label = as.character(wvhrcap), 
           fontface = "bold",          # Font style: plain, bold, italic, bold.italic
           size = 10,
           hjust = 0, vjust = 0, color = "green") +
  # Add points
  # geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  # Add 95% lines
  geom_segment(data = violin_quantiles_95,
               aes(x = as.numeric(factor(voltype)) + 0.5,  # nudge to right side
                   xend = as.numeric(factor(voltype)) - 0.05,
                   y = q95, yend = q95),
               color = "red", linewidth = 2, linetype = "dashed") +
  geom_text(data = violin_quantiles_95,
            aes(x = as.numeric(factor(voltype)) + 0.5,  # adjust for left position
                y = q95 - 200,                          # adjust vertically if needed
                label = as.character(round(q95,0))),
            color = "red", size = 10, hjust = 1, vjust = 0,
            fontface = "bold") +
  
  labs(title = paste0('Stats value distribution for ', hist_type[1], ', speed limit ', hist_type[2], ' MPH'),
       x = ' ', y = 'Volume per lane') + 
  scale_x_discrete(labels = c(vol90 = '90% quantile', vol95 = '95% quantile', 
                              volboxupper = 'calc. max', volmax = 'actual max')) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_black()
dev.off()

rm(hist_type, violin_quantiles_95)


################################################### speed comparison, NMPRDS vs SF #####################################
names(sf_2022_npmrds_2025_plot)

weave = 'A'
png(paste0("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/Sidefire_NPMRDS_SpdDiff_", weave,"_PK.png"), 
    width = 800, height = 600, bg = 'black')
par(bg = 'black')
x_y_bycategory(sf_2022_npmrds_2025_plot$avgspd[which(sf_2022_npmrds_2025_plot$weavetype == weave &
                                                       sf_2022_npmrds_2025_plot$time != 'amop' &
                                                       sf_2022_npmrds_2025_plot$time != 'pmop' &
                                                       sf_2022_npmrds_2025_plot$source == 'npm')], 
               sf_2022_npmrds_2025_plot$avgspd[which(sf_2022_npmrds_2025_plot$weavetype == weave &
                                                       sf_2022_npmrds_2025_plot$time != 'amop' &
                                                       sf_2022_npmrds_2025_plot$time != 'pmop' &
                                                       sf_2022_npmrds_2025_plot$source == 'sf')], 
               array(dim = 0), array(dim = 0), array(dim = 0), array(dim = 0), 'NPMRDS', 
               'Sidefire', paste0('Speed comparison, ', weave, ' weave'), '', legend = 0, legend.title = ' ')
abline(a = 0, b = 1, col = "yellow", lty = 2, lwd = 2)  # adds x = y
dev.off()


# this file add filters to each freeway type and then use filtered data to do the fitting (in call)

################################ Parabolic fitting & filtering, for each types ###############################
################################ Parabolic fitting & filtering, for each types ###############################
################################ Parabolic fitting & filtering, for each types ###############################
################################ Parabolic fitting & filtering, for each types ###############################

################################ FRWY_BASIC ##################################################################
row_select = which(sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC' &
                     sf_2022_npmrds_2025_plot$source == 'npm' &
                     sf_2022_npmrds_2025_plot$time != 'amop' &
                     sf_2022_npmrds_2025_plot$time != 'pmop')

# params_mle_parab_frwybasic = k_folder(sf_2022_npmrds_2025_plot$avgspd[row_select], 
#                                      sf_2022_npmrds_2025_plot$avgvol[row_select]/sf_2022_npmrds_2025_plot$lane[row_select], 
#                                      nll_parab, initial = c(10, 1, 1), lower = c(0, 1, 1e-6), upper = c(70, 100, Inf))




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
  
  "No. Obs: ", nrow(vdf_fitting_sf_npmrds_frwybasic$fitted_value), "\n",
  
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
               
               '', 1, 'Legend')
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
               
               ' ', 1, 'Legend')
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
               
               label_text_vdf, 1, 'Legend')
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


########################################## Calibrated vs. TAFT, modeling fitting effect #################################
########################################## Calibrated vs. TAFT, modeling fitting effect #################################
########################################## Calibrated vs. TAFT, modeling fitting effect #################################

####################### FRWY_BASIC WEAVE #############################
####################### FRWY_BASIC WEAVE #############################
###################### y vs y_hat ###############################
data = vdf_fitting_sf_npmrds_frwybasic$fitted_value
vc = data$x
flow = data$x * data$cap

spd = data$spd
spd_hat = as.numeric(data$length)/data$t_hat_uniform
spd_hat_init = as.numeric(data$length)/data$t_hat_init

speed = data$ffspd
speed_uniq = sort(unique(speed))
params = c(vdf_fitting_sf_npmrds_frwybasic$params_best$a_mean, vdf_fitting_sf_npmrds_frwybasic$params_best$e_mean)
metrics = vdf_fitting_sf_npmrds_frwybasic$metrics_best

library(ggplot2)

ggplot(vdf_fitting_sf_npmrds_frwybasic$fitted_value, aes(t, t_hat_uniform, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_obs') + ylab('TT_calib') + title('Travel time Obs vs. Pred, Basic weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

ggplot(vdf_fitting_sf_npmrds_frwybasic$fitted_value, aes(t, t_hat_init, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_obs') + ylab('TT_TAFT') + title('Travel time Obs vs. Pred TAFT, Basic weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

ggplot(vdf_fitting_sf_npmrds_frwybasic$fitted_value, aes(t_hat_uniform, t_hat_init, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_calib') + ylab('TT_TAFT') + title('Travel time calibrated vs TAFT, Basic weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

################################## Flow vs Speed ################################
## displayed text in figure
label_text_vdf <- paste0(
  "Calib params: ", paste(round(params[1],0),round(params[2],4), sep = ', '), "\n",
  "\n",
  
  "No. Obs: ", nrow(data), "\n",
  
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,4), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)

x_y_bycategory(flow, spd, flow, spd_hat, speed, speed_uniq, 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated', label_text_vdf, ' ')



x_y_bycategory(flow, spd, flow, spd_hat_init, speed, speed_uniq, 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, TAFT', label_text_vdf, '')

################################## TAFT vs Calibrated, by ffspd ###########################
vc_artificial = runif(nrow(data), min = 0.01, max = 1.2)
flow_art = vc_artificial * data$cap

t_hat_art = VDF(params[1], params[2],
                data$t0, vc_artificial)
t_hat_init_art = VDF(8, -0.15,
                     data$t0, vc_artificial)
v_hat_art = as.numeric(data$length)/t_hat_art
v_hat_init_art = as.numeric(data$length)/t_hat_init_art

row_select = which(speed == '70')

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_v_speed_70.png", 
    width = 800, height = 600)
x_y_bycategory(flow, spd, flow_art[row_select], v_hat_art[row_select], speed[row_select], speed_uniq[4], 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated, TAFT, BASIC weave', label_text_vdf, legend = 0, legend.title = ' ')

points(flow_art[row_select], v_hat_init_art[row_select], col = 'red', pch = 16)

# Manually set legend position using coordinates
legend_x <- par("usr")[1] + 0.005 * diff(par("usr")[1:2])  # a bit right of the left axis
legend_y <- par("usr")[3] + 0.25 * diff(par("usr")[3:4])  # a bit above the bottom axis

legend(x = legend_x, y = legend_y,
       legend = c('Field data', 'Calibrated', 'TAFT'),
       col = c('white', 'yellow', 'red'),
       pch = c(3, 16, 16),
       # title = legend,
       x.intersp = 0.5,              # Reduces space between symbol and text
       y.intersp = 1,              # Reduces vertical spacing between items
       text.col = "white",
       bg = "transparent",
       cex = 2)
dev.off()

####################### BASIC WEAVE #############################
####################### BASIC WEAVE #############################
###################### y vs y_hat ###############################
data = vdf_fitting_sf_npmrds_basic$fitted_value
vc = data$x
flow = data$x * data$cap

spd = data$spd
spd_hat = as.numeric(data$length)/data$t_hat_uniform
spd_hat_init = as.numeric(data$length)/data$t_hat_init

speed = data$ffspd
speed_uniq = sort(unique(speed))
params = c(vdf_fitting_sf_npmrds_basic$params_best$a_mean, vdf_fitting_sf_npmrds_basic$params_best$e_mean)
metrics = vdf_fitting_sf_npmrds_basic$metrics_best

library(ggplot2)

ggplot(vdf_fitting_sf_npmrds_basic$fitted_value, aes(t, t_hat_uniform, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_obs') + ylab('TT_calib') + title('Travel time Obs vs. Pred, Basic weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

ggplot(vdf_fitting_sf_npmrds_basic$fitted_value, aes(t, t_hat_init, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_obs') + ylab('TT_TAFT') + title('Travel time Obs vs. Pred TAFT, Basic weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

ggplot(vdf_fitting_sf_npmrds_basic$fitted_value, aes(t_hat_uniform, t_hat_init, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_calib') + ylab('TT_TAFT') + title('Travel time calibrated vs TAFT, Basic weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

################################## Flow vs Speed ################################
## displayed text in figure
label_text_vdf <- paste0(
  "Calib params: ", paste(round(params,2), collapse = ', '), "\n",
  "\n",
  
  "No. Obs: ", nrow(data), "\n",
  
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,4), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)

x_y_bycategory(flow, spd, flow, spd_hat, speed, speed_uniq, 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated', label_text_vdf, ' ')



x_y_bycategory(flow, spd, flow, spd_hat_init, speed, speed_uniq, 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, TAFT', label_text_vdf, '')

################################## TAFT vs Calibrated, by ffspd ###########################
vc_artificial = runif(nrow(data), min = 0.01, max = 1.2)
flow_art = vc_artificial * data$cap

t_hat_art = VDF(params[1], params[2],
                data$t0, vc_artificial)
t_hat_init_art = VDF(8, -0.15,
                     data$t0, vc_artificial)
v_hat_art = as.numeric(data$length)/t_hat_art
v_hat_init_art = as.numeric(data$length)/t_hat_init_art

row_select = which(speed == '70')

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_BASIC_PK_v_speed_70.png", 
    width = 800, height = 600)
x_y_bycategory(flow, spd, flow_art[row_select], v_hat_art[row_select], speed[row_select], speed_uniq[3], 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated, TAFT, BASIC weave', label_text_vdf, legend = 0, legend.title = ' ')

points(flow_art[row_select], v_hat_init_art[row_select], col = 'red', pch = 16)

# Manually set legend position using coordinates
legend_x <- par("usr")[1] + 0.005 * diff(par("usr")[1:2])  # a bit right of the left axis
legend_y <- par("usr")[3] + 0.25 * diff(par("usr")[3:4])  # a bit above the bottom axis

legend(x = legend_x, y = legend_y,
       legend = c('Field data', 'Calibrated', 'TAFT'),
       col = c('white', 'yellow', 'red'),
       pch = c(3, 16, 16),
       # title = legend,
       x.intersp = 0.5,              # Reduces space between symbol and text
       y.intersp = 1,              # Reduces vertical spacing between items
       text.col = "white",
       bg = "transparent",
       cex = 2)
dev.off()


####################### MD WEAVE #############################
####################### MD WEAVE #############################
###################### y vs y_hat ###############################
data = vdf_fitting_sf_npmrds_md$fitted_value
vc = data$x
flow = data$x * data$cap

spd = data$spd
spd_hat = as.numeric(data$length)/data$t_hat_uniform
spd_hat_init = as.numeric(data$length)/data$t_hat_init

speed = data$ffspd
speed_uniq = sort(unique(speed))
params = c(vdf_fitting_sf_npmrds_md$params_best$a_mean, vdf_fitting_sf_npmrds_md$params_best$e_mean)
metrics = vdf_fitting_sf_npmrds_md$metrics_best

library(ggplot2)

ggplot(vdf_fitting_sf_npmrds_md$fitted_value, aes(t, t_hat_uniform, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_obs') + ylab('TT_calib') + title('Travel time Obs vs. Pred, MD weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

ggplot(vdf_fitting_sf_npmrds_md$fitted_value, aes(t, t_hat_init, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_obs') + ylab('TT_TAFT') + title('Travel time Obs vs. Pred TAFT, MD weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

ggplot(vdf_fitting_sf_npmrds_md$fitted_value, aes(t_hat_uniform, t_hat_init, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_calib') + ylab('TT_TAFT') + title('Travel time calibrated vs TAFT, MD weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

################################## Flow vs Speed ################################
## displayed text in figure
label_text_vdf <- paste0(
  "Calib params: ", paste(round(params[1],0), round(params[2],4), sep = ', '), "\n",
  "\n",
  
  "No. Obs: ", nrow(data), "\n",
  
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,4), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)

x_y_bycategory(flow, spd, flow, spd_hat, speed, speed_uniq, 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated', label_text_vdf, ' ')



x_y_bycategory(flow, spd, flow, spd_hat_init, speed, speed_uniq, 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, TAFT', label_text_vdf, '')

################################## TAFT vs Calibrated, by ffspd ###########################
vc_artificial = runif(nrow(data), min = 0.01, max = 1.2)
flow_art = vc_artificial * data$cap

t_hat_art = VDF(params[1], params[2],
                data$t0, vc_artificial)
t_hat_init_art = VDF(8, -0.15,
                     data$t0, vc_artificial)
v_hat_art = as.numeric(data$length)/t_hat_art
v_hat_init_art = as.numeric(data$length)/t_hat_init_art

row_select = which(speed == '70')

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_MD_PK_v_speed_70.png", 
    width = 800, height = 600)
x_y_bycategory(flow, spd, flow_art[row_select], v_hat_art[row_select], speed[row_select], speed_uniq[3], 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated, TAFT, MD weave', label_text_vdf, legend = 0, legend.title = ' ')

points(flow_art[row_select], v_hat_init_art[row_select], col = 'red', pch = 16)

# Manually set legend position using coordinates
legend_x <- par("usr")[1] + 0.005 * diff(par("usr")[1:2])  # a bit right of the left axis
legend_y <- par("usr")[3] + 0.25 * diff(par("usr")[3:4])  # a bit above the bottom axis

legend(x = legend_x, y = legend_y,
       legend = c('Field data', 'Calibrated', 'TAFT'),
       col = c('white', 'yellow', 'red'),
       pch = c(3, 16, 16),
       # title = legend,
       x.intersp = 0.5,              # Reduces space between symbol and text
       y.intersp = 1,              # Reduces vertical spacing between items
       text.col = "white",
       bg = "transparent",
       cex = 2)
dev.off()


####################### A WEAVE #############################
####################### A WEAVE #############################
###################### y vs y_hat ###############################
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

library(ggplot2)

ggplot(vdf_fitting_sf_npmrds_A$fitted_value, aes(t, t_hat_uniform, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_obs') + ylab('TT_calib') + title('Travel time Obs vs. Pred, MD weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

ggplot(vdf_fitting_sf_npmrds_A$fitted_value, aes(t, t_hat_init, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_obs') + ylab('TT_TAFT') + title('Travel time Obs vs. Pred TAFT, MD weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

ggplot(vdf_fitting_sf_npmrds_A$fitted_value, aes(t_hat_uniform, t_hat_init, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_calib') + ylab('TT_TAFT') + title('Travel time calibrated vs TAFT, MD weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

################################## Flow vs Speed ################################
## displayed text in figure
label_text_vdf <- paste0(
  "Calib params: ", paste(round(params[1],0), round(params[2],4), sep = ', '), "\n",
  "\n",
  
  "No. Obs: ", nrow(data), "\n",
  
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,4), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)


x_y_bycategory(flow, spd, flow, spd_hat, speed, speed_uniq, 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated', label_text_vdf, legend = 1, legend.title =  'Legend')



x_y_bycategory(flow, spd, flow, spd_hat_init, speed, speed_uniq, 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, TAFT', label_text_vdf, '')

################################## TAFT vs Calibrated, by ffspd ###########################
vc_artificial = runif(nrow(data), min = 0.01, max = 1.2)
flow_art = vc_artificial * data$cap

t_hat_art = VDF(params[1], params[2],
                data$t0, vc_artificial)
t_hat_init_art = VDF(8, -0.15,
                     data$t0, vc_artificial)
v_hat_art = as.numeric(data$length)/t_hat_art
v_hat_init_art = as.numeric(data$length)/t_hat_init_art

# row_select = which(speed == '60')
row_select = which(speed == '70')

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_A_PK_v_speed_70.png", 
    width = 800, height = 600)
x_y_bycategory(flow, spd, flow_art[row_select], v_hat_art[row_select], speed[row_select], speed_uniq[4], 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated, TAFT, MD weave', label_text_vdf, legend = 0, legend.title = ' ')

points(flow_art[row_select], v_hat_init_art[row_select], col = 'red', pch = 16)

# Manually set legend position using coordinates
legend_x <- par("usr")[1] + 0.005 * diff(par("usr")[1:2])  # a bit right of the left axis
legend_y <- par("usr")[3] + 0.25 * diff(par("usr")[3:4])  # a bit above the bottom axis

legend(x = legend_x, y = legend_y,
       legend = c('Field data', 'Calibrated', 'TAFT'),
       col = c('white', 'yellow', 'red'),
       pch = c(3, 16, 16),
       # title = legend,
       x.intersp = 0.5,              # Reduces space between symbol and text
       y.intersp = 1,              # Reduces vertical spacing between items
       text.col = "white",
       bg = "transparent",
       cex = 2)
dev.off()

rm(flow,flow_art,label_text_vdf,legend_x, legend_y, params, row_select, spd, spd_hat, 
   spd_hat_init, data, metrics, speed, speed_uniq, t_hat_art, t_hat_init_art, vc, vc_artificial, x_vals, y_vals, v_hat_init_art, v_hat_art)
