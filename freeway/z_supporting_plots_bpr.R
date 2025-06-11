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


########################################## Calibrated vs. TAFT, modeling fitting effect #################################
########################################## Calibrated vs. TAFT, modeling fitting effect #################################
########################################## Calibrated vs. TAFT, modeling fitting effect #################################

####################### FRWY_BASIC WEAVE #############################
####################### FRWY_BASIC WEAVE #############################
###################### y vs y_hat ###############################
data = bpr_arc_fitting_sf_npmrds_frwybasic_field$fitted_value
params = c(bpr_arc_fitting_sf_npmrds_frwybasic_field$params_best$a_mean, 
           bpr_arc_fitting_sf_npmrds_frwybasic_field$params_best$b_mean,
           bpr_arc_fitting_sf_npmrds_frwybasic_field$params_best$d_mean)
metrics = bpr_fitting_sf_npmrds_frwybasic_field$metrics_best


vc = data$x
flow = data$x * data$cap

spd = data$spd
spd_hat = as.numeric(data$length)/data$t_hat_uniform
spd_hat_init = as.numeric(data$length)/data$t_hat_init

speed = data$ffspd
speed_uniq = sort(unique(speed))

library(ggplot2)

ggplot(data, aes(t, t_hat_uniform, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_obs') + ylab('TT_calib') + title('Travel time Obs vs. Pred, Basic weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

ggplot(data, aes(t, t_hat_init, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_obs') + ylab('TT_TAFT') + title('Travel time Obs vs. Pred TAFT, Basic weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

ggplot(data, aes(t_hat_init, t_hat_uniform, color = factor(ffspd))) + 
  geom_point(size = 2) + geom_abline(color = 'white', linewidth = 1.5) +
  xlab('TT_TAFT') + ylab('TT_BPR') + title('Travel time BPR vs TAFT, Basic weave') +
  coord_cartesian(xlim = c(0,0.06), ylim = c(0, 0.06)) +
  scale_color_manual(values = c('60' = 'yellow', '65' = 'blue', '70' = 'green', '75' = 'red'), name = 'FF speed') + 
  # theme(legend.position = "top") + 
  theme_black()

################################## Flow vs Speed ################################
## displayed text in figure
label_text_bpr <- paste0(
  "Calib params: ", paste(round(params[1],2),round(params[2],2), round(params[3],2), sep = ', '), "\n",
  "\n",
  
  "No. Obs: ", nrow(data), "\n",
  
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,4), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)

x_y_bycategory(flow, spd, flow, spd_hat, speed, speed_uniq, 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated', label_text_bpr, ' ')



x_y_bycategory(flow, spd, flow, spd_hat_init, speed, speed_uniq, 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, TAFT', label_text_bpr, '')

################################## TAFT vs Calibrated, by ffspd ###########################
vc_artificial = runif(nrow(data), min = 0.01, max = 1.2)
flow_art = vc_artificial * data$cap

t_hat_art = BPR_ARC(params[1], params[2], params[3],
                data$t0, vc_artificial)
t_hat_init_art = VDF(8, -0.15,
                     data$t0, vc_artificial)
t_hat_init_art_calib = VDF(vdf_fitting_sf_npmrds_frwybasic$params_best$a_mean, vdf_fitting_sf_npmrds_frwybasic$params_best$e_mean,
                     data$t0, vc_artificial)
v_hat_art = as.numeric(data$length)/t_hat_art
v_hat_init_art = as.numeric(data$length)/t_hat_init_art
v_hat_init_art_calib = as.numeric(data$length)/t_hat_init_art_calib

row_select = which(speed == '70')

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/BPR_ARC_MLE_FRWYBASIC_PK_v_speed_70_usingfieldasTrue.png", 
    width = 800, height = 600)
x_y_bycategory(flow, spd, flow_art[row_select], v_hat_art[row_select], speed[row_select], speed_uniq[4], 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated, TAFT, FRWY_BASIC', label_text_bpr, legend = 0, legend.title = ' ')

points(flow_art[row_select], v_hat_init_art[row_select], col = 'red', pch = 16)
points(flow_art[row_select], v_hat_init_art_calib[row_select], col = 'blue', pch = 16)

# Manually set legend position using coordinates
legend_x <- par("usr")[1] + 0.005 * diff(par("usr")[1:2])  # a bit right of the left axis
legend_y <- par("usr")[3] + 0.27 * diff(par("usr")[3:4])  # a bit above the bottom axis

legend(x = legend_x, y = legend_y,
       legend = c('Field data', 'BPR', 'Original TAFT', 'Calibrated TAFT'),
       col = c('white', 'yellow', 'red', 'blue'),
       pch = c(3, 16, 16, 16),
       # title = legend,
       x.intersp = 0.5,              # Reduces space between symbol and text
       y.intersp = 1,              # Reduces vertical spacing between items
       text.col = "white",
       bg = "transparent",
       cex = 2)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/BPR_MLE_FRWYBASIC_PK_v_speed_70_usingfieldasTrue_real.png", 
    width = 800, height = 600)
t_hat_init_calib = VDF(vdf_fitting_sf_npmrds_frwybasic$params_best$a_mean, vdf_fitting_sf_npmrds_frwybasic$params_best$e_mean,
                           data$t0, data$x)
v_hat_init_calib = as.numeric(data$length)/t_hat_init_calib

x_y_bycategory(flow, spd, flow[row_select], spd_hat[row_select], speed[row_select], speed_uniq[4], 'Flow (veh/hr/ln)', 
               'Speed (MPH)', 'Flow vs Speed, Calibrated, TAFT, FRWY_BASIC', label_text_bpr, legend = 0, legend.title = ' ')

points(flow[row_select], spd_hat_init[row_select], col = 'red', pch = 16)
points(flow[row_select], v_hat_init_calib[row_select], col = 'blue', pch = 16)

# Manually set legend position using coordinates
legend_x <- par("usr")[1] + 0.005 * diff(par("usr")[1:2])  # a bit right of the left axis
legend_y <- par("usr")[3] + 0.27 * diff(par("usr")[3:4])  # a bit above the bottom axis

legend(x = legend_x, y = legend_y,
       legend = c('Field data', 'BPR', 'Original TAFT', 'Calibrated TAFT'),
       col = c('white', 'yellow', 'red', 'blue'),
       pch = c(3, 16, 16, 16),
       # title = legend,
       x.intersp = 0.5,              # Reduces space between symbol and text
       y.intersp = 1,              # Reduces vertical spacing between items
       text.col = "white",
       bg = "transparent",
       cex = 2)
dev.off()

rm(flow,flow_art,label_text_vdf,legend_x, legend_y, params, row_select, spd, spd_hat, 
   spd_hat_init, data, metrics, speed, speed_uniq, t_hat_art, t_hat_init_art, vc, vc_artificial, x_vals, y_vals, v_hat_init_art, v_hat_art)
rm(i,j,k,label_text_bpr,lane,len_val,length,m,nw_val,p,q,t_hat_init_art_calib,t_hat_init_calib,type,v_hat_init_art_calib,v_hat_init_calib,vr_val)
