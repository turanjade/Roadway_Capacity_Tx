#### this file use two methods to filter:
## 1. use the parabola fitted from the SF_NPMRDS pair
## 2. fit its own parabolic curve, draw, and filter
## May 23 conclusion: use the second method to keep more data 
## data use: vol_per_day_2022_feb_workday_transpose from 2_0
## function use: nll_parab and k_folder

setwd("C:/Users/rtu/OneDrive - The North Central Texas Council of Governments/Documents/0_ModelDataDevelopment/")

library(ggplot2)
library(viridis)  # for colorblind-friendly palettes
library(dplyr)
library(ggforce)
library('see')
library(ggridges)

################################## use parabola fitted from the SF data alone ##################################
################################## use parabola fitted from the SF data alone ##################################

################################## FRWY_BASIC ################################
vol_per_day_202202_frwy = vol_per_day_2022_feb_workday_transpose[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'FRWY_BASIC' &
                                                                         vol_per_day_2022_feb_workday_transpose$time == 'PK'),]

params_mle_parab_sf_frwybasic = k_folder(vol_per_day_202202_frwy$spd, 
                                         vol_per_day_202202_frwy$vol/vol_per_day_202202_frwy$lane, 
                                      nll_parab, initial = c(10, 1, 1), lower = c(0, 1, 1e-6), upper = c(70, 100, Inf))

# Calculate fitted values manually
vol_per_day_202202_frwy <- vol_per_day_202202_frwy %>%
  mutate(fitted = params_mle_parab_sf_frwybasic$b_mean * (params_mle_parab_sf_frwybasic$a_mean - spd) * spd)

# Keep only the points at or above the parabola
vol_per_day_202202_frwy <- vol_per_day_202202_frwy %>%
  filter(vol/lane >= fitted,
         spd > 0.5 * params_mle_parab_sf_frwybasic$a_mean)

y_vals <- seq(min(vol_per_day_202202_frwy$spd), max(vol_per_day_202202_frwy$spd), length.out = 100)
# Compute x values using x = (y - a)y
x_vals <- params_mle_parab_sf_frwybasic$b_mean * (params_mle_parab_sf_frwybasic$a_mean - y_vals) * y_vals

table(vol_per_day_202202_frwy$ffspd)

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SidefireOnly/frwybasic_PK_v_speed_parabolicfilter.png",
   width = 800, height = 600, bg = 'black')

# x_y_bycategory = function(x, y, x_hat, y_hat, category, category_uniq, xlab, ylab, title, label, legend, legend.title) {
plot(vol_per_day_2022_feb_workday_transpose$vol[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'FRWY_BASIC' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')]/
       vol_per_day_2022_feb_workday_transpose$lane[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'FRWY_BASIC' &
                                                           vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     vol_per_day_2022_feb_workday_transpose$spd[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'FRWY_BASIC' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     pch = 16, size = 1.5, col = 'white',
     
     xlim = c(0, 2500), ylim = c(0, 85), 
     xlab = 'Flow per lane', ylab = 'Speed from Sidefire (MPH)', main = 'Sidefire flow-speed, FRWY_BASIC',
     
     # Axis label color
     col.lab = "white", col.axis = "white",col.main = "white",
     
     # specify font
     cex.main = 2, cex.lab = 2, font.lab = 2, 
     
     # Turn off default axes to customize them
     axes = FALSE
)
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
points(vol_per_day_202202_frwy$vol/vol_per_day_202202_frwy$lane, vol_per_day_202202_frwy$spd, pch = 4, col = 'yellow')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_sf_frwybasic$b_mean*35*(params_mle_parab_sf_frwybasic$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

################################## BASIC ################################
vol_per_day_202202_basic = vol_per_day_2022_feb_workday_transpose[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'BASIC' &
                                                                         vol_per_day_2022_feb_workday_transpose$time == 'PK'),]

params_mle_parab_sf_basic = k_folder(vol_per_day_202202_basic$spd, 
                                         vol_per_day_202202_basic$vol/vol_per_day_202202_basic$lane, 
                                         nll_parab, initial = c(10, 1, 1), lower = c(0, 1, 1e-6), upper = c(70, 100, Inf))

# Calculate fitted values manually
vol_per_day_202202_basic <- vol_per_day_202202_basic %>%
  mutate(fitted = params_mle_parab_sf_basic$b_mean * (params_mle_parab_sf_basic$a_mean - spd) * spd)

# Keep only the points at or above the parabola
vol_per_day_202202_basic <- vol_per_day_202202_basic %>%
  filter(vol/lane >= fitted,
         spd > 0.5 * params_mle_parab_sf_basic$a_mean)

y_vals <- seq(min(vol_per_day_202202_basic$spd), max(vol_per_day_202202_basic$spd), length.out = 100)
# Compute x values using x = (y - a)y
x_vals <- params_mle_parab_sf_basic$b_mean * (params_mle_parab_sf_basic$a_mean - y_vals) * y_vals

table(vol_per_day_202202_basic$ffspd)

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SidefireOnly/basic_PK_v_speed_parabolicfilter.png",
    width = 800, height = 600, bg = 'black')
# x_y_bycategory = function(x, y, x_hat, y_hat, category, category_uniq, xlab, ylab, title, label, legend, legend.title) {
plot(vol_per_day_2022_feb_workday_transpose$vol[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'BASIC' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')]/
       vol_per_day_2022_feb_workday_transpose$lane[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'BASIC' &
                                                           vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     vol_per_day_2022_feb_workday_transpose$spd[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'BASIC' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     pch = 16, size = 1.5, col = 'white',
     
     xlim = c(0, 2500), ylim = c(0, 85), 
     xlab = 'Flow per lane', ylab = 'Speed from Sidefire (MPH)', main = 'Sidefire flow-speed, BASIC',
     
     # Axis label color
     col.lab = "white", col.axis = "white",col.main = "white",
     
     # specify font
     cex.main = 2, cex.lab = 2, font.lab = 2, 
     
     # Turn off default axes to customize them
     axes = FALSE
)
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
points(vol_per_day_202202_basic$vol/vol_per_day_202202_basic$lane, vol_per_day_202202_basic$spd, pch = 4, col = 'yellow')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_sf_basic$b_mean*35*(params_mle_parab_sf_basic$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

################################## MD ################################
vol_per_day_202202_md = vol_per_day_2022_feb_workday_transpose[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'MD' &
                                                                          vol_per_day_2022_feb_workday_transpose$time == 'PK'),]

params_mle_parab_sf_md = k_folder(vol_per_day_202202_md$spd, 
                                     vol_per_day_202202_md$vol/vol_per_day_202202_md$lane, 
                                     nll_parab, initial = c(10, 1, 1), lower = c(0, 1, 1e-6), upper = c(70, 100, Inf))

# Calculate fitted values manually
vol_per_day_202202_md <- vol_per_day_202202_md %>%
  mutate(fitted = params_mle_parab_sf_md$b_mean * (params_mle_parab_sf_md$a_mean - spd) * spd)

# Keep only the points at or above the parabola
vol_per_day_202202_md <- vol_per_day_202202_md %>%
  filter(vol/lane >= fitted,
         spd > 0.5 * params_mle_parab_sf_md$a_mean)

y_vals <- seq(min(vol_per_day_202202_md$spd), max(vol_per_day_202202_md$spd), length.out = 100)
# Compute x values using x = (y - a)y
x_vals <- params_mle_parab_sf_md$b_mean * (params_mle_parab_sf_md$a_mean - y_vals) * y_vals

table(vol_per_day_202202_md$ffspd)

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SidefireOnly/md_PK_v_speed_parabolicfilter.png",
    width = 800, height = 600, bg = 'black')
# x_y_bycategory = function(x, y, x_hat, y_hat, category, category_uniq, xlab, ylab, title, label, legend, legend.title) {
plot(vol_per_day_2022_feb_workday_transpose$vol[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'MD' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')]/
       vol_per_day_2022_feb_workday_transpose$lane[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'MD' &
                                                           vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     vol_per_day_2022_feb_workday_transpose$spd[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'MD' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     pch = 16, size = 1.5, col = 'white',
     
     xlim = c(0, 2500), ylim = c(0, 85), 
     xlab = 'Flow per lane', ylab = 'Speed from Sidefire (MPH)', main = 'Sidefire flow-speed, MD',
     
     # Axis label color
     col.lab = "white", col.axis = "white",col.main = "white",
     
     # specify font
     cex.main = 2, cex.lab = 2, font.lab = 2, 
     
     # Turn off default axes to customize them
     axes = FALSE
)
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
points(vol_per_day_202202_md$vol/vol_per_day_202202_md$lane, vol_per_day_202202_md$spd, pch = 4, col = 'yellow')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_sf_md$b_mean*35*(params_mle_parab_sf_md$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

rm(flow,hrcap,params,spd,spd_hat,spd_hat_init,speed,speed_uniq,vc,weave, x_vals, y_vals)



################################## from filtered data, get the stats value, 90%, 95%, boxupper, max ##################################
################################## from filtered data, get the stats value, 90%, 95%, boxupper, max ##################################

################################## calculate stats ##################################
vol_per_day_202202_frwy_basic_md = rbind(vol_per_day_202202_frwy, vol_per_day_202202_basic, vol_per_day_202202_md)

sidefire_vol_spd_2022_filtered = data.frame(cbind(sidefire_linkmatch_frwy_2025$sf_id, sidefire_linkmatch_frwy_2025$rdwy_id,
                                                  sidefire_linkmatch_frwy_2025$TMC, sidefire_linkmatch_frwy_2025$weavetype,
                                                  sidefire_linkmatch_frwy_2025$areatype,
                                                  sidefire_linkmatch_frwy_2025$length,
                                                  sidefire_linkmatch_frwy_2025$amffspd, 
                                                  sidefire_linkmatch_frwy_2025$amlane, 
                                                  sidefire_linkmatch_frwy_2025$amhrcap/sidefire_linkmatch_frwy_2025$amlane,
                                                  sidefire_linkmatch_frwy_2025$amwvhrcap/sidefire_linkmatch_frwy_2025$amlane))

colnames(sidefire_vol_spd_2022_filtered) = c('ID_detector', 'ID_Road', 'TMC', 'weavetype','areatype','length','ffspd','lane',
                                             'hrcapperlane','wvhrcapperlane')

sidefire_vol_spd_2022_filtered$vol90 = 0; sidefire_vol_spd_2022_filtered$vol95 = 0; 
sidefire_vol_spd_2022_filtered$volmax = 0; sidefire_vol_spd_2022_filtered$volboxupper = 0

sidefire_vol_spd_2022_filtered$spd90 = 0; sidefire_vol_spd_2022_filtered$spd95 = 0; 
sidefire_vol_spd_2022_filtered$spdmax = 0; sidefire_vol_spd_2022_filtered$spdboxupper = 0

missingsensors = matrix(0, nrow = 0, ncol = 2)

for (i in 1:nrow(sidefire_vol_spd_2022_filtered)) {
  
  link_i = vol_per_day_202202_frwy_basic_md[which(vol_per_day_202202_frwy_basic_md$sf_id == sidefire_vol_spd_2022_filtered$ID_detector[i]),]
  
  # if the number of link is 0, skip
  if (nrow(link_i) == 0) {
    print(paste('Sensors ID', vol_per_day_202202_frwy_basic_md$sf_id[i],'not found',' row',i))
    missingsensors = rbind(missingsensors, c(i, vol_per_day_202202_frwy_basic_md$sf_id[i]))
    next
  }
  
  # choose corresponding volume 
  volarray_i = allNA_returnNA(t(link_i$vol)) 
  spdarray_i = allNA_returnNA(t(link_i$spd)) 
  if (all(is.na(c(volarray_i))) | all(is.na(spdarray_i))) {
    print(paste('All qualified vol & spd are NA, sensor ID', link_i$LinkID[1]))
    next
  }
  
  # we already chose records under spdff, vol and spd are already in the same order, so we don't have to change
  sidefire_vol_spd_2022_filtered$vol90[i] = quantile(volarray_i, na.rm = T, 0.90) 
  q90Index = getIndex(volarray_i,sidefire_vol_spd_2022_filtered$vol90[i])
  
  sidefire_vol_spd_2022_filtered$vol95[i] = quantile(volarray_i, na.rm = T, 0.95) 
  q95Index = getIndex(volarray_i,sidefire_vol_spd_2022_filtered$vol95[i]) 
  
  sidefire_vol_spd_2022_filtered$volmax[i] = max(volarray_i, na.rm = T) 
  maxIndex = getIndex(volarray_i,sidefire_vol_spd_2022_filtered$volmax[i]) 
  
  sidefire_vol_spd_2022_filtered$volboxupper[i] = min(quantile(volarray_i, na.rm = T, 0.75) + 
                                               1.5*(quantile(volarray_i, na.rm = T, 0.75) - quantile(volarray_i, na.rm = T, 0.25)), 
                                               sidefire_vol_spd_2022_filtered$volmax[i]) 
  boxupperIndex = getIndex(volarray_i,sidefire_vol_spd_2022_filtered$volboxupper[i]) 
  
  # speed stats from sidefire detector, vol_per_day which takes records per 15-minute interval
  sidefire_vol_spd_2022_filtered$spd90[i] = mean(spdarray_i[q90Index])
  sidefire_vol_spd_2022_filtered$spd95[i] = mean(spdarray_i[q95Index])
  sidefire_vol_spd_2022_filtered$spdmax[i] = mean(spdarray_i[maxIndex])
  sidefire_vol_spd_2022_filtered$spdboxupper[i] = mean(spdarray_i[boxupperIndex])
}

if (nrow(missingsensors) > 0) {
  sidefire_vol_spd_2022_filtered = sidefire_vol_spd_2022_filtered[-as.numeric(missingsensors[,1]),]
}

sidefire_vol_spd_2022_filtered = data.frame(sidefire_vol_spd_2022_filtered)
## convert all values to numeric
for (i in 6:18) {
  sidefire_vol_spd_2022_filtered[,i] = as.numeric(sidefire_vol_spd_2022_filtered[,i])
}

# store initial value but add additional column to store scaled vols (1.1 times than recorded)
sidefire_vol_spd_2022_filtered$vol90_s = sidefire_vol_spd_2022_filtered$vol90 * 1.1
sidefire_vol_spd_2022_filtered$vol95_s = sidefire_vol_spd_2022_filtered$vol95 * 1.1
sidefire_vol_spd_2022_filtered$volmax_s = sidefire_vol_spd_2022_filtered$volmax * 1.1
sidefire_vol_spd_2022_filtered$volboxupper_s = sidefire_vol_spd_2022_filtered$volboxupper * 1.1


################################## organize as plottable format ##################################
sidefire_vol_spd_2022_filtered_plot = cbind(sidefire_vol_spd_2022_filtered$ID_detector,
                                                    sidefire_vol_spd_2022_filtered$ID_Road,
                                                    sidefire_vol_spd_2022_filtered$TMC,
                                                    sidefire_vol_spd_2022_filtered$weavetype,
                                                    sidefire_vol_spd_2022_filtered$areatype,
                                                    sidefire_vol_spd_2022_filtered$ffspd,
                                                    sidefire_vol_spd_2022_filtered$length,
                                                    sidefire_vol_spd_2022_filtered$lane,
                                                    sidefire_vol_spd_2022_filtered$hrcapperlane,
                                                    sidefire_vol_spd_2022_filtered$wvhrcapperlane,
                                                    rbind(cbind('q90', sidefire_vol_spd_2022_filtered$vol90_s),
                                                          cbind('q95', sidefire_vol_spd_2022_filtered$vol95_s),
                                                          cbind('qboxupper', sidefire_vol_spd_2022_filtered$volboxupper_s),
                                                          cbind('qmax', sidefire_vol_spd_2022_filtered$volmax_s)))

sidefire_vol_spd_2022_filtered_plot = data.frame(sidefire_vol_spd_2022_filtered_plot)

for (i in 7:10) {
  sidefire_vol_spd_2022_filtered_plot[,i] = as.numeric(sidefire_vol_spd_2022_filtered_plot[,i])
}
sidefire_vol_spd_2022_filtered_plot[,12] = as.numeric(sidefire_vol_spd_2022_filtered_plot[,12])

colnames(sidefire_vol_spd_2022_filtered_plot) = c('ID_detector', 'ID_Road', 'TMC', 'weavetype','areatype',
                                                  'ffspd', 'length', 'lane', 'hrcapperlane', 'wvhrcapperlane', 'voltype', 'stats_vol_scaled')

sidefire_vol_spd_2022_filtered_plot$volperlane = sidefire_vol_spd_2022_filtered_plot$stats_vol_scaled/sidefire_vol_spd_2022_filtered_plot$lane

################################### plot violin distribution #################################################################
# Compute 95th percentiles per variable. sidefire_vol_spd_2022_filtered_plot, only FRWY_BASIC, BASIC, MD included
hist_type = c('MD', '60')

## summarize quantile for line marker
violin_quantiles_95 <- sidefire_vol_spd_2022_filtered_plot[which(sidefire_vol_spd_2022_filtered_plot$weavetype == hist_type[1] &
                                                          sidefire_vol_spd_2022_filtered_plot$ffspd == hist_type[2]),] %>%
  group_by(voltype) %>%
  summarise(q95 = quantile(as.numeric(volperlane), 0.95))

wvhrcap = as.numeric(unique(sidefire_vol_spd_2022_filtered_plot$wvhrcapperlane[which(sidefire_vol_spd_2022_filtered_plot$weavetype == hist_type[1] &
                                                                           sidefire_vol_spd_2022_filtered_plot$ffspd == hist_type[2])]))

hrcap = as.numeric(unique(sidefire_vol_spd_2022_filtered_plot$hrcapperlane[which(sidefire_vol_spd_2022_filtered_plot$weavetype == hist_type[1] & 
                                                                                   sidefire_vol_spd_2022_filtered_plot$ffspd == hist_type[2])]))
                                                                                     
## violin plot
png(paste0("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFireOnly/Sidefire_voldist_",
           hist_type[1],'_',hist_type[2],'mph_filtered_SFonly.png'), width = 800, height = 600)
ggplot(sidefire_vol_spd_2022_filtered_plot[which(sidefire_vol_spd_2022_filtered_plot$weavetype == hist_type[1] &
                                          sidefire_vol_spd_2022_filtered_plot$ffspd == hist_type[2]),], aes(x = factor(voltype), y = as.numeric(volperlane))) +
  geom_violinhalf(trim = FALSE, fill = "skyblue", color = 'grey', scale = "width") + 
  geom_hline(aes(yintercept = wvhrcap), color = "yellow", linetype = "dashed", linewidth = 2) + 
  annotate('text', 
           x = 0.5, y = wvhrcap + 50,  # 1 unit above the line
           label = as.character(wvhrcap), 
           fontface = "bold",          # Font style: plain, bold, italic, bold.italic
           size = 10,
           hjust = 0, vjust = 0, color = "yellow") +
  geom_hline(aes(yintercept = hrcap), color = "green", linetype = "dashed", linewidth = 2) + 
  annotate('text', 
           x = 0.5, y = hrcap + 50,  # 1 unit above the line
           label = as.character(hrcap), 
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

rm(hist_type, violin_quantiles_95, hrcap, wvhrcap)


################################## from unfiltered data, sidefire_vol_spd_2022_plot unfiltered, violin plot ##################################
################################## from unfiltered data, sidefire_vol_spd_2022_plot unfiltered, violin plot ##################################

# Compute 95th percentiles per variable
hist_type = c('BASIC', '60')

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

################################## Unfiltered data, <<Specifically for type A, B>>, convert to table capacity ##################################
################################## Unfiltered data, <<Specifically for type A, B>>, convert to table capacity ##################################
## refers to the RdwyPrep.rsc, macro Get_CAP and dim AWCAP[3,5]
## use lane number to specify. 
## if lane number > 5 & length > 500ft:
####### use lane # 5 as the base

## if lane > 5, length < 500, scale:
####### weaveCap = lanes * AWCAP[1,1]/3, or, take lane 3, length 500 as the base, then directly scale using #lanes

## if length < 500ft & #lane in [3,5], 
####### AWCAP_lower choose 500ft w. corresponding #lane, run O.w.

## if lane < 3, scale:
####### WeaveCap = -1, not possible

## O.w., Cap_est = (length - length_lower)/length_increment * (AWCAP_higher - AWCAP_lower) + AWCAP_lower

sidefire_vol_spd_2022_plot_A = sidefire_vol_spd_2022_plot[which(sidefire_vol_spd_2022_plot$weavetype == 'A'),]

sidefire_vol_spd_2022_plot_A$feet = as.numeric(sidefire_vol_spd_2022_plot_A$length) * 5280
for (i in 1:nrow(sidefire_vol_spd_2022_plot_A)) {
  if (sidefire_vol_spd_2022_plot_A$feet[i] <= 500) {
    sidefire_vol_spd_2022_plot_A$feet[i] = 500
  } 
  else if (sidefire_vol_spd_2022_plot_A$feet[i] <= 1000) {
    sidefire_vol_spd_2022_plot_A$feet[i] = 1000
  } 
  else if (sidefire_vol_spd_2022_plot_A$feet[i] <= 1500) {
    sidefire_vol_spd_2022_plot_A$feet[i] = 1500
  } 
  else if (sidefire_vol_spd_2022_plot_A$feet[i] <= 2000) {
    sidefire_vol_spd_2022_plot_A$feet[i] = 2000
  } 
  else if (sidefire_vol_spd_2022_plot_A$feet[i] <= 2500) {
    sidefire_vol_spd_2022_plot_A$feet[i] = 2500
  } 
  else {
    sidefire_vol_spd_2022_plot_A$feet[i] = 3000
  } 
  
}

AWCAP = matrix(0, nrow = 3, ncol = 5)
AWCAP[1,1] = 4300
AWCAP[1,2] = 4880
AWCAP[1,3] = 5240
AWCAP[1,4] = 5490
AWCAP[1,5] = 5690

AWCAP[2,1] = 5950
AWCAP[2,2] = 6740
AWCAP[2,3] = 7200
AWCAP[2,4] = 7530
AWCAP[2,5] = 7780

AWCAP[3,1] = 7980
AWCAP[3,2] = 9040
AWCAP[3,3] = 9610
AWCAP[3,4] = 10010
AWCAP[3,5] = 10280

colnames(AWCAP) = c('500', '1000', '1500', '2000', '2500')
rownames(AWCAP) = c('3', '4', '5')

# table(sidefire_vol_spd_2022_plot$feet[which(sidefire_vol_spd_2022_plot$weavetype == 'A')])

# sidefire_vol_spd_2022_plot$wvhrcapperlane[which(sidefire_vol_spd_2022_plot$feet == 1000 & sidefire_vol_spd_2022_plot$weavetype == 'A')]-
#  sidefire_vol_spd_2022_plot$hrcapperlane[which(sidefire_vol_spd_2022_plot$feet == 1000 & sidefire_vol_spd_2022_plot$weavetype == 'A')]

# for BASIC and MD, wvhrcapperlane all equals 2300, hrcapperlane varies; For A, B (C not sure, no data), hrcapperlane = wvhrcapperlane

# Compute 95th percentiles per variable
hist_type = c('6', '2500')

## speed distribution
png(paste0("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFireOnly/Sidefire_spddist_typeA_lane",
           hist_type[1],'_lengthLT',hist_type[2],'ft.png'), width = 800, height = 600)
ggplot(sidefire_vol_spd_2022_plot_A[which(sidefire_vol_spd_2022_plot_A$lane == hist_type[1] &
                                            sidefire_vol_spd_2022_plot_A$feet == hist_type[2] &
                                            sidefire_vol_spd_2022_plot_A$voltype == 'qmax'),], aes(x = spd)) +
  geom_histogram(fill = 'white') +
  labs(title = paste0('Speed dist for Type A at its max vol, lane', hist_type[1], ', len < ', hist_type[2], ' ft'),
       x = 'Speed', y = 'Count') + 
  # coord_cartesian(ylim = c(0, 3000))# +
  theme_black()
dev.off()

## summarize quantile for line marker
violin_quantiles_95 <- sidefire_vol_spd_2022_plot_A[which(sidefire_vol_spd_2022_plot_A$lane == as.numeric(hist_type[1]) &
                                                          sidefire_vol_spd_2022_plot_A$feet == as.numeric(hist_type[2])),] %>%
  group_by(voltype) %>%
  summarise(q95 = quantile(as.numeric(volperlane), 0.95))

if (hist_type[2] == '500') {
  hrcap_lower = AWCAP[which(rownames(AWCAP) == hist_type[1]), 1]/as.numeric(hist_type[1])
  hrcap_higher = AWCAP[which(rownames(AWCAP) == hist_type[1]), 2]/as.numeric(hist_type[1])
} else {
  hrcap_lower = AWCAP[which(rownames(AWCAP) == hist_type[1]), which(colnames(AWCAP) == hist_type[2])-1]/as.numeric(hist_type[1])
  hrcap_higher = AWCAP[which(rownames(AWCAP) == hist_type[1]), which(colnames(AWCAP) == hist_type[2])]/as.numeric(hist_type[1])
}

if (hist_type[1] == '6') {
  hrcap_lower = AWCAP[which(rownames(AWCAP) == '5'), 1]/as.numeric(hist_type[1])
  hrcap_higher = AWCAP[which(rownames(AWCAP) == '5'), 2]/as.numeric(hist_type[1])
} 


## violin plot
png(paste0("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFireOnly/Sidefire_voldist_typeA_lane",
           hist_type[1],'_lengthLT',hist_type[2],'ft.png'), width = 800, height = 600)
ggplot(sidefire_vol_spd_2022_plot_A[which(sidefire_vol_spd_2022_plot_A$lane == hist_type[1] &
                                            sidefire_vol_spd_2022_plot_A$feet == hist_type[2]),], aes(x = factor(voltype), y = as.numeric(volperlane))) +
  geom_violinhalf(trim = FALSE, fill = "skyblue", color = 'grey', scale = "width") + 
  geom_hline(aes(yintercept = hrcap_lower), color = "yellow", linetype = "dashed", linewidth = 2) + 
  annotate('text', 
           x = 0.5, y = hrcap_lower - 150,  # 1 unit above the line
           label = paste(as.character(hrcap_lower*as.numeric(hist_type[1])),', ',as.character(round(hrcap_lower,0)),' per lane', sep = ''), 
           fontface = "bold",          # Font style: plain, bold, italic, bold.italic
           size = 10,
           hjust = 0, vjust = 0, color = "yellow") +
  geom_hline(aes(yintercept = hrcap_higher), color = "green", linetype = "dashed", linewidth = 2) + 
  annotate('text', 
           x = 0.5, y = hrcap_higher + 50,  # 1 unit above the line
           label = paste(as.character(hrcap_higher*as.numeric(hist_type[1])),', ',as.character(round(hrcap_higher,0)),' per lane', sep = ''), 
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
  
  labs(title = paste0('Stats value distribution for Type A, lane', hist_type[1], ', length < ', hist_type[2], ' ft'),
       x = ' ', y = 'Volume per lane') + 
  scale_x_discrete(labels = c(vol90 = '90% quantile', vol95 = '95% quantile', 
                              volboxupper = 'calc. max', volmax = 'actual max')) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_black()
dev.off()

rm(hist_type, violin_quantiles_95)

################################################ comment out session ############################################
################################################ comment out session ############################################
################################################ comment out session ############################################
################################################ comment out session ############################################

################################## use parabola fitted from the SF_NPMRDS pair ##################################
################################## use parabola fitted from the SF_NPMRDS pair ##################################

################################## FRWY_BASIC ################################
vol_per_day_202202_frwy = vol_per_day_2022_feb_workday_transpose[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'FRWY_BASIC' &
                                                                         vol_per_day_2022_feb_workday_transpose$time == 'PK'),]

# Calculate fitted values manually
vol_per_day_202202_frwy <- vol_per_day_202202_frwy %>%
  mutate(fitted = params_mle_parab_frwybasic$b_mean * (params_mle_parab_frwybasic$a_mean - spd) * spd)

# Keep only the points at or above the parabola
vol_per_day_202202_frwy <- vol_per_day_202202_frwy %>%
  filter(vol/lane >= fitted,
         spd > 0.5 * params_mle_parab_frwybasic$a_mean)

y_vals <- seq(min(vol_per_day_202202_frwy$spd), max(vol_per_day_202202_frwy$spd), length.out = 100)
# Compute x values using x = (y - a)y
x_vals <- params_mle_parab_frwybasic$b_mean * (params_mle_parab_frwybasic$a_mean - y_vals) * y_vals

table(vol_per_day_202202_frwy$ffspd)

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFireOnly/frwybasic_PK_v_speed_filtered_SFonly.png",
    width = 800, height = 600, bg = 'black')
par(bg = 'black')
# x_y_bycategory = function(x, y, x_hat, y_hat, category, category_uniq, xlab, ylab, title, label, legend, legend.title) {
plot(vol_per_day_2022_feb_workday_transpose$vol[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'FRWY_BASIC' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')]/
       vol_per_day_2022_feb_workday_transpose$lane[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'FRWY_BASIC' &
                                                           vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     vol_per_day_2022_feb_workday_transpose$spd[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'FRWY_BASIC' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     pch = 16, size = 1.5, col = 'white',
     
     xlim = c(0, 2500), ylim = c(0, 85), 
     xlab = 'Flow per lane', ylab = 'Speed from Sidefire (MPH)', main = 'Sidefire flow-speed, FRWY_BASIC',
     
     # Axis label color
     col.lab = "white", col.axis = "white",col.main = "white",
     
     # specify font
     cex.main = 2, cex.lab = 2, font.lab = 2, 
     
     # Turn off default axes to customize them
     axes = FALSE
)
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
points(vol_per_day_202202_frwy$vol/vol_per_day_202202_frwy$lane, vol_per_day_202202_frwy$spd, pch = 4, col = 'yellow')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_frwybasic$b_mean*35*(params_mle_parab_frwybasic$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

################################## BASIC ################################
vol_per_day_202202_basic = vol_per_day_2022_feb_workday_transpose[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'BASIC' &
                                                                          vol_per_day_2022_feb_workday_transpose$time == 'PK'),]

# Calculate fitted values manually
vol_per_day_202202_basic <- vol_per_day_202202_basic %>%
  mutate(fitted = params_mle_parab_basic$b_mean * (params_mle_parab_basic$a_mean - spd) * spd)

# Keep only the points at or above the parabola
vol_per_day_202202_basic <- vol_per_day_202202_basic %>%
  filter(vol/lane >= fitted,
         spd > 0.5 * params_mle_parab_basic$a_mean)

table(vol_per_day_202202_basic$ffspd)

y_vals <- seq(min(vol_per_day_202202_basic$spd), max(vol_per_day_202202_basic$spd), length.out = 100)
# Compute x values using x = (y - a)y
x_vals <- params_mle_parab_basic$b_mean * (params_mle_parab_basic$a_mean - y_vals) * y_vals

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFireOnly/basic_PK_v_speed_filtered_SFonly.png",
    width = 800, height = 600, bg = 'black')
# x_y_bycategory = function(x, y, x_hat, y_hat, category, category_uniq, xlab, ylab, title, label, legend, legend.title) {
plot(vol_per_day_2022_feb_workday_transpose$vol[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'BASIC' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')]/
       vol_per_day_2022_feb_workday_transpose$lane[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'BASIC' &
                                                           vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     vol_per_day_2022_feb_workday_transpose$spd[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'BASIC' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     pch = 16, size = 1.5, col = 'white',
     
     xlim = c(0, 2500), ylim = c(0, 85), 
     xlab = 'Flow per lane', ylab = 'Speed from Sidefire (MPH)', main = 'Sidefire flow-speed, BASIC',
     
     # Axis label color
     col.lab = "white", col.axis = "white",col.main = "white",
     
     # specify font
     cex.main = 2, cex.lab = 2, font.lab = 2, 
     
     # Turn off default axes to customize them
     axes = FALSE
)
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
points(vol_per_day_202202_basic$vol/vol_per_day_202202_basic$lane, vol_per_day_202202_basic$spd, pch = 4, col = 'yellow')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_basic$b_mean*35*(params_mle_parab_basic$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

################################## MD ################################
vol_per_day_202202_md = vol_per_day_2022_feb_workday_transpose[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'MD' &
                                                                       vol_per_day_2022_feb_workday_transpose$time == 'PK'),]

# Calculate fitted values manually
vol_per_day_202202_md <- vol_per_day_202202_md %>%
  mutate(fitted = params_mle_parab_md$b_mean * (params_mle_parab_md$a_mean - spd) * spd)

# Keep only the points at or above the parabola
vol_per_day_202202_md <- vol_per_day_202202_md %>%
  filter(vol/lane >= fitted,
         spd > 0.5 * params_mle_parab_md$a_mean)

table(vol_per_day_202202_md$ffspd)

y_vals <- seq(min(vol_per_day_202202_md$spd), max(vol_per_day_202202_md$spd), length.out = 100)
# Compute x values using x = (y - a)y
x_vals <- params_mle_parab_md$b_mean * (params_mle_parab_md$a_mean - y_vals) * y_vals

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFireOnly/md_PK_v_speed_filtered_SFonly.png",
    width = 800, height = 600, bg = 'black')
# x_y_bycategory = function(x, y, x_hat, y_hat, category, category_uniq, xlab, ylab, title, label, legend, legend.title) {
plot(vol_per_day_2022_feb_workday_transpose$vol[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'MD' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')]/
       vol_per_day_2022_feb_workday_transpose$lane[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'MD' &
                                                           vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     vol_per_day_2022_feb_workday_transpose$spd[which(vol_per_day_2022_feb_workday_transpose$weavetype == 'MD' &
                                                        vol_per_day_2022_feb_workday_transpose$time == 'PK')],
     pch = 16, size = 1.5, col = 'white',
     
     xlim = c(0, 2500), ylim = c(0, 85), 
     xlab = 'Flow per lane', ylab = 'Speed from Sidefire (MPH)', main = 'Sidefire flow-speed, MD',
     
     # Axis label color
     col.lab = "white", col.axis = "white",col.main = "white",
     
     # specify font
     cex.main = 2, cex.lab = 2, font.lab = 2, 
     
     # Turn off default axes to customize them
     axes = FALSE
)
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
points(vol_per_day_202202_md$vol/vol_per_day_202202_md$lane, vol_per_day_202202_md$spd, pch = 4, col = 'yellow')
lines(x_vals, y_vals, col = 'red', lty = 2, lwd = 2)
lines(c(params_mle_parab_md$b_mean*35*(params_mle_parab_md$a_mean-35),2000),c(35,35), col = 'red', lty = 2, lwd = 2)
dev.off()

