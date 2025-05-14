### This code fits VDF and BPR 

# updated May 12, use real T and T0 instead of the converted value to fit

setwd("C:/Users/rtu/OneDrive - The North Central Texas Council of Governments/Documents/0_ModelDataDevelopment/")

library(ggplot2)
library(viridis)  # for colorblind-friendly palettes
library(dplyr)


#### define df to use. sf_2022_npmrds_2025 or sidefire_vol_spd_2022


########################################################## VDF fitting ###############################################################
# define x as v/c ratio, y as (1/V-1/V0)/(1/V0)
# use average volume from SF and average speed from NPMRDS, data table: sf_2022_npmrds_2025_plot, weave type = FRWY_BASIC, source = npm
sf_2022_npmrds_2025_plot$vdf_x = (sf_2022_npmrds_2025_plot$avgvol/sf_2022_npmrds_2025_plot$lane)/sf_2022_npmrds_2025_plot$sfcapperlane
sf_2022_npmrds_2025_plot$vdf_t = as.numeric(sf_2022_npmrds_2025_plot$length)/sf_2022_npmrds_2025_plot$avgspd
sf_2022_npmrds_2025_plot$vdf_t0 = as.numeric(sf_2022_npmrds_2025_plot$length)/sf_2022_npmrds_2025_plot$ffspd



####################################### for PK VDF #######################################
row_frwybasic_pk_npm = which(sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC' & 
                               sf_2022_npmrds_2025_plot$source == 'npm' & 
                               # sf_2022_npmrds_2025_plot$ffspd > sf_2022_npmrds_2025_plot$avgspd &
                               (sf_2022_npmrds_2025_plot$time == 'am' | sf_2022_npmrds_2025_plot$time == 'pm'))


######################### 10-folder cross validation VDF PK ###########################
## needs revision. Use grid * 10-folder parameter calibration to get the best result
## fit model
# initial guess: c(8, -0.15, 1)
# bound, used in L-BFGS-B or Brent (Brent only for 1-D params), lower = c(1e-6, -Inf, 1e-6)
# fitting method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),

set.seed(42)
n <- length(sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm])
folds <- sample(rep(1:10, length.out = n))

x =  sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm]
T0 = sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_pk_npm]
T_obs = sf_2022_npmrds_2025_plot$vdf_t[row_frwybasic_pk_npm]
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
  T0_train <- T0[train_idx]
  T_obs_train <- T_obs[train_idx]
  
  x_valid <- x[valid_idx]
  T0_valid <- T0[valid_idx]
  T_obs_valid <- T_obs[valid_idx]
  
  # Fit model on training fold
  fit <- optim(
    par = c(8, -0.15, 1), 
    fn = nll_vdf,
    x = x_train,
    t0 = T0_train,
    t_obs = T_obs_train,
    method = 'Nelder-Mead'
    # method = 'SANN'
    # method = "L-BFGS-B",
    # lower = c(-Inf, -Inf, 1e-6),
    # upper = c(Inf, Inf, Inf)
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

print(params_mle)

rm(a, e, x, T0, T_obs, est_params, folds, i, j, k, n, nll_valid, params, 
   T_obs_train, T_obs_valid, T_pred, T0_train, T0_valid, x_train, x_valid, train_idx, valid_idx, cv_results, fit)

######################### # compare obs. with pred. VDF PK ########################################
fit_vdf_frwybasic_pk = data.frame(x = sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm], 
                                  y = sf_2022_npmrds_2025_plot$vdf_t[row_frwybasic_pk_npm],
                                  spd = sf_2022_npmrds_2025_plot$avgspd[row_frwybasic_pk_npm],
                                  ffspd = sf_2022_npmrds_2025_plot$ffspd[row_frwybasic_pk_npm],
                                  length = sf_2022_npmrds_2025_plot$length[row_frwybasic_pk_npm],
                                  cap = sf_2022_npmrds_2025_plot$sfcapperlane[row_frwybasic_pk_npm])

fit_vdf_frwybasic_pk$y_hat = VDF(params_mle$a_mean,
                                 params_mle$e_mean,
                                 sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_pk_npm],
                                 sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm])

fit_vdf_frwybasic_pk$y_hat_init = VDF(8, -0.15, 
                                      sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_pk_npm],
                                      sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm])


metrics = calculate_metrics(fit_vdf_frwybasic_pk$y, fit_vdf_frwybasic_pk$y_hat)
print(metrics)

metrics_init = calculate_metrics(fit_vdf_frwybasic_pk$y, fit_vdf_frwybasic_pk$y_hat_init)
print(metrics_init)

label_text_vdf <- paste0(
  "A_conical: ", round(params_mle$a_mean,2), "\n",
  "VDF_shift: ", round(params_mle$e_mean,2), "\n",
  "\n",
  
  "No. Obs: ", length(row_frwybasic_pk_npm), "\n",
  
  
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,4), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)

label_text_vdf_init <- paste0(
  "No. Obs: ", length(row_frwybasic_pk_npm), "\n",
  "% Error: ", round(metrics_init$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics_init$rmse,4), '\n',
  
  "% RSQ: ", round(metrics_init$r_squared * 100, 2), '%'
)

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_x_y_obs.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_pk, aes(x = x)) +
  geom_point(aes(y = y), color = "white") +
  # geom_line(aes(y = y_hat), color = "yellow", linewidth = 1.5) +
  theme_black() +
  #annotate_stats(label_text) +
  labs(title = "VC ratio & obs. travel time, PK", y = "Observed travel time", x = 'V/C ratio')
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_y_yhat_Nelder.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_pk, aes(x = y, y = y_hat)) +
  geom_point(color = "white") +
  geom_abline(color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text_vdf) +
  labs(title = "MLE-Calibrated Model, PK", y = "Fitted travel time", x = 'Observed travel time')
dev.off()

## y_hat initial parameter in TAFT
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_y_yhat_init.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_pk, aes(x = y, y = y_hat_init)) +
  geom_point(color = "white") +
  geom_abline(color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text_vdf_init) +
  labs(title = "MLE-Calibrated Model, PK, TAFT parameter", y = "Fitted travel time", x = 'Observed travel time')
dev.off()

### qqplot
norm <- function(x) (x - min(x)) / (max(x) - min(x))
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_y_yhat_QQ.png", 
    width = 800, height = 600, bg = 'black')
qqplot(norm(fit_vdf_frwybasic_pk$y), norm(fit_vdf_frwybasic_pk$y_hat), xlab = "Observed Quantiles", ylab = "Predicted Quantiles",
       main = "Quantile-Quantile Plot: Observed vs Predicted, Calibrated param", 
       pch = 16, col = 'white',
       xlim = c(0, 1), ylim = c(0, 1),
       col.lab = "white", # Axis label color
       col.axis = "white",# Tick label color
       col.main = "white",# Title color
       cex.main = 2,
       cex.lab = 2,          # Enlarge axis labels
       font.lab = 2,           # Bold axis labels
       axes = FALSE)# Title color)
abline(0, 1, col = "yellow", lty = 2, lwd = 2)  # Identity line
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
dev.off()

### qqplot
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_y_yhat_init_QQ.png", 
    width = 800, height = 600, bg = 'black')
qqplot(norm(fit_vdf_frwybasic_pk$y), norm(fit_vdf_frwybasic_pk$y_hat_init), xlab = "Observed Quantiles", ylab = "Predicted Quantiles",
       main = "Quantile-Quantile Plot: Observed vs Predicted, TAFT param", 
       pch = 16, col = 'white',
       xlim = c(0, 1), ylim = c(0, 1),
       col.lab = "white", # Axis label color
       col.axis = "white",# Tick label color
       col.main = "white",# Title color
       cex.main = 2,
       cex.lab = 2,          # Enlarge axis labels
       font.lab = 2,           # Bold axis labels
       axes = FALSE)# Title color)
abline(0, 1, col = "yellow", lty = 2, lwd = 2)  # Identity line
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
dev.off()

####################################### plot vc ratio & speed scatter, grouped by ffspd, using taft & mle fitted ####################
# Set plot background to black, points to white solid dots
par(bg = "black")  # Set the background of the entire plotting device

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_vc_vhat_byffspd.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 60)], 
     as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 60)])/fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 60)],
     
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (calculated using fitted params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 65)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 65)])/fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 65)],
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 70)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 70)])/fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 70)],
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 75)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 75)])/fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 75)],
       col = 'red', pch = 16)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_vc_vhat_init_byffspd.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 60)], 
     as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 60)])/fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 60)],
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (TAFT params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 65)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 65)])/fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 65)],
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 70)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 70)])/fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 70)],
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 75)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 75)])/fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 75)],
       col = 'red', pch = 16)
dev.off()


## actual data
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_vc_vdata_byffspd.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 60)], 
     
     as.numeric(fit_vdf_frwybasic_pk$length[
       which(fit_vdf_frwybasic_pk$ffspd == 60)])/
       fit_vdf_frwybasic_pk$y[which(fit_vdf_frwybasic_pk$ffspd == 60)],
     
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", 
     xlim = c(0,0.8), ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (actual data), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 65)], 
       
       as.numeric(fit_vdf_frwybasic_pk$length[
         which(fit_vdf_frwybasic_pk$ffspd == 65)])/
         fit_vdf_frwybasic_pk$y[which(fit_vdf_frwybasic_pk$ffspd == 65)],
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 70)], 
       
       as.numeric(fit_vdf_frwybasic_pk$length[
         which(fit_vdf_frwybasic_pk$ffspd == 70)])/
         fit_vdf_frwybasic_pk$y[which(fit_vdf_frwybasic_pk$ffspd == 70)],
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 75)], 
       
       as.numeric(fit_vdf_frwybasic_pk$length[
         which(fit_vdf_frwybasic_pk$ffspd == 75)])/
         fit_vdf_frwybasic_pk$y[which(fit_vdf_frwybasic_pk$ffspd == 75)],
       
       col = 'red', pch = 16)
dev.off()


########### plot density-VC ratio, VDF ########### 
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_density_vc_byffspd.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 60)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 60)]/
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 60)])/
       fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 60)]),

      fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 60)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 60)], 
      
     pch = 16, 
     xlab = "Density (veh/mi/ln)", col = 'white',
     ylab = "Volume (veh/hr/ln)",
     xlim = c(0, 70), ylim = c(0, 2500),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (VDF calibrated params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 65)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 65)]/
         (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 65)])/
            fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 65)]),
       
       fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 65)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 65)], 
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 70)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 70)]/
         (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 70)])/
            fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 70)]),
       
       fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 70)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 70)], 
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 75)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 75)]/
         (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 75)])/
            fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 75)]),
       
       fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 75)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 75)], 
       
       col = 'red', pch = 16)
dev.off()


png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_density_vc_init_byffspd.png",
    width = 800, height = 600, bg = 'black')

plot(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 60)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 60)]/
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 60)])/
          fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 60)]),
     
     fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 60)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 60)], 
     
     pch = 16, 
     xlab = "Density (veh/mi/ln)", col = 'white',
     ylab = "Volume (veh/hr/ln)", 
     xlim = c(0, 70), ylim = c(0, 2500),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (VDF TAFT params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 65)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 65)]/
         (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 65)])/
            fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 65)]),
       
       fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 65)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 65)], 
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 70)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 70)]/
         (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 70)])/
            fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 70)]),
       
       fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 70)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 70)], 
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 75)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 75)]/
         (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 75)])/
            fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 75)]),
       
       fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 75)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 75)], 
       
       col = 'red', pch = 16)

dev.off()


##################### plot flow speed, VDF ####################
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_v_speed_byffspd.png",
    width = 800, height = 600, bg = 'black')
par(bg = 'black')
plot(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 60)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 60)],
     
     (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 60)])/
          fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 60)]),
     
     pch = 16, col = 'green', 
     xlab = "flow (veh/hr/ln)",
     ylab = "speed (mph)",
     xlim = c(0, 2500), ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (VDF calibrated params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 65)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 65)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 65)])/
          fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 65)]),
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 70)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 70)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 70)])/
          fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 70)]),
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 75)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 75)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 75)])/
          fit_vdf_frwybasic_pk$y_hat[which(fit_vdf_frwybasic_pk$ffspd == 75)]),
       
       col = 'red', pch = 16)

abline(a = 0, b = 1/11, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/18, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/26, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/35, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/45, col = 'white', lwd = 2, lty = 2)
dev.off()


png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_v_speed_init_byffspd.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 60)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 60)],
     
     (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 60)])/
        fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 60)]),
     
     pch = 16, 
     xlab = "flow (veh/hr/ln)", col = 'green',
     ylab = "speed (mph)",
     xlim = c(0, 2500), ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (VDF calibrated params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 65)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 65)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 65)])/
          fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 65)]),
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 70)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 70)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 70)])/
          fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 70)]),
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk$x[which(fit_vdf_frwybasic_pk$ffspd == 75)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 75)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 75)])/
          fit_vdf_frwybasic_pk$y_hat_init[which(fit_vdf_frwybasic_pk$ffspd == 75)]),
       
       col = 'red', pch = 16)

abline(a = 0, b = 1/11, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/18, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/26, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/35, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/45, col = 'white', lwd = 2, lty = 2)
dev.off()

########################### plot a continuous vc-speed by artificially assume v/c for each different ffspd, VDF ######################
sf_2022_npmrds_2025_plot$x_art = runif(nrow(sf_2022_npmrds_2025_plot), min = 0.01, max = 1.2)

fit_vdf_frwybasic_pk = data.frame(x = sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm], 
                                  x_art = sf_2022_npmrds_2025_plot$x_art[row_frwybasic_pk_npm], 
                                  y = sf_2022_npmrds_2025_plot$vdf_t[row_frwybasic_pk_npm],
                                  spd = sf_2022_npmrds_2025_plot$avgspd[row_frwybasic_pk_npm],
                                  ffspd = sf_2022_npmrds_2025_plot$ffspd[row_frwybasic_pk_npm],
                                  length = sf_2022_npmrds_2025_plot$length[row_frwybasic_pk_npm],
                                  cap = sf_2022_npmrds_2025_plot$sfcapperlane[row_frwybasic_pk_npm])

fit_vdf_frwybasic_pk$y_hat_art = VDF(params_mle$a_mean,
                                     params_mle$e_mean,
                                     sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_pk_npm],
                                     sf_2022_npmrds_2025_plot$x_art[row_frwybasic_pk_npm])

fit_vdf_frwybasic_pk$y_hat_art_init = VDF(8,
                                     -0.15,
                                     sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_pk_npm],
                                     sf_2022_npmrds_2025_plot$x_art[row_frwybasic_pk_npm])



png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_vc_vhat_byffspd_artificialVC.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 60)], 
     as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 60)])/fit_vdf_frwybasic_pk$y_hat_art[which(fit_vdf_frwybasic_pk$ffspd == 60)],
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (calculated using fitted params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 65)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 65)])/fit_vdf_frwybasic_pk$y_hat_art[which(fit_vdf_frwybasic_pk$ffspd == 65)],
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 70)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 70)])/fit_vdf_frwybasic_pk$y_hat_art[which(fit_vdf_frwybasic_pk$ffspd == 70)],
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 75)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 75)])/fit_vdf_frwybasic_pk$y_hat_art[which(fit_vdf_frwybasic_pk$ffspd == 75)],
       col = 'red', pch = 16)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_vc_vhat_init_byffspd_artificialVC.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 60)], 
     as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 60)])/fit_vdf_frwybasic_pk$y_hat_art_init[which(fit_vdf_frwybasic_pk$ffspd == 60)],
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (calculated using fitted params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 65)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 65)])/fit_vdf_frwybasic_pk$y_hat_art_init[which(fit_vdf_frwybasic_pk$ffspd == 65)],
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 70)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 70)])/fit_vdf_frwybasic_pk$y_hat_art_init[which(fit_vdf_frwybasic_pk$ffspd == 70)],
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 75)], 
       as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 75)])/fit_vdf_frwybasic_pk$y_hat_art_init[which(fit_vdf_frwybasic_pk$ffspd == 75)],
       col = 'red', pch = 16)
dev.off()



png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_v_speed_byffspd_artificialVC.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 60)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 60)],
     
     (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 60)])/
        fit_vdf_frwybasic_pk$y_hat_art[which(fit_vdf_frwybasic_pk$ffspd == 60)]),
     
     pch = 16, 
     xlab = "flow (veh/hr/ln)", col = 'green',
     ylab = "speed (mph)",
     xlim = c(0, 2500), ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (VDF calibrated params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 65)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 65)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 65)])/
          fit_vdf_frwybasic_pk$y_hat_art[which(fit_vdf_frwybasic_pk$ffspd == 65)]),
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 70)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 70)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 70)])/
          fit_vdf_frwybasic_pk$y_hat_art[which(fit_vdf_frwybasic_pk$ffspd == 70)]),
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 75)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 75)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 75)])/
          fit_vdf_frwybasic_pk$y_hat_art[which(fit_vdf_frwybasic_pk$ffspd == 75)]),
       
       col = 'red', pch = 16)

abline(a = 0, b = 1/11, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/18, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/26, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/35, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/45, col = 'white', lwd = 2, lty = 2)
dev.off()


png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_v_speed_init_byffspd_artificialVC.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 60)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 60)],
     
     (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 60)])/
        fit_vdf_frwybasic_pk$y_hat_art_init[which(fit_vdf_frwybasic_pk$ffspd == 60)]),
     
     pch = 16, 
     xlab = "flow (veh/hr/ln)", col = 'green',
     ylab = "speed (mph)",
     xlim = c(0, 2500), ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (TAFT params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 65)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 65)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 65)])/
          fit_vdf_frwybasic_pk$y_hat_art_init[which(fit_vdf_frwybasic_pk$ffspd == 65)]),
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 70)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 70)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 70)])/
          fit_vdf_frwybasic_pk$y_hat_art_init[which(fit_vdf_frwybasic_pk$ffspd == 70)]),
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk$x_art[which(fit_vdf_frwybasic_pk$ffspd == 75)] * fit_vdf_frwybasic_pk$cap[which(fit_vdf_frwybasic_pk$ffspd == 75)],
       
       (as.numeric(fit_vdf_frwybasic_pk$length[which(fit_vdf_frwybasic_pk$ffspd == 75)])/
          fit_vdf_frwybasic_pk$y_hat_art_init[which(fit_vdf_frwybasic_pk$ffspd == 75)]),
       
       col = 'red', pch = 16)

abline(a = 0, b = 1/11, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/18, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/26, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/35, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/45, col = 'white', lwd = 2, lty = 2)
dev.off()


########################################################## VDF fitting, for LOS A-D only ###############################################################
# define x as v/c ratio, y as (1/V-1/V0)/(1/V0)
# use average volume from SF and average speed from NPMRDS, data table: sf_2022_npmrds_2025_plot, weave type = FRWY_BASIC, source = npm
sf_2022_npmrds_2025_plot$vdf_x = (sf_2022_npmrds_2025_plot$avgvol/sf_2022_npmrds_2025_plot$lane)/sf_2022_npmrds_2025_plot$sfcapperlane
sf_2022_npmrds_2025_plot$vdf_t = as.numeric(sf_2022_npmrds_2025_plot$length)/sf_2022_npmrds_2025_plot$avgspd
sf_2022_npmrds_2025_plot$vdf_t0 = as.numeric(sf_2022_npmrds_2025_plot$length)/sf_2022_npmrds_2025_plot$ffspd

####################################### for PK VDF, LOS A-D only #######################################
row_frwybasic_pk_npm_losad = which(sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC' &                         #basic freeway
                               sf_2022_npmrds_2025_plot$source == 'npm' &                                       # use NPMRDS speed
                                 sf_2022_npmrds_2025_plot$los != 'E' & sf_2022_npmrds_2025_plot$los != 'F' &    #only select good LOS
                               # sf_2022_npmrds_2025_plot$ffspd > sf_2022_npmrds_2025_plot$avgspd &
                               (sf_2022_npmrds_2025_plot$time == 'am' | sf_2022_npmrds_2025_plot$time == 'pm')) # PK only


######################### 10-folder cross validation VDF PK ###########################
## needs revision. Use grid * 10-folder parameter calibration to get the best result

set.seed(42)
n <- length(sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm_losad])
folds <- sample(rep(1:10, length.out = n))

x =  sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm_losad]
T0 = sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_pk_npm_losad]
T_obs = sf_2022_npmrds_2025_plot$vdf_t[row_frwybasic_pk_npm_losad]
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
  T0_train <- T0[train_idx]
  T_obs_train <- T_obs[train_idx]
  
  x_valid <- x[valid_idx]
  T0_valid <- T0[valid_idx]
  T_obs_valid <- T_obs[valid_idx]
  
  # Fit model on training fold
  fit <- optim(
    par = c(8, -0.15, 1), 
    fn = nll_vdf,
    x = x_train,
    t0 = T0_train,
    t_obs = T_obs_train,
    method = 'Nelder-Mead'
    # method = 'SANN'
    # method = "L-BFGS-B",
    # lower = c(-Inf, -Inf, 1e-6),
    # upper = c(Inf, Inf, Inf)
  )
  
  # Evaluate on validation fold
  est_params <- fit$par
  nll_valid <- nll_vdf(est_params, x_valid, T0_valid, T_obs_valid)
  
  # Store results
  cv_results[k, c("a", "e", "sigma")] <- est_params
  cv_results[k, "negLL"] <- nll_valid
}

params_mle_losad <- cv_results %>%
  summarise(across(c(a, e, sigma, negLL), list(mean = mean, sd = sd)))

print(params_mle_losad)

rm(a, e, x, T0, T_obs, est_params, folds, i, j, k, n, nll_valid, params, 
   T_obs_train, T_obs_valid, T_pred, T0_train, T0_valid, x_train, x_valid, valid_idx, train_idx, 
   cv_results, fit)

######################### # compare obs. with pred. VDF PK ########################################
fit_vdf_frwybasic_pk_losad = data.frame(x = sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm_losad], 
                                        y = sf_2022_npmrds_2025_plot$vdf_t[row_frwybasic_pk_npm_losad],
                                        spd = sf_2022_npmrds_2025_plot$avgspd[row_frwybasic_pk_npm_losad],
                                        ffspd = sf_2022_npmrds_2025_plot$ffspd[row_frwybasic_pk_npm_losad],
                                        length = sf_2022_npmrds_2025_plot$length[row_frwybasic_pk_npm_losad],
                                        cap = sf_2022_npmrds_2025_plot$sfcapperlane[row_frwybasic_pk_npm_losad])

fit_vdf_frwybasic_pk_losad$y_hat = VDF(params_mle_losad$a_mean,
                                       params_mle_losad$e_mean,
                                 sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_pk_npm_losad],
                                 sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm_losad])

fit_vdf_frwybasic_pk_losad$y_hat_init = VDF(8, -0.15, 
                                      sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_pk_npm_losad],
                                      sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm_losad])


metrics = calculate_metrics(fit_vdf_frwybasic_pk_losad$y, fit_vdf_frwybasic_pk_losad$y_hat)
print(metrics)

metrics_init = calculate_metrics(fit_vdf_frwybasic_pk_losad$y, fit_vdf_frwybasic_pk_losad$y_hat_init)
print(metrics_init)

label_text_vdf <- paste0(
  "A_conical: ", round(params_mle_losad$a_mean,2), "\n",
  "VDF_shift: ", round(params_mle_losad$e_mean,2), "\n",
  "\n",
  
  "No. Obs: ", length(row_frwybasic_pk_npm_losad), "\n",
  
  
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,4), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)

label_text_vdf_init <- paste0(
  "No. Obs: ", length(row_frwybasic_pk_npm_losad), "\n",
  "% Error: ", round(metrics_init$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics_init$rmse,4), '\n',
  
  "% RSQ: ", round(metrics_init$r_squared * 100, 2), '%'
)

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_x_y_obs_losad.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_pk_losad, aes(x = x)) +
  geom_point(aes(y = y), color = "white") +
  # geom_line(aes(y = y_hat), color = "yellow", linewidth = 1.5) +
  theme_black() +
  #annotate_stats(label_text) +
  labs(title = "VC ratio & obs. travel time, PK", y = "Observed travel time", x = 'V/C ratio')
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_y_yhat_Nelder_losad.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_pk_losad, aes(x = y, y = y_hat)) +
  geom_point(color = "white") +
  geom_abline(color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text_vdf) +
  labs(title = "MLE-Calibrated Model, PK", y = "Fitted travel time", x = 'Observed travel time')
dev.off()

## y_hat initial parameter in TAFT
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_y_yhat_init_losad.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_pk_losad, aes(x = y, y = y_hat_init)) +
  geom_point(color = "white") +
  geom_abline(color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text_vdf_init) +
  labs(title = "MLE-Calibrated Model, PK, TAFT parameter", y = "Fitted travel time", x = 'Observed travel time')
dev.off()


####################################### plot vc ratio & speed scatter, grouped by ffspd, using taft & mle fitted, LOS A to D ####################
# Set plot background to black, points to white solid dots
par(bg = "black")  # Set the background of the entire plotting device

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_vc_vhat_byffspd_losad.png",
    width = 800, height = 600, bg = 'black')
plot(x = fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)], 
     
     y = as.numeric(fit_vdf_frwybasic_pk_losad$length[
         which(fit_vdf_frwybasic_pk_losad$ffspd == 60)])/
         fit_vdf_frwybasic_pk_losad$y_hat[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)],
     
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (calibrated VDF params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)], 
       
       as.numeric(fit_vdf_frwybasic_pk_losad$length[
         which(fit_vdf_frwybasic_pk_losad$ffspd == 65)])/
         fit_vdf_frwybasic_pk_losad$y_hat[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)],
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)], 
       
       as.numeric(fit_vdf_frwybasic_pk_losad$length[
         which(fit_vdf_frwybasic_pk_losad$ffspd == 70)])/
         fit_vdf_frwybasic_pk_losad$y_hat[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)],
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)], 
       
       as.numeric(fit_vdf_frwybasic_pk_losad$length[
         which(fit_vdf_frwybasic_pk_losad$ffspd == 75)])/
         fit_vdf_frwybasic_pk_losad$y_hat[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)],
       
       col = 'red', pch = 16)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_vc_vhat_init_byffspd_losad.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)], 
     
     as.numeric(fit_vdf_frwybasic_pk_losad$length[
       which(fit_vdf_frwybasic_pk_losad$ffspd == 60)])/
       fit_vdf_frwybasic_pk_losad$y_hat_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)],
     
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (TAFT params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)], 
       
       as.numeric(fit_vdf_frwybasic_pk_losad$length[
         which(fit_vdf_frwybasic_pk_losad$ffspd == 65)])/
         fit_vdf_frwybasic_pk_losad$y_hat_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)],
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)], 
       
       as.numeric(fit_vdf_frwybasic_pk_losad$length[
         which(fit_vdf_frwybasic_pk_losad$ffspd == 70)])/
         fit_vdf_frwybasic_pk_losad$y_hat_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)],
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)], 
       
       as.numeric(fit_vdf_frwybasic_pk_losad$length[
         which(fit_vdf_frwybasic_pk_losad$ffspd == 75)])/
         fit_vdf_frwybasic_pk_losad$y_hat_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)],
       
       col = 'red', pch = 16)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_vc_vdata_byffspd_losad.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)], 
     
     as.numeric(fit_vdf_frwybasic_pk_losad$length[
       which(fit_vdf_frwybasic_pk_losad$ffspd == 60)])/
       fit_vdf_frwybasic_pk_losad$y[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)],
     
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", 
     xlim = c(0,0.8), ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (actual data), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)], 
       
       as.numeric(fit_vdf_frwybasic_pk_losad$length[
         which(fit_vdf_frwybasic_pk_losad$ffspd == 65)])/
         fit_vdf_frwybasic_pk_losad$y[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)],
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)], 
       
       as.numeric(fit_vdf_frwybasic_pk_losad$length[
         which(fit_vdf_frwybasic_pk_losad$ffspd == 70)])/
         fit_vdf_frwybasic_pk_losad$y[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)],
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)], 
       
       as.numeric(fit_vdf_frwybasic_pk_losad$length[
         which(fit_vdf_frwybasic_pk_losad$ffspd == 75)])/
         fit_vdf_frwybasic_pk_losad$y[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)],
       
       col = 'red', pch = 16)
dev.off()

########################### plot a continuous vc-speed by artificially assume v/c for each different ffspd, VDF, LOS A to D ######################
sf_2022_npmrds_2025_plot$x_art_losad = runif(nrow(sf_2022_npmrds_2025_plot), min = 0.01, max = 0.8)

fit_vdf_frwybasic_pk_losad = data.frame(x = sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm], 
                                  x_art = sf_2022_npmrds_2025_plot$x_art_losad[row_frwybasic_pk_npm], 
                                  y = sf_2022_npmrds_2025_plot$vdf_t[row_frwybasic_pk_npm],
                                  spd = sf_2022_npmrds_2025_plot$avgspd[row_frwybasic_pk_npm],
                                  ffspd = sf_2022_npmrds_2025_plot$ffspd[row_frwybasic_pk_npm],
                                  length = sf_2022_npmrds_2025_plot$length[row_frwybasic_pk_npm],
                                  cap = sf_2022_npmrds_2025_plot$sfcapperlane[row_frwybasic_pk_npm])

fit_vdf_frwybasic_pk_losad$y_hat_art = VDF(params_mle$a_mean,
                                     params_mle$e_mean,
                                     sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_pk_npm],
                                     sf_2022_npmrds_2025_plot$x_art_losad[row_frwybasic_pk_npm])

fit_vdf_frwybasic_pk_losad$y_hat_art_init = VDF(8,
                                          -0.15,
                                          sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_pk_npm],
                                          sf_2022_npmrds_2025_plot$x_art_losad[row_frwybasic_pk_npm])



png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_vc_vhat_byffspd_artificialVC_losad.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)], 
     as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)])/fit_vdf_frwybasic_pk_losad$y_hat_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)],
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (calculated using fitted params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)], 
       as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)])/fit_vdf_frwybasic_pk_losad$y_hat_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)],
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)], 
       as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)])/fit_vdf_frwybasic_pk_losad$y_hat_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)],
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)], 
       as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)])/fit_vdf_frwybasic_pk_losad$y_hat_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)],
       col = 'red', pch = 16)

dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_vc_vhat_init_byffspd_artificialVC_losad.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)], 
     as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)])/fit_vdf_frwybasic_pk_losad$y_hat_art_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)],
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (calculated using fitted params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)], 
       as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)])/fit_vdf_frwybasic_pk_losad$y_hat_art_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)],
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)], 
       as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)])/fit_vdf_frwybasic_pk_losad$y_hat_art_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)],
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)], 
       as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)])/fit_vdf_frwybasic_pk_losad$y_hat_art_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)],
       col = 'red', pch = 16)
dev.off()



png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_v_speed_byffspd_artificialVC_losad.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)] * fit_vdf_frwybasic_pk_losad$cap[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)],
     
     (as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)])/
        fit_vdf_frwybasic_pk_losad$y_hat_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)]),
     
     pch = 16, 
     xlab = "flow (veh/hr/ln)", col = 'green',
     ylab = "speed (mph)",
     xlim = c(0, 2500), ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (VDF calibrated params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)] * fit_vdf_frwybasic_pk_losad$cap[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)],
       
       (as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)])/
          fit_vdf_frwybasic_pk_losad$y_hat_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)]),
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)] * fit_vdf_frwybasic_pk_losad$cap[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)],
       
       (as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)])/
          fit_vdf_frwybasic_pk_losad$y_hat_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)]),
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)] * fit_vdf_frwybasic_pk_losad$cap[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)],
       
       (as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)])/
          fit_vdf_frwybasic_pk_losad$y_hat_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)]),
       
       col = 'red', pch = 16)

abline(a = 0, b = 1/11, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/18, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/26, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/35, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/45, col = 'white', lwd = 2, lty = 2)
dev.off()


png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_FRWYBASIC_PK_v_speed_init_byffspd_artificialVC_losad.png",
    width = 800, height = 600, bg = 'black')
plot(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)] * fit_vdf_frwybasic_pk_losad$cap[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)],
     
     (as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)])/
        fit_vdf_frwybasic_pk_losad$y_hat_art_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 60)]),
     
     pch = 16, 
     xlab = "flow (veh/hr/ln)", col = 'green',
     ylab = "speed (mph)",
     xlim = c(0, 2500), ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (TAFT params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)] * fit_vdf_frwybasic_pk_losad$cap[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)],
       
       (as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)])/
          fit_vdf_frwybasic_pk_losad$y_hat_art_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 65)]),
       
       col = 'yellow', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)] * fit_vdf_frwybasic_pk_losad$cap[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)],
       
       (as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)])/
          fit_vdf_frwybasic_pk_losad$y_hat_art_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 70)]),
       
       col = 'lightblue', pch = 16)

points(fit_vdf_frwybasic_pk_losad$x_art[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)] * fit_vdf_frwybasic_pk_losad$cap[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)],
       
       (as.numeric(fit_vdf_frwybasic_pk_losad$length[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)])/
          fit_vdf_frwybasic_pk_losad$y_hat_art_init[which(fit_vdf_frwybasic_pk_losad$ffspd == 75)]),
       
       col = 'red', pch = 16)

abline(a = 0, b = 1/11, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/18, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/26, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/35, col = 'white', lwd = 2, lty = 2)
abline(a = 0, b = 1/45, col = 'white', lwd = 2, lty = 2)
dev.off()




########################################################## BPR fitting ###############################################################
# define x as v/c ratio, y as (1/V-1/V0)/(1/V0)
# use average volume from SF and average speed from NPMRDS, data table: sf_2022_npmrds_2025_plot, weave type = FRWY_BASIC, source = npm
sf_2022_npmrds_2025_plot$bpr_x = (sf_2022_npmrds_2025_plot$avgvol/sf_2022_npmrds_2025_plot$lane)/sf_2022_npmrds_2025_plot$sfcapperlane
sf_2022_npmrds_2025_plot$bpr_t = as.numeric(sf_2022_npmrds_2025_plot$length)/sf_2022_npmrds_2025_plot$avgspd
sf_2022_npmrds_2025_plot$bpr_t0 = as.numeric(sf_2022_npmrds_2025_plot$length)/sf_2022_npmrds_2025_plot$ffspd



####################################### for PK BPR #######################################
row_frwybasic_pk_npm = which(sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC' & 
                               sf_2022_npmrds_2025_plot$source == 'npm' & 
                               # sf_2022_npmrds_2025_plot$ffspd > sf_2022_npmrds_2025_plot$avgspd &
                               (sf_2022_npmrds_2025_plot$time == 'am' | sf_2022_npmrds_2025_plot$time == 'pm'))


######################### 10-folder cross validation BPR PK ###########################
## needs revision. Use grid * 10-folder parameter calibration to get the best result
## fit model
# initial guess: c(8, -0.15, 1)
# bound, used in L-BFGS-B or Brent (Brent only for 1-D params), lower = c(1e-6, -Inf, 1e-6)
# fitting method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),

set.seed(42)
n <- length(sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm])
folds <- sample(rep(1:10, length.out = n))

x =  sf_2022_npmrds_2025_plot$bpr_x[row_frwybasic_pk_npm]
T0 = sf_2022_npmrds_2025_plot$bpr_t0[row_frwybasic_pk_npm]
T_obs = sf_2022_npmrds_2025_plot$bpr_t[row_frwybasic_pk_npm]

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
  T0_train <- T0[train_idx]
  T_obs_train <- T_obs[train_idx]
  
  x_valid <- x[valid_idx]
  T0_valid <- T0[valid_idx]
  T_obs_valid <- T_obs[valid_idx]
  
  # Fit model on training fold
  fit <- optim(
    par = c(0.2, 2.8, 1), 
    fn = nll_bpr,
    x = x_train,
    t0 = T0_train,
    t_obs = T_obs_train,
    method = 'Nelder-Mead'
    # method = 'SANN'
    # method = "L-BFGS-B",
    # lower = c(-Inf, -Inf, 1e-6),
    # upper = c(Inf, Inf, Inf)
  )
  
  # Evaluate on validation fold
  est_params <- fit$par
  nll_valid <- nll_vdf(est_params, x_valid, T0_valid, T_obs_valid)
  
  # Store results
  cv_results[k, c("a", "e", "sigma")] <- est_params
  cv_results[k, "negLL"] <- nll_valid
}

params_mle_bpr <- cv_results %>%
  summarise(across(c(a, e, sigma, negLL), list(mean = mean, sd = sd)))

print(params_mle_bpr)

rm(a, e, x, T0, T_obs, est_params, folds, i, j, k, n, nll_valid, params, 
   T_obs_train, T_obs_valid, T_pred, T0_train, T0_valid, x_train, x_valid)

######################### # compare obs. with pred. BPR PK ########################################
fit_bpr_frwybasic_pk = data.frame(x = sf_2022_npmrds_2025_plot$bpr_x[row_frwybasic_pk_npm], 
                                  y = sf_2022_npmrds_2025_plot$bpr_t[row_frwybasic_pk_npm],
                                  spd = sf_2022_npmrds_2025_plot$avgspd[row_frwybasic_pk_npm],
                                  ffspd = sf_2022_npmrds_2025_plot$ffspd[row_frwybasic_pk_npm],
                                  length = sf_2022_npmrds_2025_plot$length[row_frwybasic_pk_npm])

fit_bpr_frwybasic_pk$y_hat = BPR_1(params_mle_bpr$a_mean,
                                 params_mle_bpr$e_mean,
                                 sf_2022_npmrds_2025_plot$bpr_t0[row_frwybasic_pk_npm],
                                 sf_2022_npmrds_2025_plot$bpr_x[row_frwybasic_pk_npm])

fit_bpr_frwybasic_pk$y_hat_init = BPR_1(0.2, 2.8, 
                                      sf_2022_npmrds_2025_plot$bpr_t0[row_frwybasic_pk_npm],
                                      sf_2022_npmrds_2025_plot$bpr_x[row_frwybasic_pk_npm])


metrics = calculate_metrics(fit_bpr_frwybasic_pk$y, fit_bpr_frwybasic_pk$y_hat)
print(metrics)

metrics_init = calculate_metrics(fit_bpr_frwybasic_pk$y, fit_bpr_frwybasic_pk$y_hat_init)
print(metrics_init)

label_text_bpr <- paste0(
  "a: ", round(params_mle_bpr$a_mean,2), "\n",
  "b: ", round(params_mle_bpr$e_mean,2), "\n",
  "\n",
  
  "No. Obs: ", length(row_frwybasic_pk_npm), "\n",
  
  
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,4), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)

label_text_bpr_init <- paste0(
  "No. Obs: ", length(row_frwybasic_pk_npm), "\n",
  "% Error: ", round(metrics_init$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics_init$rmse,4), '\n',
  
  "% RSQ: ", round(metrics_init$r_squared * 100, 2), '%'
)


png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/BPR_MLE_Normal_FRWYBASIC_PK_y_yhat_Nelder.png", 
    width = 800, height = 600)
ggplot(fit_bpr_frwybasic_pk, aes(x = y, y = y_hat)) +
  geom_point(color = "white") +
  geom_abline(color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text_bpr) +
  labs(title = "MLE-Calibrated BPR, PK", y = "Fitted travel time", x = 'Observed travel time')
dev.off()

## y_hat initial parameter in TAFT
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_y_yhat_init.png", 
    width = 800, height = 600)
ggplot(fit_bpr_frwybasic_pk, aes(x = y, y = y_hat_init)) +
  geom_point(color = "white") +
  geom_abline(color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text_bpr_init) +
  labs(title = "MLE-Calibrated Model, PK, TAFT parameter", y = "Fitted travel time", x = 'Observed travel time')
dev.off()


####################################### plot vc ratio & speed scatter, grouped by ffspd, using bpr init & mle fitted ####################
# Set plot background to black, points to white solid dots
par(bg = "black")  # Set the background of the entire plotting device

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/BPR_MLE_FRWYBASIC_PK_vc_vhat_byffspd.png",
    width = 800, height = 600, bg = 'black')
plot(fit_bpr_frwybasic_pk$x[which(fit_bpr_frwybasic_pk$ffspd == 60)], 
     as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 60)])/fit_bpr_frwybasic_pk$y_hat[which(fit_bpr_frwybasic_pk$ffspd == 60)],
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (calibrated BPR params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_bpr_frwybasic_pk$x[which(fit_bpr_frwybasic_pk$ffspd == 65)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 65)])/fit_bpr_frwybasic_pk$y_hat[which(fit_bpr_frwybasic_pk$ffspd == 65)],
       col = 'yellow', pch = 16)

points(fit_bpr_frwybasic_pk$x[which(fit_bpr_frwybasic_pk$ffspd == 70)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 70)])/fit_bpr_frwybasic_pk$y_hat[which(fit_bpr_frwybasic_pk$ffspd == 70)],
       col = 'lightblue', pch = 16)

points(fit_bpr_frwybasic_pk$x[which(fit_bpr_frwybasic_pk$ffspd == 75)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 75)])/fit_bpr_frwybasic_pk$y_hat[which(fit_bpr_frwybasic_pk$ffspd == 75)],
       col = 'red', pch = 16)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/BPR_MLE_FRWYBASIC_PK_vc_vhat_init_byffspd.png",
    width = 800, height = 600, bg = 'black')
plot(fit_bpr_frwybasic_pk$x[which(fit_bpr_frwybasic_pk$ffspd == 60)], 
     as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 60)])/fit_bpr_frwybasic_pk$y_hat_init[which(fit_bpr_frwybasic_pk$ffspd == 60)],
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (BPR init params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_bpr_frwybasic_pk$x[which(fit_bpr_frwybasic_pk$ffspd == 65)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 65)])/fit_bpr_frwybasic_pk$y_hat_init[which(fit_bpr_frwybasic_pk$ffspd == 65)],
       col = 'yellow', pch = 16)

points(fit_bpr_frwybasic_pk$x[which(fit_bpr_frwybasic_pk$ffspd == 70)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 70)])/fit_bpr_frwybasic_pk$y_hat_init[which(fit_bpr_frwybasic_pk$ffspd == 70)],
       col = 'lightblue', pch = 16)

points(fit_bpr_frwybasic_pk$x[which(fit_bpr_frwybasic_pk$ffspd == 75)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 75)])/fit_bpr_frwybasic_pk$y_hat_init[which(fit_bpr_frwybasic_pk$ffspd == 75)],
       col = 'red', pch = 16)
dev.off()


########################### plot a continuous vc-speed by artificially assume v/c for each different ffspd, BPR ######################
sf_2022_npmrds_2025_plot$x_art = runif(nrow(sf_2022_npmrds_2025_plot), min = 0.01, max = 1.2)

fit_bpr_frwybasic_pk = data.frame(x = sf_2022_npmrds_2025_plot$bpr_x[row_frwybasic_pk_npm], 
                                  x_art = sf_2022_npmrds_2025_plot$x_art[row_frwybasic_pk_npm], 
                                  y = sf_2022_npmrds_2025_plot$bpr_t[row_frwybasic_pk_npm],
                                  spd = sf_2022_npmrds_2025_plot$avgspd[row_frwybasic_pk_npm],
                                  ffspd = sf_2022_npmrds_2025_plot$ffspd[row_frwybasic_pk_npm],
                                  length = sf_2022_npmrds_2025_plot$length[row_frwybasic_pk_npm])

fit_bpr_frwybasic_pk$y_hat_art = BPR_1(params_mle_bpr$a_mean,
                                       params_mle_bpr$e_mean,
                                     sf_2022_npmrds_2025_plot$bpr_t0[row_frwybasic_pk_npm],
                                     sf_2022_npmrds_2025_plot$x_art[row_frwybasic_pk_npm])

fit_bpr_frwybasic_pk$y_hat_art_init = BPR_1(0.2, 2.8,
                                          sf_2022_npmrds_2025_plot$bpr_t0[row_frwybasic_pk_npm],
                                          sf_2022_npmrds_2025_plot$x_art[row_frwybasic_pk_npm])



png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/BPR_MLE_FRWYBASIC_PK_vc_vhat_byffspd_artificialVC.png",
    width = 800, height = 600, bg = 'black')
plot(fit_bpr_frwybasic_pk$x_art[which(fit_bpr_frwybasic_pk$ffspd == 60)], 
     as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 60)])/fit_bpr_frwybasic_pk$y_hat_art[which(fit_bpr_frwybasic_pk$ffspd == 60)],
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (BPR params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_bpr_frwybasic_pk$x_art[which(fit_bpr_frwybasic_pk$ffspd == 65)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 65)])/fit_bpr_frwybasic_pk$y_hat_art[which(fit_bpr_frwybasic_pk$ffspd == 65)],
       col = 'yellow', pch = 16)

points(fit_bpr_frwybasic_pk$x_art[which(fit_bpr_frwybasic_pk$ffspd == 70)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 70)])/fit_bpr_frwybasic_pk$y_hat_art[which(fit_bpr_frwybasic_pk$ffspd == 70)],
       col = 'lightblue', pch = 16)

points(fit_bpr_frwybasic_pk$x_art[which(fit_bpr_frwybasic_pk$ffspd == 75)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 75)])/fit_bpr_frwybasic_pk$y_hat_art[which(fit_bpr_frwybasic_pk$ffspd == 75)],
       col = 'red', pch = 16)
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/BPR_MLE_FRWYBASIC_PK_vc_vhat_init_byffspd_artificialVC.png",
    width = 800, height = 600, bg = 'black')
plot(fit_bpr_frwybasic_pk$x_art[which(fit_bpr_frwybasic_pk$ffspd == 60)], 
     as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 60)])/fit_bpr_frwybasic_pk$y_hat_art_init[which(fit_bpr_frwybasic_pk$ffspd == 60)],
     pch = 16, xlab = "V/C ratio", col = 'white',
     ylab = "Speed (MPH)", ylim = c(0, 80),
     col.lab = "white", # Axis label color
     col.axis = "white",# Tick label color
     col.main = "white",# Title color
     main = "VC ratio vs speed (calculated using fitted params), freeway basic",
     cex.main = 2,
     cex.lab = 2,          # Enlarge axis labels
     font.lab = 2,           # Bold axis labels
     axes = FALSE)  # Turn off default axes to customize them)
# Add x and y axes manually
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

points(fit_bpr_frwybasic_pk$x_art[which(fit_bpr_frwybasic_pk$ffspd == 65)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 65)])/fit_bpr_frwybasic_pk$y_hat_art_init[which(fit_bpr_frwybasic_pk$ffspd == 65)],
       col = 'yellow', pch = 16)

points(fit_bpr_frwybasic_pk$x_art[which(fit_bpr_frwybasic_pk$ffspd == 70)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 70)])/fit_bpr_frwybasic_pk$y_hat_art_init[which(fit_bpr_frwybasic_pk$ffspd == 70)],
       col = 'lightblue', pch = 16)

points(fit_bpr_frwybasic_pk$x_art[which(fit_bpr_frwybasic_pk$ffspd == 75)], 
       as.numeric(fit_bpr_frwybasic_pk$length[which(fit_bpr_frwybasic_pk$ffspd == 75)])/fit_bpr_frwybasic_pk$y_hat_art_init[which(fit_bpr_frwybasic_pk$ffspd == 75)],
       col = 'red', pch = 16)
dev.off()



####################################### for OP VDF #######################################
row_frwybasic_op_npm = which(sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC' & 
                               sf_2022_npmrds_2025_plot$source == 'npm' & 
                               # sf_2022_npmrds_2025_plot$ffspd > sf_2022_npmrds_2025_plot$avgspd &
                               (sf_2022_npmrds_2025_plot$time == 'amop' | sf_2022_npmrds_2025_plot$time == 'pmop'))



## 10-folder cross validation
set.seed(42)
n <- length(sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_op_npm])
folds <- sample(rep(1:10, length.out = n))

library(dplyr)

x =  sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_op_npm]
T0 = sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_op_npm]
T_obs = sf_2022_npmrds_2025_plot$vdf_t[row_frwybasic_op_npm]
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
  T0_train <- T0[train_idx]
  T_obs_train <- T_obs[train_idx]
  
  x_valid <- x[valid_idx]
  T0_valid <- T0[valid_idx]
  T_obs_valid <- T_obs[valid_idx]
  
  # Fit model on training fold
  fit <- optim(
    par = c(8, -0.15, 1), 
    fn = nll_vdf,
    x = x_train,
    t0 = T0_train,
    t_obs = T_obs_train,
    method = 'Nelder-Mead'
    # method = 'SANN'
    # method = "L-BFGS-B",
    # lower = c(-Inf, -Inf, 1e-6),
    # upper = c(Inf, Inf, Inf)
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

print(params_mle)


## compare obs. with pred.
fit_vdf_frwybasic_op = data.frame(x = sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_op_npm], 
                                  y = sf_2022_npmrds_2025_plot$vdf_t[row_frwybasic_op_npm])

fit_vdf_frwybasic_op$y_hat = VDF(params_mle$a_mean,
                                 params_mle$e_mean,
                                 sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_op_npm],
                                 sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_op_npm])

fit_vdf_frwybasic_op$y_hat_init = VDF(8, -0.15, 
                                      sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_op_npm],
                                      sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_op_npm])


metrics = calculate_metrics(fit_vdf_frwybasic_op$y, fit_vdf_frwybasic_op$y_hat)
print(metrics)

metrics_init = calculate_metrics(fit_vdf_frwybasic_op$y, fit_vdf_frwybasic_op$y_hat_init)
print(metrics_init)

label_text <- paste0(
  "A_conical: ", round(params_mle$a_mean,2), "\n",
  "VDF_shift: ", round(params_mle$e_mean,2), "\n",
  "\n",
  
  "No. Obs: ", length(row_frwybasic_op_npm), "\n",
  
  
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,4), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)

label_text_init <- paste0(
  "No. Obs: ", length(row_frwybasic_op_npm), "\n",
  "% Error: ", round(metrics_init$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics_init$rmse,4), '\n',
  
  "% RSQ: ", round(metrics_init$r_squared * 100, 2), '%'
)

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_OP_x_y_obs.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_op, aes(x = x)) +
  geom_point(aes(y = y), color = "white") +
  # geom_line(aes(y = y_hat), color = "yellow", linewidth = 1.5) +
  theme_black() +
  #annotate_stats(label_text) +
  labs(title = "VC ratio & obs. travel time, OP", y = "Observed travel time", x = 'V/C ratio')
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_OP_y_yhat_Nelder.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_op, aes(x = y, y = y_hat)) +
  geom_point(color = "white") +
  geom_abline(color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text) +
  labs(title = "MLE-Calibrated Model, OP", y = "Observed travel time", x = 'Fitted travel time')
dev.off()

## y_hat initial parameter in TAFT
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_OP_y_yhat_init.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_op, aes(x = y, y = y_hat)) +
  geom_point(color = "white") +
  geom_abline(color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text_init) +
  labs(title = "MLE-Calibrated Model, OP, TAFT parameter", y = "Observed travel time", x = 'Fitted travel time')
dev.off()


rm(train_idx, valid_idx, a_seq, e_seq, label_text, label_text_init, row_frwybasic_op_npm, row_frwybasic_pk_npm, 
   start_grid, params_mle, results)

rm(df, fit, fit_vdf_frwybasic_op, fit_vdf_frwybasic_pk, boot_estimates, cv_results, 
   metrics, metrics_init, model_vdf_frwybasic_op, model_vdf_frwybasic_pk, nll_matrix, result_df, sample)



################################## Function: VDF #################################
### VDF: T = T0 + Cd + Sd + Ud, T0: ff travel time, Cd: conical congestion delay, Sd: signalized delay, Ud: un-signalized delay
## CdbyT0 = (1/V-1/V0)/(1/V0), which is a function of a, b, e, h, x
##           as: 1+sqrt(a^2 * (1- V/C + e)^2 + b^2) - a*(1 - V/C + e) - b -(1 + sqrt(a^2 * (1 - e)^2 + b^2) - a * (1 - e) - b)

VDF = function(a, e, t0, x) {
  b = (2*a-1)/(2*a-2)
  h = 1+sqrt(a^2*(1-e)^2 + b^2) - a*(1-e) - b
  return(t0 + t0 * (1+sqrt(a^2 * (1-x+e)^2 + b^2) - a * (1-x+e) - b - h))
}

################################## Write excel ###################################

# save file to excel to fit conical shape
library('openxlsx')
wb = createWorkbook()
addWorksheet(wb, 'frwybasic_pk')
addWorksheet(wb, 'frwybasic_op')
writeData(wb, 'frwybasic_pk', sf_2022_npmrds_2025_plot[row_frwybasic_pk_npm,], rowNames = F)
writeData(wb, 'frwybasic_op', sf_2022_npmrds_2025_plot[row_frwybasic_op_npm,], rowNames = F)
saveWorkbook(wb, file = '20250410_capacity_recalculation\\RoadNetwork_2026\\Sensor_count\\05132025_sf_2022_npmrds_2025_pk_op_conicalcurve.xlsx', overwrite = F)



################################## Function: MLE VDF ###################################

## MLE assume standard normal distribution error
nll_vdf = function(params, x, t0, t_obs) {
  a <- params[1]
  e <- params[2]
  sigma <- params[3]
  
  # Constrain sigma to be positive
  if (sigma <= 0) return(Inf)
  
  # calculate b and h from a and e
  b = (2*a-1)/(2*a-2)
  h = 1+sqrt(a^2*(1-e)^2 + b^2) - a*(1-e) - b
  
  t_pred = t0 + t0 * (1+sqrt(a^2 * (1-x+e)^2 + b^2) - a * (1-x+e) - b - h)
  
  residuals <- t_obs - t_pred
  n <- length(t_obs)
  
  
  ll_negative <- 0.5 * n * log(2 * pi * sigma^2) + 0.5 * sum(residuals^2) / sigma^2
  return(ll_negative)
}

## likelihood estimation assume log-normal errors
ll_ln_vdf = function(params, x, y) {
  a = params[1]
  e = params[2]
  sigma = params[3] # estimated from data
  
  # calculate b and h from a and e
  b = (2*a-1)/(2*a-2)
  h = 1+sqrt(a^2*(1-e)^2 + b^2) - a*(1-e) - b
  
  y_hat = 1+sqrt(a^2 * (1-x+e)^2 + b^2) - a * (1-x+e) - b - h
  
  -sum(dlnorm(y, meanlog = log(y_hat), sdlog = sigma, log = T))
}

## likelihood estimation assume exponential errors
ll_exp_vdf = function(params, x, y) {
  a = params[1]
  e = params[2]
  sigma = params[3] # estimated from data
  
  # calculate b and h from a and e
  b = (2*a-1)/(2*a-2)
  h = 1+sqrt(a^2*(1-e)^2 + b^2) - a*(1-e) - b
  
  y_hat = 1+sqrt(a^2 * (1-x+e)^2 + b^2) - a * (1-x+e) - b - h
  
  # Check for invalid values
  if (any(y_hat <= 0) || any(!is.finite(y_hat)) || any(y < 0)) return(Inf)
  
  lambda = 1 / y_hat
  
  -sum(dexp(y, rate = lambda, log = TRUE))
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

## likelihood estimation assume exponential errors
ll_exp_bpr = function(params, x, y) {
  
  a = params[1]
  b = params[2]
  
  y_hat = 1 + a * (x)^b
  
  # Check for invalid values
  if (any(y_hat <= 0) || any(!is.finite(y_hat)) || any(y < 0)) return(Inf)
  
  lambda = 1 / y_hat
  
  -sum(dexp(y, rate = lambda, log = TRUE))
}



#################################### comment out check ########################################################
################################### Visualize likelihood surface ###############################################
# Grid search over a and e
a_seq <- seq(5, 15, length.out = 200)
e_seq <- seq(-2, 2, length.out = 200)

# Matrix to store log-likelihoods
nll_matrix <- matrix(NA, nrow = length(a_seq), ncol = length(e_seq))

for (i in seq_along(a_seq)) {
  for (j in seq_along(e_seq)) {
    params <- c(a_seq[i], e_seq[j], sigma = 0.0001)  # fix sigma arbitrarily for plotting
    nll_matrix[i, j] <- nll_vdf(params, 
                                sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm], 
                                sf_2022_npmrds_2025_plot$vdf_t0[row_frwybasic_pk_npm], 
                                sf_2022_npmrds_2025_plot$vdf_t[row_frwybasic_pk_npm])
  }
}

# Plot contour
library(ggplot2)
library(reshape2)

df <- melt(nll_matrix)
df$a <- rep(a_seq, each = length(e_seq))
df$e <- rep(e_seq, times = length(a_seq))

ggplot(df, aes(x = a, y = e, z = value)) +
  geom_contour_filled() +
  labs(title = "Negative Log-Likelihood Surface, PK", x = "a", y = "e", fill = "NLL") +
  theme_black()


## use multi-start bfgs
start_grid <- expand.grid(a = a_seq, e = e_seq)
results <- apply(start_grid, 1, function(start) {
  optim(
    par = c(start["a"], start["e"], sigma = 1),
    fn = nll_vdf,
    x = x, t0 = T0, t_obs = T_obs,
    method = "L-BFGS-B",
    lower = c(1.01, -1, 1e-6),
    upper = c(10, 1, Inf)
    
  )
})

# Extract parameter and log-likelihood values
result_df <- do.call(rbind, lapply(results, function(res) {
  c(a = res$par[1], e = res$par[2], sigma = res$par[3], negLL = res$value)
}))

result_df <- as.data.frame(result_df)
result_df$Rsq <- NA  # Placeholder for R-squared

predict_model <- function(a, e, x, T0) {
  b <- (2 * a - 1) / (2 * a - 2)
  h <- 1 + sqrt(a^2 * (1 - e)^2 + b^2) - a * (1 - e) - b
  T_pred <- T0 + T0 * (1 + sqrt(a^2 * (1 - x + e)^2 + b^2) - a * (1 - x + e) - b - h)
  return(T_pred)
}

for (i in seq_len(nrow(result_df))) {
  T_pred <- predict_model(result_df$a[i], result_df$e[i], x, T0)
  result_df$Rsq[i] <- 1 - sum((T_obs - T_pred)^2) / sum((T_obs - mean(T_obs))^2)
}


##### clean environment ### 
rm(metrics, metrics_init, fit, cv_results, params_mle, params_mle_bpr, label_text_bpr, label_text_bpr_init, 
   label_text_vdf, label_text_vdf_init, train_idx, valid_idx, wb, row_frwybasic_op_npm, row_frwybasic_pk_npm)