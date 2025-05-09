### This code fits VDF and (possibly BPR) 


# define x as v/c ratio, y as (1/V-1/V0)/(1/V0)
# use average volume from SF and average speed from NPMRDS, data table: sf_2022_npmrds_2025_plot, weave type = FRWY_BASIC, source = npm
sf_2022_npmrds_2025_plot$vdf_x = (sf_2022_npmrds_2025_plot$avgvol/sf_2022_npmrds_2025_plot$lane)/sf_2022_npmrds_2025_plot$sfcapperlane
sf_2022_npmrds_2025_plot$vdf_y = (1/sf_2022_npmrds_2025_plot$avgspd - 1/sf_2022_npmrds_2025_plot$ffspd)/(1/sf_2022_npmrds_2025_plot$ffspd)

row_frwybasic_pk_npm = which(sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC' & 
                               sf_2022_npmrds_2025_plot$source == 'npm' & 
                               sf_2022_npmrds_2025_plot$ffspd > sf_2022_npmrds_2025_plot$avgspd &
                               (sf_2022_npmrds_2025_plot$time == 'am' | sf_2022_npmrds_2025_plot$time == 'pm'))

## fit model
set.seed(200)
# initial guess: c(8, -0.15, 1)
# bound, used in L-BFGS-B or Brent (Brent only for 1-D params), lower = c(1e-6, -Inf, 1e-6)
model_vdf_frwybasic_pk = optim(c(8, -0.15, 1), fn = nll, 
                               x = sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm], 
                               y = sf_2022_npmrds_2025_plot$vdf_y[row_frwybasic_pk_npm], 
                               method = "Nelder-Mead") #   method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
model_vdf_frwybasic_pk$par

## compare obs. with pred.
fit_vdf_frwybasic_pk = data.frame(x = sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm], 
                                  y = sf_2022_npmrds_2025_plot$vdf_y[row_frwybasic_pk_npm])

fit_vdf_frwybasic_pk$y_hat = CdbyT0(model_vdf_frwybasic_pk$par[1], 
                                    model_vdf_frwybasic_pk$par[2], 
                                    sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_pk_npm])


metrics = calculate_metrics(fit_vdf_frwybasic_pk$y, fit_vdf_frwybasic_pk$y_hat)
print(metrics)

label_text <- paste0(
  "No. Obs: ", length(row_frwybasic_pk_npm), "\n",
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,2), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_x_y.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_pk, aes(x = x)) +
  geom_point(aes(y = y), color = "white") +
  geom_line(aes(y = y_hat), color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text) +
  labs(title = "MLE-Calibrated Model", y = "Observed & fitted y", x = 'x')
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_y_yhat.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_pk, aes(x = y, y = y_hat)) +
  geom_point(color = "white") +
  geom_abline(color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text) +
  labs(title = "MLE-Calibrated Model", y = "Observed y", x = 'Fitted y')
dev.off()



## for OP
row_frwybasic_op_npm = which(sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC' & 
                               sf_2022_npmrds_2025_plot$source == 'npm' & 
                               sf_2022_npmrds_2025_plot$ffspd > sf_2022_npmrds_2025_plot$avgspd &
                               (sf_2022_npmrds_2025_plot$time != 'am' & sf_2022_npmrds_2025_plot$time != 'pm'))

## fit model
set.seed(200)
# initial guess: c(8, -0.15, 1)
# bound, used in L-BFGS-B or Brent (Brent only for 1-D params), lower = c(1e-6, -Inf, 1e-6)
model_vdf_frwybasic_op = optim(c(8, -0.15, 1), fn = ll_exp, 
                               x = sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_op_npm], 
                               y = sf_2022_npmrds_2025_plot$vdf_y[row_frwybasic_op_npm], 
                               method = "Nelder-Mead") #   method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
model_vdf_frwybasic_op$par

## compare obs. with pred.
fit_vdf_frwybasic_op = data.frame(x = sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_op_npm], 
                                  y = sf_2022_npmrds_2025_plot$vdf_y[row_frwybasic_op_npm])

fit_vdf_frwybasic_op$y_hat = CdbyT0(model_vdf_frwybasic_op$par[1], 
                                    model_vdf_frwybasic_op$par[2], 
                                    sf_2022_npmrds_2025_plot$vdf_x[row_frwybasic_op_npm])


metrics = calculate_metrics(fit_vdf_frwybasic_op$y, fit_vdf_frwybasic_op$y_hat)
print(metrics)

label_text <- paste0(
  "No. Obs: ", length(row_frwybasic_op_npm), "\n",
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,2), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_x_y.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_op, aes(x = x)) +
  geom_point(aes(y = y), color = "white") +
  geom_line(aes(y = y_hat), color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text) +
  labs(title = "MLE-Calibrated Model", y = "Observed & fitted y", x = 'x')
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VDF_MLE_Normal_FRWYBASIC_PK_y_yhat.png", 
    width = 800, height = 600)
ggplot(fit_vdf_frwybasic_op, aes(x = y, y = y_hat)) +
  geom_point(color = "white") +
  geom_abline(color = "yellow", linewidth = 1.5) +
  theme_black() +
  annotate_stats(label_text) +
  labs(title = "MLE-Calibrated Model", y = "Observed y", x = 'Fitted y')
dev.off()

################################## define likelihood estimation ##################################

## MLE assume standard normal distribution error
nll = function(params, x, y) {
  a = params[1]
  e = params[2]
  sigma = params[3] # estimated from data
  
  # calculate b and h from a and e
  b = (2*a-1)/(2*a-2)
  h = 1+sqrt(a^2*(1-e)^2 + b^2) - a*(1-e) - b
  
  y_hat = 1+sqrt(a^2 * (1-x+e)^2 + b^2) - a * (1-x+e) - b - h
  
  # Negative log-likelihood for normal distribution
  -sum(dnorm(y, mean = y_hat, sd = sigma, log = TRUE))
}

## likelihood estimation assume log-normal errors
ll_ln = function(params, x, y) {
  a = params[1]
  e = params[2]
  sigma = params[3] # estimated from data
  
  # calculate b and h from a and e
  b = (2*a-1)/(2*a-2)
  h = 1+sqrt(a^2*(1-e)^2 + b^2) - a*(1-e) - b
  
  mu = 1+sqrt(a^2 * (1-x+e)^2 + b^2) - a * (1-x+e) - b - h
  
  -sum(dlnorm(y, meanlog = log(mu), sdlog = sigma, log = T))
}

## likelihood estimation assume exponential errors
ll_exp = function(params, x, y) {
  a = params[1]
  e = params[2]
  sigma = params[3] # estimated from data
  
  # calculate b and h from a and e
  b = (2*a-1)/(2*a-2)
  h = 1+sqrt(a^2*(1-e)^2 + b^2) - a*(1-e) - b
  
  mu = 1+sqrt(a^2 * (1-x+e)^2 + b^2) - a * (1-x+e) - b - h
  
  # Check for invalid values
  if (any(mu <= 0) || any(!is.finite(mu)) || any(y < 0)) return(Inf)
  
  lambda = 1 / mu
  
  -sum(dexp(y, rate = lambda, log = TRUE))
}


################################## define y_hat function for VDF #################################
### VDF: T = T0 + Cd + Sd + Ud, T0: ff travel time, Cd: conical congestion delay, Sd: signalized delay, Ud: un-signalized delay
## CdbyT0 = (1/V-1/V0)/(1/V0), which is a function of a, b, e, h, x
##           as: 1+sqrt(a^2 * (1- V/C + e)^2 + b^2) - a*(1 - V/C + e) - b -(1 + sqrt(a^2 * (1 - e)^2 + b^2) - a * (1 - e) - b)

CdbyT0 = function(a, e, x) {
  b = (2*a-1)/(2*a-2)
  h = 1+sqrt(a^2*(1-e)^2 + b^2) - a*(1-e) - b
  return(1+sqrt(a^2 * (1-x+e)^2 + b^2) - a * (1-x+e) - b - h)
}


################################## calculate metrics function #####################################

calculate_metrics <- function(y, y_hat) {
  # RMSE: Root Mean Squared Error
  rmse <- sqrt(mean((y - y_hat)^2))
  
  # % Error: Percentage Error
  percent_error <- (sum(y_hat) - sum(y))/sum(y) * 100
  
  # R-squared (R²)
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - y_hat)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  return(list(rmse = rmse, percent_error = percent_error, r_squared = r_squared))
}


################################ define a high contrast plot style, use this function in ggplot ################################
# Define your custom black background theme # 👈 Use your custom theme here
theme_black <- function(base_size = 20) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "black", color = NA),
      plot.background = element_rect(fill = "black", color = NA),
      panel.grid.major = element_line(color = "gray30"),
      panel.grid.minor = element_line(color = "gray20"),
      axis.line = element_line(color = "white", size = 1),
      axis.ticks = element_line(color = "white"),
      
      # Larger axis tick text
      axis.text = element_text(color = "white", size = base_size + 2),
      
      # Larger, bold axis titles
      axis.title = element_text(color = "white", size = base_size + 4, face = "bold"),
      
      # Bold and large title, subtitle, and caption
      plot.title = element_text(color = "white", size = base_size + 6, face = "bold"),
      # plot.subtitle = element_text(color = "white", size = base_size + 3, face = "bold"),
      # plot.caption = element_text(color = "white", size = base_size + 2),
      
      # Legend styling
      legend.background = element_rect(fill = "black"),
      legend.key = element_rect(fill = "black"),
      legend.text = element_text(color = "white", size = base_size),
      legend.title = element_text(color = "white", face = "bold", size = base_size + 2)
    )
}

annotate_stats <- function(label) {
  annotate("text", x = Inf, y = 0, hjust = 1, vjust = 0,
           label = label, color = "white", size = 8, fontface = "bold")
}
