# 7. supporting plot functions
# violin plot moved to z_6_1_vc_speed_curve_fitting_plot

################################## Function: regression metrics #####################################

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
      legend.title = element_text(color = "white", face = "bold", size = base_size + 2),
      legend.position = 'top'
    )
}

annotate_stats <- function(label) {
  annotate("text", x = Inf, y = 0, hjust = 1, vjust = 0,
           label = label, color = "white", size = 8, fontface = "bold")
}



### plot x y in black
plot_black = function(x, y, xlab, ylab, main) {
  par(bg = 'black')
  plot(x, y,
       pch = 16, 
       # size = 2,
       col = 'white', # Axis label color
       
       col.lab = "white", cex = 2,
       
       # Tick label color
       col.axis = "white",
       
       # Title color
       col.main = "white",
       
       # specify font
       cex.main = 2,
       cex.lab = 2,          # Enlarge axis labels
       font.lab = 2,           # Bold axis labels
       
       # Turn off default axes to customize them
       axes = FALSE,
       xlim = c(min(c(x,y)), max(c(x,y))), ylim = c(min(c(x,y)), max(c(x,y))),
       xlab = xlab, ylab = ylab, main = main
       )
  
  # Add x and y axes manually
  axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
  axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
  abline(a = 0, b = 1, col = "yellow", lty = 2)  # Adds x = y line
}

