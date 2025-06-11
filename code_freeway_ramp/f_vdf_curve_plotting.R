## vdf fitting plot

## this page specifies the curve fitting plots

x_y_bycategory = function(x, y, x_hat, y_hat, category, category_uniq, xlab, ylab, title, label, legend, legend.title) {
  # data includes at least x (vc), y (v), category
  # category_uniq specifies all possible category
  # params includes all params
  library(ggplot2)
  library(dplyr)
  
  # specify colors. Priority is light colors. If contrast is needed, add deep colors
  colors_deep = c('white', 'yellow', 'blue', 'green', 'red', 'orange')
  colors_light = c('white', 'lightyellow', 'lightblue', 'lightgreen', 'pink', rgb(1, 0.8, 0.5))
  
  par(bg = 'black')  # Bottom, Left, Top, Right margins
  
  # plot all field data
  plot(x, y,
       
       pch = 3, col = 'white',
       
       xlab = xlab, ylab = ylab, 
       xlim = c(0,max(c(x,x_hat, na.rm = T))), ylim = c(0,max(c(y, y_hat, na.rm = T))),
       
       # Axis label color
       col.lab = "white", 
       
       # Tick label color
       col.axis = "white",
       
       # Title color
       col.main = "white",
       
       # title 
       main = title,
       
       # specify font
       cex.main = 2,
       cex.lab = 2,          # Enlarge axis labels
       font.lab = 2,           # Bold axis labels
       
       # Turn off default axes to customize them
       axes = FALSE)  
  
  # Add x and y axes manually
  axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
  axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
  
  # add text
  usr <- par("usr")
  text(x = usr[2], y = usr[3], label, 
       adj = c(1, 0), cex = 2, font = 2, col = 'white')
  
  for (v in 1:length(category_uniq)) {
    points(x_hat[which(category == category_uniq[v])], 
           y_hat[which(category == category_uniq[v])],
           col = colors_deep[v+1], pch = 16)
  }
  
  if (legend == 1) {
    # Manually set legend position using coordinates
    legend_x <- par("usr")[1] + 0.005 * diff(par("usr")[1:2])  # a bit right of the left axis
    legend_y <- par("usr")[3] + 0.11* (length(category_uniq)) * diff(par("usr")[3:4])  # a bit above the bottom axis
    
    legend(x = legend_x, y = legend_y,
           legend = category_uniq,
           col = colors_deep[2:(length(category_uniq) + 1)],
           pch = rep(16, n = length(category_uniq)),
           title = legend.title,
           x.intersp = 0.5,              # Reduces space between symbol and text
           y.intersp = 1,              # Reduces vertical spacing between items
           text.col = "white",
           bg = "transparent",
           cex = 2)
  }

  
}


