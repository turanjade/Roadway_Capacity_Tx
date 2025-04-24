library('arcgisbinding')
library('sf')
library('dplyr')
library(ggplot2)

setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

# try to use 95% percentile
sidefire_vol_spd_2022_am$vol95_perlane = sidefire_vol_spd_2022_am$vol95/sidefire_vol_spd_2022_am$AMlane
ggplot(sidefire_vol_spd_2022_am[which(sidefire_vol_spd_2022_am$FUNCL == 1 & sidefire_vol_spd_2022_am$vol95/sidefire_vol_spd_2022_am$AMlane <= 2500),], 
       aes(x = spd95, y = vol95/AMlane)) + geom_point()


ggplot(sidefire_vol_spd_2022_am[which(sidefire_vol_spd_2022_am$FUNCL < 8 & sidefire_vol_spd_2022_am$vol95/sidefire_vol_spd_2022_am$AMlane <= 2500),], 
       aes(x = factor(FUNCL), y = vol95/AMlane)) + geom_boxplot()

