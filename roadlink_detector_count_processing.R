library('arcgisbinding')
library('sf')
library('dplyr')


setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

vol_per_day_2022 = read.csv('20250410_capacity_recalculation/Database/vol_per_day_2022.csv', header = T)
vol_per_day_2022 = vol_per_day_2022[which(vol_per_day_2022$Year == 2022),] # only Y2022 records selected
# aggregate linkID in the detector file. Each row represents the count statistics (min, 15%, Q1 (25%), mean, 50%, Q3 (75%), 85%, 90%, 95%, 97.5%, 98%, 99%, max) & its corresponding at peak hours

# take downtown count as examples: ID = 1083, 456
vol_eg = t(vol_per_day_2022[which(vol_per_day_2022$LinkID == 1116)[150], seq(20, 112)])
spd_eg = t(vol_per_day_2022[which(vol_per_day_2022$LinkID == 1116)[150], seq(113,205)])
plot(vol_eg, spd_eg)

# create a sidefire data that matches with 2022 records
sidefire_vol_spd_2022 = data.frame(cbind(sidefire_inTaz_2025$ID, sidefire_inTaz_2025$LINKNAME, 
                              sidefire_inTaz_2025$nearlink_id_26, sidefire_inTaz_2025$nearlink_name_26))
colnames(sidefire_vol_spd_2022) = c('ID_detector', 'LinkName_detector', 'ID_Road', 'LinkName_Road')
sidefire_vol_spd_2022$volmin = 0; sidefire_vol_spd_2022$vol15 = 0; sidefire_vol_spd_2022$vol25 = 0; sidefire_vol_spd_2022$vol50 = 0
sidefire_vol_spd_2022$volavg = 0; sidefire_vol_spd_2022$vol75 = 0; sidefire_vol_spd_2022$vol85 = 0; sidefire_vol_spd_2022$vol90 = 0
sidefire_vol_spd_2022$vol95 = 0; sidefire_vol_spd_2022$vol975 = 0; sidefire_vol_spd_2022$vol99 = 0; sidefire_vol_spd_2022$volmax = 0

sidefire_vol_spd_2022$spdmin = 0; sidefire_vol_spd_2022$spd15 = 0; sidefire_vol_spd_2022$spd25 = 0; sidefire_vol_spd_2022$spd50 = 0
sidefire_vol_spd_2022$spdavg = 0; sidefire_vol_spd_2022$spd75 = 0; sidefire_vol_spd_2022$spd85 = 0; sidefire_vol_spd_2022$spd90 = 0
sidefire_vol_spd_2022$spd95 = 0; sidefire_vol_spd_2022$spd975 = 0; sidefire_vol_spd_2022$spd99 = 0; sidefire_vol_spd_2022$spdmax = 0

for (i in 1:nrow(sidefire_vol_spd_2022)) {
  link_i = vol_per_day_2022[which(vol_per_day_2022$LinkID == sidefire_vol_spd_2022$ID_detector[i])]
  # 6:30-9:00, count col 46-56, spd col 139-149; 15-18:30, count col 80-94, spd col 173-187
  
}

#### get quantile value
# Step 1: Get the 25th percentile value from A
q1_A <- quantile(df$A, probs = 0.25)
# e.g., q1_A = 20

# Step 2: Find the row where A is equal to the 25th percentile
# (If exact match doesn't exist, you might need to find the closest)
matching_row <- which.min(abs(df$A - q1_A))

# Step 3: Get the corresponding B value
corresponding_B <- df$B[matching_row]

# Result
corresponding_B