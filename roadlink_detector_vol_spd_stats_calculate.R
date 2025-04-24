library('arcgisbinding')
library('sf')
library('dplyr')


setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

vol_per_day_2022 = read.csv('20250410_capacity_recalculation/Database/vol_per_day_2022.csv', header = T)
vol_per_day_2022 = vol_per_day_2022[which(vol_per_day_2022$Year == 2022),] # only Y2022 records selected
# aggregate linkID in the detector file. Each row represents the count statistics (min, 15%, Q1 (25%), mean, 50%, Q3 (75%), 85%, 90%, 95%, 97.5%, 98%, 99%, max) & its corresponding at peak hours

# create a sidefire data that matches with 2022 records
###### peak hour speed & vol
# AM and PM should be separate due to different lane settings
# freeway speed in roadlink: PKFRSPD_A, PKFRSPD_B, Col 66, 67, speed in mph
# peak hours & cols in sensors: 
# AM 6:30-9:00, count col 46-56, spd col 139-149; 
# PM 15-18:30, count col 80-94, spd col 173-187
# initiate peak hour vol_speed statistical records 
# vol per day file obtained from sql server collected by detectors. the link ID corresponds to sensors ID
# split pk to am and pm
sidefire_vol_spd_2022_am = data.frame(cbind(sidefire_inTaz_2025$ID, sidefire_inTaz_2025$LINKNAME, 
                              sidefire_inTaz_2025$nearlink_id_26, sidefire_inTaz_2025$nearlink_name_26))
colnames(sidefire_vol_spd_2022_am) = c('ID_detector', 'LinkName_detector', 'ID_Road', 'LinkName_Road')

sidefire_vol_spd_2022_am$FUNCL = sidefire_inTaz_2025$FUNCL
sidefire_vol_spd_2022_am$AMlane = 0 #; sidefire_vol_spd_2022_am$PMlane = 0
sidefire_vol_spd_2022_am$spdff = 0

sidefire_vol_spd_2022_am$volmin = 0; sidefire_vol_spd_2022_am$vol15 = 0; sidefire_vol_spd_2022_am$vol25 = 0; sidefire_vol_spd_2022_am$vol50 = 0
sidefire_vol_spd_2022_am$volavg = 0; sidefire_vol_spd_2022_am$vol75 = 0; sidefire_vol_spd_2022_am$vol85 = 0; sidefire_vol_spd_2022_am$vol90 = 0
sidefire_vol_spd_2022_am$vol95 = 0; sidefire_vol_spd_2022_am$vol975 = 0; sidefire_vol_spd_2022_am$vol99 = 0; sidefire_vol_spd_2022_am$volmax = 0


sidefire_vol_spd_2022_am$spdmin = 0; sidefire_vol_spd_2022_am$spd15 = 0; sidefire_vol_spd_2022_am$spd25 = 0; sidefire_vol_spd_2022_am$spd50 = 0
sidefire_vol_spd_2022_am$spdavg = 0; sidefire_vol_spd_2022_am$spd75 = 0; sidefire_vol_spd_2022_am$spd85 = 0; sidefire_vol_spd_2022_am$spd90 = 0
sidefire_vol_spd_2022_am$spd95 = 0; sidefire_vol_spd_2022_am$spd975 = 0; sidefire_vol_spd_2022_am$spd99 = 0; sidefire_vol_spd_2022_am$spdmax = 0

# function: return NA stats if all values of an array is NA
allNA_returnNA = function(array) {
  if (all(is.na(array))) {
    return(NA)
  } else {
    return(array)
  }
}

missingsensors = matrix(0, nrow = 0, ncol = 2)

# select AM 
for (i in 1:nrow(sidefire_vol_spd_2022_am)) {
  link_i = vol_per_day_2022[which(vol_per_day_2022$LinkID == sidefire_vol_spd_2022_am$ID_detector[i]),]
  skipflag = F
  if (nrow(link_i) == 0) {
    print(paste('Sensors ID', sidefire_inTaz_2025$ID[i],'not found',' row',i))
    missingsensors = rbind(missingsensors, c(i, sidefire_inTaz_2025$ID[i]))
    skipflag = T
  }
  
  if (skipflag == T) {
    next
  }
    # free flow from MRDWY file
  sidefire_vol_spd_2022_am$spdff[i] = max(roadlink_2026$PKFRSPD_A[which(roadlink_2026$ID == sidefire_vol_spd_2022_am$ID_Road[i])],
                                          roadlink_2026$PKFRSPD_B[which(roadlink_2026$ID == sidefire_vol_spd_2022_am$ID_Road[i])], na.rm = T)
  
  # lane number from MRDWY file
  sidefire_vol_spd_2022_am$AMlane[i] = max(roadlink_2026$AMLN_AB[which(roadlink_2026$ID == sidefire_vol_spd_2022_am$ID_Road[i])],
                                           roadlink_2026$AMLN_BA[which(roadlink_2026$ID == sidefire_vol_spd_2022_am$ID_Road[i])], na.rm = T)
  #sidefire_vol_spd_2022_am$PMlane[i] = max(roadlink_2026$PMLN_AB[which(roadlink_2026$ID == sidefire_vol_spd_2022_am$ID_Road[i])],
  #                                         roadlink_2026$PMLN_BA[which(roadlink_2026$ID == sidefire_vol_spd_2022_am$ID_Road[i])], na.rm = T)
  
  # first calculate speed. find rows in link_i that are lower than spdff
  spd_lowerff = which(c(t(link_i[,c(139,149)])) < sidefire_vol_spd_2022_am$spdff[i])
  if (length(spd_lowerff) == 0) {
    print(paste('All speeds higher than ff at sensor ID', link_i$LinkID[1]))
    skipflag = T
  }
  
  if (skipflag == T) {
    next
  }
  
  # choose corresponding volume 
  volarray_i = allNA_returnNA(c(t(link_i[spd_lowerff,c(46,56)]))) #,t(link_i[spd_lowerff,c(80,94)])))
  spdarray_i = allNA_returnNA(c(t(link_i[spd_lowerff,c(139,149)]))) #,t(link_i[spd_lowerff,c(173,187)])))
  if (all(is.na(c(volarray_i))) | all(is.na(spdarray_i))) {
    print(paste('All qualified vol & spd are NA, sensor ID', link_i$LinkID[1]))
    next
  }
  
  # volume stats from sidefire detector, vol_per_day which takes records per 15-minute interval
  
  sidefire_vol_spd_2022_am$volmin[i] = min(volarray_i, na.rm = T)
  sidefire_vol_spd_2022_am$vol15[i] = quantile(volarray_i, na.rm = T, 0.15)
  sidefire_vol_spd_2022_am$vol25[i] = quantile(volarray_i, na.rm = T, 0.25)
  sidefire_vol_spd_2022_am$vol50[i] = quantile(volarray_i, na.rm = T, 0.50)
  sidefire_vol_spd_2022_am$volavg[i] = mean(volarray_i, na.rm = T)
  sidefire_vol_spd_2022_am$vol75[i] = quantile(volarray_i, na.rm = T, 0.75)
  sidefire_vol_spd_2022_am$vol85[i] = quantile(volarray_i, na.rm = T, 0.85)
  sidefire_vol_spd_2022_am$vol90[i] = quantile(volarray_i, na.rm = T, 0.90)
  sidefire_vol_spd_2022_am$vol95[i] = quantile(volarray_i, na.rm = T, 0.95)
  sidefire_vol_spd_2022_am$vol975[i] = quantile(volarray_i, na.rm = T, 0.975)
  sidefire_vol_spd_2022_am$vol99[i] = quantile(volarray_i, na.rm = T, 0.99)
  sidefire_vol_spd_2022_am$volmax[i] = max(volarray_i, na.rm = T)
  
  # speed stats from sidefire detector, vol_per_day which takes records per 15-minute interval
  sidefire_vol_spd_2022_am$spdmin[i] = min(spdarray_i, na.rm = T)
  sidefire_vol_spd_2022_am$spd15[i] = quantile(spdarray_i, na.rm = T, 0.15)
  sidefire_vol_spd_2022_am$spd25[i] = quantile(spdarray_i, na.rm = T, 0.25)
  sidefire_vol_spd_2022_am$spd50[i] = quantile(spdarray_i, na.rm = T, 0.50)
  sidefire_vol_spd_2022_am$spdavg[i] = mean(spdarray_i, na.rm = T)
  sidefire_vol_spd_2022_am$spd75[i] = quantile(spdarray_i, na.rm = T, 0.75)
  sidefire_vol_spd_2022_am$spd85[i] = quantile(spdarray_i, na.rm = T, 0.85)
  sidefire_vol_spd_2022_am$spd90[i] = quantile(spdarray_i, na.rm = T, 0.90)
  sidefire_vol_spd_2022_am$spd95[i] = quantile(spdarray_i, na.rm = T, 0.95)
  sidefire_vol_spd_2022_am$spd975[i] = quantile(spdarray_i, na.rm = T, 0.975)
  sidefire_vol_spd_2022_am$spd99[i] = quantile(spdarray_i, na.rm = T, 0.99)
  sidefire_vol_spd_2022_am$spdmax[i] = max(spdarray_i, na.rm = T)
}


###########################################PM#############################################
###########################################PM#############################################
###########################################PM#############################################
###########################################PM#############################################
###### peak hour speed & vol, PM
# freeway speed in roadlink: PKFRSPD_A, PKFRSPD_B, Col 66, 67, speed in mph
# PM 15-18:30, count col 80-94, spd col 173-187
# initiate peak hour vol_speed statistical records 
# vol per day file obtained from sql server collected by detectors. the link ID corresponds to sensors ID
sidefire_vol_spd_2022_pm = data.frame(cbind(sidefire_inTaz_2025$ID, sidefire_inTaz_2025$LINKNAME, 
                                            sidefire_inTaz_2025$nearlink_id_26, sidefire_inTaz_2025$nearlink_name_26))
colnames(sidefire_vol_spd_2022_pm) = c('ID_detector', 'LinkName_detector', 'ID_Road', 'LinkName_Road')

sidefire_vol_spd_2022_pm$FUNCL = sidefire_inTaz_2025$FUNCL
sidefire_vol_spd_2022_pm$AMlane = 0 #; sidefire_vol_spd_2022_pm$PMlane = 0
sidefire_vol_spd_2022_pm$spdff = 0

sidefire_vol_spd_2022_pm$volmin = 0; sidefire_vol_spd_2022_pm$vol15 = 0; sidefire_vol_spd_2022_pm$vol25 = 0; sidefire_vol_spd_2022_pm$vol50 = 0
sidefire_vol_spd_2022_pm$volavg = 0; sidefire_vol_spd_2022_pm$vol75 = 0; sidefire_vol_spd_2022_pm$vol85 = 0; sidefire_vol_spd_2022_pm$vol90 = 0
sidefire_vol_spd_2022_pm$vol95 = 0; sidefire_vol_spd_2022_pm$vol975 = 0; sidefire_vol_spd_2022_pm$vol99 = 0; sidefire_vol_spd_2022_pm$volmax = 0


sidefire_vol_spd_2022_pm$spdmin = 0; sidefire_vol_spd_2022_pm$spd15 = 0; sidefire_vol_spd_2022_pm$spd25 = 0; sidefire_vol_spd_2022_pm$spd50 = 0
sidefire_vol_spd_2022_pm$spdavg = 0; sidefire_vol_spd_2022_pm$spd75 = 0; sidefire_vol_spd_2022_pm$spd85 = 0; sidefire_vol_spd_2022_pm$spd90 = 0
sidefire_vol_spd_2022_pm$spd95 = 0; sidefire_vol_spd_2022_pm$spd975 = 0; sidefire_vol_spd_2022_pm$spd99 = 0; sidefire_vol_spd_2022_pm$spdmax = 0

# function: return NA stats if all values of an array is NA
allNA_returnNA = function(array) {
  if (all(is.na(array))) {
    return(NA)
  } else {
    return(array)
  }
}

missingsensors = matrix(0, nrow = 0, ncol = 2)

# select PM
for (i in 1:nrow(sidefire_vol_spd_2022_pm)) {
  link_i = vol_per_day_2022[which(vol_per_day_2022$LinkID == sidefire_vol_spd_2022_pm$ID_detector[i]),]
  skipflag = F
  if (nrow(link_i) == 0) {
    print(paste('Sensors ID', sidefire_inTaz_2025$ID[i],'not found',' row',i))
    missingsensors = rbind(missingsensors, c(i, sidefire_inTaz_2025$ID[i]))
    skipflag = T
  }
  
  if (skipflag == T) {
    next
  }
  # free flow from MRDWY file
  sidefire_vol_spd_2022_pm$spdff[i] = max(roadlink_2026$PKFRSPD_A[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm$ID_Road[i])],
                                          roadlink_2026$PKFRSPD_B[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm$ID_Road[i])], na.rm = T)
  
  # lane number from MRDWY file
  # sidefire_vol_spd_2022_pm$AMlane[i] = max(roadlink_2026$AMLN_AB[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm$ID_Road[i])],
  #                                         roadlink_2026$AMLN_BA[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm$ID_Road[i])], na.rm = T)
  sidefire_vol_spd_2022_pm$PMlane[i] = max(roadlink_2026$PMLN_AB[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm$ID_Road[i])],
                                           roadlink_2026$PMLN_BA[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm$ID_Road[i])], na.rm = T)
  
  # first calculate speed. find rows in link_i that are lower than spdff
  spd_lowerff = which(c(t(link_i[,c(173,187)])) < sidefire_vol_spd_2022_pm$spdff[i])
  if (length(spd_lowerff) == 0) {
    print(paste('All speeds higher than ff at sensor ID', link_i$LinkID[1]))
    skipflag = T
  }
  
  if (skipflag == T) {
    next
  }
  
  # choose corresponding volume 
  volarray_i = allNA_returnNA(c(t(link_i[spd_lowerff,c(80,94)])))
  spdarray_i = allNA_returnNA(c(t(link_i[spd_lowerff,c(173,187)])))
  if (all(is.na(c(volarray_i))) | all(is.na(spdarray_i))) {
    print(paste('All qualified vol & spd are NA, sensor ID', link_i$LinkID[1]))
    next
  }
  
  # volume stats from sidefire detector, vol_per_day which takes records per 15-minute interval
  
  sidefire_vol_spd_2022_pm$volmin[i] = min(volarray_i, na.rm = T)
  sidefire_vol_spd_2022_pm$vol15[i] = quantile(volarray_i, na.rm = T, 0.15)
  sidefire_vol_spd_2022_pm$vol25[i] = quantile(volarray_i, na.rm = T, 0.25)
  sidefire_vol_spd_2022_pm$vol50[i] = quantile(volarray_i, na.rm = T, 0.50)
  sidefire_vol_spd_2022_pm$volavg[i] = mean(volarray_i, na.rm = T)
  sidefire_vol_spd_2022_pm$vol75[i] = quantile(volarray_i, na.rm = T, 0.75)
  sidefire_vol_spd_2022_pm$vol85[i] = quantile(volarray_i, na.rm = T, 0.85)
  sidefire_vol_spd_2022_pm$vol90[i] = quantile(volarray_i, na.rm = T, 0.90)
  sidefire_vol_spd_2022_pm$vol95[i] = quantile(volarray_i, na.rm = T, 0.95)
  sidefire_vol_spd_2022_pm$vol975[i] = quantile(volarray_i, na.rm = T, 0.975)
  sidefire_vol_spd_2022_pm$vol99[i] = quantile(volarray_i, na.rm = T, 0.99)
  sidefire_vol_spd_2022_pm$volmax[i] = max(volarray_i, na.rm = T)
  
  # speed stats from sidefire detector, vol_per_day which takes records per 15-minute interval
  sidefire_vol_spd_2022_pm$spdmin[i] = min(spdarray_i, na.rm = T)
  sidefire_vol_spd_2022_pm$spd15[i] = quantile(spdarray_i, na.rm = T, 0.15)
  sidefire_vol_spd_2022_pm$spd25[i] = quantile(spdarray_i, na.rm = T, 0.25)
  sidefire_vol_spd_2022_pm$spd50[i] = quantile(spdarray_i, na.rm = T, 0.50)
  sidefire_vol_spd_2022_pm$spdavg[i] = mean(spdarray_i, na.rm = T)
  sidefire_vol_spd_2022_pm$spd75[i] = quantile(spdarray_i, na.rm = T, 0.75)
  sidefire_vol_spd_2022_pm$spd85[i] = quantile(spdarray_i, na.rm = T, 0.85)
  sidefire_vol_spd_2022_pm$spd90[i] = quantile(spdarray_i, na.rm = T, 0.90)
  sidefire_vol_spd_2022_pm$spd95[i] = quantile(spdarray_i, na.rm = T, 0.95)
  sidefire_vol_spd_2022_pm$spd975[i] = quantile(spdarray_i, na.rm = T, 0.975)
  sidefire_vol_spd_2022_pm$spd99[i] = quantile(spdarray_i, na.rm = T, 0.99)
  sidefire_vol_spd_2022_pm$spdmax[i] = max(spdarray_i, na.rm = T)
}

rm(i, skipflag, spd_lowerff, spdarray_i, volarray_i,link_i)


