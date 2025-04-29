library('arcgisbinding')
library('sf')
library('dplyr')


setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

vol_per_day_2022 = read.csv('20250410_capacity_recalculation/Database/vol_per_day_2022.csv', header = T)
vol_per_day_2022 = vol_per_day_2022[which(vol_per_day_2022$Year == 2022),] # only Y2022 records selected

# in vol_per_day_2022, the data is hourly records. for example, volume_0_00 means the count from 12am to 1am, volume_0_15 means the count from 12:15am to 1:15am; the same for speed

# aggregate linkID in the detector file. Each row represents the count statistics (min, 15%, Q1 (25%), mean, 50%, Q3 (75%), 85%, 90%, 95%, 97.5%, 98%, 99%, max) & its corresponding at peak hours

# function: return NA stats if all values of an array is NA
allNA_returnNA = function(array) {
  if (all(is.na(array))) {
    return(NA)
  } else {
    return(array)
  }
}

# function: get the matrix index
getRowCol = function(mat, value) {
  # Get the index of the minimum value in the matrix
  index_flat <- which.min(abs(mat - value))
  # Convert the flat index to row and column indices
  row_index <- (index_flat - 1) %/% ncol(mat) + 1
  col_index <- (index_flat - 1) %% ncol(mat) + 1
  # Output
  return(row_index, col_index)
}

# function: get array index
getIndex = function(array, value) {
  # Get the index of the minimum value in the matrix
  index_flat <- which.min(abs(array - value))
  # Output
  return(index_flat)
}

# create a sidefire data that matches with 2022 records
###### peak hour speed & vol
# AM and PM should be separate due to different lane settings
# freeway speed in roadlink: PKFRSPD_A, PKFRSPD_B, Col 66, 67, speed in mph
# peak hours & cols in sensors: 
# AM 6:30-9:00, count col 46-55, spd col 139-148;  
# PM 15-18:30, count col 80-94, spd col 173-187
# initiate peak hour vol_speed statistical records 
# vol per day file obtained from sql server collected by detectors. the link ID corresponds to sensors ID
# split pk to am and pm
# volume stats from sidefire detector, vol_per_day which takes records per 15-minute interval
sidefire_vol_spd_2022_am = data.frame(cbind(sidefire_inTaz_2025$ID, sidefire_inTaz_2025$LINKNAME, 
                              sidefire_inTaz_2025$nearlink_id_26, sidefire_inTaz_2025$nearlink_name_26))
colnames(sidefire_vol_spd_2022_am) = c('ID_detector', 'LinkName_detector', 'ID_Road', 'LinkName_Road')

sidefire_vol_spd_2022_am$FUNCL = sidefire_inTaz_2025$FUNCL
sidefire_vol_spd_2022_am$AMlane = 0 #; sidefire_vol_spd_2022_am$PMlane = 0
sidefire_vol_spd_2022_am$spdff = 0

sidefire_vol_spd_2022_am$volmin = 0; sidefire_vol_spd_2022_am$vol15 = 0; sidefire_vol_spd_2022_am$vol25 = 0; sidefire_vol_spd_2022_am$vol50 = 0
sidefire_vol_spd_2022_am$volavg = 0; sidefire_vol_spd_2022_am$vol75 = 0; sidefire_vol_spd_2022_am$vol85 = 0; sidefire_vol_spd_2022_am$vol90 = 0
sidefire_vol_spd_2022_am$vol95 = 0; sidefire_vol_spd_2022_am$vol975 = 0; sidefire_vol_spd_2022_am$vol99 = 0; sidefire_vol_spd_2022_am$volmax = 0
sidefire_vol_spd_2022_am$volboxupper = 0

sidefire_vol_spd_2022_am$spdmin = 0; sidefire_vol_spd_2022_am$spd15 = 0; sidefire_vol_spd_2022_am$spd25 = 0; sidefire_vol_spd_2022_am$spd50 = 0
sidefire_vol_spd_2022_am$spdavg = 0; sidefire_vol_spd_2022_am$spd75 = 0; sidefire_vol_spd_2022_am$spd85 = 0; sidefire_vol_spd_2022_am$spd90 = 0
sidefire_vol_spd_2022_am$spd95 = 0; sidefire_vol_spd_2022_am$spd975 = 0; sidefire_vol_spd_2022_am$spd99 = 0; sidefire_vol_spd_2022_am$spdmax = 0
sidefire_vol_spd_2022_am$spdboxupper = 0

missingsensors = matrix(0, nrow = 0, ncol = 2)

# select AM 
for (i in 1:nrow(sidefire_vol_spd_2022_am)) {
  link_i = vol_per_day_2022[which(vol_per_day_2022$LinkID == sidefire_vol_spd_2022_am$ID_detector[i]),]
  
  # if the number of link is 0, skip
  if (nrow(link_i) == 0) {
    print(paste('Sensors ID', sidefire_inTaz_2025$ID[i],'not found',' row',i))
    missingsensors = rbind(missingsensors, c(i, sidefire_inTaz_2025$ID[i]))
    next
  }
  # free flow from MRDWY file
  sidefire_vol_spd_2022_am$spdff[i] = max(roadlink_2026$PKFRSPD_A[which(roadlink_2026$ID == sidefire_vol_spd_2022_am$ID_Road[i])],
                                          roadlink_2026$PKFRSPD_B[which(roadlink_2026$ID == sidefire_vol_spd_2022_am$ID_Road[i])], na.rm = T)
  
  # lane number from MRDWY file
  sidefire_vol_spd_2022_am$AMlane[i] = max(roadlink_2026$AMLN_AB[which(roadlink_2026$ID == sidefire_vol_spd_2022_am$ID_Road[i])],
                                           roadlink_2026$AMLN_BA[which(roadlink_2026$ID == sidefire_vol_spd_2022_am$ID_Road[i])], na.rm = T)

  # find rows in link_i that are lower than spdff. if all > spdff, skip to next 
  spd_lowerff = which(c(t(link_i[,seq(139,148)])) < sidefire_vol_spd_2022_am$spdff[i])
  if (length(spd_lowerff) == 0) {
    print(paste('All speeds higher than ff at sensor ID', link_i$LinkID[1]))
    next
  }
  
  # choose corresponding volume 
  volarray_i = allNA_returnNA(t(link_i[,seq(46,58)])[spd_lowerff]) 
  spdarray_i = allNA_returnNA(t(link_i[,seq(139,148)])[spd_lowerff]) 
  if (all(is.na(c(volarray_i))) | all(is.na(spdarray_i))) {
    print(paste('All qualified vol & spd are NA, sensor ID', link_i$LinkID[1]))
    next
  }
  
  # we already chose records under spdff, vol and spd are already in the same order, so we don't have to change
  sidefire_vol_spd_2022_am$volmin[i] = min(volarray_i, na.rm = T); minIndex = getIndex(volarray_i,sidefire_vol_spd_2022_am$volmin[i])[1]
  sidefire_vol_spd_2022_am$vol15[i] = quantile(volarray_i, na.rm = T, 0.15); q15Index = getIndex(volarray_i,sidefire_vol_spd_2022_am$vol15[i])[1]
  sidefire_vol_spd_2022_am$vol25[i] = quantile(volarray_i, na.rm = T, 0.25); q25Index = getIndex(volarray_i,sidefire_vol_spd_2022_am$vol25[i])[1]
  sidefire_vol_spd_2022_am$vol50[i] = quantile(volarray_i, na.rm = T, 0.50); q50Index = getIndex(volarray_i,sidefire_vol_spd_2022_am$vol50[i])[1]
  sidefire_vol_spd_2022_am$volavg[i] = mean(volarray_i, na.rm = T); avgIndex = getIndex(volarray_i,sidefire_vol_spd_2022_am$volavg[i])[1]
  sidefire_vol_spd_2022_am$vol75[i] = quantile(volarray_i, na.rm = T, 0.75); q75Index = getIndex(volarray_i,sidefire_vol_spd_2022_am$vol75[i])[1]
  sidefire_vol_spd_2022_am$vol85[i] = quantile(volarray_i, na.rm = T, 0.85); q85Index = getIndex(volarray_i,sidefire_vol_spd_2022_am$vol85[i])[1]
  sidefire_vol_spd_2022_am$vol90[i] = quantile(volarray_i, na.rm = T, 0.90); q90Index = getIndex(volarray_i,sidefire_vol_spd_2022_am$vol90[i])[1]
  sidefire_vol_spd_2022_am$vol95[i] = quantile(volarray_i, na.rm = T, 0.95); q95Index = getIndex(volarray_i,sidefire_vol_spd_2022_am$vol95[i])[1]
  sidefire_vol_spd_2022_am$voltopoutlier[i] = sidefire_vol_spd_2022_am$vol75[i] + 1.5*(sidefire_vol_spd_2022_am$vol75[i] - sidefire_vol_spd_2022_am$vol25[i])
  qTOIndex = getIndex(volarray_i,sidefire_vol_spd_2022_am$voloutlier[i])[1]
  sidefire_vol_spd_2022_am$vol975[i] = quantile(volarray_i, na.rm = T, 0.975); q975Index = getIndex(volarray_i,sidefire_vol_spd_2022_am$vol975[i])[1]
  sidefire_vol_spd_2022_am$vol99[i] = quantile(volarray_i, na.rm = T, 0.99); q99Index = getIndex(volarray_i,sidefire_vol_spd_2022_am$vol99[i])[1]
  sidefire_vol_spd_2022_am$volmax[i] = max(volarray_i, na.rm = T); maxIndex = getIndex(volarray_i,sidefire_vol_spd_2022_am$volmax[i])[1]
  sidefire_vol_spd_2022_am$volboxupper[i] = sidefire_vol_spd_2022_am$vol75[i] + 
    1.5*(sidefire_vol_spd_2022_am$vol75[i] - sidefire_vol_spd_2022_am$vol25[i])
  boxupperIndex = getIndex(volarray_i,sidefire_vol_spd_2022_am$volboxupper[i])[1]
  
  # speed stats from sidefire detector, vol_per_day which takes records per 15-minute interval
  sidefire_vol_spd_2022_am$spdmin[i] = spdarray_i[minIndex]
  sidefire_vol_spd_2022_am$spd15[i] = spdarray_i[q15Index]
  sidefire_vol_spd_2022_am$spd25[i] = spdarray_i[q25Index]
  sidefire_vol_spd_2022_am$spd50[i] = spdarray_i[q50Index]
  sidefire_vol_spd_2022_am$spdavg[i] = spdarray_i[avgIndex]
  sidefire_vol_spd_2022_am$spd75[i] = spdarray_i[q75Index]
  sidefire_vol_spd_2022_am$spd85[i] = spdarray_i[q85Index]
  sidefire_vol_spd_2022_am$spd90[i] = spdarray_i[q90Index]
  sidefire_vol_spd_2022_am$spd95[i] = spdarray_i[q95Index]
  sidefire_vol_spd_2022_am$spdtopoutlier[i] = spdarray_i[qTOIndex]
  sidefire_vol_spd_2022_am$spd975[i] = spdarray_i[q975Index]
  sidefire_vol_spd_2022_am$spd99[i] = spdarray_i[q99Index]
  sidefire_vol_spd_2022_am$spdmax[i] = spdarray_i[maxIndex]
  sidefire_vol_spd_2022_am$spdboxupper[i] = spdarray_i[boxupperIndex]
}


########################################### PM #############################################
########################################### PM #############################################
########################################### PM #############################################
########################################### PM #############################################
###### peak hour speed & vol, PM
# freeway speed in roadlink: PKFRSPD_A, PKFRSPD_B, Col 66, 67, speed in mph
# PM 15-18:30, count col 80-94, spd col 173-187
# initiate peak hour vol_speed statistical records 
# vol per day file obtained from sql server collected by detectors. the link ID corresponds to sensors ID
sidefire_vol_spd_2022_pm = data.frame(cbind(sidefire_inTaz_2025$ID, sidefire_inTaz_2025$LINKNAME, 
                                            sidefire_inTaz_2025$nearlink_id_26, sidefire_inTaz_2025$nearlink_name_26))
colnames(sidefire_vol_spd_2022_pm) = c('ID_detector', 'LinkName_detector', 'ID_Road', 'LinkName_Road')

sidefire_vol_spd_2022_pm$FUNCL = sidefire_inTaz_2025$FUNCL
sidefire_vol_spd_2022_pm$PMlane = 0 
sidefire_vol_spd_2022_pm$spdff = 0

sidefire_vol_spd_2022_pm$volmin = 0; sidefire_vol_spd_2022_pm$vol15 = 0; sidefire_vol_spd_2022_pm$vol25 = 0; sidefire_vol_spd_2022_pm$vol50 = 0
sidefire_vol_spd_2022_pm$volavg = 0; sidefire_vol_spd_2022_pm$vol75 = 0; sidefire_vol_spd_2022_pm$vol85 = 0; sidefire_vol_spd_2022_pm$vol90 = 0
sidefire_vol_spd_2022_pm$vol95 = 0; sidefire_vol_spd_2022_pm$vol975 = 0; sidefire_vol_spd_2022_pm$vol99 = 0; sidefire_vol_spd_2022_pm$volmax = 0
sidefire_vol_spd_2022_pm$volboxupper = 0

sidefire_vol_spd_2022_pm$spdmin = 0; sidefire_vol_spd_2022_pm$spd15 = 0; sidefire_vol_spd_2022_pm$spd25 = 0; sidefire_vol_spd_2022_pm$spd50 = 0
sidefire_vol_spd_2022_pm$spdavg = 0; sidefire_vol_spd_2022_pm$spd75 = 0; sidefire_vol_spd_2022_pm$spd85 = 0; sidefire_vol_spd_2022_pm$spd90 = 0
sidefire_vol_spd_2022_pm$spd95 = 0; sidefire_vol_spd_2022_pm$spd975 = 0; sidefire_vol_spd_2022_pm$spd99 = 0; sidefire_vol_spd_2022_pm$spdmax = 0
sidefire_vol_spd_2022_pm$spdboxupper = 0

missingsensors = matrix(0, nrow = 0, ncol = 2)

# select PM
for (i in 1:nrow(sidefire_vol_spd_2022_pm)) {
  link_i = vol_per_day_2022[which(vol_per_day_2022$LinkID == sidefire_vol_spd_2022_pm$ID_detector[i]),]
  
  # if the number of link is 0, skip
  if (nrow(link_i) == 0) {
    print(paste('Sensors ID', sidefire_inTaz_2025$ID[i],'not found',' row',i))
    missingsensors = rbind(missingsensors, c(i, sidefire_inTaz_2025$ID[i]))
    next
  }
  # free flow from MRDWY file
  sidefire_vol_spd_2022_pm$spdff[i] = max(roadlink_2026$PKFRSPD_A[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm$ID_Road[i])],
                                          roadlink_2026$PKFRSPD_B[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm$ID_Road[i])], na.rm = T)
  
  # lane number from MRDWY file
  sidefire_vol_spd_2022_pm$PMlane[i] = max(roadlink_2026$PMLN_AB[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm$ID_Road[i])],
                                           roadlink_2026$PMLN_BA[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm$ID_Road[i])], na.rm = T)
  
  # find rows in link_i that are lower than spdff. if all > spdff, skip to next 
  spd_lowerff = which(c(t(link_i[,seq(173,187)])) < sidefire_vol_spd_2022_pm$spdff[i])
  if (length(spd_lowerff) == 0) {
    print(paste('All speeds higher than ff at sensor ID', link_i$LinkID[1]))
    next
  }
  
  # choose corresponding volume 
  volarray_i = allNA_returnNA(t(link_i[,seq(80,94)])[spd_lowerff]) #,t(link_i[spd_lowerff,c(80,94)])))
  spdarray_i = allNA_returnNA(t(link_i[,seq(173,187)])[spd_lowerff]) #,t(link_i[spd_lowerff,c(173,187)])))
  if (all(is.na(c(volarray_i))) | all(is.na(spdarray_i))) {
    print(paste('All qualified vol & spd are NA, sensor ID', link_i$LinkID[1]))
    next
  }
  
  # we already chose records under spdff, vol and spd are already in the same order, so we don't have to change
  sidefire_vol_spd_2022_pm$volmin[i] = min(volarray_i, na.rm = T); minIndex = getIndex(volarray_i,sidefire_vol_spd_2022_pm$volmin[i])[1]
  sidefire_vol_spd_2022_pm$vol15[i] = quantile(volarray_i, na.rm = T, 0.15); q15Index = getIndex(volarray_i,sidefire_vol_spd_2022_pm$vol15[i])[1]
  sidefire_vol_spd_2022_pm$vol25[i] = quantile(volarray_i, na.rm = T, 0.25); q25Index = getIndex(volarray_i,sidefire_vol_spd_2022_pm$vol25[i])[1]
  sidefire_vol_spd_2022_pm$vol50[i] = quantile(volarray_i, na.rm = T, 0.50); q50Index = getIndex(volarray_i,sidefire_vol_spd_2022_pm$vol50[i])[1]
  sidefire_vol_spd_2022_pm$volavg[i] = mean(volarray_i, na.rm = T); avgIndex = getIndex(volarray_i,sidefire_vol_spd_2022_pm$volavg[i])[1]
  sidefire_vol_spd_2022_pm$vol75[i] = quantile(volarray_i, na.rm = T, 0.75); q75Index = getIndex(volarray_i,sidefire_vol_spd_2022_pm$vol75[i])[1]
  sidefire_vol_spd_2022_pm$vol85[i] = quantile(volarray_i, na.rm = T, 0.85); q85Index = getIndex(volarray_i,sidefire_vol_spd_2022_pm$vol85[i])[1]
  sidefire_vol_spd_2022_pm$vol90[i] = quantile(volarray_i, na.rm = T, 0.90); q90Index = getIndex(volarray_i,sidefire_vol_spd_2022_pm$vol90[i])[1]
  sidefire_vol_spd_2022_pm$vol95[i] = quantile(volarray_i, na.rm = T, 0.95); q95Index = getIndex(volarray_i,sidefire_vol_spd_2022_pm$vol95[i])[1]
  sidefire_vol_spd_2022_pm$vol975[i] = quantile(volarray_i, na.rm = T, 0.975); q975Index = getIndex(volarray_i,sidefire_vol_spd_2022_pm$vol975[i])[1]
  sidefire_vol_spd_2022_pm$vol99[i] = quantile(volarray_i, na.rm = T, 0.99); q99Index = getIndex(volarray_i,sidefire_vol_spd_2022_pm$vol99[i])[1]
  sidefire_vol_spd_2022_pm$volmax[i] = max(volarray_i, na.rm = T); maxIndex = getIndex(volarray_i,sidefire_vol_spd_2022_pm$volmax[i])[1]
  sidefire_vol_spd_2022_pm$volboxupper[i] = sidefire_vol_spd_2022_pm$vol75[i] + 
    1.5*(sidefire_vol_spd_2022_pm$vol75[i] - sidefire_vol_spd_2022_pm$vol25[i])
  boxupperIndex = getIndex(volarray_i,sidefire_vol_spd_2022_pm$volboxupper[i])[1]
  
  # speed stats from sidefire detector, vol_per_day which takes records per 15-minute interval
  sidefire_vol_spd_2022_pm$spdmin[i] = spdarray_i[minIndex]
  sidefire_vol_spd_2022_pm$spd15[i] = spdarray_i[q15Index]
  sidefire_vol_spd_2022_pm$spd25[i] = spdarray_i[q25Index]
  sidefire_vol_spd_2022_pm$spd50[i] = spdarray_i[q50Index]
  sidefire_vol_spd_2022_pm$spdavg[i] = spdarray_i[avgIndex]
  sidefire_vol_spd_2022_pm$spd75[i] = spdarray_i[q75Index]
  sidefire_vol_spd_2022_pm$spd85[i] = spdarray_i[q85Index]
  sidefire_vol_spd_2022_pm$spd90[i] = spdarray_i[q90Index]
  sidefire_vol_spd_2022_pm$spd95[i] = spdarray_i[q95Index]
  sidefire_vol_spd_2022_pm$spd975[i] = spdarray_i[q975Index]
  sidefire_vol_spd_2022_pm$spd99[i] = spdarray_i[q99Index]
  sidefire_vol_spd_2022_pm$spdmax[i] = spdarray_i[maxIndex]
  sidefire_vol_spd_2022_pm$spdboxupper[i] = spdarray_i[boxupperIndex]
}


########################################### OP #############################################
########################################### OP #############################################
########################################### OP #############################################
########################################### OP #############################################
###### peak hour speed & vol, OP
# freeway speed in roadlink: PKFRSPD_A, PKFRSPD_B, Col 66, 67, speed in mph
# AM 6:30-9:00, count col 46-55, spd col 139-148; 
# PM 15-18:30, count col 80-94, spd col 173-187

# OP 0-6:30, count col 20-45, spd col 113-138; 
# 9:00-15:00, count col 56-79, spd col 149-172; 
# 18:30-24:00, count col 95-112, spd col  188-205;
# initiate peak hour vol_speed statistical records 
# vol per day file obtained from sql server collected by detectors. the link ID corresponds to sensors ID
sidefire_vol_spd_2022_op = data.frame(cbind(sidefire_inTaz_2025$ID, sidefire_inTaz_2025$LINKNAME, 
                                            sidefire_inTaz_2025$nearlink_id_26, sidefire_inTaz_2025$nearlink_name_26))
colnames(sidefire_vol_spd_2022_op) = c('ID_detector', 'LinkName_detector', 'ID_Road', 'LinkName_Road')

sidefire_vol_spd_2022_op$FUNCL = sidefire_inTaz_2025$FUNCL
sidefire_vol_spd_2022_op$oplane = 0 
sidefire_vol_spd_2022_op$spdff = 0

sidefire_vol_spd_2022_op$volmin = 0; sidefire_vol_spd_2022_op$vol15 = 0; sidefire_vol_spd_2022_op$vol25 = 0; sidefire_vol_spd_2022_op$vol50 = 0
sidefire_vol_spd_2022_op$volavg = 0; sidefire_vol_spd_2022_op$vol75 = 0; sidefire_vol_spd_2022_op$vol85 = 0; sidefire_vol_spd_2022_op$vol90 = 0
sidefire_vol_spd_2022_op$vol95 = 0; sidefire_vol_spd_2022_op$vol975 = 0; sidefire_vol_spd_2022_op$vol99 = 0; sidefire_vol_spd_2022_op$volmax = 0
sidefire_vol_spd_2022_op$volboxupper = 0

sidefire_vol_spd_2022_op$spdmin = 0; sidefire_vol_spd_2022_op$spd15 = 0; sidefire_vol_spd_2022_op$spd25 = 0; sidefire_vol_spd_2022_op$spd50 = 0
sidefire_vol_spd_2022_op$spdavg = 0; sidefire_vol_spd_2022_op$spd75 = 0; sidefire_vol_spd_2022_op$spd85 = 0; sidefire_vol_spd_2022_op$spd90 = 0
sidefire_vol_spd_2022_op$spd95 = 0; sidefire_vol_spd_2022_op$spd975 = 0; sidefire_vol_spd_2022_op$spd99 = 0; sidefire_vol_spd_2022_op$spdmax = 0
sidefire_vol_spd_2022_op$spdboxupper = 0

missingsensors = matrix(0, nrow = 0, ncol = 2)

# select op

volcol = c(seq(20,45),seq(56,79),seq(95,112)); spdcol = c(seq(113,138),seq(149,172),seq(188,205))
for (i in 1:nrow(sidefire_vol_spd_2022_op)) {
  link_i = vol_per_day_2022[which(vol_per_day_2022$LinkID == sidefire_vol_spd_2022_op$ID_detector[i]),]
  
  # if the number of link is 0, skip
  if (nrow(link_i) == 0) {
    print(paste('Sensors ID', sidefire_inTaz_2025$ID[i],'not found',' row',i))
    missingsensors = rbind(missingsensors, c(i, sidefire_inTaz_2025$ID[i]))
    next
  }
  # free flow from MRDWY file
  sidefire_vol_spd_2022_op$spdff[i] = max(roadlink_2026$OPFRSPD_A[which(roadlink_2026$ID == sidefire_vol_spd_2022_op$ID_Road[i])],
                                          roadlink_2026$OPFRSPD_B[which(roadlink_2026$ID == sidefire_vol_spd_2022_op$ID_Road[i])], na.rm = T)
  
  # lane number from MRDWY file
  sidefire_vol_spd_2022_op$oplane[i] = max(roadlink_2026$OPLN_AB[which(roadlink_2026$ID == sidefire_vol_spd_2022_op$ID_Road[i])],
                                           roadlink_2026$OPLN_BA[which(roadlink_2026$ID == sidefire_vol_spd_2022_op$ID_Road[i])], na.rm = T)
  
  # find rows in link_i that are lower than spdff. if all > spdff, skip to next 
  spd_lowerff = which(c(t(link_i[,spdcol])) < sidefire_vol_spd_2022_op$spdff[i])
  if (length(spd_lowerff) == 0) {
    print(paste('All speeds higher than ff at sensor ID', link_i$LinkID[1]))
    next
  }
  
  # choose corresponding volume 
  volarray_i = allNA_returnNA(t(link_i[,volcol])[spd_lowerff]) #,t(link_i[spd_lowerff,c(80,94)])))
  spdarray_i = allNA_returnNA(t(link_i[,spdcol])[spd_lowerff]) #,t(link_i[spd_lowerff,c(173,187)])))
  if (all(is.na(c(volarray_i))) | all(is.na(spdarray_i))) {
    print(paste('All qualified vol & spd are NA, sensor ID', link_i$LinkID[1]))
    next
  }
  
  # we already chose records under spdff, vol and spd are already in the same order, so we don't have to change
  sidefire_vol_spd_2022_op$volmin[i] = min(volarray_i, na.rm = T); minIndex = getIndex(volarray_i,sidefire_vol_spd_2022_op$volmin[i])[1]
  sidefire_vol_spd_2022_op$vol15[i] = quantile(volarray_i, na.rm = T, 0.15); q15Index = getIndex(volarray_i,sidefire_vol_spd_2022_op$vol15[i])[1]
  sidefire_vol_spd_2022_op$vol25[i] = quantile(volarray_i, na.rm = T, 0.25); q25Index = getIndex(volarray_i,sidefire_vol_spd_2022_op$vol25[i])[1]
  sidefire_vol_spd_2022_op$vol50[i] = quantile(volarray_i, na.rm = T, 0.50); q50Index = getIndex(volarray_i,sidefire_vol_spd_2022_op$vol50[i])[1]
  sidefire_vol_spd_2022_op$volavg[i] = mean(volarray_i, na.rm = T); avgIndex = getIndex(volarray_i,sidefire_vol_spd_2022_op$volavg[i])[1]
  sidefire_vol_spd_2022_op$vol75[i] = quantile(volarray_i, na.rm = T, 0.75); q75Index = getIndex(volarray_i,sidefire_vol_spd_2022_op$vol75[i])[1]
  sidefire_vol_spd_2022_op$vol85[i] = quantile(volarray_i, na.rm = T, 0.85); q85Index = getIndex(volarray_i,sidefire_vol_spd_2022_op$vol85[i])[1]
  sidefire_vol_spd_2022_op$vol90[i] = quantile(volarray_i, na.rm = T, 0.90); q90Index = getIndex(volarray_i,sidefire_vol_spd_2022_op$vol90[i])[1]
  sidefire_vol_spd_2022_op$vol95[i] = quantile(volarray_i, na.rm = T, 0.95); q95Index = getIndex(volarray_i,sidefire_vol_spd_2022_op$vol95[i])[1]
  sidefire_vol_spd_2022_op$vol975[i] = quantile(volarray_i, na.rm = T, 0.975); q975Index = getIndex(volarray_i,sidefire_vol_spd_2022_op$vol975[i])[1]
  sidefire_vol_spd_2022_op$vol99[i] = quantile(volarray_i, na.rm = T, 0.99); q99Index = getIndex(volarray_i,sidefire_vol_spd_2022_op$vol99[i])[1]
  sidefire_vol_spd_2022_op$volmax[i] = max(volarray_i, na.rm = T); maxIndex = getIndex(volarray_i,sidefire_vol_spd_2022_op$volmax[i])[1]
  sidefire_vol_spd_2022_op$volboxupper[i] = sidefire_vol_spd_2022_op$vol75[i] + 
    1.5*(sidefire_vol_spd_2022_op$vol75[i] - sidefire_vol_spd_2022_op$vol25[i])
  boxupperIndex = getIndex(volarray_i,sidefire_vol_spd_2022_op$volboxupper[i])[1]
  
  # speed stats from sidefire detector, vol_per_day which takes records per 15-minute interval
  sidefire_vol_spd_2022_op$spdmin[i] = spdarray_i[minIndex]
  sidefire_vol_spd_2022_op$spd15[i] = spdarray_i[q15Index]
  sidefire_vol_spd_2022_op$spd25[i] = spdarray_i[q25Index]
  sidefire_vol_spd_2022_op$spd50[i] = spdarray_i[q50Index]
  sidefire_vol_spd_2022_op$spdavg[i] = spdarray_i[avgIndex]
  sidefire_vol_spd_2022_op$spd75[i] = spdarray_i[q75Index]
  sidefire_vol_spd_2022_op$spd85[i] = spdarray_i[q85Index]
  sidefire_vol_spd_2022_op$spd90[i] = spdarray_i[q90Index]
  sidefire_vol_spd_2022_op$spd95[i] = spdarray_i[q95Index]
  sidefire_vol_spd_2022_op$spd975[i] = spdarray_i[q975Index]
  sidefire_vol_spd_2022_op$spd99[i] = spdarray_i[q99Index]
  sidefire_vol_spd_2022_op$spdmax[i] = spdarray_i[maxIndex]
  sidefire_vol_spd_2022_op$spdboxupper[i] = spdarray_i[boxupperIndex]
}


rm(i,spd_lowerff,spdarray_i,volarray_i,link_i,
   minIndex,maxIndex,q15Index,q25Index,q50Index,q75Index,q90Index,q99Index,q975Index,avgIndex,q85Index,q95Index)
