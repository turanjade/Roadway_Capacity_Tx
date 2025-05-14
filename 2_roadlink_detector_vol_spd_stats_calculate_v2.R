## replaced 2_roadlink_detector_vol_spd_stats_cal, now only calculate for all records


########################################### vol_per_day, 15min count #############################################
########################################### vol_per_day, 15min count #############################################
vol_per_day_2022_raw = read.csv('20250410_capacity_recalculation/Database/vol_per_day_2022.csv', header = T)
vol_per_day_2022_raw = vol_per_day_2022_raw[which(vol_per_day_2022_raw$Year == 2022),] # only Y2022 records selected

## in vol_per_day_2022, the data is hourly records. for example, volume_0_00 means the count from 12am to 1am, volume_0_15 means the count from 12:15am to 1:15am; the same for speed
## check with vol_per_day 2022. Keep only Records_Per_Lane between 250 and 288, 
vol_per_day_2022 = vol_per_day_2022_raw[which(vol_per_day_2022_raw$Records_Per_Lane >= 250 &
                                                vol_per_day_2022_raw$Records_Per_Lane <= 288),]
## delete all dups if sensors ID & recorded data are exactly the same
vol_per_day_2022$sensor_date_merge = paste(vol_per_day_2022$LinkID, vol_per_day_2022$Month, vol_per_day_2022$Date, sep = '_')
vol_per_day_2022 = vol_per_day_2022[!duplicated(vol_per_day_2022$sensor_date_merge) & 
                                      !duplicated(vol_per_day_2022$sensor_date_merge, fromLast = TRUE),]
## only select Tuesday, Wednesday & Thursday
vol_per_day_2022_workday = vol_per_day_2022[which(vol_per_day_2022$DOW == 3 | vol_per_day_2022$DOW == 4 | vol_per_day_2022$DOW == 5),]

# aggregate linkID in the detector file. Each row represents the count statistics (min, 15%, Q1 (25%), mean, 50%, Q3 (75%), 85%, 90%, 95%, 97.5%, 98%, 99%, max) & its corresponding at peak hours

######################## filter out records in vol_per_day_2022_raw of which sensor ID matches #############
sidefire_ID = unique(sidefire_linkmatch_frwy_2025$sf_id)

# instead of appending, first select rows from vol_per_day data, then select the data of corresponding row --> fasten the process
## use processed vol_per_day_2022 instead of XX_raw (keep reasonable records_per_lane & delete dups)
row_to_select = array(0, dim = 0)
for (i in 1:length(sidefire_ID)) {
  row_to_select = c(row_to_select, which(vol_per_day_2022$LinkID == sidefire_ID[i]))
}

vol_per_day_2022_linkmatch = vol_per_day_2022[row_to_select,] # 956 detectors in 2022 hourly count match with the roadlink & 2025 detector location
rm(row_to_select)

# select workday, Tuesday, Wednesday, Thursday & February (matches with npmrds data), 956 remains
vol_per_day_2022_feb_workday = vol_per_day_2022_linkmatch[which((vol_per_day_2022_linkmatch$DOW == 3 |
                                                                   vol_per_day_2022_linkmatch$DOW == 4 |
                                                                   vol_per_day_2022_linkmatch$DOW == 5) & 
                                                                  vol_per_day_2022_linkmatch$Month == 2),] # select Feb workday records, 956 detectors

vol_per_day_2022_workday = vol_per_day_2022_linkmatch[which((vol_per_day_2022_linkmatch$DOW == 3 |
                                                               vol_per_day_2022_linkmatch$DOW == 4 |
                                                               vol_per_day_2022_linkmatch$DOW == 5)),] # select Feb workday records, 956 detectors


####################################### Get max stats value of volume & corresponding spd #######################################
sidefire_vol_spd_2022 = data.frame(cbind(sidefire_linkmatch_frwy_2025$sf_id, sidefire_linkmatch_frwy_2025$rdwy_id,
                                         sidefire_linkmatch_frwy_2025$TMC, sidefire_linkmatch_frwy_2025$weavetype,
                                         sidefire_linkmatch_frwy_2025$areatype,
                                         sidefire_linkmatch_frwy_2025$length,
                                         sidefire_linkmatch_frwy_2025$amffspd, 
                                         sidefire_linkmatch_frwy_2025$amlane, 
                                         sidefire_linkmatch_frwy_2025$amhrcap))

colnames(sidefire_vol_spd_2022) = c('ID_detector', 'ID_Road', 'TMC', 'weavetype','areatype','length','ffspd','lane','hrcap')

sidefire_vol_spd_2022$vol90 = 0; sidefire_vol_spd_2022$vol95 = 0; 
sidefire_vol_spd_2022$vol975 = 0; sidefire_vol_spd_2022$vol99 = 0; 
sidefire_vol_spd_2022$volmax = 0; sidefire_vol_spd_2022$volboxupper = 0

sidefire_vol_spd_2022$spd90 = 0; sidefire_vol_spd_2022$spd95 = 0; 
sidefire_vol_spd_2022$spd975 = 0; sidefire_vol_spd_2022$spd99 = 0; 
sidefire_vol_spd_2022$spdmax = 0; sidefire_vol_spd_2022$spdboxupper = 0

missingsensors = matrix(0, nrow = 0, ncol = 2)
volcol = seq(20,112); spdcol = seq(113, 205)

for (i in 1:nrow(sidefire_vol_spd_2022)) {
  
  link_i = vol_per_day_2022_linkmatch[which(vol_per_day_2022_linkmatch$LinkID == sidefire_vol_spd_2022$ID_detector[i]),]
  
  # if the number of link is 0, skip
  if (nrow(link_i) == 0) {
    print(paste('Sensors ID', sidefire_linkmatch_frwy_2025$sf_id[i],'not found',' row',i))
    missingsensors = rbind(missingsensors, c(i, sidefire_linkmatch_frwy_2025$sf_id[i]))
    next
  }
  
  # choose corresponding volume 
  volarray_i = allNA_returnNA(t(link_i[,volcol])) 
  spdarray_i = allNA_returnNA(t(link_i[,spdcol])) 
  if (all(is.na(c(volarray_i))) | all(is.na(spdarray_i))) {
    print(paste('All qualified vol & spd are NA, sensor ID', link_i$LinkID[1]))
    next
  }
  
  # we already chose records under spdff, vol and spd are already in the same order, so we don't have to change
  sidefire_vol_spd_2022$vol90[i] = quantile(volarray_i, na.rm = T, 0.90) 
  q90Index = getIndex(volarray_i,sidefire_vol_spd_2022$vol90[i])
  
  sidefire_vol_spd_2022$vol95[i] = quantile(volarray_i, na.rm = T, 0.95) 
  q95Index = getIndex(volarray_i,sidefire_vol_spd_2022$vol95[i]) 
  
  sidefire_vol_spd_2022$vol975[i] = quantile(volarray_i, na.rm = T, 0.975) 
  q975Index = getIndex(volarray_i,sidefire_vol_spd_2022$vol975[i])
  
  sidefire_vol_spd_2022$vol99[i] = quantile(volarray_i, na.rm = T, 0.99) 
  q99Index = getIndex(volarray_i,sidefire_vol_spd_2022$vol99[i]) 
  
  sidefire_vol_spd_2022$volmax[i] = max(volarray_i, na.rm = T) 
  maxIndex = getIndex(volarray_i,sidefire_vol_spd_2022$volmax[i]) 
  
  sidefire_vol_spd_2022$volboxupper[i] = min(quantile(volarray_i, na.rm = T, 0.75) + 
                                               1.5*(quantile(volarray_i, na.rm = T, 0.75) - quantile(volarray_i, na.rm = T, 0.25)), 
                                             sidefire_vol_spd_2022$volmax[i]) 
  boxupperIndex = getIndex(volarray_i,sidefire_vol_spd_2022$volboxupper[i]) 
  
  # speed stats from sidefire detector, vol_per_day which takes records per 15-minute interval
  sidefire_vol_spd_2022$spd90[i] = mean(spdarray_i[q90Index])
  sidefire_vol_spd_2022$spd95[i] = mean(spdarray_i[q95Index])
  sidefire_vol_spd_2022$spd975[i] = mean(spdarray_i[q975Index])
  sidefire_vol_spd_2022$spd99[i] = mean(spdarray_i[q99Index])
  sidefire_vol_spd_2022$spdmax[i] = mean(spdarray_i[maxIndex])
  sidefire_vol_spd_2022$spdboxupper[i] = mean(spdarray_i[boxupperIndex])
}

## only 956 links can be matched
sidefire_vol_spd_2022 = sidefire_vol_spd_2022[-as.numeric(missingsensors[,1]),]

sidefire_vol_spd_2022 = data.frame(sidefire_vol_spd_2022)
## convert all values to numeric
for (i in 6:21) {
  sidefire_vol_spd_2022[,i] = as.numeric(sidefire_vol_spd_2022[,i])
}

# store initial value but add additional column to store scaled vols (1.1 times than recorded)
sidefire_vol_spd_2022$vol90_s = sidefire_vol_spd_2022$vol90 * 1.1
sidefire_vol_spd_2022$vol95_s = sidefire_vol_spd_2022$vol95 * 1.1
sidefire_vol_spd_2022$vol975_s = sidefire_vol_spd_2022$vol975 * 1.1
sidefire_vol_spd_2022$vol99_s = sidefire_vol_spd_2022$vol99 * 1.1
sidefire_vol_spd_2022$volmax_s = sidefire_vol_spd_2022$volmax * 1.1
sidefire_vol_spd_2022$volboxupper_s = sidefire_vol_spd_2022$volboxupper * 1.1


library('openxlsx')
# write.xlsx(sidefire_vol_spd_2022, '20250410_capacity_recalculation\\RoadNetwork_2026\\Sensor_count\\05072025_sidefire_vol_spd_2022_stats.xlsx', 
#           rowNames = F)

rm(boxupperIndex, maxIndex, q90Index, q95Index, q975Index, q99Index, spdcol, volcol, wb, spdarray_i, volarray_i, missingsensors, link_i, i)

################################################## make summary table for different weave types ####################
## calculate per lane vol & capacity. Now per lane is scaled up value
sidefire_vol_spd_2022$hrcapperlane = sidefire_vol_spd_2022$hrcap/sidefire_vol_spd_2022$lane
sidefire_vol_spd_2022$vol90perlane = sidefire_vol_spd_2022$vol90_s/sidefire_vol_spd_2022$lane
sidefire_vol_spd_2022$vol95perlane = sidefire_vol_spd_2022$vol95_s/sidefire_vol_spd_2022$lane
sidefire_vol_spd_2022$volmaxperlane = sidefire_vol_spd_2022$volmax_s/sidefire_vol_spd_2022$lane
sidefire_vol_spd_2022$volboxupperperlane = sidefire_vol_spd_2022$volboxupper_s/sidefire_vol_spd_2022$lane


################################################## create a df that stores all the records from vol_per_day_2022_linkmatch ####################
vol_per_day_2022_feb_workday_transpose = matrix(0, nrow = 0, ncol = 12)

for (i in 1:nrow(vol_per_day_2022_feb_workday)) {
  vol_per_day_2022_feb_workday_transpose = rbind(vol_per_day_2022_feb_workday_transpose,
                                               cbind(vol_per_day_2022_feb_workday$LinkID[i], 
                                                     sidefire_linkmatch_frwy_2025$rdwy_id[which(sidefire_linkmatch_frwy_2025$sf_id == vol_per_day_2022_feb_workday$LinkID[i])],
                                                     sidefire_linkmatch_frwy_2025$TMC[which(sidefire_linkmatch_frwy_2025$sf_id == vol_per_day_2022_feb_workday$LinkID[i])],
                                                     sidefire_linkmatch_frwy_2025$weavetype[which(sidefire_linkmatch_frwy_2025$sf_id == vol_per_day_2022_feb_workday$LinkID[i])],
                                                     sidefire_linkmatch_frwy_2025$areatype[which(sidefire_linkmatch_frwy_2025$sf_id == vol_per_day_2022_feb_workday$LinkID[i])],
                                                     sidefire_linkmatch_frwy_2025$length[which(sidefire_linkmatch_frwy_2025$sf_id == vol_per_day_2022_feb_workday$LinkID[i])],
                                                     sidefire_linkmatch_frwy_2025$amlane[which(sidefire_linkmatch_frwy_2025$sf_id == vol_per_day_2022_feb_workday$LinkID[i])],
                                                     sidefire_linkmatch_frwy_2025$amffspd[which(sidefire_linkmatch_frwy_2025$sf_id == vol_per_day_2022_feb_workday$LinkID[i])],
                                                     sidefire_linkmatch_frwy_2025$amhrcap[which(sidefire_linkmatch_frwy_2025$sf_id == vol_per_day_2022_feb_workday$LinkID[i])],
                                                     sidefire_linkmatch_frwy_2025$sfcapperlane[which(sidefire_linkmatch_frwy_2025$sf_id == vol_per_day_2022_feb_workday$LinkID[i])],
                                                     t(vol_per_day_2022_feb_workday[i, volcol]), t(vol_per_day_2022_feb_workday[i, spdcol])
                                                     ))
}
vol_per_day_2022_feb_workday_transpose = data.frame(vol_per_day_2022_feb_workday_transpose)

colnames(vol_per_day_2022_feb_workday_transpose) = c(
  'sf_id','rdwy_id','TMC','weavetype','areatype','length','lane','ffspd','hrcap','sfcap','spd','vol'
)

rm(i, spdcol, volcol)
