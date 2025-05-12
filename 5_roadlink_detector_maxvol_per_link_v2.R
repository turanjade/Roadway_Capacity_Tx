## Note 1: this file tries to estimate capacity of each weave type 
## Note 2: this code is similar as roadlink_detector_vol_spd_stats_calculate_v2, but v2 calculates stats vol and corresponding spd for different 
##         time period. however, this code gets max vol (from 90%, boxupper, 95%, 97.5%, 100%) and corresponding spd. 
##         Each matched link gets one single result, Do not need to specify time period 
## Note 3: at current stage, do not think of NMPRDS, because we need as many data as possible
## Note 4: Run this code after 2_roadlink_detector_vol_spd_stats_calculate

## Data sources: 
## 1) sidefire_linkmatch_frwy_2025 is the SF MRDWY match, generated from roadlink_detector_geographic_matching_v2.R
## 2) vol_per_day_2022 is the processed data from the first section of this code, only delete dups & invalid records_per_lane
## 3) vol_per_day_2022_linkmatch is the processed data that only contains detector records that have matched links
##    from 2_roadlink_detector_vol_spd_stats_calculate_v2.R

####################################### Filter out valid dataset with matched link ##############################################
## delete records that has 250-288 records_per_lane, and all dups

# check with vol_per_day 2022. Keep only Records_Per_Lane between 250 and 288, 
# vol_per_day_2022 = vol_per_day_2022_raw[which(vol_per_day_2022_raw$Records_Per_Lane >= 250 &
#                                                vol_per_day_2022_raw$Records_Per_Lane <= 288),]
# delete all dups if sensors ID & recorded data are exactly the same
# vol_per_day_2022$sensor_date_merge = paste(vol_per_day_2022$LinkID, vol_per_day_2022$Month, vol_per_day_2022$Date, sep = '_')
# vol_per_day_2022 = vol_per_day_2022[!duplicated(vol_per_day_2022$sensor_date_merge) & 
#                                      !duplicated(vol_per_day_2022$sensor_date_merge, fromLast = TRUE),] # 1322 link ID remains

## create summary tables for available weave types, starting from FRWY_BASIC
## each column represents different area types, rows represent vol95, volmax, volboxupper, capacity (perlane), count of selected links, count of records
area_frwybasic = unique(sidefire_vol_spd_2022$areatype[which(sidefire_vol_spd_2022$weavetype == 'FRWY_BASIC')])
sf_vol_spd_2022_summary_frwybasic = matrix(0, nrow = length(area_frwybasic), ncol = 5)
for (i in 1:length(area_frwybasic)) {
  sf_vol_spd_2022_summary_frwybasic[i,1] = max(sidefire_vol_spd_2022$vol95perlane[which(sidefire_vol_spd_2022$weavetype == 'FRWY_BASIC' &
                                                                                          sidefire_vol_spd_2022$areatype == area_frwybasic[i] &
                                                                                          sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spd95 > 0)]) 
  sf_vol_spd_2022_summary_frwybasic[i,2] = max(sidefire_vol_spd_2022$volmaxperlane[which(sidefire_vol_spd_2022$weavetype == 'FRWY_BASIC' &
                                                                                           sidefire_vol_spd_2022$areatype == area_frwybasic[i] &
                                                                                           sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spdmax > 0)]) 
  sf_vol_spd_2022_summary_frwybasic[i,3] = max(sidefire_vol_spd_2022$volboxupperperlane[which(sidefire_vol_spd_2022$weavetype == 'FRWY_BASIC' &
                                                                                                sidefire_vol_spd_2022$areatype == area_frwybasic[i] &
                                                                                                sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spdboxupper > 0)])
  # problematic, multiple capacity in one weave type and area type
  sf_vol_spd_2022_summary_frwybasic[i,4] = max(sidefire_vol_spd_2022$hrcapperlane[which(sidefire_vol_spd_2022$weavetype == 'FRWY_BASIC' &
                                                                                          sidefire_vol_spd_2022$areatype == area_frwybasic[i])]) 
  
  # total number of records
  sf_vol_spd_2022_summary_frwybasic[i,5] = length(which(sidefire_vol_spd_2022$weavetype == 'FRWY_BASIC' &
                                                          sidefire_vol_spd_2022$areatype == area_frwybasic[i]))
  
}

sf_vol_spd_2022_summary_frwybasic = data.frame(sf_vol_spd_2022_summary_frwybasic)
colnames(sf_vol_spd_2022_summary_frwybasic) = c('vol95perlane','volmaxperlane','volboxupperperlane','hrcapperlane','record_count')
rownames(sf_vol_spd_2022_summary_frwybasic) = as.character(area_frwybasic)

## create summary tables for available weave types, from BASIC 
## each column represents different area types, rows represent vol95, volmax, volboxupper, capacity (perlane), count of records
area_basic = unique(sidefire_vol_spd_2022$areatype[which(sidefire_vol_spd_2022$weavetype == 'BASIC')])
sf_vol_spd_2022_summary_basic = matrix(0, nrow = length(area_basic), ncol = 5)
for (i in 1:length(area_basic)) {
  sf_vol_spd_2022_summary_basic[i,1] = max(sidefire_vol_spd_2022$vol95perlane[which(sidefire_vol_spd_2022$weavetype == 'BASIC' &
                                                                                      sidefire_vol_spd_2022$areatype == area_basic[i] &
                                                                                      sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spd95 > 0)])
  sf_vol_spd_2022_summary_basic[i,2] = max(sidefire_vol_spd_2022$volmaxperlane[which(sidefire_vol_spd_2022$weavetype == 'BASIC' &
                                                                                       sidefire_vol_spd_2022$areatype == area_basic[i] &
                                                                                       sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spdmax > 0)])
  sf_vol_spd_2022_summary_basic[i,3] = max(sidefire_vol_spd_2022$volboxupperperlane[which(sidefire_vol_spd_2022$weavetype == 'BASIC' &
                                                                                            sidefire_vol_spd_2022$areatype == area_basic[i] &
                                                                                            sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spdboxupper > 0)])
  # problematic, multiple capacity in one weave type and area type
  sf_vol_spd_2022_summary_basic[i,4] = max(sidefire_vol_spd_2022$hrcapperlane[which(sidefire_vol_spd_2022$weavetype == 'BASIC' &
                                                                                      sidefire_vol_spd_2022$areatype == area_basic[i])]) 
  
  # total number of records
  sf_vol_spd_2022_summary_basic[i,5] = length(which(sidefire_vol_spd_2022$weavetype == 'BASIC' &
                                                      sidefire_vol_spd_2022$areatype == area_basic[i]))
}

sf_vol_spd_2022_summary_basic = data.frame(sf_vol_spd_2022_summary_basic)
colnames(sf_vol_spd_2022_summary_basic) = c('vol95perlane','volmaxperlane','volboxupperperlane','hrcapperlane','record_count')
rownames(sf_vol_spd_2022_summary_basic) = as.character(area_basic)

## create summary tables for available weave types, from MD 
## each column represents different area types, rows represent vol95, volmax, volboxupper, capacity (perlane), count of records
area_md = unique(sidefire_vol_spd_2022$areatype[which(sidefire_vol_spd_2022$weavetype == 'MD')])
sf_vol_spd_2022_summary_md = matrix(0, nrow = length(area_md), ncol = 5)
for (i in 1:length(area_md)) {
  sf_vol_spd_2022_summary_md[i,1] = max(sidefire_vol_spd_2022$vol95perlane[which(sidefire_vol_spd_2022$weavetype == 'MD' &
                                                                                   sidefire_vol_spd_2022$areatype == area_md[i] &
                                                                                   sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spd95 > 0)]) 
  sf_vol_spd_2022_summary_md[i,2] = max(sidefire_vol_spd_2022$volmaxperlane[which(sidefire_vol_spd_2022$weavetype == 'MD' &
                                                                                    sidefire_vol_spd_2022$areatype == area_md[i] &
                                                                                    sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spdmax > 0)]) 
  sf_vol_spd_2022_summary_md[i,3] = max(sidefire_vol_spd_2022$volboxupperperlane[which(sidefire_vol_spd_2022$weavetype == 'MD' &
                                                                                         sidefire_vol_spd_2022$areatype == area_md[i] &
                                                                                         sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spdboxupper > 0)]) 
  # problematic, multiple capacity in one weave type and area type
  sf_vol_spd_2022_summary_md[i,4] = max(sidefire_vol_spd_2022$hrcapperlane[which(sidefire_vol_spd_2022$weavetype == 'MD' &
                                                                                   sidefire_vol_spd_2022$areatype == area_md[i])]) 
  
  # total number of records
  sf_vol_spd_2022_summary_md[i,5] = length(which(sidefire_vol_spd_2022$weavetype == 'MD' &
                                                   sidefire_vol_spd_2022$areatype == area_md[i]))
}

sf_vol_spd_2022_summary_md = data.frame(sf_vol_spd_2022_summary_md)
colnames(sf_vol_spd_2022_summary_md) = c('vol95perlane','volmaxperlane','volboxupperperlane','hrcapperlane','record_count')
rownames(sf_vol_spd_2022_summary_md) = as.character(area_md)

## create summary tables for available weave types, from A 
## each column represents different area types, rows represent vol95, volmax, volboxupper, capacity (perlane), count of records
area_a = unique(sidefire_vol_spd_2022$areatype[which(sidefire_vol_spd_2022$weavetype == 'A')])
sf_vol_spd_2022_summary_a = matrix(0, nrow = length(area_a), ncol = 5)
for (i in 1:length(area_a)) {
  sf_vol_spd_2022_summary_a[i,1] = max(sidefire_vol_spd_2022$vol95perlane[which(sidefire_vol_spd_2022$weavetype == 'A' &
                                                                                  sidefire_vol_spd_2022$areatype == area_a[i] &
                                                                                  sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spd95 > 0)]) 
  sf_vol_spd_2022_summary_a[i,2] = max(sidefire_vol_spd_2022$volmaxperlane[which(sidefire_vol_spd_2022$weavetype == 'A' &
                                                                                   sidefire_vol_spd_2022$areatype == area_a[i] &
                                                                                   sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spdmax > 0)]) 
  sf_vol_spd_2022_summary_a[i,3] = max(sidefire_vol_spd_2022$volboxupperperlane[which(sidefire_vol_spd_2022$weavetype == 'A' &
                                                                                        sidefire_vol_spd_2022$areatype == area_a[i] &
                                                                                        sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spdboxupper > 0)]) 
  # problematic, multiple capacity in one weave type and area type
  sf_vol_spd_2022_summary_a[i,4] = max(sidefire_vol_spd_2022$hrcapperlane[which(sidefire_vol_spd_2022$weavetype == 'A' &
                                                                                  sidefire_vol_spd_2022$areatype == area_a[i])]) 
  
  # total number of records
  sf_vol_spd_2022_summary_a[i,5] = length(which(sidefire_vol_spd_2022$weavetype == 'A' &
                                                  sidefire_vol_spd_2022$areatype == area_a[i]))
}

sf_vol_spd_2022_summary_a = data.frame(sf_vol_spd_2022_summary_a)
colnames(sf_vol_spd_2022_summary_a) = c('vol95perlane','volmaxperlane','volboxupperperlane','hrcapperlane','record_count')
rownames(sf_vol_spd_2022_summary_a) = as.character(area_a)

arealookup = cbind(c(1,2,3,4,5), c('CBD','OBD','Urban Res','Suburb Res','Rural'))

wb = createWorkbook()
addWorksheet(wb, 'frwy_basic')
addWorksheet(wb, 'basic')
addWorksheet(wb, 'md')
addWorksheet(wb, 'a')
addWorksheet(wb, 'overallsummary')
addWorksheet(wb, 'arealookup')
writeData(wb, 'frwy_basic', sf_vol_spd_2022_summary_frwybasic, rowNames = TRUE)
writeData(wb, 'basic', sf_vol_spd_2022_summary_basic, rowNames = TRUE)
writeData(wb, 'md', sf_vol_spd_2022_summary_md, rowNames = TRUE)
writeData(wb, 'a', sf_vol_spd_2022_summary_a, rowNames = TRUE)
writeData(wb, 'arealookup', arealookup, rowNames = F, colNames = F)
writeData(wb, 'overallsummary', sidefire_vol_spd_2022)
saveWorkbook(wb, file = '20250410_capacity_recalculation\\RoadNetwork_2026\\Sensor_count\\05072025_sidefire_vol_spd_2022_stats.xlsx', overwrite = T)

rm(sf_vol_spd_2022_summary_a, sf_vol_spd_2022_summary_basic, sf_vol_spd_2022_summary_frwybasic, sf_vol_spd_2022_summary_md,
   area_a, area_basic, area_frwybasic, area_md, i, wb)


## create summary data for all area types, all weave types, volboxupperperlane
weavetype = unique(sidefire_vol_spd_2022$weavetype)
areatype = unique(sidefire_vol_spd_2022$areatype)
sf_vol_spd_summary_all = matrix(0, nrow = length(weavetype) * length(areatype), ncol = 3)
m = 0
for (i in 1:length(weavetype)) {
  for (j in 1:length(areatype)) {
    m = m + 1
    sf_vol_spd_summary_all[m,] = c(weavetype[i], areatype[j], 
                                   max(sidefire_vol_spd_2022$volboxupperperlane[which(sidefire_vol_spd_2022$weavetype == weavetype[i] &
                                                                                        sidefire_vol_spd_2022$areatype == areatype[j] &
                                                                                        sidefire_vol_spd_2022$ffspd - sidefire_vol_spd_2022$spdboxupper > 0)]))
  }
}

sf_vol_spd_summary_all = data.frame(sf_vol_spd_summary_all)
colnames(sf_vol_spd_summary_all) = c('weavetype', 'areatype', 'sfcapperlane')


############################################### plot VC ratio by using the max vol identified above ##########################################
## add calculated capacity to sf_2022_npmrds_2025_plot data table, generated in 4_roadlink_sf_npmrds_match_compare.R 
## if Inf value of the specific weave * area type, we use model capacity
sf_2022_npmrds_2025_plot$sfcapperlane = 0
for (i in 1:nrow(sf_2022_npmrds_2025_plot)) {
  if (length(which(is.infinite(sf_vol_spd_summary_all$sfcapperlane[which(sf_vol_spd_summary_all$weavetype == sf_2022_npmrds_2025_plot$weavetype[i] &
                                                             sf_vol_spd_summary_all$areatype == sf_2022_npmrds_2025_plot$areatype[i])]))) == 0) {
    sf_2022_npmrds_2025_plot$sfcapperlane[i] = as.numeric(sf_vol_spd_summary_all$sfcapperlane[which(sf_vol_spd_summary_all$weavetype == sf_2022_npmrds_2025_plot$weavetype[i] &
                                                                                                sf_vol_spd_summary_all$areatype == sf_2022_npmrds_2025_plot$areatype[i])])
  }
  else {
    sf_2022_npmrds_2025_plot$sfcapperlane[i] = sf_2022_npmrds_2025_plot$hrcap[i]/sf_2022_npmrds_2025_plot$lane[i]
  }
}

rm(weavetype, areatype, i, j, m)

#################################### plot VC ratio vs Speed by weave type, use SF capacity, use NPMRDS speed ########################################
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VCratio vs Spd, PK by data source npm, cap from SF_Frwybasic.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025_plot[which(sf_2022_npmrds_2025_plot$time != 'amop' &
                                        sf_2022_npmrds_2025_plot$time != 'pmop' &
                                        sf_2022_npmrds_2025_plot$source == 'npm' &
                                        sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC'),],
       aes(x = (as.numeric(avgvol)/as.numeric(lane))/as.numeric(sfcapperlane), y = as.numeric(avgspd))) + geom_point(color = '#B3E5FC', size = 2) +
  xlab('VC ratio') + ylab('Speed (MPH)') + coord_cartesian(ylim = c(0, 85), xlim = c(0,1)) + 
  labs(title = 'PK VC ratio vs Speed, FRWYBASIC') +
  theme_black()
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VCratio vs Spd, OP by data source npm, cap from SF_Frwybasic.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025_plot[which((sf_2022_npmrds_2025_plot$time == 'amop' |
                                         sf_2022_npmrds_2025_plot$time == 'pmop') &
                                        sf_2022_npmrds_2025_plot$source == 'npm' &
                                        sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC'),],
       aes(x = (as.numeric(avgvol)/as.numeric(lane))/as.numeric(sfcapperlane), y = as.numeric(avgspd))) + geom_point(color = '#B3E5FC', size = 2) +
  xlab('VC ratio') + ylab('Speed (MPH)') + coord_cartesian(ylim = c(0, 85)) + 
  labs(title = 'OP VC ratio vs Speed, FRWYBASIC') +
  theme_black()   
dev.off()


################################################ create plots to show the capacity of different weave type #################################### 
## done in Excel

#### clean environment ####
rm(spdarray_i, volarray_i, spdcol, volcol, boxupperIndex, maxIndex, q90Index, q95Index, q975Index, q99Index, i, link_i, missingsensors)
rm(arealookup, area_a, area_basic, area_md, area_frwybasic, i, wb)

