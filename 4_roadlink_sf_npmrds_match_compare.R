## new code created on May 6, 2025
## identify congested rdwy segments using sidefire data and npmrds
## updated May 09, sf_2022_npmrds_2025 has been scaled up to 1.1
## updated May 12, specific capacity categorized by weave type and ffspd. 

setwd("C:/Users/rtu/OneDrive - The North Central Texas Council of Governments/Documents/0_ModelDataDevelopment/")

library(ggplot2)
library(viridis)  # for colorblind-friendly palettes

## Data sources: 
## 1) sidefire_linkmatch_frwy_2025 is the SF MRDWY match, generated from roadlink_detector_geographic_matching_v2.R
## 2) sf_2022_npmrds_2025 and sf_2022_npmrds_2025_plot from roadlink_sf_npmrds_match_compare.R
## 3) vol_per_day_2022 is the processed data from the first section, different from roadlink_detector_vol_spd_stats_calculate_v2.R, which only has workday
## 4) vol_per_day_2022_linkmatch is the processed data from roadlink_sf_npmrds_match_compare.R, which deletes invalid data (Records_per_lane & dups)
## 5) vol_per_day_2022_raw is the raw data read from file

####################################### Filter out valid dataset with matched link ##############################################
## delete records that has 250-288 records_per_lane, and all dups

# check with vol_per_day 2022. Keep only Records_Per_Lane between 250 and 288, 
# vol_per_day_2022 = vol_per_day_2022_raw[which(vol_per_day_2022_raw$Records_Per_Lane >= 250 &
#                                                vol_per_day_2022_raw$Records_Per_Lane <= 288),]
# delete all dups if sensors ID & recorded data are exactly the same
# vol_per_day_2022$sensor_date_merge = paste(vol_per_day_2022$LinkID, vol_per_day_2022$Month, vol_per_day_2022$Date, sep = '_')
# vol_per_day_2022 = vol_per_day_2022[!duplicated(vol_per_day_2022$sensor_date_merge) & 
#                                      !duplicated(vol_per_day_2022$sensor_date_merge, fromLast = TRUE),] # 1322 link ID remains


# select workday, Tuesday, Wednesday, Thursday & February (matches with npmrds data), 956 remains
# vol_per_day_2022_feb_workday = vol_per_day_2022_linkmatch[which((vol_per_day_2022_linkmatch$DOW == 3 |
#                                                                   vol_per_day_2022_linkmatch$DOW == 4 |
#                                                                   vol_per_day_2022_linkmatch$DOW == 5) & 
#                                                                  vol_per_day_2022_linkmatch$Month == 2),] # select Feb workday records, 956 detectors

# vol_per_day_2022_workday = vol_per_day_2022_linkmatch[which((vol_per_day_2022_linkmatch$DOW == 3 |
#                                                               vol_per_day_2022_linkmatch$DOW == 4 |
#                                                               vol_per_day_2022_linkmatch$DOW == 5)),] # select Feb workday records, 956 detectors

############################### read npmrds feb 2024 average speed and match with sidefire ################
# first select locations (key: TMC) where average speed is lower than a certain threshold --> based on HCM
npmrds_feb2024 = data.frame(read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\NPMDRS_Feb_2024_Workday_AvgSpeed.csv', header = T))
# use TMC to match, each row represents one TMC, or locations

# because one TMC can be associated with several sidefires, match npmrds with sidefire
sidefire_linkmatch_frwy_2025$npmrds_tmc = 0

## match using TMC as the key
a = 0 # count how many npmrds tmc cannot find the sf match
b = 0 # count how many sidefires can be associated with several npmrds
for (i in 1:nrow(sidefire_linkmatch_frwy_2025)) {
  if (length(which(npmrds_feb2024$TMC == sidefire_linkmatch_frwy_2025$TMC[i])) == 1) {
    sidefire_linkmatch_frwy_2025$npmrds_tmc[i] = npmrds_feb2024$TMC[which(npmrds_feb2024$TMC == sidefire_linkmatch_frwy_2025$TMC[i])]
  } 
  else if (length(which(npmrds_feb2024$TMC == sidefire_linkmatch_frwy_2025$TMC[i])) == 0) {
    a = a + 1
    print(paste('TMC', sidefire_linkmatch_frwy_2025$TMC[i], 'not found in npmrds 2024'))
  }
  else if (length(which(npmrds_feb2024$TMC == sidefire_linkmatch_frwy_2025$TMC[i])) > 1) {
    b = b + 1
    print(paste('TMC', sidefire_linkmatch_frwy_2025$TMC[i], 'found multiple matches in npmrds'))
  }
}

## create a matrix that stores both npmrds, sidefire, and rdwy info in pk & op, including: lane, ffspd, cap, avgspd_npm, avgspd_sf, avgvol_sf
## updated May 9, all the sf count in this plot file has been scaled up to 1.1
sf_2022_npmrds_2025 = data.frame(cbind(sidefire_linkmatch_frwy_2025$sf_id, sidefire_linkmatch_frwy_2025$rdwy_id, 
                            sidefire_linkmatch_frwy_2025$TMC, sidefire_linkmatch_frwy_2025$npmrds_tmc,
                            sidefire_linkmatch_frwy_2025$weavetype, sidefire_linkmatch_frwy_2025$areatype, 
                            sidefire_linkmatch_frwy_2025$length,
                            
                            sidefire_linkmatch_frwy_2025$amffspd, sidefire_linkmatch_frwy_2025$pmffspd, 
                            sidefire_linkmatch_frwy_2025$opffspd, sidefire_linkmatch_frwy_2025$opffspd, # am op and pm op, two separated columns for convenience
                            
                            sidefire_linkmatch_frwy_2025$amlane, sidefire_linkmatch_frwy_2025$pmlane, 
                            sidefire_linkmatch_frwy_2025$oplane, sidefire_linkmatch_frwy_2025$oplane,
                            
                            sidefire_linkmatch_frwy_2025$amhrcap, sidefire_linkmatch_frwy_2025$pmhrcap, 
                            sidefire_linkmatch_frwy_2025$ophrcap, sidefire_linkmatch_frwy_2025$ophrcap))

sf_2022_npmrds_2025$amavgspd_sf = 0; sf_2022_npmrds_2025$pmavgspd_sf = 0; sf_2022_npmrds_2025$amopavgspd_sf = 0; sf_2022_npmrds_2025$pmopavgspd_sf = 0
sf_2022_npmrds_2025$amavgspd_npm = 0; sf_2022_npmrds_2025$pmavgspd_npm = 0; sf_2022_npmrds_2025$amopavgspd_npm = 0; sf_2022_npmrds_2025$pmopavgspd_npm = 0
sf_2022_npmrds_2025$amavgvol_sf = 0; sf_2022_npmrds_2025$pmavgvol_sf = 0; sf_2022_npmrds_2025$amopavgvol_sf = 0; sf_2022_npmrds_2025$pmopavgvol_sf = 0


colnames(sf_2022_npmrds_2025) = c('sf_id','rdwy_id','TMC_sf', 'TMC_npm', 'weavetype', 'areatype','length',
                                  'amffspd','pmffspd','amopffspd','pmopffspd',
                                  'amlane','pmlane','amoplane','pmoplane',
                                  'amhrcap','pmhrcap','amophrcap','pmophrcap',
                                  'amavgspd_sf','pmavgspd_sf','amopavgspd_sf','pmopavgspd_sf',
                                  'amavgspd_npm','pmavgspd_npm','amopavgspd_npm','pmopavgspd_npm',
                                  'amavgvol_sf','pmavgvol_sf','amopavgvol_sf','pmopavgvol_sf')


# delete rows where TMC of npm & sf does not match (npmrds_tmc == 0)
sf_2022_npmrds_2025 = sf_2022_npmrds_2025[which(sf_2022_npmrds_2025$TMC_npm != 0),] # in total 1207 can be matched

## calculate average speed and volume from sidefire vol-per-day dataset for matched TMC locations
## (upated May 07, 2025) split amop and pmop, instead of an aggregated op
## this dataset can be used to fit the VDF curve, the capacity should be estimated separately from vol-per-day only (not in this coding file)
# specify rows in sidefire vol per day table for am, pm, op
# 03-05, spdcol: 125-132, volcol: 32-36 # note: since vol is hourly basis, to calculate average volume from 3-5, we select 3, 3.15, 3.30, 3.45, 4, that's it
# 07-09, spdcol: 141-148, volcol: 48-52
# 16-18, spdcol: 177-184, volcol: 84-88
# 21-23, spdcol: 197-204, volcol: 104-108

amvolcol = seq(48, 52); amspdcol = seq(141, 148)
pmvolcol = seq(84, 88); pmspdcol = seq(177, 184)
amopvolcol = seq(32, 36); amopspdcol = seq(125, 132)
pmopvolcol = seq(104, 108); pmopspdcol = seq(197, 204)

row_to_delete = array(0, dim = 0) # count how many sf_id in the sidefire_2025 cannot be matched with vol per day 2022
## use Feb workday
for (i in 1:nrow(sf_2022_npmrds_2025)) {
  ## first find if vol per day has data or not
  link_i = vol_per_day_2022_feb_workday[which(vol_per_day_2022_feb_workday$LinkID == sf_2022_npmrds_2025$sf_id[i]),]
  if (nrow(link_i) == 0) {
    row_to_delete = c(row_to_delete, i)
    print(paste('SF detector', sf_2022_npmrds_2025$sf_id[i], 'not found in vol per day 2022 raw'))
    next
  }
  
  ## calculate SF vol and spd
  # get all am data from SF, vol per day
  volarray_am = allNA_returnNA(t(link_i[,amvolcol])) 
  spdarray_am = allNA_returnNA(t(link_i[,amspdcol])) 
  if (all(is.na(c(volarray_am))) | all(is.na(spdarray_am))) {
    print(paste('All AM vol & spd are NA, sensor ID', sf_2022_npmrds_2025$sf_id[i]))
  } else {
    sf_2022_npmrds_2025$amavgspd_sf[i] = mean(spdarray_am, na.rm = T)
    sf_2022_npmrds_2025$amavgvol_sf[i] = mean(volarray_am, na.rm = T)
  }
  
  # get all pm data from vol per day
  volarray_pm = allNA_returnNA(t(link_i[,pmvolcol])) 
  spdarray_pm = allNA_returnNA(t(link_i[,pmspdcol])) 
  if (all(is.na(c(volarray_pm))) | all(is.na(spdarray_pm))) {
    print(paste('All PM vol & spd are NA, sensor ID', sf_2022_npmrds_2025$sf_id[i]))
  } else {
    sf_2022_npmrds_2025$pmavgspd_sf[i] = mean(spdarray_pm, na.rm = T)
    sf_2022_npmrds_2025$pmavgvol_sf[i] = mean(volarray_pm, na.rm = T)
  }
  
  # get all amop data from vol per day
  volarray_amop = allNA_returnNA(t(link_i[,amopvolcol])) 
  spdarray_amop = allNA_returnNA(t(link_i[,amopspdcol])) 
  if (all(is.na(c(volarray_amop))) | all(is.na(spdarray_amop))) {
    print(paste('All OP vol & spd are NA, sensor ID', sf_2022_npmrds_2025$sf_id[i]))
  } else {
    sf_2022_npmrds_2025$amopavgspd_sf[i] = mean(spdarray_amop, na.rm = T)
    sf_2022_npmrds_2025$amopavgvol_sf[i] = mean(volarray_amop, na.rm = T)
  }
  
  # get all pmop data from vol per day
  volarray_pmop = allNA_returnNA(t(link_i[,pmopvolcol])) 
  spdarray_pmop = allNA_returnNA(t(link_i[,pmopspdcol])) 
  if (all(is.na(c(volarray_pmop))) | all(is.na(spdarray_pmop))) {
    print(paste('All OP vol & spd are NA, sensor ID', sf_2022_npmrds_2025$sf_id[i]))
  } else {
    sf_2022_npmrds_2025$pmopavgspd_sf[i] = mean(spdarray_pmop, na.rm = T)
    sf_2022_npmrds_2025$pmopavgvol_sf[i] = mean(volarray_pmop, na.rm = T)
  }
  
  ## finally, match avg speed from npm
  sf_2022_npmrds_2025$amavgspd_npm[i] = npmrds_feb2024$Avg_Speed_07_09[which(npmrds_feb2024$TMC == sf_2022_npmrds_2025$TMC_npm[i])]
  sf_2022_npmrds_2025$pmavgspd_npm[i] = npmrds_feb2024$Avg_Speed_16_18[which(npmrds_feb2024$TMC == sf_2022_npmrds_2025$TMC_npm[i])]
  sf_2022_npmrds_2025$amopavgspd_npm[i] = as.numeric(npmrds_feb2024$Avg_Speed_03_05[which(npmrds_feb2024$TMC == sf_2022_npmrds_2025$TMC_npm[i])])
  sf_2022_npmrds_2025$pmopavgspd_npm[i] = as.numeric(npmrds_feb2024$Avg_Speed_21_23[which(npmrds_feb2024$TMC == sf_2022_npmrds_2025$TMC_npm[i])])
}

#### delete rows that do not find matched vol spd, 782 remains (??)
## run every time when sf_npmrds matching performs
# sf_2022_npmrds_2025 = sf_2022_npmrds_2025[-row_to_delete,] 

# scale up to 1.1
sf_2022_npmrds_2025$amavgvol_sf_s = sf_2022_npmrds_2025$amavgvol_sf * 1.1
sf_2022_npmrds_2025$pmavgvol_sf_s = sf_2022_npmrds_2025$pmavgvol_sf * 1.1
sf_2022_npmrds_2025$amopavgvol_sf_s = sf_2022_npmrds_2025$amopavgvol_sf * 1.1
sf_2022_npmrds_2025$pmopavgvol_sf_s = sf_2022_npmrds_2025$pmopavgvol_sf * 1.1


## calculate spd diff (SF - NPM)
sf_2022_npmrds_2025$amspddiff = sf_2022_npmrds_2025$amavgspd_sf - sf_2022_npmrds_2025$amavgspd_npm
sf_2022_npmrds_2025$pmspddiff = sf_2022_npmrds_2025$pmavgspd_sf - sf_2022_npmrds_2025$pmavgspd_npm
sf_2022_npmrds_2025$amopspddiff = sf_2022_npmrds_2025$amopavgspd_sf - sf_2022_npmrds_2025$amopavgspd_npm
sf_2022_npmrds_2025$pmopspddiff = sf_2022_npmrds_2025$pmopavgspd_sf - sf_2022_npmrds_2025$pmopavgspd_npm

## convert columns to numeric
for (i in 7:39) {
  sf_2022_npmrds_2025[,i] = as.numeric(sf_2022_npmrds_2025[,i])
}

# calculate

#### delete extremely large vol
## run every time when sf_npmrds matching performs
row_to_delete = which(sf_2022_npmrds_2025$amavgvol_sf/sf_2022_npmrds_2025$amlane > 2500 |
                        sf_2022_npmrds_2025$pmavgvol_sf/sf_2022_npmrds_2025$pmlane > 2500 |
                        sf_2022_npmrds_2025$amopavgvol_sf/sf_2022_npmrds_2025$amoplane > 2500 |
                        sf_2022_npmrds_2025$pmopavgvol_sf/sf_2022_npmrds_2025$pmoplane > 2500)
if (length(row_to_delete) > 0) {
  sf_2022_npmrds_2025 = sf_2022_npmrds_2025[-row_to_delete,] # run every time when sf_npmrds matching performs
}

## clean environment 
rm(amspdcol,amvolcol, pmspdcol,pmvolcol, amopspdcol, amopvolcol, pmopspdcol, pmopvolcol,row_to_delete, 
   volarray_am, volarray_pm, volarray_amop, volarray_pmop, spdarray_am, spdarray_pm, spdarray_amop, spdarray_pmop, 
   link_i, a, b, i, sidefire_ID, tmc_npmrds, weavetype)

# export sf_2022_npmrds_2025 & map in ArcGIS
write.csv(sf_2022_npmrds_2025, '20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\sf_2022_npmrds_2024_FebWorkday.csv', row.names = F)

################## compare npm & sf spd side-by-side ###############
## am
ggplot(sf_2022_npmrds_2025, aes(x = amspddiff)) + geom_histogram(bins = 15, fill = 'lightblue', color = 'white', linewidth = 0.5) +
  xlab('Speed Difference (SideFire - NPMRDS)') + ylab('Frequency') + 
  labs(title = 'AM average speed difference between sidefire and NMPRDS') +
  theme_black() 

## pm
ggplot(sf_2022_npmrds_2025, aes(x = pmspddiff)) + geom_histogram(bins = 15, fill = 'lightblue', color = 'white', linewidth = 0.5) +
  xlab('Speed Difference (SideFire - NPMRDS)') + ylab('Frequency') + 
  labs(title = 'PM average speed difference between sidefire and NMPRDS') +
  theme_black()

## am op
ggplot(sf_2022_npmrds_2025, aes(x = amopspddiff)) + geom_histogram(bins = 15, fill = 'lightblue', color = 'white', linewidth = 0.5) +
  xlab('Speed Difference (SideFire - NPMRDS)') + ylab('Frequency') + 
  labs(title = 'AM OP average speed difference between sidefire and NMPRDS') +
  theme_black() 

## pm op
ggplot(sf_2022_npmrds_2025, aes(x = pmopspddiff)) + geom_histogram(bins = 15, fill = 'lightblue', color = 'white', linewidth = 0.5) +
  xlab('Speed Difference (SideFire - NPMRDS)') + ylab('Frequency') + 
  labs(title = 'PM OP average speed difference between sidefire and NMPRDS') +
  theme_black() 


######################### re-organize sf_2022_npmrds_2025 as a dataframe for ggplot ##############
## updated May 09, vol has been scaled up to 1.1
sf_2022_npmrds_2025_plot = matrix(0, nrow = 0, ncol = 13)
for (i in 1:nrow(sf_2022_npmrds_2025)) {
  sf_am = c('sf','am',
            sf_2022_npmrds_2025$amlane[i], sf_2022_npmrds_2025$amffspd[i], sf_2022_npmrds_2025$amhrcap[i],
            sf_2022_npmrds_2025$amavgspd_sf[i], sf_2022_npmrds_2025$amavgvol_sf_s[i])
  npm_am = c('npm','am',
             sf_2022_npmrds_2025$amlane[i], sf_2022_npmrds_2025$amffspd[i], sf_2022_npmrds_2025$amhrcap[i],
             sf_2022_npmrds_2025$amavgspd_npm[i], sf_2022_npmrds_2025$amavgvol_sf_s[i])
  
  sf_pm = c('sf','pm',
            sf_2022_npmrds_2025$pmlane[i], sf_2022_npmrds_2025$pmffspd[i], sf_2022_npmrds_2025$pmhrcap[i],
            sf_2022_npmrds_2025$pmavgspd_sf[i], sf_2022_npmrds_2025$pmavgvol_sf_s[i])
  npm_pm = c('npm','pm',
             sf_2022_npmrds_2025$pmlane[i], sf_2022_npmrds_2025$pmffspd[i], sf_2022_npmrds_2025$pmhrcap[i],
             sf_2022_npmrds_2025$pmavgspd_npm[i], sf_2022_npmrds_2025$pmavgvol_sf_s[i])
  
  sf_amop = c('sf','amop',
            sf_2022_npmrds_2025$amoplane[i], sf_2022_npmrds_2025$amopffspd[i], sf_2022_npmrds_2025$amophrcap[i],
            sf_2022_npmrds_2025$amopavgspd_sf[i], sf_2022_npmrds_2025$amopavgvol_sf_s[i])
  npm_amop = c('npm','amop',
             sf_2022_npmrds_2025$amoplane[i], sf_2022_npmrds_2025$amopffspd[i], sf_2022_npmrds_2025$amophrcap[i],
             sf_2022_npmrds_2025$amopavgspd_npm[i], sf_2022_npmrds_2025$amopavgvol_sf_s[i])
  
  sf_pmop = c('sf','pmop',
            sf_2022_npmrds_2025$pmoplane[i], sf_2022_npmrds_2025$pmopffspd[i], sf_2022_npmrds_2025$pmophrcap[i],
            sf_2022_npmrds_2025$pmopavgspd_sf[i], sf_2022_npmrds_2025$pmopavgvol_sf_s[i])
  npm_pmop = c('npm','pmop',
             sf_2022_npmrds_2025$pmoplane[i], sf_2022_npmrds_2025$pmopffspd[i], sf_2022_npmrds_2025$pmophrcap[i],
             sf_2022_npmrds_2025$pmopavgspd_npm[i], sf_2022_npmrds_2025$pmopavgvol_sf_s[i])
  
  sf_2022_npmrds_2025_plot = rbind(sf_2022_npmrds_2025_plot,
                                   cbind(sf_2022_npmrds_2025$sf_id[i], sf_2022_npmrds_2025$rdwy_id[i], 
                                         sf_2022_npmrds_2025$TMC_sf[i], 
                                         sf_2022_npmrds_2025$weavetype[i], sf_2022_npmrds_2025$areatype[i], 
                                         sf_2022_npmrds_2025$length[i],
                                         rbind(sf_am, npm_am, sf_pm, npm_pm, sf_amop, npm_amop, sf_pmop, npm_pmop)))
}
## format as data frame, name cols, and convert to numeric
sf_2022_npmrds_2025_plot = data.frame(sf_2022_npmrds_2025_plot)
colnames(sf_2022_npmrds_2025_plot) = c('sf_id', 'rdwy_id', 'TMC','weavetype', 'areatype','length', 'source','time','lane','ffspd','hrcap','avgspd','avgvol')
for (i in 9:13) {
  sf_2022_npmrds_2025_plot[,i] = as.numeric(sf_2022_npmrds_2025_plot[,i])
}

## clean environment
rm(amopspdcol,pmopspdcol,amopvolcol,pmopvolcol,i,npm_am,npm_amop,npm_pm,npm_pmop,sf_am,sf_amop,sf_pm,sf_pmop)

#################################### plot speed comparison by time period and weave type ###########################################
library(Metrics)
library(ModelMetrics)

## AM frwybasic
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/Sidefire_NPMRDS_SpdDiff_AM_Frwybasic.png", 
    width = 800, height = 600)

label_text <- paste0(
  "No. Obs: ", length(which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')), "\n",
  "% Error: ", round((sum(sf_2022_npmrds_2025$amavgspd_sf[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]) - 
                  sum(sf_2022_npmrds_2025$amavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]))/
    sum(sf_2022_npmrds_2025$amavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]) * 100, 2), '%', "\n",
  
  "RMSE: ", round(rmse(sf_2022_npmrds_2025$amavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')], 
                   sf_2022_npmrds_2025$amavgspd_sf[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]),2), '\n',
  
  "% RSQ: ", round(summary(
    lm(amavgspd_sf ~ amavgspd_npm, data= sf_2022_npmrds_2025[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC'),]))$r.squared * 100, 2), '%'
)

ggplot(sf_2022_npmrds_2025[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC'),],
       aes(x = as.numeric(amavgspd_npm), y = as.numeric(amavgspd_sf))) + 
  geom_point(color = '#B3E5FC', size = 2) +
  geom_abline(intercept = 0, slope = 1, color = 'yellow', linewidth = 1.5) +
  xlab('NPMRDS Speed (MPH)') + ylab('SideFire Speed (MPH)') + 
  coord_cartesian(ylim = c(0, 85), xlim = c(0,85)) + 
  labs(title = 'AM Speed Freeway Basic, SideFire vs. NPMRDS') +
  annotate_stats(label_text) +
  theme_black() # 👈 Use your custom theme here
dev.off()

## PM frwybasic
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/Sidefire_NPMRDS_SpdDiff_PM_Frwybasic.png", 
    width = 800, height = 600)
label_text <- paste0(
  "No. Obs: ", length(which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')), "\n",
  "% Error: ", round((sum(sf_2022_npmrds_2025$pmavgspd_sf[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]) - 
                        sum(sf_2022_npmrds_2025$pmavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]))/
                       sum(sf_2022_npmrds_2025$pmavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]) * 100, 2), '%', "\n",
  
  "RMSE: ", round(rmse(sf_2022_npmrds_2025$pmavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')], 
                       sf_2022_npmrds_2025$pmavgspd_sf[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]),2), '\n',
  
  "% RSQ: ", round(summary(
    lm(pmavgspd_sf ~ pmavgspd_npm, data= sf_2022_npmrds_2025[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC'),]))$r.squared * 100, 2), '%'
)

ggplot(sf_2022_npmrds_2025,
       aes(x = as.numeric(pmavgspd_npm), y = as.numeric(pmavgspd_sf))) + 
  geom_point(color = '#B3E5FC', size = 2) +
  geom_abline(intercept = 0, slope = 1, color = 'yellow', linewidth = 1.5) +
  xlab('NPMRDS Speed (MPH)') + ylab('SideFire Speed (MPH)') + 
  coord_cartesian(ylim = c(0, 85), xlim = c(0,85)) + 
  labs(title = 'PM Speed Freeway Basic, SideFire vs. NPMRDS') +
  annotate_stats(label_text) +
  theme_black()
dev.off()

## pmop frwybasic
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/Sidefire_NPMRDS_SpdDiff_PMOP_Frwybasic.png", 
    width = 800, height = 600)
label_text <- paste0(
  "No. Obs: ", length(which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')), "\n",
  "% Error: ", round((sum(sf_2022_npmrds_2025$pmopavgspd_sf[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]) - 
                        sum(sf_2022_npmrds_2025$pmopavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]))/
                       sum(sf_2022_npmrds_2025$pmopavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]) * 100, 2), '%', "\n",
  
  "RMSE: ", round(rmse(sf_2022_npmrds_2025$pmopavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')], 
                       sf_2022_npmrds_2025$pmopavgspd_sf[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]),2), '\n',
  
  "% RSQ: ", round(summary(
    lm(pmopavgspd_sf ~ pmopavgspd_npm, data= sf_2022_npmrds_2025[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC'),]))$r.squared * 100, 2), '%'
)

ggplot(sf_2022_npmrds_2025,
       aes(x = as.numeric(pmopavgspd_npm), y = as.numeric(pmopavgspd_sf))) + 
  geom_point(color = '#B3E5FC', size = 2) +
  geom_abline(intercept = 0, slope = 1, color = 'yellow', linewidth = 1.5) +
  xlab('NPMRDS Speed (MPH)') + ylab('SideFire Speed (MPH)') + 
  coord_cartesian(ylim = c(0, 85), xlim = c(0,85)) + 
  labs(title = '9-11PM Speed Freeway Basic, SideFire vs. NPMRDS') +
  annotate_stats(label_text) +
  theme_black()
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/Sidefire_NPMRDS_SpdDiff_AMOP_Frwybasic.png", 
    width = 800, height = 600)
label_text <- paste0(
  "No. Obs: ", length(which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')), "\n",
  "% Error: ", round((sum(sf_2022_npmrds_2025$amopavgspd_sf[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]) - 
                        sum(sf_2022_npmrds_2025$amopavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]))/
                       sum(sf_2022_npmrds_2025$amopavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]) * 100, 2), '%', "\n",
  
  "RMSE: ", round(rmse(sf_2022_npmrds_2025$amopavgspd_npm[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')], 
                       sf_2022_npmrds_2025$amopavgspd_sf[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC')]),2), '\n',
  
  "% RSQ: ", round(summary(
    lm(amopavgspd_sf ~ amopavgspd_npm, data= sf_2022_npmrds_2025[which(sf_2022_npmrds_2025$weavetype == 'FRWY_BASIC'),]))$r.squared * 100, 2), '%'
)
ggplot(sf_2022_npmrds_2025,
       aes(x = as.numeric(amopavgspd_npm), y = as.numeric(amopavgspd_sf))) + 
  geom_point(color = '#B3E5FC', size = 2) +
  geom_abline(intercept = 0, slope = 1, color = 'yellow', linewidth = 1.5) +
  xlab('NPMRDS Speed (MPH)') + ylab('SideFire Speed (MPH)') + 
  coord_cartesian(ylim = c(0, 85), xlim = c(0,85)) + 
  labs(title = '3-5AM Speed Freeway Basic, SideFire vs. NPMRDS') +
  annotate_stats(label_text) +
  theme_black()
dev.off()


#################################### plot VC ratio vs Speed by data source, use model cap, use NPMRDS speed only ########################################
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VCratio vs Spd, PK by data source npm_Frwybasic.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025_plot[which(sf_2022_npmrds_2025_plot$time != 'amop' &
                                        sf_2022_npmrds_2025_plot$time != 'pmop' &
                                        sf_2022_npmrds_2025_plot$source == 'npm' &
                                        sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC'),],
       aes(x = as.numeric(avgvol)/as.numeric(hrcap), y = as.numeric(avgspd))) + geom_point(color = '#B3E5FC', size = 2) +
  xlab('VC ratio') + ylab('Speed (MPH)') + coord_cartesian(ylim = c(0, 85), xlim = c(0,1)) + 
  labs(title = 'PK VC ratio vs Speed, FRWYBASIC') +
  theme_black()
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/VCratio vs Spd, OP by data source npm_Frwybasic.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025_plot[which((sf_2022_npmrds_2025_plot$time == 'amop' |
                                         sf_2022_npmrds_2025_plot$time == 'pmop') &
                                        sf_2022_npmrds_2025_plot$source == 'npm' &
                                        sf_2022_npmrds_2025_plot$weavetype == 'FRWY_BASIC'),],
       aes(x = as.numeric(avgvol)/as.numeric(hrcap), y = as.numeric(avgspd))) + geom_point(color = '#B3E5FC', size = 2) +
  xlab('VC ratio') + ylab('Speed (MPH)') + coord_cartesian(ylim = c(0, 85)) + 
  labs(title = 'OP VC ratio vs Speed, FRWYBASIC') +
  theme_black()   
dev.off()



################################################ calculate density and categorized to LOS according to HCM 2016 ###############################
sf_2022_npmrds_2025_plot$density = sf_2022_npmrds_2025_plot$avgvol/sf_2022_npmrds_2025_plot$avgspd/sf_2022_npmrds_2025_plot$lane
# mark LOS to A, B, C, D, E, F (1-6), according to HCM 2016, needs to consider area type
loslookup = data.frame(cbind(los = c('A','B','C','D','E'), 
              densityurban = c(11, 18, 26, 35, 45),
              densityrural = c(6, 14, 22, 29, 39)))
sf_2022_npmrds_2025_plot$los = 0
for (i in 1:nrow(sf_2022_npmrds_2025_plot)) {
  if (sf_2022_npmrds_2025_plot$areatype[i] == '5') {
    j = 1
    while (j <= 5) {
      if (sf_2022_npmrds_2025_plot$density[i] <= as.numeric(loslookup$densityrural[j])) {
        sf_2022_npmrds_2025_plot$los[i] = loslookup$los[j]
        break
      }
      else {
        j = j + 1
        next
      }
    }
  }
  else {
    j = 1
    while (j <= 5) {
      if (sf_2022_npmrds_2025_plot$density[i] <= as.numeric(loslookup$densityurban[j])) {
        sf_2022_npmrds_2025_plot$los[i] = loslookup$los[j]
        break
      }
      else {
        j = j + 1
        next
      }
    }
  }
}

sf_2022_npmrds_2025_plot$los[which(sf_2022_npmrds_2025_plot$los == '0')] = 'F'

################################### comment out sessions ################################################
## including: keep records where ffspd > spd; re-organize weave type and write to csv to compare capacity & vol. 
## since we use average speed & col in this coding file, the weave type comparison is no longer needed. This is only required in the capacity estimation

## conclusion: use NPMRDS of the corresponding link as the speed, in this case, we can only select average speed
# delete those spd > ffspd
# sf_2022_npmrds_2025 = sf_2022_npmrds_2025[which(sf_2022_npmrds_2025$amffspd - sf_2022_npmrds_2025$amavgspd_npm > 0 |
#                                                  sf_2022_npmrds_2025$pmffspd - sf_2022_npmrds_2025$pmavgspd_npm > 0 |
#                                                  sf_2022_npmrds_2025$opffspd - sf_2022_npmrds_2025$opavgspd_npm > 0),]

# by weave type, calculate max vol, corresponding sf & npm spd, capacity
# get unique weave type & time
weavetype = unique(sf_2022_npmrds_2025_plot$weavetype)
timeperiod = c("am", "pm", "op")
# create a new data frame
vol_cap_byweave_time = matrix(0, nrow = 0, ncol = 6)

for (i in 1:3) {
  for (j in 1:length(weavetype)) {
    row_ij = sf_2022_npmrds_2025_plot[which(sf_2022_npmrds_2025_plot$weavetype == weavetype[j] &
                                              sf_2022_npmrds_2025_plot$time == timeperiod[i]),]
    vol_ij = max(row_ij$avgvol/row_ij$lane)
    spd_ij = row_ij$avgspd[getIndex(row_ij$avgvol/row_ij$lane, vol_ij)]
    cap_ij = row_ij$hrcap[getIndex(row_ij$avgvol/row_ij$lane, vol_ij)]/row_ij$lane[getIndex(row_ij$avgvol/row_ij$lane, vol_ij)]
    spd_ij_npm = row_ij$avgspd[getIndex(row_ij$avgvol/row_ij$lane, vol_ij) + 1]
    vol_cap_byweave_time = rbind(vol_cap_byweave_time, c(timeperiod[i], weavetype[j], vol_ij, spd_ij, spd_ij_npm, cap_ij))
  }
}
colnames(vol_cap_byweave_time) = c('time','weavetype','maxvolperlane','spd_sf', 'spd_npm','capperlane')
vol_cap_byweave_time = data.frame(vol_cap_byweave_time)

write.csv(vol_cap_byweave_time, '20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/vol_cap_byweave_time.csv', row.names = F)


