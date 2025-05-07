## new code created on May 6, 2025
## identify congested rdwy segments using sidefire data and npmrds

setwd("C:/Users/rtu/OneDrive - The North Central Texas Council of Governments/Documents/0_ModelDataDevelopment/")

library(ggplot2)
library(viridis)  # for colorblind-friendly palettes

######################## filter out records in vol_per_day_2022_raw of which sensor ID matches #############
sidefire_ID = unique(sidefire_linkmatch_frwy_2025$sf_id)
# instead of appending, first select rows from vol_per_day data, then select the data of corresponding row --> fasten the process
row_to_select = array(0, dim = 0)
for (i in 1:length(sidefire_ID)) {
  row_to_select = c(row_to_select, which(vol_per_day_2022_raw$LinkID == sidefire_ID[i]))
}

vol_per_day_2022_raw_linkmatch = vol_per_day_2022_raw[row_to_select,] # 1334 detectors in 2022 hourly count match with the roadlink & 2025 detector location

# select workday, Tuesday, Wednesday, Thursday & February (matches with npmrds data)
vol_per_day_2022_workday = vol_per_day_2022_raw_linkmatch[which((vol_per_day_2022_raw_linkmatch$DOW == 3 |
                                                                  vol_per_day_2022_raw_linkmatch$DOW == 4 |
                                                                  vol_per_day_2022_raw_linkmatch$DOW == 5) & # after workday filter, 1334 detectors 
                                                                  vol_per_day_2022_raw_linkmatch$Records_Per_Lane >= 250 &
                                                                  vol_per_day_2022_raw_linkmatch$Records_Per_Lane <= 288 & # after records_per_lane filter, 948 detectors
                                                                  vol_per_day_2022_raw_linkmatch$Month == 2),] # select Feb records, 898 detectors

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
sf_2022_npmrds_2025 = data.frame(cbind(sidefire_linkmatch_frwy_2025$sf_id, sidefire_linkmatch_frwy_2025$rdwy_id, 
                            sidefire_linkmatch_frwy_2025$TMC, sidefire_linkmatch_frwy_2025$npmrds_tmc,
                            sidefire_linkmatch_frwy_2025$weavetype, 
                            
                            sidefire_linkmatch_frwy_2025$amffspd, sidefire_linkmatch_frwy_2025$pmffspd, 
                            sidefire_linkmatch_frwy_2025$opffspd, sidefire_linkmatch_frwy_2025$opffspd, # am op and pm op, two separated columns for convenience
                            
                            sidefire_linkmatch_frwy_2025$amlane, sidefire_linkmatch_frwy_2025$pmlane, 
                            sidefire_linkmatch_frwy_2025$oplane, sidefire_linkmatch_frwy_2025$oplane,
                            
                            sidefire_linkmatch_frwy_2025$amhrcap, sidefire_linkmatch_frwy_2025$pmhrcap, 
                            sidefire_linkmatch_frwy_2025$ophrcap, sidefire_linkmatch_frwy_2025$ophrcap))

sf_2022_npmrds_2025$amavgspd_sf = 0; sf_2022_npmrds_2025$pmavgspd_sf = 0; sf_2022_npmrds_2025$amopavgspd_sf = 0; sf_2022_npmrds_2025$pmopavgspd_sf = 0
sf_2022_npmrds_2025$amavgspd_npm = 0; sf_2022_npmrds_2025$pmavgspd_npm = 0; sf_2022_npmrds_2025$amopavgspd_npm = 0; sf_2022_npmrds_2025$pmopavgspd_npm = 0
sf_2022_npmrds_2025$amavgvol_sf = 0; sf_2022_npmrds_2025$pmavgvol_sf = 0; sf_2022_npmrds_2025$amopavgvol_sf = 0; sf_2022_npmrds_2025$pmopavgvol_sf = 0


colnames(sf_2022_npmrds_2025) = c('sf_id','rdwy_id','TMC_sf', 'TMC_npm', 'weavetype',
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
for (i in 1:nrow(sf_2022_npmrds_2025)) {
  ## first find if vol per day has data or not
  link_i = vol_per_day_2022_raw_linkmatch[which(vol_per_day_2022_raw_linkmatch$LinkID == sf_2022_npmrds_2025$sf_id[i]),]
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
  } else {sf_2022_npmrds_2025$pmavgspd_sf[i] = mean(spdarray_pm, na.rm = T)
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

## delete rows that do not find matched vol spd
# sf_2022_npmrds_2025 = sf_2022_npmrds_2025[-row_to_delete,] # run every time when sf_npmrds matching performs

# delete extremely large vol
row_to_delete = which(sf_2022_npmrds_2025$amavgvol_sf/sf_2022_npmrds_2025$amlane > 2500 |
                        sf_2022_npmrds_2025$pmavgvol_sf/sf_2022_npmrds_2025$pmlane > 2500 |
                        sf_2022_npmrds_2025$amopavgvol_sf/sf_2022_npmrds_2025$amoplane > 2500 |
                        sf_2022_npmrds_2025$pmopavgvol_sf/sf_2022_npmrds_2025$pmoplane > 2500)
## sf_2022_npmrds_2025 = sf_2022_npmrds_2025[-row_to_delete,] # run every time when sf_npmrds matching performs # 1127 remaining

## convert columns to numeric
for (i in 6:33) {
  sf_2022_npmrds_2025[,i] = as.numeric(sf_2022_npmrds_2025[,i])
}

## clean environment 
rm(amspdcol,amvolcol, pmspdcol,pmvolcol, opspdcol,opvolcol, row_to_delete, 
   volarray_am, volarray_pm, volarray_amop, volarray_pmop, spdarray_am, spdarray_pm, spdarray_amop, spdarray_pmop, link_i)

# export sf_2022_npmrds_2025 & map in ArcGIS
write.csv(sf_2022_npmrds_2025, '20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\sf_2022_npmrds_2024.csv', row.names = F)

################## compare npm & sf spd side-by-side ###############
## am
sf_2022_npmrds_2025$amspddiff = sf_2022_npmrds_2025$amavgspd_sf - sf_2022_npmrds_2025$amavgspd_npm
ggplot(sf_2022_npmrds_2025, aes(x = amspddiff)) + geom_histogram(bins = 15, fill = 'lightblue', color = 'white', linewidth = 0.5) +
  xlab('Speed Difference (SideFire - NPMRDS)') + ylab('Frequency') + 
  labs(title = 'AM average speed difference between sidefire and NMPRDS') +
  theme_black() 

## pm
sf_2022_npmrds_2025$pmspddiff = sf_2022_npmrds_2025$pmavgspd_sf - sf_2022_npmrds_2025$pmavgspd_npm
ggplot(sf_2022_npmrds_2025, aes(x = pmspddiff)) + geom_histogram(bins = 15, fill = 'lightblue', color = 'white', linewidth = 0.5) +
  xlab('Speed Difference (SideFire - NPMRDS)') + ylab('Frequency') + 
  labs(title = 'PM average speed difference between sidefire and NMPRDS') +
  theme_black()

## am op
sf_2022_npmrds_2025$amopspddiff = sf_2022_npmrds_2025$amopavgspd_sf - sf_2022_npmrds_2025$amopavgspd_npm
ggplot(sf_2022_npmrds_2025, aes(x = amopspddiff)) + geom_histogram(bins = 15, fill = 'lightblue', color = 'white', linewidth = 0.5) +
  xlab('Speed Difference (SideFire - NPMRDS)') + ylab('Frequency') + 
  labs(title = 'AM OP average speed difference between sidefire and NMPRDS') +
  theme_black() 

## pm op
sf_2022_npmrds_2025$pmopspddiff = sf_2022_npmrds_2025$pmopavgspd_sf - sf_2022_npmrds_2025$pmopavgspd_npm
ggplot(sf_2022_npmrds_2025, aes(x = pmopspddiff)) + geom_histogram(bins = 15, fill = 'lightblue', color = 'white', linewidth = 0.5) +
  xlab('Speed Difference (SideFire - NPMRDS)') + ylab('Frequency') + 
  labs(title = 'PM OP average speed difference between sidefire and NMPRDS') +
  theme_black() 


######################### re-organize sf_2022_npmrds_2025 as a dataframe for ggplot ##############
sf_2022_npmrds_2025_plot = matrix(0, nrow = 0, ncol = 11)
for (i in 1:nrow(sf_2022_npmrds_2025)) {
  sf_am = c('sf','am',
            sf_2022_npmrds_2025$amlane[i], sf_2022_npmrds_2025$amffspd[i], sf_2022_npmrds_2025$amhrcap[i],
            sf_2022_npmrds_2025$amavgspd_sf[i], sf_2022_npmrds_2025$amavgvol_sf[i])
  npm_am = c('npm','am',
             sf_2022_npmrds_2025$amlane[i], sf_2022_npmrds_2025$amffspd[i], sf_2022_npmrds_2025$amhrcap[i],
             sf_2022_npmrds_2025$amavgspd_npm[i], sf_2022_npmrds_2025$amavgvol_sf[i])
  
  sf_pm = c('sf','pm',
            sf_2022_npmrds_2025$pmlane[i], sf_2022_npmrds_2025$pmffspd[i], sf_2022_npmrds_2025$pmhrcap[i],
            sf_2022_npmrds_2025$pmavgspd_sf[i], sf_2022_npmrds_2025$pmavgvol_sf[i])
  npm_pm = c('npm','pm',
             sf_2022_npmrds_2025$pmlane[i], sf_2022_npmrds_2025$pmffspd[i], sf_2022_npmrds_2025$pmhrcap[i],
             sf_2022_npmrds_2025$pmavgspd_npm[i], sf_2022_npmrds_2025$pmavgvol_sf[i])
  
  sf_amop = c('sf','amop',
            sf_2022_npmrds_2025$amoplane[i], sf_2022_npmrds_2025$amopffspd[i], sf_2022_npmrds_2025$amophrcap[i],
            sf_2022_npmrds_2025$amopavgspd_sf[i], sf_2022_npmrds_2025$amopavgvol_sf[i])
  npm_amop = c('npm','amop',
             sf_2022_npmrds_2025$amoplane[i], sf_2022_npmrds_2025$amopffspd[i], sf_2022_npmrds_2025$amophrcap[i],
             sf_2022_npmrds_2025$amopavgspd_npm[i], sf_2022_npmrds_2025$amopavgvol_sf[i])
  
  sf_pmop = c('sf','pmop',
            sf_2022_npmrds_2025$pmoplane[i], sf_2022_npmrds_2025$pmopffspd[i], sf_2022_npmrds_2025$pmophrcap[i],
            sf_2022_npmrds_2025$pmopavgspd_sf[i], sf_2022_npmrds_2025$pmopavgvol_sf[i])
  npm_pmop = c('npm','pmop',
             sf_2022_npmrds_2025$pmoplane[i], sf_2022_npmrds_2025$pmopffspd[i], sf_2022_npmrds_2025$pmophrcap[i],
             sf_2022_npmrds_2025$pmopavgspd_npm[i], sf_2022_npmrds_2025$pmopavgvol_sf[i])
  
  sf_2022_npmrds_2025_plot = rbind(sf_2022_npmrds_2025_plot,
                                   cbind(sf_2022_npmrds_2025$sf_id[i], sf_2022_npmrds_2025$rdwy_id[i], 
                                         sf_2022_npmrds_2025$TMC_sf[i], sf_2022_npmrds_2025$weavetype[i],
                                         rbind(sf_am, npm_am, sf_pm, npm_pm, sf_amop, npm_amop, sf_pmop, npm_pmop)))
}
## format as data frame, name cols, and convert to numeric
sf_2022_npmrds_2025_plot = data.frame(sf_2022_npmrds_2025_plot)
colnames(sf_2022_npmrds_2025_plot) = c('sf_id', 'rdwy_id', 'TMC','weavetype', 'source','time','lane','ffspd','hrcap','avgspd','avgvol')
for (i in 7:11) {
  sf_2022_npmrds_2025_plot[,i] = as.numeric(sf_2022_npmrds_2025_plot[,i])
}

#################################### plot speed comparison by time period ###########################################
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/Sidefire_NPMRDS_SpdDiff_AM.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025,
       aes(x = as.numeric(amavgspd_sf), y = as.numeric(amavgspd_npm))) + 
  geom_point(color = '#B3E5FC', size = 2) +
  geom_abline(intercept = 0, slope = 1, color = 'yellow', linewidth = 1.5) +
  xlab('SideFire Speed (MPH)') + ylab('NPMRDS Speed (MPH)') + 
  coord_cartesian(ylim = c(0, 85), xlim = c(0,85)) + 
  labs(title = 'AM Speed side-by-side comparison, SideFire vs. NPMRDS') +
  theme_black() # 👈 Use your custom theme here
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/Sidefire_NPMRDS_SpdDiff_PM.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025,
       aes(x = as.numeric(pmavgspd_sf), y = as.numeric(pmavgspd_npm))) + 
  geom_point(color = '#B3E5FC', size = 2) +
  geom_abline(intercept = 0, slope = 1, color = 'yellow', linewidth = 1.5) +
  xlab('SideFire Speed (MPH)') + ylab('NPMRDS Speed (MPH)') + 
  coord_cartesian(ylim = c(0, 85), xlim = c(0,85)) + 
  labs(title = 'PM Speed side-by-side comparison, SideFire vs. NPMRDS') +
  theme_black()
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/Sidefire_NPMRDS_SpdDiff_PMOP.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025,
       aes(x = as.numeric(pmopavgspd_sf), y = as.numeric(pmopavgspd_npm))) + 
  geom_point(color = '#B3E5FC', size = 2) +
  geom_abline(intercept = 0, slope = 1, color = 'yellow', linewidth = 1.5) +
  xlab('SideFire Speed (MPH)') + ylab('NPMRDS Speed (MPH)') + 
  coord_cartesian(ylim = c(0, 85), xlim = c(0,85)) + 
  labs(title = 'Night (9-11pm) Speed side-by-side comparison, SideFire vs. NPMRDS') +
  theme_black()
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/SideFire_NPMRDS/Sidefire_NPMRDS_SpdDiff_AMOP.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025,
       aes(x = as.numeric(amopavgspd_sf), y = as.numeric(amopavgspd_npm))) + 
  geom_point(color = '#B3E5FC', size = 2) +
  geom_abline(intercept = 0, slope = 1, color = 'yellow', linewidth = 1.5) +
  xlab('SideFire Speed (MPH)') + ylab('NPMRDS Speed (MPH)') + 
  coord_cartesian(ylim = c(0, 85), xlim = c(0,85)) + 
  labs(title = 'Morning (3-5am) Speed side-by-side comparison, SideFire vs. NPMRDS') +
  theme_black()
dev.off()


#################################### plot VC ratio vs Speed by data source ########################################
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Spd, PK by data source sf.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025_plot[which(sf_2022_npmrds_2025_plot$time != 'amop' &
                                        sf_2022_npmrds_2025_plot$time != 'pmop' &
                                        sf_2022_npmrds_2025_plot$source == 'sf'),],
       aes(x = as.numeric(avgvol)/as.numeric(hrcap), y = as.numeric(avgspd))) + geom_point(color = '#B3E5FC', size = 2) +
  xlab('VC ratio') + ylab('Speed (MPH)') + coord_cartesian(ylim = c(0, 85)) + 
  labs(title = 'PK VC ratio vs Speed, from SideFire') +
  theme_black()
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Spd, PK by data source npm.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025_plot[which(sf_2022_npmrds_2025_plot$time != 'amop' &
                                        sf_2022_npmrds_2025_plot$time != 'pmop' &
                                        sf_2022_npmrds_2025_plot$source == 'npm'),],
       aes(x = as.numeric(avgvol)/as.numeric(hrcap), y = as.numeric(avgspd))) + geom_point(color = '#B3E5FC', size = 2) +
  xlab('VC ratio') + ylab('Speed (MPH)') + coord_cartesian(ylim = c(0, 85)) + 
  labs(title = 'PK VC ratio vs Speed, from NPMRDS') +
  theme_black()
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Spd, OP by data source sf.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025_plot[which((sf_2022_npmrds_2025_plot$time == 'amop' |
                                        sf_2022_npmrds_2025_plot$time == 'pmop') &
                                        sf_2022_npmrds_2025_plot$source == 'sf'),],
       aes(x = as.numeric(avgvol)/as.numeric(hrcap), y = as.numeric(avgspd))) + geom_point(color = '#B3E5FC', size = 2) +
  xlab('VC ratio') + ylab('Speed (MPH)') + coord_cartesian(ylim = c(0, 85)) + 
  labs(title = 'OP VC ratio vs Speed, from SideFire') +
  theme_black()   

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Spd, OP by data source npm.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025_plot[which((sf_2022_npmrds_2025_plot$time == 'amop' |
                                         sf_2022_npmrds_2025_plot$time == 'pmop') &
                                        sf_2022_npmrds_2025_plot$source == 'npm'),],
       aes(x = as.numeric(avgvol)/as.numeric(hrcap), y = as.numeric(avgspd))) + geom_point(color = '#B3E5FC', size = 2) +
  xlab('VC ratio') + ylab('Speed (MPH)') + coord_cartesian(ylim = c(0, 85)) + 
  labs(title = 'OP VC ratio vs Speed, from NMPRDS') +
  theme_black()   
dev.off()





rm(npm_am, npm_pm, npm_op, sf_am, sf_pm, sf_op, row_to_delete, b, i)


################################ define a high contrast plot style, use this function in ggplot ################################
# Define your custom black background theme # 👈 Use your custom theme here
theme_black <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "black", color = NA),
      plot.background = element_rect(fill = "black", color = NA),
      panel.grid.major = element_line(color = "gray30"),
      panel.grid.minor = element_line(color = "gray20"),
      text = element_text(color = "white"),
      axis.line = element_line(color = "white", size = 1),  # Add axis lines
      axis.ticks = element_line(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      legend.background = element_rect(fill = "black"),
      legend.key = element_rect(fill = "black"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white")
    )
}


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


