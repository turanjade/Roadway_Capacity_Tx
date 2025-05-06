## new code created on May 6, 2025
## identify congested rdwy segments using sidefire data and npmrds

setwd("C:/Users/rtu/OneDrive - The North Central Texas Council of Governments/Documents/0_ModelDataDevelopment/")

library(ggplot2)

########### filter out records in vol_per_day_2022_raw of which sensor ID matches #########
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

# first select locations (key: TMC) where average speed is lower than a certain threshold --> based on HCM
npmrds_feb2024 = data.frame(read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\NPMDRS_Feb_2024_Workday_AvgSpeed.csv', header = T))
# use TMC to match, each row represents one TMC, or locations
# specify rows in sidefire vol per day table for am, pm, op
# 03-05, spdcol: 125-132, volcol: 32-36 # note: since vol is hourly basis, to calculate average volume from 3-5, we select 3, 3.15, 3.30, 3.45, 4, that's it
# 07-09, spdcol: 141-148, volcol: 48-52
# 16-18, spdcol: 177-184, volcol: 84-88
# 21-23, spdcol: 197-204, volcol: 104-108

amvolcol = seq(48, 52); amspdcol = seq(141, 148)
pmvolcol = seq(84, 88); pmspdcol = seq(177, 184)
opvolcol = c(seq(32, 36), seq(104, 108)); opspdcol = c(seq(125, 132), seq(197, 204))

# because one TMC can be associated with several sidefires, match npmrds with sidefire
sidefire_linkmatch_frwy_2025$npmrds_tmc = 0

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

# create a matrix that stores both npmrds, sidefire, and rdwy info in pk & op, including: lane, ffspd, cap, avgspd_npm, avgspd_sf, avgvol_sf
sf_2022_npmrds_2025 = data.frame(cbind(sidefire_linkmatch_frwy_2025$sf_id, sidefire_linkmatch_frwy_2025$rdwy_id, 
                            sidefire_linkmatch_frwy_2025$TMC, sidefire_linkmatch_frwy_2025$npmrds_tmc,
                            sidefire_linkmatch_frwy_2025$weavetype, 
                            sidefire_linkmatch_frwy_2025$amffspd, sidefire_linkmatch_frwy_2025$pmffspd, sidefire_linkmatch_frwy_2025$opffspd,
                            sidefire_linkmatch_frwy_2025$amlane, sidefire_linkmatch_frwy_2025$pmlane, sidefire_linkmatch_frwy_2025$oplane,
                            sidefire_linkmatch_frwy_2025$amhrcap, sidefire_linkmatch_frwy_2025$pmhrcap, sidefire_linkmatch_frwy_2025$ophrcap))

sf_2022_npmrds_2025$amavgspd_sf = 0; sf_2022_npmrds_2025$pmavgspd_sf = 0; sf_2022_npmrds_2025$opavgspd_sf = 0
sf_2022_npmrds_2025$amavgspd_npm = 0; sf_2022_npmrds_2025$pmavgspd_npm = 0; sf_2022_npmrds_2025$opavgspd_npm = 0
sf_2022_npmrds_2025$amavgvol_sf = 0; sf_2022_npmrds_2025$pmavgvol_sf = 0; sf_2022_npmrds_2025$opavgvol_sf = 0


colnames(sf_2022_npmrds_2025) = c('sf_id','rdwy_id','TMC_sf', 'TMC_npm', 
                                  'weavetype','amffspd','pmffspd','opffspd',
                                  'amlane','pmlane','oplane','amhrcap','pmhrcap','ophrcap',
                                  'amavgspd_sf','pmavgspd_sf','opavgspd_sf','amavgspd_npm','pmavgspd_npm','opavgspd_npm',
                                  'amavgvol_sf','pmavgvol_sf','opavgvol_sf')


# delete rows where TMC of npm & sf does not match (npmrds_tmc == 0)
sf_2022_npmrds_2025 = sf_2022_npmrds_2025[which(sf_2022_npmrds_2025$TMC_npm != 0),] # in total 1207 can be matched

row_to_delete = array(0, dim = 0) # count how many sf_id in the sidefire_2025 cannot be matched with vol per day 2022
for (i in 1:nrow(sf_2022_npmrds_2025)) {
  # first find if vol per day has data or not
  link_i = vol_per_day_2022_raw_linkmatch[which(vol_per_day_2022_raw_linkmatch$LinkID == sf_2022_npmrds_2025$sf_id[i]),]
  if (nrow(link_i) == 0) {
    row_to_delete = c(row_to_delete, i)
    print(paste('SF detector', sf_2022_npmrds_2025$sf_id[i], 'not found in vol per day 2022 raw'))
    next
  }
  
  # get all am data from vol per day
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
  
  # get all op data from vol per day
  volarray_op = allNA_returnNA(t(link_i[,opvolcol])) 
  spdarray_op = allNA_returnNA(t(link_i[,opspdcol])) 
  if (all(is.na(c(volarray_op))) | all(is.na(spdarray_op))) {
    print(paste('All OP vol & spd are NA, sensor ID', sf_2022_npmrds_2025$sf_id[i]))
  } else {
    sf_2022_npmrds_2025$opavgspd_sf[i] = mean(spdarray_op, na.rm = T)
    sf_2022_npmrds_2025$opavgvol_sf[i] = mean(volarray_op, na.rm = T)
  }
  
  # finally, match avg speed from npm
  sf_2022_npmrds_2025$amavgspd_npm[i] = npmrds_feb2024$Avg_Speed_07_09[which(npmrds_feb2024$TMC == sf_2022_npmrds_2025$TMC_npm[i])]
  sf_2022_npmrds_2025$pmavgspd_npm[i] = npmrds_feb2024$Avg_Speed_16_18[which(npmrds_feb2024$TMC == sf_2022_npmrds_2025$TMC_npm[i])]
  sf_2022_npmrds_2025$opavgspd_npm[i] = mean(as.numeric(npmrds_feb2024$Avg_Speed_03_05[which(npmrds_feb2024$TMC == sf_2022_npmrds_2025$TMC_npm[i])]),
                                             as.numeric(npmrds_feb2024$Avg_Speed_21_23[which(npmrds_feb2024$TMC == sf_2022_npmrds_2025$TMC_npm[i])]),
                                             na.rm = T)
}
# delete rows that do not find matched vol spd
# sf_2022_npmrds_2025 = sf_2022_npmrds_2025[-row_to_delete,] # run every time when sf_npmrds matching performs

# clean environment 
rm(amspdcol,amvolcol, pmspdcol,pmvolcol, opspdcol,opvolcol, row_to_delete, 
   volarray_am, volarray_pm, volarray_op, spdarray_am, spdarray_pm, spdarray_op, link_i)

# compare npm & sf spd side-by-side 
sf_2022_npmrds_2025$amspddiff = sf_2022_npmrds_2025$amavgspd_sf - sf_2022_npmrds_2025$amavgspd_npm
hist(sf_2022_npmrds_2025$amspddiff)

sf_2022_npmrds_2025$pmspddiff = sf_2022_npmrds_2025$pmavgspd_sf - sf_2022_npmrds_2025$pmavgspd_npm
hist(sf_2022_npmrds_2025$pmspddiff)

sf_2022_npmrds_2025$opspddiff = sf_2022_npmrds_2025$opavgspd_sf - sf_2022_npmrds_2025$opavgspd_npm
hist(sf_2022_npmrds_2025$opspddiff)

for (i in 6:26) {
  sf_2022_npmrds_2025[,i] = as.numeric(sf_2022_npmrds_2025[,i])
}

# delete extremely large vol
row_to_delete = which(sf_2022_npmrds_2025$amavgvol_sf/sf_2022_npmrds_2025$amlane > 2500 |
                        sf_2022_npmrds_2025$pmavgvol_sf/sf_2022_npmrds_2025$pmlane > 2500 |
                        sf_2022_npmrds_2025$opavgvol_sf/sf_2022_npmrds_2025$oplane > 2500)
sf_2022_npmrds_2025 = sf_2022_npmrds_2025[-row_to_delete,]

# export sf_2022_npmrds_2025 & map in ArcGIS
write.csv(sf_2022_npmrds_2025, '20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\sf_2022_npmrds_2024.csv', row.names = F)

# re-organize sf_2022_npmrds_2025 as a dataframe for ggplot
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
  
  sf_op = c('sf','op',
            sf_2022_npmrds_2025$oplane[i], sf_2022_npmrds_2025$opffspd[i], sf_2022_npmrds_2025$ophrcap[i],
            sf_2022_npmrds_2025$opavgspd_sf[i], sf_2022_npmrds_2025$opavgvol_sf[i])
  npm_op = c('npm','op',
             sf_2022_npmrds_2025$oplane[i], sf_2022_npmrds_2025$opffspd[i], sf_2022_npmrds_2025$ophrcap[i],
             sf_2022_npmrds_2025$opavgspd_npm[i], sf_2022_npmrds_2025$opavgvol_sf[i])
  
  sf_2022_npmrds_2025_plot = rbind(sf_2022_npmrds_2025_plot,
                                   cbind(sf_2022_npmrds_2025$sf_id[i], sf_2022_npmrds_2025$rdwy_id[i], 
                                         sf_2022_npmrds_2025$TMC_sf[i], sf_2022_npmrds_2025$weavetype[i],
                                         rbind(sf_am, npm_am, sf_pm, npm_pm, sf_op, npm_op)))
}
colnames(sf_2022_npmrds_2025_plot) = c('sf_id', 'rdwy_id', 'TMC','weavetype', 'source','time','lane','ffspd','hrcap','avgspd','avgvol')
sf_2022_npmrds_2025_plot = data.frame(sf_2022_npmrds_2025_plot)
for (i in 7:11) {
  sf_2022_npmrds_2025_plot[,i] = as.numeric(sf_2022_npmrds_2025_plot[,i])
}


# plot
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Spd, PK by data source.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025_plot[which(sf_2022_npmrds_2025_plot$time != 'op'),],
       aes(x = as.numeric(avgvol)/as.numeric(hrcap), y = as.numeric(avgspd), color = factor(source))) + geom_point() +
  xlab('VC ratio') + ylab('Speed (MPH)') + 
  labs(title = 'PK VC ratio vs Speed, by data source') +
  scale_color_discrete(name = "Data source", labels = c('NPMRDS','Sidefire')) +  
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Spd, OP by data source.png", 
    width = 800, height = 600)
ggplot(sf_2022_npmrds_2025_plot[which(sf_2022_npmrds_2025_plot$time == 'op'),],
       aes(x = as.numeric(avgvol)/as.numeric(hrcap), y = as.numeric(avgspd), color = factor(source))) + geom_point() +
  xlab('VC ratio') + ylab('Speed (MPH)') + 
  labs(title = 'OP VC ratio vs Speed, by data source') +
  scale_color_discrete(name = "Data source", labels = c('NPMRDS','Sidefire')) +  
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

rm(npm_am, npm_pm, npm_op, sf_am, sf_pm, sf_op, row_to_delete, b, i)

hist(sf_2022_npmrds_2025$amspddiff, xlab = 'Speed Diff', main = 'Speed Diff (Sidefire-NPMRDS), AM') # Sidefire_NPMRDS_SpdDiff_AM.png
hist(sf_2022_npmrds_2025$pmspddiff, xlab = 'Speed Diff', main = 'Speed Diff (Sidefire-NPMRDS), PM')
hist(sf_2022_npmrds_2025$opspddiff, xlab = 'Speed Diff', main = 'Speed Diff (Sidefire-NPMRDS), OP')

## conclusion: use NPMRDS of the corresponding link as the speed, in this case, we can only select average speed
# delete those spd > ffspd
sf_2022_npmrds_2025 = sf_2022_npmrds_2025[which(sf_2022_npmrds_2025$amffspd - sf_2022_npmrds_2025$amavgspd_npm > 0 |
                                                  sf_2022_npmrds_2025$pmffspd - sf_2022_npmrds_2025$pmavgspd_npm > 0 |
                                                  sf_2022_npmrds_2025$opffspd - sf_2022_npmrds_2025$opavgspd_npm > 0),]

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



