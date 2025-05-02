## updates in v2: 
## use sidefire_linkmatch_frwy_2025 to match count records instead of sidefire_inTAZ_2025

setwd("C:/Users/rtu/OneDrive - The North Central Texas Council of Governments/Documents/0_ModelDataDevelopment/")

##################################### load packages ######################################

# use leaflet
library('arcgisbinding')
library('leaflet')
library('sf')
library('dplyr')
library(RColorBrewer)
library(ggplot2)
library(htmlwidgets)
library('lwgeom')


########################################### data filter + weave type match #############################################
########################################### data filter + weave type match #############################################
########################################### data filter + weave type match #############################################
########################################### data filter + weave type match #############################################
# for AM
# delete records if vol per lane > 2500
sidefire_vol_spd_2022_am_filter = sidefire_vol_spd_2022_am[which(sidefire_vol_spd_2022_am$volboxupper/sidefire_vol_spd_2022_am$amlane < 2500),]
sidefire_vol_spd_2022_am_filter$weavetype = 0
for (i in 1:nrow(sidefire_vol_spd_2022_am_filter)) {
  sidefire_vol_spd_2022_am_filter$weavetype[i] = roadlink_2026$WEAVE_T[which(roadlink_2026$ID == sidefire_vol_spd_2022_am_filter$ID_Road[i])]
}

# for PM
# delete records if vol per lane > 2500
sidefire_vol_spd_2022_pm_filter = sidefire_vol_spd_2022_pm[which(sidefire_vol_spd_2022_pm$volboxupper/sidefire_vol_spd_2022_pm$pmlane < 2500),]
sidefire_vol_spd_2022_pm_filter$weavetype = 0
for (i in 1:nrow(sidefire_vol_spd_2022_pm_filter)) {
  sidefire_vol_spd_2022_pm_filter$weavetype[i] = roadlink_2026$WEAVE_T[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm_filter$ID_Road[i])]
}

# for OP
# delete records if vol per lane > 2500
sidefire_vol_spd_2022_op_filter = sidefire_vol_spd_2022_op[which(sidefire_vol_spd_2022_op$volboxupper/sidefire_vol_spd_2022_op$oplane < 2500),]
sidefire_vol_spd_2022_op_filter$weavetype = 0
for (i in 1:nrow(sidefire_vol_spd_2022_op_filter)) {
  sidefire_vol_spd_2022_op_filter$weavetype[i] = roadlink_2026$WEAVE_T[which(roadlink_2026$ID == sidefire_vol_spd_2022_op_filter$ID_Road[i])]
}



########################################### MFD for FUNCL 1, op, pm, and am #############################################
########################################### MFD for FUNCL 1, op, pm, and am #############################################
########################################### MFD for FUNCL 1, op, pm, and am #############################################
########################################### MFD for FUNCL 1, op, pm, and am #############################################
# combine op, pm, and am
sidefire_vol_spd_alltime = rbind(cbind(sidefire_vol_spd_2022_am_filter$ID_detector, sidefire_vol_spd_2022_am_filter$LinkName_detector,
                                       sidefire_vol_spd_2022_am_filter$ID_Road, sidefire_vol_spd_2022_am_filter$LinkName_Road, 
                                       'AM', sidefire_vol_spd_2022_am_filter$weavetype,
                                       sidefire_vol_spd_2022_am_filter$amlane, 
                                       sidefire_vol_spd_2022_am_filter$volboxupper, sidefire_vol_spd_2022_am_filter$spdboxupper),
                                 
                                 cbind(sidefire_vol_spd_2022_pm_filter$ID_detector, sidefire_vol_spd_2022_pm_filter$LinkName_detector,
                                       sidefire_vol_spd_2022_pm_filter$ID_Road, sidefire_vol_spd_2022_pm_filter$LinkName_Road, 
                                       'PM', sidefire_vol_spd_2022_pm_filter$weavetype,
                                       sidefire_vol_spd_2022_pm_filter$pmlane, 
                                       sidefire_vol_spd_2022_pm_filter$volboxupper, sidefire_vol_spd_2022_pm_filter$spdboxupper),
                                 
                                 cbind(sidefire_vol_spd_2022_op_filter$ID_detector, sidefire_vol_spd_2022_op_filter$LinkName_detector,
                                       sidefire_vol_spd_2022_op_filter$ID_Road, sidefire_vol_spd_2022_op_filter$LinkName_Road, 
                                       'OP', sidefire_vol_spd_2022_op_filter$weavetype,
                                       sidefire_vol_spd_2022_op_filter$oplane, 
                                       sidefire_vol_spd_2022_op_filter$volboxupper, sidefire_vol_spd_2022_op_filter$spdboxupper))

colnames(sidefire_vol_spd_alltime) = c('ID_detector','LinkName_detector','ID_Road','LinkName_Road', 'Time', 'weavetype', 'lane', 'vol_max','spd_at_max')
sidefire_vol_spd_alltime = data.frame(sidefire_vol_spd_alltime)

for (i in 7:9) {
  sidefire_vol_spd_alltime[,i] = as.numeric(sidefire_vol_spd_alltime[,i])
}

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/Vol vs Spd, PK at weave type FRWY_BASIC.png", 
    width = 800, height = 600)
ggplot(sidefire_vol_spd_alltime[which(sidefire_vol_spd_alltime$weavetype == 'FRWY_BASIC' & 
                                        sidefire_vol_spd_alltime$Time != 'OP'),],
       aes(x = vol_max/lane, y = spd_at_max)) + geom_point(color = 'red') +
  xlab('Max volume per lane per hour') + ylab('Speed at max vol (MPH)') + labs(title = 'Vol vs Spd on FRWY_BASIC') +
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))

dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/Vol vs Spd, PK at weave type BASIC.png", 
    width = 800, height = 600)
ggplot(sidefire_vol_spd_alltime[which(sidefire_vol_spd_alltime$weavetype == 'BASIC' & 
                                        sidefire_vol_spd_alltime$Time != 'OP'),],
       aes(x = vol_max/lane, y = spd_at_max)) + geom_point(color = 'red') +
  xlab('Max volume per lane per hour') + ylab('Speed at max vol (MPH)') + labs(title = 'Vol vs Spd on BASIC') +
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/Vol vs Spd, PK at weave type A.png", 
    width = 800, height = 600)
ggplot(sidefire_vol_spd_alltime[which(sidefire_vol_spd_alltime$weavetype == 'A' & 
                                        sidefire_vol_spd_alltime$Time != 'OP'),],
       aes(x = vol_max/lane, y = spd_at_max)) + geom_point(color = 'red') +
  xlab('Max volume per lane per hour') + ylab('Speed at max vol (MPH)') + labs(title = 'Vol vs Spd on Weave type A') +
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/Vol vs Spd, PK at weave type MD.png", 
    width = 800, height = 600)
ggplot(sidefire_vol_spd_alltime[which(sidefire_vol_spd_alltime$weavetype == 'MD' & 
                                        sidefire_vol_spd_alltime$Time != 'OP'),],
       aes(x = vol_max/lane, y = spd_at_max)) + geom_point(color = 'red') +
  xlab('Max volume per lane per hour') + ylab('Speed at max vol (MPH)') + labs(title = 'Vol vs Spd on Weave type MD') +
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

# no need to plot
# weave type with less data
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/MFD, PK at weave type B.png", 
    width = 800, height = 600)
ggplot(sidefire_vol_spd_alltime[which(sidefire_vol_spd_alltime$weavetype == 'B' & 
                                        sidefire_vol_spd_alltime$Time != 'OP'),],
       aes(x = vol_max/lane, y = spd_at_max)) + geom_point(color = 'red') +
  xlab('Max volume per lane per hour') + ylab('Speed at max vol (MPH)') + labs(title = 'Vol vs Spd on Weave type B') +
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/MFD, PK at weave type C.png", 
    width = 800, height = 600)
ggplot(sidefire_vol_spd_alltime[which(sidefire_vol_spd_alltime$weavetype == 'C' & 
                                        sidefire_vol_spd_alltime$Time != 'OP'),],
       aes(x = vol_max/lane, y = spd_at_max)) + geom_point(color = 'red') +
  xlab('Max volume per lane per hour') + ylab('Speed at max vol (MPH)') + labs(title = 'Vol vs Spd on Weave type C') +
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/MFD, PK at weave type LBE.png", 
    width = 800, height = 600)
ggplot(sidefire_vol_spd_alltime[which(sidefire_vol_spd_alltime$weavetype == 'LBE' & 
                                        sidefire_vol_spd_alltime$Time != 'OP'),],
       aes(x = vol_max/lane, y = spd_at_max)) + geom_point(color = 'red') +
  xlab('Max volume per lane per hour') + ylab('Speed at max vol (MPH)') + labs(title = 'Vol vs Spd on Weave type LBE') +
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

########################################### Compare theoretical cap with 90% or 95% of boxupper for each weave type, save to csv #############################################
########################################### Compare theoretical cap with 90% or 95% of boxupper for each weave type, save to csv #############################################
########################################### Compare theoretical cap with 90% or 95% of boxupper for each weave type, save to csv #############################################
########################################### Compare theoretical cap with 90% or 95% of boxupper for each weave type, save to csv #############################################
# match capacity per lane to each link, needs to add to initial check
sidefire_vol_spd_alltime$capperlane = 0
for (i in 1:nrow(sidefire_vol_spd_alltime)) {
  if (sidefire_vol_spd_alltime$Time[i] == 'AM') {
    sidefire_vol_spd_alltime$capperlane[i] = max(roadlink_2026$AMHRCAP_A[which(roadlink_2026$ID == sidefire_vol_spd_alltime$ID_Road[i])],
                                                 roadlink_2026$AMHRCAP_B[which(roadlink_2026$ID == sidefire_vol_spd_alltime$ID_Road[i])])/sidefire_vol_spd_alltime$lane[i]
      
  } 
  else if (sidefire_vol_spd_alltime$Time[i] == 'PM') {
    sidefire_vol_spd_alltime$capperlane[i] = max(roadlink_2026$PMHRCAP_A[which(roadlink_2026$ID == sidefire_vol_spd_alltime$ID_Road[i])],
                                                 roadlink_2026$PMHRCAP_A[which(roadlink_2026$ID == sidefire_vol_spd_alltime$ID_Road[i])])/sidefire_vol_spd_alltime$lane[i]
  }
  else if (sidefire_vol_spd_alltime$Time[i] == 'OP') {
    sidefire_vol_spd_alltime$capperlane[i] = max(roadlink_2026$OPHRCAP_A[which(roadlink_2026$ID == sidefire_vol_spd_alltime$ID_Road[i])],
                                                 roadlink_2026$OPHRCAP_A[which(roadlink_2026$ID == sidefire_vol_spd_alltime$ID_Road[i])])/sidefire_vol_spd_alltime$lane[i]
  }
}


# calculate max 90% of max for each weave type, compare with theoretical capacity (AM, PM, OP separately)
library('ggplot2')
weavetype = unique(sidefire_vol_spd_alltime$weavetype)
timeperiod = c("AM", "PM", "OP")
# calculate the 95% highest volume in AM and its corresponding capacity for six weave types
vol_cap_byweave_time = matrix(0, nrow = 0, ncol = 5)
sidefire_vol_spd_alltime$volperlane = sidefire_vol_spd_alltime$vol_max/sidefire_vol_spd_alltime$lane
for (i in 1:3) {
  for (j in 1:length(weavetype)) {
    row_ij = sidefire_vol_spd_alltime[which(sidefire_vol_spd_alltime$weavetype == weavetype[j] & 
                                              sidefire_vol_spd_alltime$Time == timeperiod[i]),]
    vol_ij = quantile(row_ij$volperlane, 0.95)
    spd_ij = row_ij$spd_at_max[getIndex(row_ij$volperlane, vol_ij)]
    cap_ij = row_ij$capperlane[getIndex(row_ij$volperlane, vol_ij)]
    vol_cap_byweave_time = rbind(vol_cap_byweave_time, c(timeperiod[i], weavetype[j], vol_ij, spd_ij, cap_ij))
  }
}
colnames(vol_cap_byweave_time) = c('time','weavetype','pct95_max_vol','spd','cap')
vol_cap_byweave_time = data.frame(vol_cap_byweave_time)

write.csv(vol_cap_byweave_time, 'vol_cap_byweave_time.csv', row.names = F)


########################################### v/c & speed at different weave type, typical workday April ##############################################
########################################### v/c & speed at different weave type, typical workday April  #############################################
########################################### v/c & speed at different weave type, typical workday April  #############################################
########################################### v/c & speed at different weave type, typical workday April  #############################################

# AM, count col 46-55, spd col 139-148
amvolcol = seq(46,55); amspdcol = seq(139, 148)
# PM, count col 80-94, spd col 173-187
pmvolcol = seq(80,94); pmspdcol = seq(173, 187)
# OP, volcol = c(seq(20,45),seq(56,79),seq(95,112)); spdcol = c(seq(113,138),seq(149,172),seq(188,205))
opvolcol = c(seq(20,45),seq(56,79),seq(95,112)); opspdcol = c(seq(113,138),seq(149,172),seq(188,205))

link_id = unique(vol_per_day_2022_04$LinkID) 
timeperiod = c('AM', 'PM', 'OP')

# create a dataframe to plot v/c & speed diagram
vol_cap_spd_frwy_2022_plot = matrix(0, nrow = 0, ncol = 8)

for (i in 1:length(link_id)) {
  id = link_id[i]
  vol_cap_spd_frwy_2022_plot = rbind(vol_cap_spd_frwy_2022_plot,
                                     cbind(id,                                                                                       # link ID
                                           unique(vol_per_day_2022_04$weavetype[which(vol_per_day_2022_04$LinkID == id)]),           # weaving type 
                                           
                                           rbind(cbind('AM', unique(vol_per_day_2022_04$amlane[which(vol_per_day_2022_04$LinkID == id)]),    # lane @ am
                                                       unique(vol_per_day_2022_04$amffspd[which(vol_per_day_2022_04$LinkID == id)]),         # ffspd @ am
                                                       unique(vol_per_day_2022_04$amhrcap[which(vol_per_day_2022_04$LinkID == id)]),         # hrcap @ am
                                                       as.vector(t(vol_per_day_2022_04[which(vol_per_day_2022_04$LinkID == id),amvolcol])),  # all hrly vol @ am 
                                                       as.vector(t(vol_per_day_2022_04[which(vol_per_day_2022_04$LinkID == id),amspdcol]))), # all hrly spd @ am
                                                 
                                                 # same comments for the following PM and OP
                                                 cbind('PM', unique(vol_per_day_2022_04$pmlane[which(vol_per_day_2022_04$LinkID == id)]), 
                                                       unique(vol_per_day_2022_04$pmffspd[which(vol_per_day_2022_04$LinkID == id)]), 
                                                       unique(vol_per_day_2022_04$pmhrcap[which(vol_per_day_2022_04$LinkID == id)]), 
                                                       as.vector(t(vol_per_day_2022_04[which(vol_per_day_2022_04$LinkID == id),pmvolcol])), 
                                                       as.vector(t(vol_per_day_2022_04[which(vol_per_day_2022_04$LinkID == id),pmspdcol]))),
                                                 
                                                 cbind('OP', unique(vol_per_day_2022_04$oplane[which(vol_per_day_2022_04$LinkID == id)]), 
                                                       unique(vol_per_day_2022_04$opffspd[which(vol_per_day_2022_04$LinkID == id)]), 
                                                       unique(vol_per_day_2022_04$ophrcap[which(vol_per_day_2022_04$LinkID == id)]), 
                                                       as.vector(t(vol_per_day_2022_04[which(vol_per_day_2022_04$LinkID == id),opvolcol])), 
                                                       as.vector(t(vol_per_day_2022_04[which(vol_per_day_2022_04$LinkID == id),opspdcol]))))))
}

# convert to numeric
for (i in 4:8) {
  vol_cap_spd_frwy_2022_plot[,i] = as.numeric(vol_cap_spd_frwy_2022_plot[,i])
}

# convert to df for plotting purposes
vol_cap_spd_frwy_2022_plot = data.frame(vol_cap_spd_frwy_2022_plot)
colnames(vol_cap_spd_frwy_2022_plot) = c('DetectorID','weavetype','time','lane','ffspd','cap','vol','spd')

# calculate per lane vol & cap
vol_cap_spd_frwy_2022_plot$capperlane = as.numeric(vol_cap_spd_frwy_2022_plot$cap)/as.numeric(vol_cap_spd_frwy_2022_plot$lane)
vol_cap_spd_frwy_2022_plot$volperlane = as.numeric(vol_cap_spd_frwy_2022_plot$vol)/as.numeric(vol_cap_spd_frwy_2022_plot$lane)

# delete NA
vol_cap_spd_frwy_2022_plot = vol_cap_spd_frwy_2022_plot[!is.na(vol_cap_spd_frwy_2022_plot$spd),]
# vol_cap_spd_frwy_2022_plot = vol_cap_spd_frwy_2022_plot[!is.na(vol_cap_spd_frwy_2022_plot$vol),]
# vol_cap_spd_frwy_2022_plot = vol_cap_spd_frwy_2022_plot[!is.na(vol_cap_spd_frwy_2022_plot$ffspd),]

# determine if the record is ff or not. If ff, spddiff should <= 0
vol_cap_spd_frwy_2022_plot$spddiff = as.numeric(vol_cap_spd_frwy_2022_plot$ffspd) - as.numeric(vol_cap_spd_frwy_2022_plot$spd)

## plot frwy_basic 
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Speed by lane, FRWY_BASIC.png", 
    width = 800, height = 600)
ggplot(vol_cap_spd_frwy_2022_plot[which(vol_cap_spd_frwy_2022_plot$weavetype == 'FRWY_BASIC' & 
                                          vol_cap_spd_frwy_2022_plot$time != 'OP',
                                          vol_cap_spd_frwy_2022_plot$spddiff > 0),], 
       aes(x = as.numeric(volperlane)/as.numeric(capperlane), y = as.numeric(spd), color = factor(lane))) + geom_point() +
  xlab('VC ratio') + ylab('Speed (MPH)') + 
  labs(title = 'PK VC ratio vs Speed, weave type FRWY_BASIC, by lane number', color = 'Lane #') +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Speed by lane, BASIC.png", 
    width = 800, height = 600)
ggplot(vol_cap_spd_frwy_2022_plot[which(vol_cap_spd_frwy_2022_plot$weavetype == 'BASIC' & 
                                          vol_cap_spd_frwy_2022_plot$time != 'OP',
                                        vol_cap_spd_frwy_2022_plot$spddiff > 0),], 
       aes(x = as.numeric(volperlane)/as.numeric(capperlane), y = as.numeric(spd), color = factor(lane))) + geom_point() +
  xlab('VC ratio') + ylab('Speed (MPH)') + 
  labs(title = 'PK VC ratio vs Speed, weave type BASIC, by lane number', color = 'Lane #') +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Speed by lane, A.png", 
    width = 800, height = 600)
ggplot(vol_cap_spd_frwy_2022_plot[which(vol_cap_spd_frwy_2022_plot$weavetype == 'A' & 
                                          vol_cap_spd_frwy_2022_plot$time != 'OP',
                                        vol_cap_spd_frwy_2022_plot$spddiff > 0),], 
       aes(x = as.numeric(volperlane)/as.numeric(capperlane), y = as.numeric(spd), color = factor(lane))) + geom_point() +
  xlab('VC ratio') + ylab('Speed (MPH)') + 
  labs(title = 'PK VC ratio vs Speed, weave type A, by lane number', color = 'Lane #') +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Speed by lane, B.png", 
    width = 800, height = 600)
ggplot(vol_cap_spd_frwy_2022_plot[which(vol_cap_spd_frwy_2022_plot$weavetype == 'B' & 
                                          vol_cap_spd_frwy_2022_plot$time != 'OP',
                                        vol_cap_spd_frwy_2022_plot$spddiff > 0),], 
       aes(x = as.numeric(volperlane)/as.numeric(capperlane), y = as.numeric(spd), color = factor(lane))) + geom_point() +
  xlab('VC ratio') + ylab('Speed (MPH)') + 
  labs(title = 'PK VC ratio vs Speed, weave type B, by lane number', color = 'Lane #') +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Speed by lane, C.png", 
    width = 800, height = 600) # no C (?)
ggplot(vol_cap_spd_frwy_2022_plot[which(vol_cap_spd_frwy_2022_plot$weavetype == 'C' & 
                                          vol_cap_spd_frwy_2022_plot$time != 'OP',
                                        vol_cap_spd_frwy_2022_plot$spddiff > 0),], 
       aes(x = as.numeric(volperlane)/as.numeric(capperlane), y = as.numeric(spd), color = factor(lane))) + geom_point() +
  xlab('VC ratio') + ylab('Speed (MPH)') + 
  labs(title = 'PK VC ratio vs Speed, weave type C, by lane number', color = 'Lane #') +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Speed by lane, MD.png", 
    width = 800, height = 600)
ggplot(vol_cap_spd_frwy_2022_plot[which(vol_cap_spd_frwy_2022_plot$weavetype == 'MD' & 
                                          vol_cap_spd_frwy_2022_plot$time != 'OP',
                                        vol_cap_spd_frwy_2022_plot$spddiff > 0),], 
       aes(x = as.numeric(volperlane)/as.numeric(capperlane), y = as.numeric(spd), color = factor(lane))) + geom_point() +
  xlab('VC ratio') + ylab('Speed (MPH)') + 
  labs(title = 'PK VC ratio vs Speed, weave type MD, by lane number', color = 'Lane #') +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/VCratio vs Speed by lane, LBE.png", 
    width = 800, height = 600)
ggplot(vol_cap_spd_frwy_2022_plot[which(vol_cap_spd_frwy_2022_plot$weavetype == 'LBE' & 
                                          vol_cap_spd_frwy_2022_plot$time != 'OP',
                                        vol_cap_spd_frwy_2022_plot$spddiff > 0),], 
       aes(x = as.numeric(volperlane)/as.numeric(capperlane), y = as.numeric(spd), color = factor(lane))) + geom_point() +
  xlab('VC ratio') + ylab('Speed (MPH)') + 
  labs(title = 'PK VC ratio vs Speed, weave type LBE, by lane number', color = 'Lane #') +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),
        plot.title = element_text(face = 'bold', size = 18))
dev.off()

rm(amhrcap, amlane,amspd,amspdcol,amvol,amvolcol,i,id,link_id,ophrcap,oplane,opspd,opspdcol,opvol,opvolcol,pmlane,pmspd,pmspdcol,
   pmvol,pmvolcol,pmhrcap,weave,link_i, amffspd,opffspd,pmffspd,timeperiod,link_id)
########################################### map AM #############################################
########################################### map AM #############################################
########################################### map AM #############################################
########################################### map AM #############################################

# plot has bug - needs to join sidefire shape with sidefire vol spd
sidefire_vol_spd_2022_am_filter$ID_detector = as.numeric(sidefire_vol_spd_2022_am_filter$ID_detector)
sidefire_inTaz_2025_am = left_join(sidefire_inTaz_2025, sidefire_vol_spd_2022_am_filter,
                                    by = c('ID' = 'ID_detector'))
sidefire_inTaz_2025_am = sidefire_inTaz_2025_am[which(!is.na(sidefire_inTaz_2025_am$volmin)),]

sidefire_inTaz_2025_am$popup <- paste0(
  "<b>Sensor ID:</b> ", sidefire_inTaz_2025_am$ID, "<br>",
  "<b>Street Name:</b> ", sidefire_inTaz_2025_am$LINKNAME, "<br>",
  "<b>FUNCL:</b> ", sidefire_inTaz_2025_am$FUNCL, "<br>",
  "<b>Lane:</b> ", sidefire_inTaz_2025_am$AMlane, "<br>",
  "<b>Vol 95%:</b> ", round(sidefire_inTaz_2025_am$volboxupper/sidefire_inTaz_2025_am$AMlane,0), "<br>",
  "<b>Speed 95%:</b> ", sidefire_inTaz_2025_am$spdboxupper, "<br>")

# Define a color palette for volume
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_inTaz_2025_am$volboxupper/sidefire_inTaz_2025_am$AMlane, bins = 5)
mymap = 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025_am, crs = 4326), fillOpacity = 1,
                   color = ~pal(round(sidefire_inTaz_2025_am$volboxupper/sidefire_inTaz_2025_am$AMlane,0)), 
                   label = ~paste("Value:", round(sidefire_inTaz_2025_am$volboxupper/sidefire_inTaz_2025_am$AMlane,0)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_inTaz_2025_am$volboxupper/sidefire_inTaz_2025_am$AMlane,0),
    title = "AM, Max volume when speed < ff_speed", opacity = 1)
saveWidget(mymap, file = "20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/vol_am_boxupper.html", selfcontained = TRUE)
rm(mymap)
# Define a color palette for speed
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_inTaz_2025_am$spdboxupper, bins = 5)
mymap = 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025_am, crs = 4326), fillOpacity = 1,
                   color = ~pal(sidefire_inTaz_2025_am$spdboxupper), 
                   label = ~paste("Value:", round(sidefire_inTaz_2025_am$spdboxupper,2)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_inTaz_2025_am$spdboxupper,2),
            title = "AM, Max volume when speed < ff_speed", opacity = 1)
saveWidget(mymap, file = "20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/spd_am_boxupper.html", selfcontained = TRUE)
rm(mymap)

########################################### map PM #############################################
########################################### map PM #############################################
########################################### map PM #############################################
########################################### map PM #############################################
sidefire_vol_spd_2022_pm_filter$ID_detector = as.numeric(sidefire_vol_spd_2022_pm_filter$ID_detector)
sidefire_inTaz_2025_pm = left_join(sidefire_inTaz_2025, sidefire_vol_spd_2022_pm_filter,
                                   by = c('ID' = 'ID_detector'))
sidefire_inTaz_2025_pm = sidefire_inTaz_2025_pm[which(!is.na(sidefire_inTaz_2025_pm$volmin)),]

sidefire_inTaz_2025_pm$popup <- paste0(
  "<b>Sensor ID:</b> ", sidefire_inTaz_2025_pm$ID, "<br>",
  "<b>Street Name:</b> ", sidefire_inTaz_2025_pm$LINKNAME, "<br>",
  "<b>FUNCL:</b> ", sidefire_inTaz_2025_pm$FUNCL, "<br>",
  "<b>Lane:</b> ", sidefire_inTaz_2025_pm$PMlane, "<br>",
  "<b>Vol 95%:</b> ", round(sidefire_inTaz_2025_pm$volboxupper/sidefire_inTaz_2025_pm$PMlane,0), "<br>",
  "<b>Speed 95%:</b> ", sidefire_inTaz_2025_pm$spdboxupper, "<br>")

# Define a color palette for volume
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_inTaz_2025_pm$volboxupper/sidefire_inTaz_2025_pm$PMlane, bins = 5)
mymap = 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025_pm, crs = 4326), fillOpacity = 1,
                   color = ~pal(round(sidefire_inTaz_2025_pm$volboxupper/sidefire_inTaz_2025_pm$PMlane,0)), 
                   label = ~paste("Value:", round(sidefire_inTaz_2025_pm$volboxupper/sidefire_inTaz_2025_pm$PMlane,0)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_inTaz_2025_pm$volboxupper/sidefire_inTaz_2025_pm$PMlane,0),
            title = "PM, Max volume when speed < ff_speed", opacity = 1)
saveWidget(mymap, file = "20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/vol_pm_boxupper.html", selfcontained = TRUE)
rm(mymap)
# Define a color palette for speed
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_inTaz_2025_pm$spdboxupper, bins = 5)
mymap = 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025_pm, crs = 4326), fillOpacity = 1,
                   color = ~pal(sidefire_inTaz_2025_pm$spdboxupper), 
                   label = ~paste("Value:", round(sidefire_inTaz_2025_pm$spdboxupper,2)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_inTaz_2025_pm$spdboxupper,2),
            title = "PM, Max volume when speed < ff_speed", opacity = 1)
saveWidget(mymap, file = "20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/spd_pm_boxupper.html", selfcontained = TRUE)
rm(mymap)


########################################### box & map OP #############################################
########################################### box & map OP #############################################
########################################### box & map OP #############################################
########################################### box & map OP #############################################
sidefire_vol_spd_2022_op_filter$ID_detector = as.numeric(sidefire_vol_spd_2022_op_filter$ID_detector)
sidefire_inTaz_2025_op = left_join(sidefire_inTaz_2025, sidefire_vol_spd_2022_op_filter,
                                   by = c('ID' = 'ID_detector'))
sidefire_inTaz_2025_op = sidefire_inTaz_2025_op[which(!is.na(sidefire_inTaz_2025_op$volmin)),]

sidefire_inTaz_2025_op$popup <- paste0(
  "<b>Sensor ID:</b> ", sidefire_inTaz_2025_op$ID, "<br>",
  "<b>Street Name:</b> ", sidefire_inTaz_2025_op$LINKNAME, "<br>",
  "<b>FUNCL:</b> ", sidefire_inTaz_2025_op$FUNCL, "<br>",
  "<b>Lane:</b> ", sidefire_inTaz_2025_op$oplane, "<br>",
  "<b>Vol 95%:</b> ", round(sidefire_inTaz_2025_op$volboxupper/sidefire_inTaz_2025_op$oplane,0), "<br>",
  "<b>Speed 95%:</b> ", sidefire_inTaz_2025_op$spdboxupper, "<br>")

# Define a color palette for volume
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_inTaz_2025_op$volboxupper/sidefire_inTaz_2025_op$oplane, bins = 5)
mymap = 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025_op, crs = 4326), fillOpacity = 1,
                   color = ~pal(round(sidefire_inTaz_2025_op$volboxupper/sidefire_inTaz_2025_op$oplane,0)), 
                   label = ~paste("Value:", round(sidefire_inTaz_2025_op$volboxupper/sidefire_inTaz_2025_op$oplane,0)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_inTaz_2025_op$volboxupper/sidefire_inTaz_2025_op$oplane,0),
            title = "op, Max volume when speed < ff_speed", opacity = 1)
saveWidget(mymap, file = "20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/vol_op_boxupper.html", selfcontained = TRUE)
rm(mymap)
# Define a color palette for speed
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_inTaz_2025_op$spdboxupper, bins = 5)
mymap = 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025_op, crs = 4326), fillOpacity = 1,
                   color = ~pal(sidefire_inTaz_2025_op$spdboxupper), 
                   label = ~paste("Value:", round(sidefire_inTaz_2025_op$spdboxupper,2)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_inTaz_2025_op$spdboxupper,2),
            title = "op, Max volume when speed < ff_speed", opacity = 1)
saveWidget(mymap, file = "20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/spd_op_boxupper.html", selfcontained = TRUE)
rm(mymap)