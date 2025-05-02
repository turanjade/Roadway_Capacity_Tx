setwd("C:/Users/rtu/OneDrive - The North Central Texas Council of Governments/Documents/0_ModelDataDevelopment/")

# use leaflet
library('arcgisbinding')
library('leaflet')
library('sf')
library('dplyr')
library(RColorBrewer)
library(ggplot2)
library(htmlwidgets)
library('lwgeom')
library('openxlsx')

roadlink_2026$MaxLn = 0
for (i in 1:nrow(roadlink_2026)) {
  roadlink_2026$MaxLn[i] = max(roadlink_2026$AMLN_AB[i], roadlink_2026$AMLN_BA[i], roadlink_2026$PMLN_AB[i], roadlink_2026$PMLN_BA[i])
}

## **  names(sidefire2025)
# [1] "ID"         "LONGITUDE1" "LATITUDE1"  "DISTRICT"   "DETECTORNA" "LINKNAME"   "OLD_LINKNA" "RECORDS"    "LATITUDE_0" "LONGITUDE_" "DIR"       
# [12] "FUNCL"      "ROAD"       "FROMROAD"   "TOROAD"     "LINKID_201" "TMC"        "FROMSENSOR" "TOSENSORID" "TRUECOORDS" "FEETTOITEM" "DATETIMEAD"
# [23] "ININVENTOR" "INVENTORYI" "HOV_EXPRES" "LOCATIONCO" "COUNTYNAME" "ROADWAYID"  "IMPUTEDCOU" "DAILYMODEL" "DAILYWEEKD" "COUNT_00"   "COUNT_01"  
# [34] "COUNT_02"   "COUNT_03"   "COUNT_04"   "COUNT_05"   "COUNT_06"   "COUNT_07"   "COUNT_08"   "COUNT_09"   "COUNT_10"   "COUNT_11"   "COUNT_12"  
# [45] "COUNT_13"   "COUNT_14"   "COUNT_15"   "COUNT_16"   "COUNT_17"   "COUNT_18"   "COUNT_19"   "COUNT_20"   "COUNT_21"   "COUNT_22"   "COUNT_23"  
# [56] "LANES_REPO" "LINKID_202" "LON_NETWOR" "LAT_NETWOR" "LAF_ID"     "LOCATIONC1" "PRE2022"    "LINKID_203" "NEXT_ID"    "PREV_ID"    "CORRIDOR"  
# [67] "SEQUENCE"   "AADT_2022"  "LANES_2022" "REVIEW_202" "REVIEW_TOD" "REVIEW_AAD"

# TMC: transport management center (?)

# plot volume & speed
# (only select records on Tuesday, Wednesday, Thursday)
# summarize total # of days for each sidefire
# for each LinkID in vol_per_day_2022, in other words, ID in sidefire2025
detectorID = data.frame(unique(vol_per_day_2022$LinkID)); colnames(detectorID) = c('ID')
detectorID$recordday = 0
for (i in 1:nrow(detectorID)) {
  detectorID$recordday[i] = length(which(vol_per_day_2022$LinkID == detectorID$ID[i]))
}

plot(detectorID$recordday)

detectorID$recordtue = 0
detectorID$recordwed = 0
detectorID$recordthu = 0

for (i in 1:nrow(detectorID)) {
  detectorID$recordtue[i] = length(which(vol_per_day_2022$LinkID == detectorID$ID[i] &
                                           vol_per_day_2022$DOW == 3))
  detectorID$recordwed[i] = length(which(vol_per_day_2022$LinkID == detectorID$ID[i] &
                                           vol_per_day_2022$DOW == 4))
  detectorID$recordthu[i] = length(which(vol_per_day_2022$LinkID == detectorID$ID[i] &
                                           vol_per_day_2022$DOW == 5))
}

plot(detectorID$recordtue) # 3 sensors that have more than 53 records on tuesday -- ID same as those with more than 500 records in this year
points(detectorID$recordwed, col = 'red') # 3 sensors that have more than 53 records on wednesday -- ID same as those with more than 500 records in this year
points(detectorID$recordthu, col = 'blue') # 52 sensors that have more than 53 records on thursday

# Find duplicated date for each sensor, get reasons why there are duplicated records for each date
vol_per_day_2022$Date_merge = paste(vol_per_day_2022$Month, vol_per_day_2022$Day, sep='-')

# for each detector in detectorID, find duplicated date, if yes, label
detectorID$dupdate = 0
for (i in 1:nrow(detectorID)) {
  date_i = vol_per_day_2022$Date_merge[which(vol_per_day_2022$LinkID == detectorID$ID[i])]
  detectorID$dupdate[i] = sum(table(date_i) > 1) # check how many dates are duplicated
}

# for each detector, delete duplicated date
vol_per_day_2022_deldupdate = data.frame(matrix(0, nrow = 0, ncol = ncol(vol_per_day_2022)))
for (i in 1:nrow(detectorID)) {
  vol_i = vol_per_day_2022[which(vol_per_day_2022$LinkID == detectorID$ID[i]),]
  vol_i = vol_i[!vol_i$Date_merge %in% vol_i$Date_merge[duplicated(vol_i$Date_merge)],]
  vol_per_day_2022_deldupdate = rbind(vol_per_day_2022_deldupdate, vol_i)
}

# select weekday, Tuesday, Thursday & Wednesday
vol_per_day_2022_workday = vol_per_day_2022_deldupdate
vol_per_day_2022_workday = vol_per_day_2022_workday[which(vol_per_day_2022_workday$DOW == 3 | 
                                                            vol_per_day_2022_workday$DOW == 4 | 
                                                            vol_per_day_2022_workday$DOW == 5),]

# All the selected workdays have rows that only 1 records per lane -- how? -- what is the minimum records per lane that can be used?
plot(table(vol_per_day_2022_workday$Records_Per_Lane))
length(unique(vol_per_day_2022_workday$LinkID[which(vol_per_day_2022_workday$Records_Per_Lane == 1)]))
cbind(vol_per_day_2022_workday$LinkID[which(vol_per_day_2022_workday$Records_Per_Lane == 1)],
      vol_per_day_2022_workday$Date_merge[which(vol_per_day_2022_workday$Records_Per_Lane == 1)])
vol_per_day_2022_workday[which(vol_per_day_2022_workday$Date_merge == '2-3' & vol_per_day_2022_workday$LinkID == '557'),seq(20,112)]
length(which(!is.na(vol_per_day_2022_workday[which(vol_per_day_2022_workday$Date_merge == '2-3' & 
                                                     vol_per_day_2022_workday$LinkID == '557'),seq(113,208)])))


# create a nested table that the main table stores all the detector specs and the nested table stores the time, vol, speed (3 cols)
library('tibble')

df <- as_tibble(df)  # convert to tibble if it's a data.frame
df$nested_table <- vector("list", nrow(df))



