library('arcgisbinding')
library('sf')
library('dplyr')
library('openxlsx')


setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

sidefire2025 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\Sidefire_2025\\sidefire_2025.shp')
roadlink_2026 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\road_shp_addedinfo.shp')
taz_2026 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\TAZ\\taz_shp.shp')

sidefire_2025 <- sidefire2025 %>%
  select(ID, LINKNAME, FUNCL, DIR, geometry)
# colnames(sidefire_2025) = c('ID', 'LINKNAME', 'FUNCL', 'DIR', 'geometry')

st_crs(sidefire_2025) == st_crs(roadlink_2026)  # should be TRUE, check if two layers are in the same coordinate
st_crs(sidefire_2025) == st_crs(taz_2026) # should be TRUE, check if two layers are in the same coordinate


# find the nearest line of each point
nearest_line_indices <- st_nearest_feature(sidefire_2025, roadlink_2026)
nearest_lines <- roadlink_2026[nearest_line_indices, ]

sidefire_2025$nearlink_id_26 = nearest_lines$ID
sidefire_2025$nearlink_name_26 = nearest_lines$STREET
sidefire_2025$nearlink_dir_26 = nearest_lines$FAC_DIR
sidefire_2025$nearlink_funcl_26 = nearest_lines$FUNCL

# calculate the nearest distance
distances <- st_distance(sidefire_2025, nearest_lines, by_element = TRUE)
sidefire_2025$distance_meters_26 <- as.numeric(distances)

# mark distance > 100ft (1) # only choose the closest, need to find more links in the next round
sidefire_2025$distance_notmatch_26 = 0
sidefire_2025$distance_notmatch_26[which(sidefire_2025$distance_meters_26 > 30.48)] = 1
length(which(sidefire_2025$distance_notmatch_26 == 1))
unique(sidefire_2025$FUNCL[which(sidefire_2025$funcl_notmatch == 0 & sidefire_2025$distance_notmatch_26 == 0)])

# mark sidefire FUNCL >= 8
sidefire_2025$managelane = 0; sidefire_2025$managelane[which(sidefire_2025$FUNCL >= 8)] = 1

# mark link FUNCL not match (1)
sidefire_2025$funcl_notmatch = 0
for (i in 1:nrow(sidefire_2025)) {
  if (sidefire_2025$FUNCL[i] != sidefire_2025$nearlink_funcl_26[i]) {
    sidefire_2025$funcl_notmatch[i] = 1
  }
}
length(which(sidefire_2025$funcl_notmatch == 1))
unique(sidefire_2025$FUNCL[which(sidefire_2025$funcl_notmatch == 0 & sidefire_2025$distance_notmatch_26 == 0)])
table(sidefire_2025$FUNCL[which(sidefire_2025$funcl_notmatch == 0 & sidefire_2025$distance_notmatch_26 == 0)])

# mark direction not match (1)
# NB, SB, EB, WB
# NB_match = grepl(" NB", sidefire_2025$LINKNAME) * grepl(" NB", sidefire_2025$near_line_name_26)
# SB_match = grepl(" SB", sidefire_2025$LINKNAME) * grepl(" SB", sidefire_2025$near_line_name_26)
# EB_match = grepl(" EB", sidefire_2025$LINKNAME) * grepl(" EB", sidefire_2025$near_line_name_26)
# WB_match = grepl(" WB", sidefire_2025$LINKNAME) * grepl(" WB", sidefire_2025$near_line_name_26)

# check if NB, SB, EB, WB match. can be both linkname and dir
sidefire_2025$dir_notmatch_26 = 0
for (i in 1:nrow(sidefire_2025)) {
  # count name contains NB but road link name does not
  if (isTRUE(grepl("NB", sidefire_2025$LINKNAME[i])) | isTRUE(grepl('N', sidefire_2025$DIR[i], ignore.case = T))) {
    if (isFALSE(grepl('NB', sidefire_2025$nearlink_name_26[i])) & isFALSE(grepl('N', sidefire_2025$nearlink_dir_26[i], ignore.case = T))) {
      sidefire_2025$dir_notmatch_26[i] = 1
    }
  } 
  # count name contains SB but road link name does not
  else if (isTRUE(grepl("SB", sidefire_2025$LINKNAME[i])) | isTRUE(grepl('S', sidefire_2025$DIR[i], ignore.case = T))) {
    if (isFALSE(grepl('SB', sidefire_2025$nearlink_name_26[i])) & isFALSE(grepl('S', sidefire_2025$nearlink_dir_26[i], ignore.case = T))) {
      sidefire_2025$dir_notmatch_26[i] = 1
    }
  } 
  # count name contains EB but road link name does not
  else if (isTRUE(grepl("EB", sidefire_2025$LINKNAME[i])) | isTRUE(grepl('E', sidefire_2025$DIR[i], ignore.case = T))) {
    if (isFALSE(grepl('EB', sidefire_2025$nearlink_name_26[i])) & isFALSE(grepl('E', sidefire_2025$nearlink_dir_26[i], ignore.case = T))) {
      sidefire_2025$dir_notmatch_26[i] = 1
    }
  }
  # count name contains WB but road link name does not
  else if (isTRUE(grepl("WB", sidefire_2025$LINKNAME[i])) | isTRUE(grepl('W', sidefire_2025$DIR[i], ignore.case = T))) {
    if (isFALSE(grepl('WB', sidefire_2025$nearlink_name_26[i])) & isFALSE(grepl('W', sidefire_2025$nearlink_dir_26[i], ignore.case = T))) {
      sidefire_2025$dir_notmatch_26[i] = 1
    }
  } 
}

# check if there is any SW, SE, etc.
# manual checked, all of those SE, SW, NE, NW are correct -- do not have to check
length(which(sidefire_2025$dir_notmatch_26 == 1 | sidefire_2025$funcl_notmatch == 1))
unique(sidefire_2025$FUNCL[which(sidefire_2025$dir_notmatch_26 == 0 & sidefire_2025$funcl_notmatch == 0 & sidefire_2025$distance_notmatch_26 == 0)])
table(sidefire_2025$FUNCL[which(sidefire_2025$dir_notmatch_26 == 0 & sidefire_2025$funcl_notmatch == 0 & sidefire_2025$distance_notmatch_26 == 0)])

# mark ramp not match (1)
sidefire_2025$ramp_notmatch_26 = 0
for (i in 1:nrow(sidefire_2025)) {
  # if count link is ramp while road link is not ramp
  if (isTRUE(grepl('ramp', sidefire_2025$LINKNAME[i], ignore.case = T))) {
    if (isFALSE(grepl('RAMP', sidefire_2025$nearlink_name_26[i], ignore.case = T))) {
      sidefire_2025$ramp_notmatch_26[i] = 1
    }
  }
  # if road link is ramp while count link is not ramp
  if (isTRUE(grepl('RAMP', sidefire_2025$nearlink_name_26[i], ignore.case = T))) {
    if (isTRUE(grepl('ramp', sidefire_2025$LINKNAME[i], ignore.case = T))) {
      sidefire_2025$ramp_notmatch_26[i] = 1
    }
  }
}
length(which(sidefire_2025$dir_notmatch_26 == 1 | sidefire_2025$funcl_notmatch == 1 | sidefire_2025$ramp_notmatch_26 == 1))
unique(sidefire_2025$FUNCL[which(sidefire_2025$dir_notmatch_26 == 0 & sidefire_2025$funcl_notmatch == 0 & 
                                   sidefire_2025$ramp_notmatch_26 == 0 & sidefire_2025$distance_notmatch_26 == 0)])
table(sidefire_2025$FUNCL[which(sidefire_2025$dir_notmatch_26 == 0 & sidefire_2025$funcl_notmatch == 0 & 
                                  sidefire_2025$ramp_notmatch_26 == 0 & sidefire_2025$distance_notmatch_26 == 0)])

# mark name not match (1) strsplit string can be '-', '.', ' ', in count link name 
# --> logic wrong. some I highway can also called IH, US highway has its nick name (like Spur)
# re-write this part of code
# first use strict match
sidefire_2025$name_strictnotmatch_26 = 0
for (i in 1:nrow(sidefire_2025)) {
  # for count links that are not ramp
  if (isFALSE(grepl('ramp', sidefire_2025$LINKNAME[i], ignore.case = T)) & 
      isFALSE(grepl('ramp', sidefire_2025$nearlink_name_26[i], ignore.case = T))) {
    if (strsplit(sidefire_2025$nearlink_name_26[i],"[-. ]+")[[1]][1] != strsplit(sidefire_2025$LINKNAME[i], '[-. ]+')[[1]][1]) {
      sidefire_2025$name_strictnotmatch_26[i] = 1
      }
  }
}
length(which(sidefire_2025$dir_notmatch_26 == 1 | sidefire_2025$funcl_notmatch == 1 | 
               sidefire_2025$ramp_notmatch_26 == 1 | sidefire_2025$name_strictnotmatch_26 == 1))
unique(sidefire_2025$FUNCL[which(sidefire_2025$dir_notmatch_26 == 0 & sidefire_2025$funcl_notmatch == 0 &
                                   sidefire_2025$ramp_notmatch_26 == 0 & sidefire_2025$name_strictnotmatch_26 == 0 & 
                                   sidefire_2025$distance_notmatch_26 == 0)])
table(sidefire_2025$FUNCL[which(sidefire_2025$dir_notmatch_26 == 0 & sidefire_2025$funcl_notmatch == 0 &
                                  sidefire_2025$ramp_notmatch_26 == 0 & sidefire_2025$name_strictnotmatch_26 == 0 & 
                                  sidefire_2025$distance_notmatch_26 == 0)])

# find counters that match with link according to distance, funcl, dir, name
sidefire_2025$link_notmatch_26 = sidefire_2025$dir_notmatch_26 + sidefire_2025$name_strictnotmatch_26 + 
  sidefire_2025$funcl_notmatch + sidefire_2025$ramp_notmatch_26
length(which(sidefire_2025$link_notmatch_26 >= 1))

sidefire_match_2025 = sidefire_2025[which(sidefire_2025$link_notmatch_26 == 0),]

# find sidefire that within the taz
within_matrix <- st_intersects(sidefire_match_2025, taz_2026, sparse = FALSE) 
any_inside <- apply(within_matrix, 1, any) # apply to all TAZs # check if sidefire is within TAZ
sidefire_inTaz_2025 <- sidefire_match_2025[any_inside, ]

# clear environment
rm(within_matrix, any_inside, i, NB_match, SB_match, WB_match, EB_match, distances, nearest_line_indices)
rm(inside)
rm(nearline, points_inside)


####################################################################check removed ramp###########################################
####################################################################check removed ramp###########################################
####################################################################check removed ramp###########################################
####################################################################check removed ramp###########################################
####################################################################check removed ramp###########################################

sidefire_notmatch_ramp = sidefire_2025[which(sidefire_2025$FUNCL == 6 & sidefire_2025$nearlink_funcl_26 == 6 & sidefire_2025$link >= 1),]
sidefire_notmatch_ramp = sidefire_notmatch_ramp[,-5] #delete geography
write.csv(sidefire_notmatch_ramp,"~/0_ModelDataDevelopment/20250410_capacity_recalculation/RoadNetwork_2026/Sensor_count/SensorsNotMatch_ramp.csv",
          row.names = F)

#### updates in 04292025, checked deleted ramps (90), and add 59 ramps back from file SensorsNothMatch_ramp_addbackLabeled
ramp_to_addback = read.csv("~/0_ModelDataDevelopment/20250410_capacity_recalculation/RoadNetwork_2026/Sensor_count/SensorsNotMatch_ramp_addbackLabeled.csv",
                           header = T)
ramp_to_addback = ramp_to_addback[which(ramp_to_addback$add_back == 1),]

# first store initial colnames of the processed selected TAZ
colname = colnames(sidefire_notmatch_ramp)
# from deleted shapefile rows, find the ramp that should not be deleted and add back to sidefire set
for (i in 1:nrow(ramp_to_addback)) {
  addedramp = sidefire_notmatch_ramp[which(sidefire_notmatch_ramp$ID == ramp_to_addback$ID[i]),]
  sidefire_inTaz_2025 = rbind(sidefire_inTaz_2025, addedramp)
}
colnames(sidefire_inTaz_2025) = colname

####################################################################check consistency with Francisco###########################################
####################################################################check consistency with Francisco###########################################
####################################################################check consistency with Francisco###########################################
####################################################################check consistency with Francisco###########################################
####################################################################check consistency with Francisco###########################################
# Francisco's records are all FUNCL 1, keep my criteria

# check the consistency with Francisco
sidefire_2025_Frcsc = read.xlsx('~/0_ModelDataDevelopment/20250410_capacity_recalculation/RoadNetwork_2026/Sensor_count/ClosestLinksToSidefires_FromFrancisco.xlsx', sheet = 'FromFrancisco_updated0423')
sidefire_2025_Frcsc$consistency = 0
# add sensors FUNCL to sidefire Frcsc
sidefire_2025_Frcsc$FUNCL = 0
for (i in 1:nrow(sidefire_2025_Frcsc)) {
  sidefire_2025_Frcsc$FUNCL[i] = sidefire_2025$FUNCL[which(sidefire_2025$ID == sidefire_2025_Frcsc$Sidefire_ID[i])]
}

for (i in 1:nrow(sidefire_2025_Frcsc)) {
  if (length(which(sidefire_inTaz_2025$ID == sidefire_2025_Frcsc$Sidefire_ID[i])) > 0) {
    sidefire_2025_Frcsc$consistency[i] = 1
  }
}
length(which(sidefire_2025_Frcsc$consistency == 0))

sidefire_inTaz_2025$consistency = 0
for (i in 1:nrow(sidefire_inTaz_2025)) {
  if (length(which(sidefire_2025_Frcsc$Sidefire_ID == sidefire_inTaz_2025$ID[i])) > 0) {
    sidefire_inTaz_2025$consistency[i] = 1
  }
}
length(which(sidefire_inTaz_2025$consistency == 0))


# check if it is wrong in my methods or it is his error --> April 22
# find sensors that I exclude & corresponding links
sensors_exclude_rtu = sidefire_2025_Frcsc$Sidefire_ID[which(sidefire_2025_Frcsc$consistency == 0)]
sidefire_2025$rtu_exclude = 0
for (i in 1:nrow(sidefire_2025)) {
  if (length(which(sensors_exclude_rtu == sidefire_2025$ID[i])) > 0) {
    sidefire_2025$rtu_exclude[i] = 1
  }
}
sensors_exclude_rtu = sidefire_2025[which(sidefire_2025$rtu_exclude == 1),]

# find sensors that I include but Frcsc exclude & corresponding links
sensors_exclude_frcsc = sidefire_inTaz_2025$ID[which(sidefire_inTaz_2025$consistency == 0)]
sidefire_2025$frcsc_exclude = 0
for (i in 1:nrow(sidefire_2025)) {
  if (length(which(sensors_exclude_frcsc == sidefire_2025$ID[i])) > 0) {
    sidefire_2025$frcsc_exclude[i] = 1
  }
}
sensors_exclude_frcsc = sidefire_2025[which(sidefire_2025$frcsc_exclude == 1),]

write.csv(sensors_exclude_frcsc,"~/0_ModelDataDevelopment/20250410_capacity_recalculation/RoadNetwork_2026/Sensor_count/SensorsExclude_frcsc.csv",
          row.names = F)
write.csv(sensors_exclude_rtu,"~/0_ModelDataDevelopment/20250410_capacity_recalculation/RoadNetwork_2026/Sensor_count/SensorsExclude_rtu.csv",
          row.names = F)
# write_sf(sensors_exclude_frcsc,"~/0_ModelDataDevelopment/20250410_capacity_recalculation/RoadNetwork_2026/Sensor_count/SensorsExclude_frcsc.shp")

# Create a workbook
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "excludeby_Francisco")
addWorksheet(wb, "excludeby_Rtu")

# Write data to each sheet
writeData(wb, sheet = "excludeby_Francisco", x = sensors_exclude_frcsc)
writeData(wb, sheet = "excludeby_Rtu", x = sensors_exclude_rtu)

# Save workbook
saveWorkbook(wb, file = "~/0_ModelDataDevelopment/20250410_capacity_recalculation/RoadNetwork_2026/Sensor_count/SensorsExclude_updated0423.xlsx", overwrite = TRUE)

####################################################################comment out session###########################################
####################################################################comment out session###########################################
####################################################################comment out session###########################################
####################################################################comment out session###########################################
####################################################################comment out session###########################################
