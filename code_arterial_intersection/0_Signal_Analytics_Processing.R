##### data memo
## signal_oct2024am_xxx: data from signal analytics, which comes from probe vehicles representing 2-8% of the traffic stream. Delay & travel time included
##### Each sampled vehicle is assigned to a 230-meter zone (one for each movement); they also provide the framework to calculate travel times, 
##### control delays, and whether or not a vehicle is regarded as having stopped within the intersection.

## NPMRDS_202410_tmclink_spdVol: shp from db [TravelTimes].[dbo].[HourlySpeedsByTMC_October_2024], calculate workday avg spd & vol per hour, calculate mean TT

## Arterial_MatchedTMC_1: shp from roadlink 2026, that are closest to the existing recorded intersections from signal analytics, only FUNCL 234 are included
## NPMRDS_202410_intersection: shp read from signal analytics intersection (delete dup intersection ID)


### this file specifies the matching of npmrds and roadlink
library('arcgisbinding')
library('sf')
library(dplyr)
library(tidyr)
library('openxlsx')
library(geosphere)
library('DBI')
library('odbc')
library('ggplot2')
library(leaflet)


setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

### read base data
sidefire2025 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\Sidefire_2025\\sidefire_2025.shp')
roadlink_2026 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\road_shp_addedinfo.shp')
taz_2026 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\TAZ\\taz_shp.shp')
roadlink_26_ab = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\YR26_JAN2024_RDWY_start_end_added.csv', header = T)
roadlink_26_mma = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\Y26_TAFT_Assignment_AM_MMA_LinkFlows5.csv', header = T)

sidefire_2025 <- sidefire2025 %>%
  select(ID, LINKNAME, FUNCL, DIR, TMC, geometry)
colnames(sidefire_2025) = c('ID', 'LINKNAME', 'FUNCL', 'DIR', 'TMC', 'geometry')

############################### first, convert Oct average (with starting and ending points) to link polyline and find examples #######
tmc_oct2024 = data.frame(read.csv('20250410_capacity_recalculation\\Database\\TMC_2024_OctHrSpd.csv', header = T))
tmc_2024loc = data.frame(read.csv('20250410_capacity_recalculation\\Database\\TMC_location_2024.csv', header = T, fileEncoding = "latin1"))

## connect polyline 
geoms <- vector("list", length = nrow(tmc_2024loc))

for (i in seq_len(nrow(tmc_2024loc))) {
  coords <- matrix(c(
    tmc_2024loc$start_longitude[i], tmc_2024loc$start_latitude[i],
    tmc_2024loc$end_longitude[i],   tmc_2024loc$end_latitude[i]
  ), byrow = TRUE, ncol = 2)
  geoms[[i]] <- st_linestring(coords)
}

# Wrap geometries in sfc and assign CRS
sfc_geom <- st_sfc(geoms, crs = 4326)

# Combine attributes with geometry into an sf object
tmc_2024loc_line <- st_sf(tmc_2024loc, geometry = sfc_geom)

# Calculate average speed and vol of all workdays by hour
tmc_oct2024_workdayavg = tmcoct2024_hrspd[!duplicated(tmcoct2024_hrspd$tmc_code),]

library(dplyr)

# Filter out weekends first
filtered_data <- tmcoct2024_hrspd %>%
  filter(DOW != 1 & DOW != 7)


### Calculate AM mean spd and mean vol
mean_AM_data <- filtered_data %>%
  group_by(tmc_code) %>%
  summarise(
    across(11:13, ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
    across(36:38, ~mean(.x, na.rm = TRUE), .names = "mean_{.col}")
  )

mean_AM_data$mean_SpdAM = rowMeans(cbind(mean_AM_data$mean_SpdTMC06, mean_AM_data$mean_SpdTMC07, mean_AM_data$mean_SpdTMC08), na.rm = T)
mean_AM_data$tot_ColAM = rowSums(cbind(mean_AM_data$mean_Vol_TMC_06, mean_AM_data$mean_Vol_TMC_07, mean_AM_data$mean_Vol_TMC_08), na.rm = T)

# Join back to your output frame by tmc_code
tmc_oct2024_workdayavg_AM <- tmc_oct2024_workdayavg[,c(1,5)] %>%
  left_join(mean_AM_data, by = "tmc_code")

tmc_oct2024_workdayavg_AM <- tmc_oct2024_workdayavg_AM %>%
  left_join(tmc_2024loc_line %>% select(Funcl, road, dir, intersection, miles, 
                                        thrulanes, thrulanes_unidir, tmc, geometry), 
            by = c("tmc_code" = "tmc"))
# calculate average travel time
tmc_oct2024_workdayavg_AM$mean_TT = tmc_oct2024_workdayavg_AM$miles/tmc_oct2024_workdayavg_AM$mean_SpdAM * 3600 # to second

### Calculate PM mean spd and mean vol
mean_PM_data <- filtered_data %>%
  group_by(tmc_code) %>%
  summarise(
    across(21:23, ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
    across(46:48, ~mean(.x, na.rm = TRUE), .names = "mean_{.col}")
  )

mean_PM_data$mean_SpdPM = rowMeans(cbind(mean_PM_data$mean_SpdTMC16, mean_PM_data$mean_SpdTMC17, mean_PM_data$mean_SpdTMC18), na.rm = T)
mean_PM_data$tot_ColPM = rowSums(cbind(mean_PM_data$mean_Vol_TMC_16, mean_PM_data$mean_Vol_TMC_17, mean_PM_data$mean_Vol_TMC_18), na.rm = T)

# Join back to your output frame by tmc_code
tmc_oct2024_workdayavg_PM <- tmc_oct2024_workdayavg[,c(1,5)] %>%
  left_join(mean_PM_data, by = "tmc_code")

tmc_oct2024_workdayavg_PM <- tmc_oct2024_workdayavg_PM %>%
  left_join(tmc_2024loc_line %>% select(Funcl, road, dir, intersection, miles, 
                                        thrulanes, thrulanes_unidir, tmc, geometry), 
            by = c("tmc_code" = "tmc"))
# calculate average travel time
tmc_oct2024_workdayavg_PM$mean_TT = tmc_oct2024_workdayavg_PM$miles/tmc_oct2024_workdayavg_PM$mean_SpdPM * 3600 # to second


# Calculate daily mean for columns 6–29 and 31–54 grouped by tmc_code
mean_data <- filtered_data %>%
  group_by(tmc_code) %>%
  summarise(
    across(5:28, ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
    across(30:53, ~mean(.x, na.rm = TRUE), .names = "mean_{.col}")
  )
# Join back to your output frame by tmc_code
tmc_oct2024_workdayavg <- tmc_oct2024_workdayavg[,c(1,5)] %>%
  left_join(mean_data, by = "tmc_code")
# Calculate average speed of all hour and attach to the end of the monthly hour average
tmc_oct2024_workdayavg$SpdTMC_Day = rowMeans(tmc_oct2024_workdayavg[,3:26], na.rm = T)
tmc_oct2024_workdayavg$Vol_TMC_Day = rowSums(tmc_oct2024_workdayavg[,27:50], na.rm = T)
# add line geometry
tmc_oct2024_workdayavg <- tmc_oct2024_workdayavg %>%
  left_join(tmc_2024loc_line %>% select(Funcl, road, dir, intersection, miles, 
                                        thrulanes, thrulanes_unidir, tmc, geometry), 
            by = c("tmc_code" = "tmc"))
# calculate average travel time
tmc_oct2024_workdayavg$mean_TT = tmc_oct2024_workdayavg$miles/tmc_oct2024_workdayavg$SpdTMC_Day * 3600 # to second
# write data to shpfile
st_write(tmc_oct2024_workdayavg, '20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\NPMDRS_202410_tmclink_spdVol.shp', delete_layer = T)

rm(mean_data, filtered_data, sfc_geom, geoms, coords, con, i, j)


############################# second, convert approaching data to shapefile and find examples ###################
signal_oct2024am_intersect = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\Arterial_NPMRDS\\2024OctAM\\Intersection.csv', header = T)
signal_oct2024am_approach = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\Arterial_NPMRDS\\2024OctAM\\Approach.csv', header = T)
signal_oct2024am_movement = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\Arterial_NPMRDS\\2024OctAM\\Movement.csv', header = T)

### convert three files to GIS file in order to calculate distance

## first, create ID for each intersection point. They do not match with each other
# use rank + intersection to check if they are identical across three files 
signal_oct2024am_intersect$ID = seq(1, nrow(signal_oct2024am_intersect))
# signal_oct2024am_intersect$uniqName = paste(signal_oct2024am_intersect$Rank, signal_oct2024am_intersect$Intersection)
# signal_oct2024am_approach$uniqName = paste(signal_oct2024am_approach$Rank, signal_oct2024am_approach$Intersection)
#### tested --> rank is not identical across intersection, approach, movement -- discard
# find duplicated intersect ID, if they are the same one or not
signal_dup = signal_oct2024am_intersect[duplicated(signal_oct2024am_intersect$Intersection.ID) | 
                                          duplicated(signal_oct2024am_intersect$Intersection.ID, fromLast = TRUE), ]
# Yes, they are dups, delete dups and keep only the first occurrence
rm(signal_dup)
signal_oct2024am_intersect_nodup = signal_oct2024am_intersect[!duplicated(signal_oct2024am_intersect$Intersection.ID),]
signal_oct2024am_intersect_nodup$ID = seq(1, nrow(signal_oct2024am_intersect_nodup))

# match ID to approach
signal_oct2024am_approach <- signal_oct2024am_approach %>%
  left_join(signal_oct2024am_intersect_nodup %>% select(Intersection.ID, ID), 
            by = 'Intersection.ID')
# delete where average travel time is NA
signal_oct2024am_approach = signal_oct2024am_approach[!is.na(signal_oct2024am_approach$Travel.Time..Avg),]

# match ID to movement
signal_oct2024am_movement <- signal_oct2024am_movement %>%
  left_join(signal_oct2024am_intersect_nodup %>% select(Intersection.ID, ID), 
            by = 'Intersection.ID')
# delete where average travel time is NA
signal_oct2024am_movement = signal_oct2024am_movement[!is.na(signal_oct2024am_movement$Travel.Time..Avg),]

## test if they have the same Intersection.ID by checking if their intersection name is same
difnames_app = 0; difnames_move = 0
for (i in 1:nrow(signal_oct2024am_intersect_nodup)) {
  name_intersect = signal_oct2024am_intersect_nodup$Intersection[i]
  name_approach = unique(signal_oct2024am_approach$Intersection[which(
    signal_oct2024am_approach$ID == signal_oct2024am_intersect_nodup$ID[i])])
  name_movement = unique(signal_oct2024am_movement$Intersection[which(
    signal_oct2024am_movement$ID == signal_oct2024am_intersect_nodup$ID[i])])
  if (name_intersect != name_approach) {
    print(paste('intersection, approach name not match', name_intersect, name_approach))
    difnames_app = difnames_app + 1
  }
  if (name_intersect != name_movement) {
    print(paste('intersection, movement name not match', name_intersect, name_movement))
    difnames_move = difnames_move + 1
  }
}
rm(signal_oct2024am_intersect_nodup)
## check complete. Intersection name with unique intersection.ID is the same

############################## Third, match intersection TMC points to roadlinks to get correct FUNCL, keep arterial (2,3,4) & ramp (6)
# Ensure both are in the same projected CRS (must use feet as unit!)
# For example, NAD 1983 StatePlane Arizona East FIPS 0201 Feet
# Replace with your actual CRS


# create a simplified intersection file
signal_oct2024am_intersect_points = data.frame(cbind(
  signal_oct2024am_intersect_nodup$ID,
  signal_oct2024am_intersect_nodup$Intersection, 
  signal_oct2024am_intersect_nodup$Intersection.ID,
  signal_oct2024am_intersect_nodup$Longitude, 
  signal_oct2024am_intersect_nodup$Latitude
))
colnames(signal_oct2024am_intersect_points) = c(
  'uniqId',
  'intersection','intersectionId','lon','lat'
)
# convert to points, then save to intersection
signal_oct2024am_intersect_points_sf <- st_as_sf(signal_oct2024am_intersect_points, 
                                                 coords = c("lon", "lat"), crs = 4326)
st_write(signal_oct2024am_intersect_points_sf, '20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\NPMDRS_202410_Intersection.shp', delete_layer = T)

# find links with the closest distance to each point
crs_feet <- 2223  # example EPSG code for feet
lines <- st_transform(roadlink_2026, crs_feet)
# select only arterial: Principal, Minor, Connector
lines = lines[which(lines$FUNCL == 2 | lines$FUNCL == 3 | lines$FUNCL == 4),]
points <- st_transform(signal_oct2024am_intersect_points_sf, crs_feet)

# Find the nearest line for each point
nearest_line_index <- st_nearest_feature(points, lines)

# Extract the nearest line(s)
nearest_lines <- lines[nearest_line_index, ]
nearest_lines = cbind(nearest_lines, points$id, points$intersectionId)

## add FUNCL to signal TMC, according to the nearest distance
signal_oct2024am_intersect_lines <- signal_oct2024am_intersect_points %>%
  left_join(nearest_lines %>% select(points.id, ID, LENGTH, FUNCL, LNKNM), 
            by = c('uniqId' = 'points.id'))

## save line 
st_write(signal_oct2024am_intersect_lines, '20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\Arterial_MatchedTMC_1.shp',
         delete_layer = T)

########################### check intersections with duplicated movements or approaches, save as dup csv, filter out, save filtered intersection ######
# check any dup approaches for all intersections and record ID
ID_dupapproach = array(NA, dim = 0)
number_dupapproach = 0
for (i in 1:nrow(signal_oct2024am_intersect_nodup)) {
  approach_i = signal_oct2024am_approach$Approach[which(
    signal_oct2024am_approach$ID == signal_oct2024am_intersect_nodup$ID[i]
  )]
  if (any(duplicated(approach_i))) {
    ID_dupapproach = c(ID_dupapproach, signal_oct2024am_intersect_nodup$ID[i])
    number_dupapproach = number_dupapproach + 1
  }
}

# check any dup movements at each intersection & approaches and record ID
ID_dupmovement = array(NA, dim = 0)
number_dupmovement = 0
for (i in 1:nrow(signal_oct2024am_intersect_nodup)) {
  approach_i = signal_oct2024am_approach$Approach[which(
    signal_oct2024am_approach$ID == signal_oct2024am_intersect_nodup$ID[i]
  )]
  for (j in 1:length(approach_i)) {
    movement_ij = signal_oct2024am_movement$Movement[which(
      signal_oct2024am_movement$ID == signal_oct2024am_intersect_nodup$ID[i] &
        signal_oct2024am_movement$Approach == approach_i[j]
    )]
    if (any(duplicated(movement_ij))) {
      ID_dupmovement = c(ID_dupmovement, signal_oct2024am_intersect_nodup$ID[i])
      number_dupmovement = number_dupmovement + 1
    }
  }
  
}
ID_dupmovement = ID_dupmovement[!duplicated(ID_dupmovement)]

# save IDs of intersection with either dup approach or dup movement, remove dup IDs
ID_dupappormove = c(ID_dupmovement, ID_dupapproach)
ID_dupappormove = ID_dupappormove[!duplicated(ID_dupappormove)]

rm(approach_i, movement_ij, i, j)
rm(sample_approach, sample_movement, sample_intersect)

# save dup approach or movements to csv
ID_dupapproach = data.frame(cbind(ID_dupapproach, ID_dupapproach)); 
colnames(ID_dupapproach) = c('id_1','id_2')
dupapproach <- signal_oct2024am_approach %>%
  left_join(data.frame(ID_dupapproach),
            by = c('ID' = 'id_1'))
dupapproach = dupapproach[!is.na(dupapproach$id_2),]

ID_dupmovement = data.frame(cbind(ID_dupmovement, ID_dupmovement)); 
colnames(ID_dupmovement) = c('id_1','id_2')
dupmovement <- signal_oct2024am_movement %>%
  left_join(data.frame(ID_dupmovement),
            by = c('ID' = 'id_1'))
dupmovement = dupmovement[!is.na(dupmovement$id_2),]

write.csv(dupapproach, '20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\Dup_Approach.csv', row.names = F)
write.csv(dupmovement, '20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\Dup_Movement.csv', row.names = F)

### delete those intersections that have duplicated movements or approaches
signal_oct2024am_intersect_nodup <- signal_oct2024am_intersect_nodup %>%
  left_join(ID_dupapproach, by = c("ID" = 'id_1'))
signal_oct2024am_intersect_nodup <- signal_oct2024am_intersect_nodup %>%
  left_join(ID_dupmovement, by = c("ID" = 'id_1'))
signal_oct2024am_intersect_filtered = signal_oct2024am_intersect_nodup[which(is.na(signal_oct2024am_intersect_nodup$id_2.x)),]
signal_oct2024am_intersect_filtered = signal_oct2024am_intersect_filtered[which(is.na(signal_oct2024am_intersect_filtered$id_2.y)),]
# select filtered approaches 
signal_oct2024am_approach <- signal_oct2024am_approach %>%
  left_join(ID_dupapproach, by = c("ID" = 'id_1'))
signal_oct2024am_approach <- signal_oct2024am_approach %>%
  left_join(ID_dupmovement, by = c("ID" = 'id_1'))
signal_oct2024am_approach_filtered = signal_oct2024am_approach[which(is.na(signal_oct2024am_approach$id_2.x)),]
signal_oct2024am_approach_filtered = signal_oct2024am_approach_filtered[which(is.na(signal_oct2024am_approach_filtered$id_2.y)),]
# select filtered movements
signal_oct2024am_movement <- signal_oct2024am_movement %>%
  left_join(ID_dupapproach, by = c("ID" = 'id_1'))
signal_oct2024am_movement <- signal_oct2024am_movement %>%
  left_join(ID_dupmovement, by = c("ID" = 'id_1'))
signal_oct2024am_movement_filtered = signal_oct2024am_movement[which(is.na(signal_oct2024am_movement$id_2.x)),]
signal_oct2024am_movement_filtered = signal_oct2024am_movement_filtered[which(is.na(signal_oct2024am_movement_filtered$id_2.y)),]

# create a simplified intersection file - filtered
signal_oct2024am_intersect_points_filtered = data.frame(cbind(
  signal_oct2024am_intersect_filtered$ID,
  signal_oct2024am_intersect_filtered$Intersection, 
  signal_oct2024am_intersect_filtered$Intersection.ID,
  signal_oct2024am_intersect_filtered$Longitude, 
  signal_oct2024am_intersect_filtered$Latitude
))
colnames(signal_oct2024am_intersect_points_filtered) = c(
  'uniqId',
  'intersection','intersectionId','lon','lat'
)
# convert to points, then save to intersection
signal_oct2024am_intersect_points_filtered_sf <- st_as_sf(signal_oct2024am_intersect_points_filtered, 
                                                 coords = c("lon", "lat"), crs = 4326)
st_write(signal_oct2024am_intersect_points_filtered_sf, '20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\NPMDRS_202410_Intersection_filtered.shp', delete_layer = T)


############################### (no longer used) match link with intersection nodes, attach direction to each link ########################
# convert intersection to node points
roadlink_26_ab = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\YR26_JAN2024_RDWY_start_end_added.csv', header = T)
roadlink_26_mma = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\Y26_TAFT_Assignment_AM_MMA_LinkFlows5.csv', header = T)
intersections = st_as_sf(signal_oct2024am_approach_filtered, 
                         coords = c("Longitude", "Latitude"), crs = 4326)

lines <- roadlink_2026 %>%
  left_join(roadlink_26_ab %>% select(ID, From.ID, From.Longitude, From.Latitude, To.ID, To.Longitude, To.Latitude), 
            by = c('ID' = 'ID'))# LINESTRING, with fields AB and BA
nodes <- intersections       # POINTs, one per approach direction

roadlink_26_signalmatch = data.frame(matrix(0, nrow = 0, ncol = ncol(lines)))
for (i in 1:nrow(lines)) {
  roadlink_26_signalmatch = rbind(roadlink_26_signalmatch, match_link_movements(lines[i,], nodes, dist_threshold = 20))
}
