### this file specifies the matching of npmrds and roadlink
library('arcgisbinding')
library('sf')
library('dplyr')
library('openxlsx')
library(geosphere)


setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

sidefire2025 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\Sidefire_2025\\sidefire_2025.shp')
roadlink_2026 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\road_shp_addedinfo.shp')
taz_2026 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\TAZ\\taz_shp.shp')

sidefire_2025 <- sidefire2025 %>%
  select(ID, LINKNAME, FUNCL, DIR, TMC, geometry)
colnames(sidefire_2025) = c('ID', 'LINKNAME', 'FUNCL', 'DIR', 'TMC', 'geometry')

###### first, convert Oct average (with starting and ending points) to link polyline and find examples #######
# examples found combined with arcgis: row ID 42 48 73 75 78 82
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

# Calculate mean for columns 6–29 and 31–54 grouped by tmc_code
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

tmc_oct2024_workdayavg <- tmc_oct2024_workdayavg %>%
  left_join(tmc_2024loc_line %>% select(Funcl, road, dir, intersection, miles, 
                                        thrulanes, thrulanes_unidir, tmc, geometry), 
            by = c("tmc_code" = "tmc"))

tmc_oct2024_workdayavg$mean_TT = tmc_oct2024_workdayavg$miles/tmc_oct2024_workdayavg$SpdTMC_Day * 3600 # to second

# write data to shpfile
st_write(tmc_oct2024_workdayavg, '20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\NPMDRS_202410_tmclink_spdVol.shp', delete_layer = T)

rm(mean_data, filtered_data, sfc_geom, geoms, coords, con, i, j)


###### second, convert approaching data to shapefile and find examples ######
signal_oct2024am_intersect = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\Arterial_NPMRDS\\2024OctAM\\Intersection.csv', header = T)
signal_oct2024am_approach = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\Arterial_NPMRDS\\2024OctAM\\Approach.csv', header = T)
signal_oct2024am_movement = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\Arterial_NPMRDS\\2024OctAM\\Movement.csv', header = T)

### convert three files to GIS file in order to calculate distance

### first, create ID for each intersection point. They do not match with each other
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
## check complete. Intersection name with unique intersection.ID is the same

###### Third, match intersection TMC points to roadlinks to get correct FUNCL, keep arterial (2,3,4) & ramp (6)
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

