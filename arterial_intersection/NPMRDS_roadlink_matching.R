### this file specifies the matching of npmrds and roadlink
library('arcgisbinding')
library('sf')
library('dplyr')
library('openxlsx')


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

# first use approach data
signal_oct2024am_approach_points = data.frame(cbind(seq(1, nrow(signal_oct2024am_approach)), 
                                                    signal_oct2024am_approach$Rank, signal_oct2024am_approach$Intersection, 
                                                    signal_oct2024am_approach$Latitude, signal_oct2024am_approach$Longitude, 
                                                    signal_oct2024am_approach$Movement, signal_oct2024am_approach$Approach,
                                                    signal_oct2024am_approach$Vehicle.Count..Total, signal_oct2024am_approach$Vehicle.Count..Through, signal_oct2024am_approach$Vehicle.Count..Stopped,
                                                    signal_oct2024am_approach$Percent.Arrival.On.Green..POG., signal_oct2024am_approach$Turn.Percentage,
                                                    signal_oct2024am_approach$Travel.Time..Avg,
                                                    signal_oct2024am_approach$Control.Delay..Avg, 
                                                    signal_oct2024am_approach$Approach.Speed..Avg, 
                                                    signal_oct2024am_approach$Approach.Speed..Stop..Avg, signal_oct2024am_approach$Approach.Speed..Thru..Avg))

names(signal_oct2024am_approach_points) = c('id', 'rank', 'intersection', 'lat', 'lon', 'movement', 'approach', 
                                            'vehtot', 'vehthru', 'vehstop', 'percentarrivalgreen','turnperct', 
                                            'ttavg', 'ctrldelay',
                                            'approachspdavg',
                                            'approachstopspdavg','approachthruspdavg')

# Convert to sf object with point geometry
signal_oct2024am_approach_points <- st_as_sf(signal_oct2024am_approach_points, coords = c("lon", "lat"), crs = 4326)

# Save as ESRI Shapefile
st_write(signal_oct2024am_approach_points, '20250410_capacity_recalculation\\RoadNetwork_2026\\INRIX\\NPMDRS_202410_approach_1.shp', delete_layer = T)

###### third, find example links and intersections, find ffspd, fftt, potential capacity ######

# look at link row ID 47

signal_oct2024am_approach_47 = signal_oct2024am_approach_points[which((signal_oct2024am_approach_points$id == 20127 & 
                                                                  signal_oct2024am_approach_points$approach == 'Southbound')|
                                                                 (signal_oct2024am_approach_points$id == 20510 & 
                                                                    signal_oct2024am_approach_points$approach == 'Southbound') |
                                                                 (signal_oct2024am_approach_points$id == 21366 & 
                                                                    signal_oct2024am_approach_points$approach == 'Southbound') |
                                                                 (signal_oct2024am_approach_points$id == 22531 & 
                                                                    signal_oct2024am_approach_points$approach == 'Southbound') |
                                                                 
                                                                 (signal_oct2024am_approach_points$id == 20109 &
                                                                    signal_oct2024am_approach_points$approach == 'Northbound') |
                                                                 (signal_oct2024am_approach_points$id == 21312 &
                                                                     signal_oct2024am_approach_points$approach == 'Northbound') |
                                                                 (signal_oct2024am_approach_points$id == 21699 &
                                                                     signal_oct2024am_approach_points$approach == 'Northbound') |
                                                                 (signal_oct2024am_approach_points$id == 22308 &
                                                                    signal_oct2024am_approach_points$approach == 'Northbound')
                                                                      ),]
signal_feb2024_lines_am_8486 = signal_feb2024_lines[which(signal_feb2024_lines$ID == 8486),]

signal_oct2024am_movement[which(signal_oct2024am_movement$Intersection == '5261 - IH 20 WBFR & HOUSTON SCHOOL RD'),]

signal_oct2024am_approach[which(signal_oct2024am_approach$Intersection== '5261 - IH 20 WBFR & HOUSTON SCHOOL RD'),]

signal_oct2024am_movement[which(signal_oct2024am_movement$Intersection == '5260 - IH 20 EBFR & HOUSTON SCHOOL RD'),]

signal_oct2024am_approach[which(signal_oct2024am_approach$Intersection== '5260 - IH 20 EBFR & HOUSTON SCHOOL RD'),]


## find where is the problematic intersection
try1_folder = 'I:\\Model_Development\\_General\\Zihao\\Projects\\SiginalAnalytics\\2024OctAM\\Int Analys for OctAMZ7_2024-10-01 to 2024-10-31_Mo,Tu,We,Th,Fr_06-30 to 09-00 (981 int)'
try1_movement = read.csv(paste0(try1_folder, '\\Movement.csv'), header = T)
which(try1_movement$Intersection == '5260 - IH 20 EBFR & HOUSTON SCHOOL RD')

###### Third, match intersection TMC points to roadlinks to get correct FUNCL, keep arterial (2,3,4) & ramp (6)
# Ensure both are in the same projected CRS (must use feet as unit!)
# For example, NAD 1983 StatePlane Arizona East FIPS 0201 Feet
# Replace with your actual CRS
crs_feet <- 2223  # example EPSG code for feet
lines <- st_transform(roadlink_2026, crs_feet)
points <- st_transform(sidefire_2025, crs_feet)

# Buffer points by 100 feet
points_buffer <- st_buffer(points, dist = 100) # ease to 150? -- no, keep consistent with Francisco

# Spatial join: get all lines within 100 ft of any point
# This creates a row for each line-point pair
lines_within_100ft <- st_join(lines, points_buffer, join = st_intersects, left = FALSE)






