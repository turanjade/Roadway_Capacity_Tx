

############################### match reference station to npmrds links ############################
## first, save unique station locations from the data, import to arcgis, check location and matching strategy
reference_station_2019 = reference_count_2019[!duplicated(reference_count_2019$ID), seq(1,6)]
reference_station_2023 = reference_count_2023[!duplicated(reference_count_2023$ID), seq(1,6)]
reference_stations = data.frame(rbind(reference_station_2019, reference_station_2023))
reference_stations = reference_stations[!duplicated(reference_stations$ID),]
reference_stations$Longitude = reference_stations$Longitude/1e6
reference_stations$Latitude = reference_stations$Latitude/1e6
# convert reference stations to shapefile
reference_stations_shp = st_as_sf(reference_stations, 
                                  coords = c("Longitude", "Latitude"), crs = 4326)
write.csv(reference_stations, '20250410_capacity_recalculation\\RoadNetwork_2026\\TxDOT_PermanentStation\\reference_stations.csv', row.names = F)

## data check correct, second, match the link with detector.
## distance buffer: 100 ft
# for each node, find all links within 100 ft, find the best match funcl & dir, select the closest npmrds link
crs_feet <- 2223  # example EPSG code for feet
line_ <- st_transform(tmc_2024loc_line[which(tmc_2024loc_line$Funcl == 2 | 
                                               tmc_2024loc_line$Funcl == 3 |
                                               tmc_2024loc_line$Funcl == 4), c(1,2,3,4,5,6,7,8)], crs_feet)
point_ <- st_transform(reference_stations_shp[which(reference_stations_shp$FUNCL == 2 |
                                                      reference_stations_shp$FUNCL == 3 |
                                                      reference_stations_shp$FUNCL == 4),], crs_feet)
# clean column string by removing all spaces in the string
line_$dir = gsub(" ", "", line_$dir)
point_$Dir = gsub(" ", "", point_$Dir)

# rename
colnames(line_) = c('ID','ID0','tmc','FUNCL','road','Dir','Direction','intersection','geometry')
colnames(point_) = c('ID','road','FUNCL','Dir','geometry')

station_npmrds_match = find_closest_linkmatch(line_, point_, buffer = 100)
station_npmrds_match_clean = data.frame(cbind(station_npmrds_match$tmc, station_npmrds_match$ID.y))
names(station_npmrds_match_clean) = c('tmc', 'reference_station_id')
rm(line_, point_, crs_feet)
rm(station_npmrds_match)

######################### calculate hourly total count for each station, then calculate daily average count (excluding weekends) ######
reference_hrtotal_2019 = daily_avg_hr_total(reference_count_2019)
reference_hrtotal_2023 = daily_avg_hr_total(reference_count_2023)

reference_hrtotal_2019_am <- reference_hrtotal_2019 %>%
  filter(hour %in% c("06", "07", "08")) %>%
  pivot_wider(
    names_from = hour,
    values_from = avg_total,
    names_prefix = "tot_Vol"
  )
reference_hrtotal_2023_am <- reference_hrtotal_2023 %>%
  filter(hour %in% c("06", "07", "08")) %>%
  pivot_wider(
    names_from = hour,
    values_from = avg_total,
    names_prefix = "tot_Vol"
  )

######################## for each NPMRDS, get the hourly total average volume ###########
station_npmrds_match_spd_vol <- station_npmrds_match_clean %>%
  left_join(tmc_oct2024_workdayavg_AM %>%
              select(tmc_code, mean_SpdAM, mean_SpdTMC06, mean_SpdTMC07, mean_SpdTMC08, miles),
            by = c('tmc' = 'tmc_code'))

station_npmrds_match_spd_vol$reference_station_id = as.numeric(station_npmrds_match_spd_vol$reference_station_id)
station_npmrds_match_spd_vol_2019 <- station_npmrds_match_spd_vol %>%
  left_join(reference_hrtotal_2019_am, by = c('reference_station_id' = 'ID'))
station_npmrds_match_spd_vol_2023 <- station_npmrds_match_spd_vol %>%
  left_join(reference_hrtotal_2023_am, by = c('reference_station_id' = 'ID'))

############################ reference station data quality check ################################
station_npmrds_match_spd_vol_1923dup = rbind(station_npmrds_match_spd_vol_2019, station_npmrds_match_spd_vol_2023)
station_npmrds_match_spd_vol_1923dup = station_npmrds_match_spd_vol_1923dup[which(duplicated(station_npmrds_match_spd_vol_1923dup$reference_station_id)),c(1,2)]
station_npmrds_match_spd_vol_1923dup <- station_npmrds_match_spd_vol_1923dup %>%
  left_join(reference_hrtotal_2019_am %>%
              select(ID, tot_Vol06, tot_Vol07, tot_Vol08), by = c('reference_station_id' = 'ID'))

station_npmrds_match_spd_vol_1923dup$tot_Vol2019 = station_npmrds_match_spd_vol_1923dup$tot_Vol06 +
  station_npmrds_match_spd_vol_1923dup$tot_Vol07 + station_npmrds_match_spd_vol_1923dup$tot_Vol08

station_npmrds_match_spd_vol_1923dup <- station_npmrds_match_spd_vol_1923dup %>%
  left_join(reference_hrtotal_2023_am %>%
              select(ID, tot_Vol06, tot_Vol07, tot_Vol08), by = c('reference_station_id' = 'ID'))
station_npmrds_match_spd_vol_1923dup$tot_Vol2023 = station_npmrds_match_spd_vol_1923dup$tot_Vol06.y +
  station_npmrds_match_spd_vol_1923dup$tot_Vol07.y + station_npmrds_match_spd_vol_1923dup$tot_Vol08.y
# remove NA
station_npmrds_match_spd_vol_1923dup = station_npmrds_match_spd_vol_1923dup[which(!is.na(station_npmrds_match_spd_vol_1923dup$tot_Vol2019)),]
station_npmrds_match_spd_vol_1923dup = station_npmrds_match_spd_vol_1923dup[which(!is.na(station_npmrds_match_spd_vol_1923dup$tot_Vol2023)),]


metrics = calculate_metrics(station_npmrds_match_spd_vol_1923dup$tot_Vol2019, station_npmrds_match_spd_vol_1923dup$tot_Vol2023)
label <- paste0(
  "% Error: ", round(metrics$percent_error,2), '%', "\n",
  
  "RMSE: ", round(metrics$rmse,4), '\n',
  
  "% RSQ: ", round(metrics$r_squared * 100, 2), '%'
)

plot_black(station_npmrds_match_spd_vol_1923dup$tot_Vol2019, station_npmrds_match_spd_vol_1923dup$tot_Vol2023, 
           'AM total 2019', 'AM total 2023', 'Year-wise count comparison', label)
rm(reference_stations_shp, metrics, label, station_npmrds_match_spd_vol_1923dup)

