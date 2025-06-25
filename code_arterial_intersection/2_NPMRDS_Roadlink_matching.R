######################## match parallel roadway link, find capacity and FFspd ######### # capacity is wrong, just use as a reference

# needs to check, if they can match a good fit between two link layers 
source("20250410_capacity_recalculation/Roadway_Capacity_Tx\\code_arterial_intersection\\func_find_parallel_match.R")
source("20250410_capacity_recalculation/Roadway_Capacity_Tx\\code_arterial_intersection\\background_run_roadlink_npmrds_match.R")

roadlink_npmrds_match = data.frame(cbind(roadlink_npmrds_match$ID, roadlink_npmrds_match$tmc, roadlink_npmrds_match$Funcl,
                                         roadlink_npmrds_match$miles, 
                                         roadlink_npmrds_match$ID.1, roadlink_npmrds_match$FUNCL,
                                         roadlink_npmrds_match$AMLN_AB, roadlink_npmrds_match$AMLN_BA, 
                                         roadlink_npmrds_match$PKFRSPD_A, roadlink_npmrds_match$PKFRSPD_B,
                                         roadlink_npmrds_match$AMHRCAP_A, roadlink_npmrds_match$AMHRCAP_B,
                                         roadlink_npmrds_match$start_longitude, roadlink_npmrds_match$start_latitude,
                                         roadlink_npmrds_match$end_longitude, roadlink_npmrds_match$end_latitude))
colnames(roadlink_npmrds_match) = c('ID_tmc','tmc','funcl_tmc','miles','ID_rdwy','funcl_rdwy',
                                    'amlane_ab','amlane_ba','pkffspd_ab','pkffspd_ba','amhrcap_ab','amhrcap_ba',
                                    'start_lon','start_lat','end_lon','end_lat')

roadlink_npmrds_vol_spd = roadlink_npmrds_match %>%
  left_join(station_npmrds_match_spd_vol_2019, by = 'tmc')

### updated Jun 23, 2025, use npmrds-rdwy link match from Zihao to get:
##### -- RDWY ID, FUNCL, FFSPD, LANE, Area code, Length, avg speed AM & hourly, avg count AM & hourly
roadlink_npmrds_match = read.csv("20250410_capacity_recalculation\\RoadNetwork_2026\\Sensor_count\\rdwyID_tmc_matching.csv", header = T)
roadlink_2026_simple = data.frame(cbind(roadlink_2026$ID, roadlink_2026$LENGTH, 
                                        roadlink_2026$FUNCL, roadlink_2026$STREET, 
                                        roadlink_2026$PKFRSPD_A, roadlink_2026$AMHRCAP_A,
                                        roadlink_2026$AMLN_AB, roadlink_2026$AREATYP))
colnames(roadlink_2026_simple) = c('ID','Length','FUNCL','STREET','FFSPD',
                                   'HRCAP','Lane','Area')
## left join roadlink mrdwy and roadlink assignment result -- as a comparison
roadlink_2026_simple$ID = as.numeric(roadlink_2026_simple$ID)
roadlink_2026_simple <- roadlink_2026_simple %>%
  left_join(roadlink_26_mma %>%
              select(ID1, AB_Time, BA_Time, AB_VOC, BA_VOC), by = c('ID' = 'ID1'))

## left join roadlink information with roadlink ID and npmrds
roadlink_npmrds_match <- roadlink_npmrds_match %>%
  left_join(roadlink_2026_simple, by = 'ID')

## calculate average vol of each npmrds
npmrds_match_spd_vol_2019_stationavg <- station_npmrds_match_spd_vol_2019 %>%
  group_by(tmc) %>%
  summarise(across(mean_SpdAM:tot_Vol08, ~mean(.x, na.rm = TRUE)), .groups = "drop")

## left join station & npmrds to roadlink & npmrds - 0
roadlink_npmrds_station_2019_0 <- roadlink_npmrds_match %>%
  left_join(npmrds_match_spd_vol_2019_stationavg, by = c('tmc_0' = 'tmc'))

## left join station & npmrds to roadlink & npmrds - 180
roadlink_npmrds_station_2019_180 <- roadlink_npmrds_match %>%
  left_join(npmrds_match_spd_vol_2019_stationavg, by = c('tmc_180' = 'tmc'))

## delete NAs in both dir
roadlink_npmrds_station_2019_0 = roadlink_npmrds_station_2019_0[which(
  !is.na(roadlink_npmrds_station_2019_0$tot_Vol07)),]
roadlink_npmrds_station_2019_180 = roadlink_npmrds_station_2019_180[which(
  !is.na(roadlink_npmrds_station_2019_180$tot_Vol07)),]

## calculate average count of three columns
roadlink_npmrds_station_2019_0$mean_VolAM = rowMeans(
  cbind(  roadlink_npmrds_station_2019_0$tot_Vol06, 
          roadlink_npmrds_station_2019_0$tot_Vol07,
          roadlink_npmrds_station_2019_0$tot_Vol08), na.rm = T)

roadlink_npmrds_station_2019_180$mean_VolAM = rowMeans(
  cbind(  roadlink_npmrds_station_2019_180$tot_Vol06, 
          roadlink_npmrds_station_2019_180$tot_Vol07,
          roadlink_npmrds_station_2019_180$tot_Vol08), na.rm = T)

## calculate average travel time of AM
roadlink_npmrds_station_2019_0$mean_TTAM = roadlink_npmrds_station_2019_0$miles/
  roadlink_npmrds_station_2019_0$mean_SpdAM * 3600
roadlink_npmrds_station_2019_180$mean_TTAM = roadlink_npmrds_station_2019_180$miles/
  roadlink_npmrds_station_2019_180$mean_SpdAM * 3600

## delete those length < 0.5 miles
roadlink_npmrds_station_2019_0 = roadlink_npmrds_station_2019_0[which(
  roadlink_npmrds_station_2019_0$miles > 0.5),]
roadlink_npmrds_station_2019_180 = roadlink_npmrds_station_2019_180[which(
  roadlink_npmrds_station_2019_180$miles > 0.5),]

rm(npmrds_match_spd_vol_2019_stationavg)

################################## NPMRDS count, speed, ffspd data quality check #################################
plot(roadlink_npmrds_station_2019_0$mean_SpdAM[which(roadlink_npmrds_station_2019_0$FFSPD == 30)],
     roadlink_npmrds_station_2019_0$mean_VolAM[which(roadlink_npmrds_station_2019_0$FFSPD == 30)]/
       as.numeric(roadlink_npmrds_station_2019_0$Lane[which(roadlink_npmrds_station_2019_0$FFSPD == 30)]), 
     col = 'white', pch = 16)
points(roadlink_npmrds_station_2019_0$mean_SpdAM[which(roadlink_npmrds_station_2019_0$FFSPD == 35)],
       roadlink_npmrds_station_2019_0$mean_VolAM[which(roadlink_npmrds_station_2019_0$FFSPD == 35)]/
         as.numeric(roadlink_npmrds_station_2019_0$Lane[which(roadlink_npmrds_station_2019_0$FFSPD == 35)]), 
       col = 'yellow', pch = 16)
points(roadlink_npmrds_station_2019_0$mean_SpdAM[which(roadlink_npmrds_station_2019_0$FFSPD == 40)],
       roadlink_npmrds_station_2019_0$mean_VolAM[which(roadlink_npmrds_station_2019_0$FFSPD == 40)]/
         as.numeric(roadlink_npmrds_station_2019_0$Lane[which(roadlink_npmrds_station_2019_0$FFSPD == 40)]), 
       col = 'red', pch = 16)
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

table(roadlink_npmrds_station_2019_0$FUNCL, roadlink_npmrds_station_2019_0$Area)
ggplot(roadlink_npmrds_station_2019_0, aes(
  x = factor(FFSPD), y = as.numeric(mean_VolAM)/as.numeric(HRCAP), fill = factor(FUNCL))) + 
  geom_boxplot(color = "white") +
  xlab('FFSPD (mph)') + ylab('actual Vol/Model cap') + 
  theme_black()

ggplot(roadlink_npmrds_station_2019_0, aes(
  x = factor(FFSPD), y = as.numeric(mean_VolAM)/as.numeric(Lane), fill = factor(FUNCL))) + 
  geom_boxplot(color = "white") +
  xlab('FFSPD (mph)') + ylab('actual Vol') + 
  theme_black()

ggplot(roadlink_npmrds_station_2019_0, aes(
  x = factor(FFSPD), y = as.numeric(mean_SpdAM), fill = factor(FUNCL))) + 
  geom_boxplot(color = "white") +
  xlab('FFSPD (mph)') + ylab('actual speed (mph)') + 
  theme_black()



