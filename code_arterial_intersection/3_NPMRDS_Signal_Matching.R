############################################### match NPMRDS with signal analytics ####################################
# for each link, create a 100-ft (cannot relax to 200) buffer at the ToNode, find intersections within, match link dir with approaching
# result: each link matches one intersection ID (Intersection.ID), one approach ID (approach.ID)
# npmrds: tmc_2024loc_line; intersection: signal_oct2024am_intersect_filtered

# tmc_2024loc_line_proj = st_transform(tmc_2024loc_line, crs = 2277)

# Step 1: Create point geometries from the end_lat and end_long, convert to crs 2277 for us ft
tmc_2024loc_endpoint = data.frame(cbind(tmc_2024loc_line$ID, tmc_2024loc_line$tmc, 
                                        tmc_2024loc_line$dir, tmc_2024loc_line$end_latitude, tmc_2024loc_line$end_longitude))
colnames(tmc_2024loc_endpoint) = c('ID','tmc','dir','end_latitude','end_longitude')
tmc_end_points_ft <- st_transform(st_as_sf(tmc_2024loc_endpoint, coords = c("end_longitude", "end_latitude"), crs = 4326), 2277)

# Step 2: Create 200-foot buffer around the end points
tmc_buffers <- st_buffer(tmc_end_points_ft, dist = 100)

# Step 4, convert intersection to shp, first map as 4326, then convert to 2277
signal_oct2024_intersect_shp = st_transform(st_as_sf(signal_oct2024am_approach_filtered, 
                                                     coords = c("Longitude", "Latitude"), crs = 4326), 2277)
# find points within each buffer
intersects_in_tmc_buffer = st_join(signal_oct2024_intersect_shp, tmc_buffers, join = st_within, left = F)
intersects_in_tmc_buffer$dir = gsub(" ", "", intersects_in_tmc_buffer$dir)

intersects_in_tmc_buffer$select = 0
for (i in 1:nrow(intersects_in_tmc_buffer)) {
  intersects_i = intersects_in_tmc_buffer[which(intersects_in_tmc_buffer$tmc == intersects_in_tmc_buffer$tmc[i]),]
  if (length(intersects_i) > 0) {
    intersects_id = unique(intersects_i$Intersection.ID)
    for (j in 1:length(intersects_id)) {
      # row = which(intersects_i$Intersection.ID == intersects_id[j])
      if ((intersects_i$Approach[j] == 'Southbound' & intersects_i$dir[j] == 'S') |
          (intersects_i$Approach[j] == 'Eastbound' & intersects_i$dir[j] == 'E') |
          (intersects_i$Approach[j] == 'Westbound' & intersects_i$dir[j] == 'W') |
          (intersects_i$Approach[j] == 'Northbound' & intersects_i$dir[j] == 'N')) {
        intersects_in_tmc_buffer$select[which(intersects_in_tmc_buffer$tmc == intersects_in_tmc_buffer$tmc[i] &
                                                intersects_in_tmc_buffer$Intersection.ID == intersects_id[j] &
                                                intersects_in_tmc_buffer$Approach == intersects_i$Approach[j])] = 1
      }
    }
  }
  
}

intersects_in_tmc_buffer = intersects_in_tmc_buffer[which(intersects_in_tmc_buffer$select == 1),]

# check duplicated match
any(duplicated(intersects_in_tmc_buffer$tmc))

rm(intersects_i, intersects_id, tmc_end_points_ft, tmc_2024loc_endpoint, tmc_buffers, signal_oct2024_intersect_shp, i, j)

#### for each TMC, get avg control delay, avg travel time, avg approaching speed, POG 
npmrds_station_roadlink_signal_2019_0 = roadlink_npmrds_station_2019_0 %>%
  left_join(intersects_in_tmc_buffer %>%
              select(Percent.Arrival.On.Green..POG., Travel.Time..Avg, Approach.Speed..Avg, Control.Delay..Avg, tmc),
            by = c('tmc_0' = 'tmc'))

npmrds_station_roadlink_signal_2019_180 = roadlink_npmrds_station_2019_180 %>%
  left_join(intersects_in_tmc_buffer %>%
              select(Percent.Arrival.On.Green..POG., Travel.Time..Avg, Approach.Speed..Avg, Control.Delay..Avg, tmc),
            by = c('tmc_180' = 'tmc'))

npmrds_station_roadlink_signal_2019_0 = npmrds_station_roadlink_signal_2019_0[which(!is.na(npmrds_station_roadlink_signal_2019_0$Percent.Arrival.On.Green..POG.)),]
npmrds_station_roadlink_signal_2019_180 = npmrds_station_roadlink_signal_2019_180[which(!is.na(npmrds_station_roadlink_signal_2019_180$Percent.Arrival.On.Green..POG.)),]

npmrds_station_roadlink_signal_2019_0$abdir = 0
npmrds_station_roadlink_signal_2019_180$abdir = 180

npmrds_station_roadlink_signal_2019 = data.frame(rbind(
  npmrds_station_roadlink_signal_2019_0, npmrds_station_roadlink_signal_2019_180
))

npmrds_station_roadlink_signal_2019$mean_TTAM_nodelay = npmrds_station_roadlink_signal_2019$mean_TTAM - npmrds_station_roadlink_signal_2019$Control.Delay..Avg


################################## within those matched TMCs, find TMCs that do not have intersections in between ############
tmc_loc_ft <- st_transform(tmc_2024loc_line, 2277)

signal_oct2024_intersect_shp = st_transform(st_as_sf(signal_oct2024am_intersect_filtered, 
                                                     coords = c("Longitude", "Latitude"), crs = 4326), 2277)
# Step 1: Find which nodes lie along which lines (within 100 foot)
node_on_line_pairs <- st_is_within_distance(signal_oct2024_intersect_shp, tmc_loc_ft, dist = 100)  # 1 foot tolerance
# Step 2: Flatten the result into a data frame
node_line_df <- do.call(rbind, lapply(seq_along(node_on_line_pairs), function(i) {
  if (length(node_on_line_pairs[[i]]) > 0) {
    data.frame(node_id = i, line_id = node_on_line_pairs[[i]])
  }
}))
# Step 3: Get coordinates of nodes and line endpoints
node_coords <- st_coordinates(signal_oct2024_intersect_shp)
line_endpoints <- lapply(st_geometry(tmc_loc_ft), function(line) {
  coords <- st_coordinates(line)
  list(start = coords[1, 1:2], end = coords[nrow(coords), 1:2])
})

# Step 4: Keep only node-line pairs where the node is NOT near an endpoint
tolerance = 100
internal_node_df <- node_line_df %>%
  rowwise() %>%
  filter({
    n_xy <- node_coords[node_id, 1:2]
    endpoints <- line_endpoints[[line_id]]
    dist_to_start <- sqrt(sum((n_xy - endpoints$start)^2))
    dist_to_end <- sqrt(sum((n_xy - endpoints$end)^2))
    dist_to_start > tolerance & dist_to_end > tolerance
  }) %>%
  ungroup()

# Step 5: Lines with internal nodes (i.e., middle nodes)
lines_with_internal_nodes <- tmc_loc_ft[unique(internal_node_df$line_id), ]

# Step 6: Lines without internal nodes
lines_without_internal_nodes <- tmc_loc_ft[!seq_len(nrow(tmc_loc_ft)) %in% unique(internal_node_df$line_id), ]

rm(tolerence, tolerance, internal_node_df, line_endpoints, node_coords, node_line_df, node_on_line_pairs, 
   tmc_loc_ft, tmc_line_buffers, endpoints_list, internal_nodes)

################################## from matched links, find links that do not have internal points ###############
npmrds_station_roadlink_signal_2019_nointernal <- npmrds_station_roadlink_signal_2019 %>%
  left_join(lines_without_internal_nodes %>%
              select(tmc, Funcl), by = c('tmc_0' = 'tmc'))
npmrds_station_roadlink_signal_2019_nointernal <- npmrds_station_roadlink_signal_2019_nointernal %>%
  left_join(lines_without_internal_nodes %>%
              select(tmc, Funcl), by = c('tmc_180' = 'tmc'))
npmrds_station_roadlink_signal_2019_nointernal = npmrds_station_roadlink_signal_2019_nointernal[
  !is.na(npmrds_station_roadlink_signal_2019_nointernal$Funcl.x | !is.na(npmrds_station_roadlink_signal_2019_nointernal$Funcl.y)),
]



################################## NPMRDS count, speed, ffspd data quality check #################################
#################### find neighborhood vol & speed, whether their relationship is consistent  
npmrds_paircheck = npmrds_station_roadlink_signal_2019_nointernal
npmrds_paircheck$mean_SpdAM_rev = 0; npmrds_paircheck$mean_VolAM_rev = 0
for (i in 1:nrow(npmrds_paircheck)) {
  if (npmrds_paircheck$abdir[i] == 0) {
    if (any(npmrds_paircheck$tmc_180 == npmrds_paircheck$tmc_180[i] &
              npmrds_paircheck$tmc_0 != npmrds_paircheck$tmc_0[i])) {
      npmrds_paircheck$mean_SpdAM_rev[i] = mean(npmrds_paircheck$mean_SpdAM[which(npmrds_paircheck$tmc_180 == npmrds_paircheck$tmc_180[i] &
                                                                               npmrds_paircheck$tmc_0 != npmrds_paircheck$tmc_0[i])])
      npmrds_paircheck$mean_VolAM_rev[i] = mean(npmrds_paircheck$mean_VolAM[which(npmrds_paircheck$tmc_180 == npmrds_paircheck$tmc_180[i] &
                                                                               npmrds_paircheck$tmc_0 != npmrds_paircheck$tmc_0[i])])
    }
  }
  if (npmrds_paircheck$abdir[i] == 180) {
    if (any(npmrds_paircheck$tmc_180 != npmrds_paircheck$tmc_180[i] &
            npmrds_paircheck$tmc_0 == npmrds_paircheck$tmc_0[i])) {
      npmrds_paircheck$mean_SpdAM_rev[i] = mean(npmrds_paircheck$mean_SpdAM[which(npmrds_paircheck$tmc_180 != npmrds_paircheck$tmc_180[i] &
                                                                               npmrds_paircheck$tmc_0 == npmrds_paircheck$tmc_0[i])])
      npmrds_paircheck$mean_VolAM_rev[i] = mean(npmrds_paircheck$mean_VolAM[which(npmrds_paircheck$tmc_180 != npmrds_paircheck$tmc_180[i] &
                                                                               npmrds_paircheck$tmc_0 == npmrds_paircheck$tmc_0[i])])
    }
  }
}
length(which(npmrds_paircheck$mean_SpdAM_rev == 0))
npmrds_paircheck = npmrds_paircheck[which(npmrds_paircheck$mean_SpdAM_rev != 0),c('tmc_0','tmc_180','mean_SpdAM','mean_VolAM','mean_SpdAM_rev','mean_VolAM_rev')]

length(unique(npmrds_station_roadlink_signal_2019_nointernal$tmc_0)) + length(unique(npmrds_station_roadlink_signal_2019_nointernal$tmc_180))

#################### plot check
par(bg = 'black')
row_1 = which(npmrds_station_roadlink_signal_2019_nointernal$FUNCL == 3 & npmrds_station_roadlink_signal_2019_nointernal$FFSPD == 40)
row_2 = which(npmrds_station_roadlink_signal_2019_nointernal$FUNCL == 3 & npmrds_station_roadlink_signal_2019_nointernal$FFSPD == 40)
plot(npmrds_station_roadlink_signal_2019_nointernal$mean_VolAM[row_1]/
       as.numeric(npmrds_station_roadlink_signal_2019_nointernal$Lane[row_1]),
     npmrds_station_roadlink_signal_2019_nointernal$mean_SpdAM[row_1],
     col = 'white', pch = 16, ylab = 'mean speed mph', xlab = 'hourly volume per lane', col.lab = "white", cex.lab = 2, font.lab = 2)
points(npmrds_station_roadlink_signal_2019_nointernal$mean_VolAM[row_2]/
         as.numeric(npmrds_station_roadlink_signal_2019_nointernal$Lane[row_2]), 
       npmrds_station_roadlink_signal_2019_nointernal$mean_SpdAM[row_2],
       col = 'yellow', pch = 16)
points(npmrds_station_roadlink_signal_2019_nointernal$mean_VolAM[which(npmrds_station_roadlink_signal_2019_nointernal$FUNCL == 4)]/
         as.numeric(npmrds_station_roadlink_signal_2019_nointernal$Lane[which(npmrds_station_roadlink_signal_2019_nointernal$FUNCL == 4)]), 
       npmrds_station_roadlink_signal_2019_nointernal$mean_SpdAM[which(npmrds_station_roadlink_signal_2019_nointernal$FUNCL == 4)],
       col = 'red', pch = 16)
axis(1, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)
axis(2, col = "white", col.axis = "white", cex.axis = 2, font.axis = 2)

ggplot(npmrds_station_roadlink_signal_2019_nointernal, aes(
  x = factor(FFSPD), y = as.numeric(mean_VolAM)/as.numeric(HRCAP), fill = factor(FUNCL))) + 
  geom_boxplot(color = "white") +
  xlab('FFSPD (mph)') + ylab('actual Vol/Model cap') + 
  theme_black()

ggplot(npmrds_station_roadlink_signal_2019_nointernal, aes(
  x = factor(FFSPD), y = as.numeric(mean_VolAM)/as.numeric(Lane), fill = factor(FUNCL))) + 
  geom_boxplot(color = "white") +
  xlab('FFSPD (mph)') + ylab('actual Vol') + 
  theme_black()

table(npmrds_station_roadlink_signal_2019_nointernal$FFSPD, npmrds_station_roadlink_signal_2019_nointernal$FUNCL)
ggplot(npmrds_station_roadlink_signal_2019_nointernal, aes(
  x = factor(FFSPD), y = as.numeric(mean_SpdAM), fill = factor(FUNCL))) + 
  geom_boxplot(color = "white") +
  xlab('FFSPD (mph)') + ylab('actual speed (mph)') + 
  theme_black()


