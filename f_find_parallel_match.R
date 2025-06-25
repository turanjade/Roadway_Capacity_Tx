library(sf)
library(dplyr)
library(geosphere)

find_parallel_match <- function(link_a_sf, line_b_sf,
                                buffer_dist = 30,
                                angle_thresh = 15,
                                endpoint_thresh = 50) {
  
  # Ensure both are in projected CRS (for distance in meters)
  # if (st_is_longlat(link_a_sf)) {
  #  warning("Input is in geographic CRS. Reprojecting to EPSG:3857.")
    # link_a_sf <- st_transform(link_a_sf, 3857)
    # line_b_sf <- st_transform(line_b_sf, 3857)
  # }
  
  # Buffer link_a
  link_a_buffer <- st_buffer(link_a_sf, dist = buffer_dist)
  
  # Intersect: keep only line_b features within 30m
  line_b_near <- st_intersection(line_b_sf, link_a_buffer)
  if (nrow(line_b_near) == 0) return(NULL)
  
  # Get bearing of line
  get_bearing <- function(line) {
    coords <- st_coordinates(line)
    start <- coords[1, ]
    end <- coords[nrow(coords), ]
    bearing <- bearingRhumb(start[1:2], end[1:2]) %% 360
    return(bearing)
  }
  
  # Bearing of link_a
  link_a_bearing <- get_bearing(link_a_sf$geometry[[1]])
  
  # Endpoint function
  get_endpoints <- function(line) {
    coords <- st_coordinates(line)
    list(start = st_point(coords[1, ]), end = st_point(coords[nrow(coords), ]))
  }
  link_a_pts <- get_endpoints(link_a_sf$geometry[[1]])
  
  # Compute bearing vectorized (one value per row)
  line_b_near$bearing <- sapply(line_b_near$geometry, get_bearing)
  
  results <- line_b_near %>%
    rowwise() %>%
    mutate(
      bearing_diff = abs(link_a_bearing - bearing),
      bearing_diff = ifelse(bearing_diff > 180, 360 - bearing_diff, bearing_diff),
      bearing_diff_inverse = abs((link_a_bearing + 180) %% 360 - bearing),
      start_dist = as.numeric(st_distance(st_sfc(link_a_pts$start, crs = st_crs(link_a_sf)), 
                                          st_sfc(st_point(st_coordinates(geometry)[1, ]), crs = st_crs(link_a_sf)))),
      end_dist = as.numeric(st_distance(st_sfc(link_a_pts$end, crs = st_crs(link_a_sf)), 
                                        st_sfc(st_point(st_coordinates(geometry)[nrow(st_coordinates(geometry)), ]), crs = st_crs(link_a_sf))))
    ) %>%
    ungroup() %>%
    filter(
      bearing_diff < angle_thresh | bearing_diff_inverse < angle_thresh,
      start_dist < endpoint_thresh,
      end_dist < endpoint_thresh
    ) %>%
    mutate(total_score = bearing_diff + start_dist + end_dist) %>%
    arrange(total_score)
  
  # Return best match (or NULL)
  if (nrow(results) == 0) return(NULL)
  return(results)
}

