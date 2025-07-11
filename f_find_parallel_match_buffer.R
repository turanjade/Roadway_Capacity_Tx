library(sf)
library(dplyr)
library(geosphere)

find_parallel_match_buffer <- function(A, B, # A: roadlink, B: NPMRDS
                                buffer_dist = 100,
                                angle_thresh = 15) {
  
  
  # 1. Reproject to EPSG:2276 (Texas North Central feet)
  A <- st_transform(A, 2276)
  B <- st_transform(B, 2276)
  
  # 2. Define a helper function to calculate line direction
  get_angle <- function(line) {
    coords <- st_coordinates(line)
    start <- coords[1, ]
    end <- coords[nrow(coords), ]
    angle <- atan2(end["Y"] - start["Y"], end["X"] - start["X"]) * 180 / pi
    angle %% 360  # Normalize to [0, 360)
  }
  
  A$angle <- sapply(st_geometry(A), get_angle)
  B$angle <- sapply(st_geometry(B), get_angle)
  
  # 3. Buffer layer by 100 ft
  A_buffered <- st_buffer(A, dist = buffer_dist)
  
  # 4. Find B links that intersect A's buffer
  AB_join <- st_join(A_buffered, B, join = st_intersects, left = FALSE)
  
  # 5. Compute angle difference and classify direction
  AB_join <- AB_join %>%
    mutate(
      angle_diff = abs(angle.x - angle.y),
      angle_diff = ifelse(angle_diff > 180, 360 - angle_diff, angle_diff),
      direction_type = case_when(
        angle_diff <= angle_thresh ~ "forward",
        abs(angle_diff - 180) <= angle_thresh ~ "reverse",
        TRUE ~ "none"
      )
    ) %>%
    filter(direction_type != "none")
  
  # 6. Keep one forward and one reverse match per link in A
  # AB_best <- AB_join %>%
  #   group_by(A_id = row_number()) %>%  # or use a unique ID from A
  #   slice_min(order_by = angle_diff, n = 1, with_ties = FALSE) %>%
  #   ungroup()
  
  AB_matches <- AB_join %>%
    group_by(A_id = row_number(), direction_type) %>%
    slice_min(order_by = angle_diff, n = 1, with_ties = FALSE) %>%
    ungroup()
  
   
  return(AB_matches)
}

