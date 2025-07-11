library(sf)
library(dplyr)
library(lwgeom)

find_parallel_match_buffer <- function(layer_a, layer_b, # A: roadlink, B: NPMRDS
                                buffer_dist = 100,
                                angle_thresh = 15,
                                endpoint_tol = 5) {
  
  
  layer_a = roadlink_2019[which(roadlink_2019$FUNCL == 2 |
                                                 roadlink_2019$FUNCL == 3 |
                                                 roadlink_2019$FUNCL == 4),]
  layer_b = tmc_2024loc_line[which(tmc_2024loc_line$Funcl == 2 |
                                                  tmc_2024loc_line$Funcl == 3 |
                                                  tmc_2024loc_line$Funcl == 4),]
  
    # Load and project layers
  layer_a <- st_transform(layer_a, 2277)
  layer_b <- st_transform(layer_b, 2277)
  
  # Create 100ft buffer for all A features
  a_buffers <- st_buffer(layer_a, buffer_dist)
  
  # Union all buffers to reduce overlaps and speed up
  buffer_union <- st_union(a_buffers)
  
  # Filter B features that intersect any A buffer
  b_pool <- layer_b[st_intersects(layer_b, buffer_union, sparse = FALSE), ]
  # b_pool <- st_join(a_buffers, layer_b, join = st_intersects, left = FALSE)
  print('potential B selected')
  
  # Function to compute line bearing
  bearing <- function(geom) {
    coords <- st_coordinates(geom)
    
    # If MultiLineString, only use first linestring part
    if ("L1" %in% colnames(coords)) {
      coords <- coords[coords[, "L1"] == 1, ]
    }
    
    if (nrow(coords) < 2) return(NA)  # not enough points for a bearing
    
    dx <- coords[nrow(coords), 1] - coords[1, 1]
    dy <- coords[nrow(coords), 2] - coords[1, 2]
    angle <- atan2(dy, dx) * 180 / pi
    
    # Normalize to [0, 360)
    angle <- (angle + 360) %% 360
    return(angle)
  }
  
  
  
  # Extract endpoints for B layer once
  b_coords <- st_coordinates(b_pool)
  b_geom_list <- st_geometry(b_pool)
  
  b_start_pts <- st_point_on_surface(st_linesubstring(b_geom_list, 0.0, 0.00001))
  b_end_pts   <- st_point_on_surface(st_linesubstring(b_geom_list, 0.99999, 1.0))
  
  # Result list
  results <- list()
  
  for (i in seq_len(nrow(layer_a))) {
    a <- layer_a[i, ]
    a_geom <- st_geometry(a)
    a_id <- a$ID
    a_dir <- a$DIR
    
    a_coords <- st_coordinates(a_geom)
    a_start <- a_coords[1, ]
    a_end <- a_coords[nrow(a_coords), ]
    
    b_candidates <- b_pool[st_intersects(b_pool, st_buffer(a_geom, 100), sparse = FALSE), ]
    if (nrow(b_candidates) == 0) next
    
    # Exclude B lines that connect to A's endpoints
    b_starts <- st_coordinates(st_point_on_surface(st_linesubstring(st_geometry(b_candidates), 0.0, 0.00001)))
    b_ends   <- st_coordinates(st_point_on_surface(st_linesubstring(st_geometry(b_candidates), 0.99999, 1.0)))
    
    b_candidates <- b_candidates[!(
      sqrt((b_starts[,1] - a_start[1])^2 + (b_starts[,2] - a_start[2])^2) < endpoint_tol |
        sqrt((b_starts[,1] - a_end[1])^2   + (b_starts[,2] - a_end[2])^2)   < endpoint_tol |
        sqrt((b_ends[,1]   - a_start[1])^2 + (b_ends[,2]   - a_start[2])^2) < endpoint_tol |
        sqrt((b_ends[,1]   - a_end[1])^2   + (b_ends[,2]   - a_end[2])^2)   < endpoint_tol
    ), ]
    
    if (nrow(b_candidates) == 0) next
    
    a_bearing <- bearing(a_geom)
    b_candidates$bearing <- sapply(st_geometry(b_candidates), bearing)
    b_candidates <- b_candidates %>% filter(!is.na(bearing))
    
    b_candidates$direction <- ifelse(
      abs(b_candidates$bearing - a_bearing) < angle_thresh, "forward",
      ifelse(abs(abs(b_candidates$bearing - a_bearing) - 180) < angle_thresh, "reverse", "other")
    )
    
    b_candidates <- b_candidates %>% filter(direction %in% c("forward", "reverse"))
    if (nrow(b_candidates) == 0) next
    
    b_candidates$perp_dist <- as.numeric(st_distance(st_geometry(b_candidates), a_geom))
    
    matches <- list()
    
    if (a_dir == 1) {
      # One-direction A: find closest forward B
      b_forward <- b_candidates %>%
        filter(direction == "forward") %>%
        slice_min(perp_dist, n = 1)
      if (nrow(b_forward) > 0) {
        b_forward$A_ID <- a_id
        matches <- list(b_forward)
      }
    } else {
      # Two-direction A: get both forward and reverse from same location
      # Step 1: find closest forward B
      b_forward <- b_candidates %>%
        filter(direction == "forward") %>%
        slice_min(perp_dist, n = 1)
      
      if (nrow(b_forward) > 0) {
        # Step 2: find B features with exact same geometry
        b_overlap_group <- b_candidates[st_equals(b_candidates, b_forward, sparse = FALSE)[,1], ]
        
        # Step 3: filter this group for both directions
        b_forward2 <- b_overlap_group %>% filter(direction == "forward") %>% slice_min(perp_dist, n = 1)
        b_reverse2 <- b_overlap_group %>% filter(direction == "reverse") %>% slice_min(perp_dist, n = 1)
        
        if (nrow(b_forward2) > 0) b_forward2$A_ID <- a_id
        if (nrow(b_reverse2) > 0) b_reverse2$A_ID <- a_id
        
        matches <- list(b_forward2, b_reverse2)
      }
    }
    
    if (length(matches) > 0) {
      results[[i]] <- bind_rows(matches)
    }
  }
  
  matched_b <- do.call(bind_rows, results)
  
  
   
  return(matched_b)
}

