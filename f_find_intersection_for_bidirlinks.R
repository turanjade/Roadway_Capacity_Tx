library(sf)
library(dplyr)
library(geosphere)
library(tidyr)

match_link_movements <- function(links, nodes, dist_threshold = 20) {
  
  # the link should include ID, AMTIME_A, AMTIME_B, From.Longitude/Latitude, To.Longitude/Latitude
  # Filter valid movement approaches
  # nodes_incoming <- nodes %>% filter(movement_type == "incoming")
  # Helper function to extract endpoints
  # the link should include from and to nodes, no need to add new 
  # get_endpoints <- function(line) {
  #  coords <- st_coordinates(line)
  #  list(
  #    A = st_point(coords[1, ]),
  #    B = st_point(coords[nrow(coords), ])
  #  )
  #}
  
  # if abs value of from/to is > 180, convert by / 1e6
  if (abs(links$From.Latitude) > 180) {
    links$From.Latitude = links$From.Latitude / 1e6
  }
  if (abs(links$From.Longitude) > 180) {
    links$From.Longitude = links$From.Longitude / 1e6
  }
  if (abs(links$To.Latitude) > 180) {
    links$To.Latitude = links$To.Latitude / 1e6
  }
  if (abs(links$To.Longitude) > 180) {
    links$To.Longitude = links$To.Longitude / 1e6
  }
  
  # Expand AB
  ab_links <- links %>% filter(!is.na(AMTIME_A) & AMTIME_A != 0) %>%
    rowwise() %>%
    mutate(
      direction = "AB",
      start_pt = list(st_point(c(links$From.Longitude, links$From.Latitude))),
      end_pt = list(st_point(c(links$To.Longitude, links$To.Latitude))),
      bearing = bearingRhumb(st_coordinates(links$geometry)[1, 1:2], st_coordinates(links$geometry)[nrow(st_coordinates(links$geometry)), 1:2]) %% 360
    ) %>%
    ungroup()
  
  # Expand BA
  ba_links <- links %>% filter(!is.na(AMTIME_B) & AMTIME_B != 0) %>%
    rowwise() %>%
    mutate(
      direction = "BA",
      start_pt = list(st_point(c(links$To.Longitude, links$To.Latitude))),
      end_pt = list(st_point(c(links$From.Longitude, links$From.Latitude))),
      bearing = bearingRhumb(st_coordinates(links$geometry)[nrow(st_coordinates(links$geometry)), 1:2], st_coordinates(links$geometry)[1, 1:2]) %% 360
    ) %>%
    ungroup()
  
  # Combine and determine direction labels
  ab_links <- ab_links %>%
    mutate(
      compass = case_when(
        bearing >= 45 & bearing < 135 ~ "Eastbound",
        bearing >= 135 & bearing < 225 ~ "Southbound",
        bearing >= 225 & bearing < 315 ~ "Westbound",
        TRUE ~ "Northbound"
      )
    )
  
  ba_links <- ba_links %>%
    mutate(
      compass = case_when(
        bearing >= 45 & bearing < 135 ~ "Eastbound",
        bearing >= 135 & bearing < 225 ~ "Southbound",
        bearing >= 225 & bearing < 315 ~ "Westbound",
        TRUE ~ "Northbound"
      )
    )
  # Create sf object of end points
  end_points_ab <- st_sf(
    link_id = ab_links$ID,
    # node_id = ab_links$To.ID,
    direction = ab_links$direction,
    compass = ab_links$compass,
    geometry = st_sfc(ab_links$end_pt, crs = st_crs(links))
  )
  end_points_ba <- st_sf(
    link_id = ba_links$ID,
    # node_id = ba_links$To.ID,
    direction = ba_links$direction,
    compass = ba_links$compass,
    geometry = st_sfc(ba_links$end_pt, crs = st_crs(links))
  )
  
  # Spatial + directional join to node movements
  joined_ab <- end_points_ab %>%
    st_join(nodes, join = st_is_within_distance, dist = 20) %>%
    filter(compass == Approach)  # Match EB/WB/etc
  
  # Now we have matched movement info per direction (AB or BA)
  joined_wide_ab <- joined_ab %>%
    select(ID, direction, ID, compass) %>%
    pivot_wider(names_from = direction, values_from = c(ID, compass),
                names_sep = "_")
  
  joined_ba <- end_points_ba %>%
    st_join(nodes, join = st_is_within_distance, dist = 20) %>%
    filter(compass == Approach)  # Match EB/WB/etc
  
  # Now we have matched movement info per direction (AB or BA)
  joined_wide_ba <- joined_ba %>%
    select(ID, direction, ID, compass) %>%
    pivot_wider(names_from = direction, values_from = c(ID, compass),
                names_sep = "_")
  
  links_with_movements <- links %>%
    st_join(joined_wide_ab)
  links_with_movements <- links_with_movements %>%
    st_join(joined_wide_ba)
  
  if (nrow(joined_ab) == 0) {
    links_with_movements$ID_AB = 0;
    links_with_movements$compass_AB = 0
  }
  if (nrow(joined_ba) == 0) {
    links_with_movements$ID_BA = 0
    links_with_movements$compass_BA = 0
  }
  
  
  return(links_with_movements)
}
