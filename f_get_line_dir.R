# function, for each line, get dir of that line by S N E W NE SW NW SE
getlinedir_8dir = function(lines) {
  # Get coordinates of first and last point of each line
  get_endpoints <- function(geom) {
    coords <- st_coordinates(geom)
    start <- coords[1, ]
    end   <- coords[nrow(coords), ]
    cbind(start_x = start[1], start_y = start[2],
          end_x = end[1], end_y = end[2])
  }
  
  endpoints <- do.call(rbind, lapply(lines$geometry, get_endpoints))
  lines <- cbind(lines, endpoints)
  
  # atan2 gives radians, convert to degrees
  lines <- lines %>%
    mutate(
      angle = (atan2(end_x - start_x, end_y - start_y) * 180 / pi) %% 360
    )
  
  get_dir <- function(angle) {
    if (angle >= 337.5 | angle < 22.5) return("N")
    else if (angle < 67.5) return("NE")
    else if (angle < 112.5) return("E")
    else if (angle < 157.5) return("SE")
    else if (angle < 202.5) return("S")
    else if (angle < 247.5) return("SW")
    else if (angle < 292.5) return("W")
    else return("NW")
  }
  
  dir <- sapply(lines$angle, get_dir)
  return(dir)
  
}

getlinedir_4dir = function(lines) {
  # Get coordinates of first and last point of each line
  get_endpoints <- function(geom) {
    coords <- st_coordinates(geom)
    start <- coords[1, ]
    end   <- coords[nrow(coords), ]
    cbind(start_x = start[1], start_y = start[2],
          end_x = end[1], end_y = end[2])
  }
  
  endpoints <- do.call(rbind, lapply(lines$geometry, get_endpoints))
  lines <- cbind(lines, endpoints)
  
  # atan2 gives radians, convert to degrees
  lines <- lines %>%
    mutate(
      angle = (atan2(end_x - start_x, end_y - start_y) * 180 / pi) %% 360
    )
  
  get_dir <- function(angle) {
    if (angle >= 315 | angle < 45) return("N")
    else if (angle < 135) return("E")
    else if (angle < 225) return("S")
    else return("W")
  }
  
  dir <- sapply(lines$angle, get_dir)
  return(dir)
  
}