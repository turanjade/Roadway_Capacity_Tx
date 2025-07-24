### this file specifies the matching of npmrds and roadlink
library('arcgisbinding')
library('sf')
library(dplyr)
library(tidyr)
library('openxlsx')
library(geosphere)
library('DBI')
library('odbc')
library('ggplot2')
library(leaflet)


rd_ref_match = function(roadlink, refstation, buffer = 100) {
  # required col:
    # sidefire (shp, point): 
    #  [1] "ID"         "LINKNAME"   "DIR"       
    #  [12] "FUNCL"     "geometry"  
    # refstation
    # [1] "ID"       "Roadway"  "FUNCL"    "Dir"      "DateTime" "Total"    "geometry"
  crs_feet <- 2223  # example EPSG code for feet
  lines <- st_transform(roadlink_2019, crs_feet)
  points <- st_transform(refstation_2019, crs_feet)
  
  # select required columns and reformat
  lines = lines %>%
    transmute(ID_rd = ID, STREET_rd = STREET, FUNCL_rd = FUNCL)
  points = points %>%
    transmute(ID_ref = ID, STREET_ref = Roadway, FUNCL_ref = FUNCL, DIR_ref = Dir)
  
  # line_$dir = gsub(" ", "", line_$dir)
  points$DIR_ref = gsub(" ", "", points$DIR_ref)
  
  # Buffer points by 100 feet
  points_buffer <- st_buffer(points, dist = buffer) # ease to 150? -- no, keep consistent with Francisco
  
  # Spatial join: get all lines within 100 ft of any point
  # This creates a row for each line-point pair
  lines_within_100ft <- st_join(lines, points_buffer, join = st_intersects, left = FALSE)
  
  # option 2 is to use street name. First use FUNCL 
  lines_within_100ft = lines_within_100ft[which(!is.na(lines_within_100ft$ID_rd)),] # remove na
  
  ref_id = unique(lines_within_100ft$ID_ref)
  
  point_linkmatch = data.frame(matrix(0, nrow = 0, ncol = ncol(lines_within_100ft)))
  
  for (i in 1:length(ref_id)) {
    link_i = lines_within_100ft[which(lines_within_100ft$ID_ref == ref_id[i]),]
    if (nrow(link_i) == 0) {
      print(paste('reference station', ref_id[i],'cannot find matched street link within 100ft buffer'))
      next
    }
    match_i = matrix(0, nrow = 0, ncol = 2)
    for (j in 1:nrow(link_i)) {
      #initiate dir and funcl decision factor as 0. If it becomes 1 after the check, then it passes
      # dir_j = 0; use name instead of dir, needs to be revised
      funcl_j = 0 
      ## check funcl 
      if (link_i$FUNCL_rd[j] == link_i$FUNCL_ref[j]) {
        funcl_j = 1
      } 
      if (funcl_j == 0) {
        next
      }
      ## check dir by street name & sidefire name # linkname is from sidefire detector, street is from roadlink
      #if (isTRUE(grepl('N', link_i$DIR_ref[j])) & isTRUE(grepl('NB', link_i$STREET_rd[j])) | 
      #    isTRUE(grepl('S', link_i$DIR_ref[j])) & isTRUE(grepl('SB', link_i$STREET_rd[j])) | 
      #    isTRUE(grepl('W', link_i$DIR_ref[j])) & isTRUE(grepl('WB', link_i$STREET_rd[j])) | 
      #    isTRUE(grepl('E', link_i$DIR_ref[j])) & isTRUE(grepl('EB', link_i$STREET_rd[j]))) {
      #  dir_j = 1 # pass dir check
      #} 

      # combine the distance with two criteria check
      match_i = rbind(match_i, c(funcl_j, 
                                 st_length(st_nearest_points(points[which(points$ID_ref == ref_id[i]),], 
                                                             lines[which(lines$ID_rd == link_i$ID_rd[j]),])))) 
      
    }
    choose_i = which(match_i[,1] != 0 & match_i[,2] == min(match_i[,2]))
    rowtocombine = link_i[choose_i,]
    
    point_linkmatch = rbind(point_linkmatch, rowtocombine)
  }
  colnames(point_linkmatch) = colnames(lines_within_100ft)
  point_linkmatch = point_linkmatch[which(point_linkmatch$FUNCL_rd == point_linkmatch$FUNCL_ref),]
  return(point_linkmatch)
}



rd_sf_match = function(roadlink, sidefire, buffer = 100) {
  
  # required col:
  # sidefire (shp, point): 
  #  [1] "ID"         "LINKNAME"   "DIR"       
  #  [12] "FUNCL"     "geometry"  
  # roadlink (shp, polyline):
  #  [1] "ID"         "STREET"     "FUNCL"      "DIVID"  "geometry" 
  
  crs_feet <- 2223  # example EPSG code for feet
  lines <- st_transform(roadlink, crs_feet)
  points <- st_transform(sidefire, crs_feet)
  
  # select required columns and reformat
  lines = lines %>%
    transmute(ID_rd = ID, STREET_rd = STREET, FUNCL_rd = FUNCL)
  points = points %>%
    transmute(ID_sf = ID, STREET_sf = LINKNAME, FUNCL_sf = FUNCL, DIR_sf = DIR)
  
  # Buffer points by 100 feet
  points_buffer <- st_buffer(points, dist = buffer)  
  
  # Spatial join: get all lines within 100 ft of any point
  lines_within_100ft <- st_join(lines, points_buffer, join = st_intersects, left = FALSE)
  # delete NAs
  lines_within_100ft = lines_within_100ft[which(!is.na(lines_within_100ft$ID_sf)),] # type freeway in sidefire
  
  # sidefire_ID = unique(sidefire_2025_frwy$ID)
  point_line_match = data.frame(matrix(0, nrow = 0, ncol = 5))
  
  a = 0 
  b = 0
  # for each sf ID, find matched links
  ID_sf = unique(lines_within_100ft$ID_sf)
  for (i in 1:length(ID_sf)) {
    link_i = lines_within_100ft[which(lines_within_100ft$ID_sf == ID_sf[i]),]
    if (nrow(link_i) == 0) {
      a = a + 1
      print(paste('no nearby station', a))
      next
    }
    match_i = matrix(0, nrow = 0, ncol = 2)
    for (j in 1:nrow(link_i)) {
      #initiate dir and name decision factor as 0. If it becomes 1 after the check, then it passes
      dir_j = 0; name_j = 0 
      ## check dir by street name & sidefire name # linkname is from sidefire detector, street is from roadlink
      if (isTRUE(grepl('NB', link_i$STREET_sf[j])) & isTRUE(grepl('NB', link_i$STREET_rd[j])) | 
          isTRUE(grepl('SB', link_i$STREET_sf[j])) & isTRUE(grepl('SB', link_i$STREET_rd[j])) | 
          isTRUE(grepl('WB', link_i$STREET_sf[j])) & isTRUE(grepl('WB', link_i$STREET_rd[j])) | 
          isTRUE(grepl('EB', link_i$STREET_sf[j])) & isTRUE(grepl('EB', link_i$STREET_rd[j]))) {
        dir_j = 1 # pass dir check
      } 
      
      ## check check strict street name #
      # street name can be NA, if NA, next
      if (is.na(strsplit(link_i$STREET_rd[j], '[-. ]+')[[1]][1]) | is.na(strsplit(link_i$STREET_sf[j],"[-. ]+")[[1]][1])) {
        b = b + 1
        print(paste('street name NA', b))
        next
      }
      if (strsplit(link_i$STREET_sf[j],"[-. ]+")[[1]][1] == strsplit(link_i$STREET_rd[j], '[-. ]+')[[1]][1]) {
        name_j = 1
      } 
      ## check nicknames of highway #
      #### SRT refers to Sam Rayburn tollway, SH121; PGBT refers to President George Bush Turnpike,  SH 190
      if (strsplit(link_i$STREET_sf[j],"[-. ]+")[[1]][1] == 'SH121' & strsplit(link_i$STREET_rd[j], '[-. ]+')[[1]][1] == 'SRT' |
          strsplit(link_i$STREET_sf[j],"[-. ]+")[[1]][1] == 'SH190' & strsplit(link_i$STREET_rd[j], '[-. ]+')[[1]][1] == 'PGBT') {
        name_j = 1
      } 
      #### IH can be I
      if (grepl("^I(?!H)",strsplit(link_i$STREET_sf[j],"[-. ]+")[[1]][1], perl = T) &
          sub("^I([^H])", "IH\\1", strsplit(link_i$STREET_sf[j],"[-. ]+")[[1]][1]) == strsplit(link_i$STREET_rd[j], '[-. ]+')[[1]][1]) {
        name_j = 1
      }
      
      # combine the distance with two criteria check
      match_i = rbind(match_i, c(dir_j*name_j, st_length(st_nearest_points(points[which(points$ID_sf == ID_sf[i]),], 
                                                                           lines[which(lines$ID_rd == link_i$ID_rd[j]),])))) 
      
    }
    choose_i = which(match_i[,1] != 0 & match_i[,2] == min(match_i[,2]))
    rowtocombine = c(link_i$ID_sf[choose_i], link_i$STREET_sf[choose_i], link_i$ID_rd[choose_i], link_i$STREET_rd[choose_i], match_i[choose_i,2])
    point_line_match = rbind(point_line_match, rowtocombine)
    rm(match_i, choose_i, dir_j, name_j, rowtocombine, link_i)
  }
  colnames(point_line_match) = c('ID_sf','STREET_sf','ID_rd','STREET_rd','distance')
  point_line_match$distance = as.numeric(point_line_match$distance)
  
  return(point_line_match)
}

###################### match link #############################
setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

### read base data
sidefire_2025 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\Sidefire_2025\\sidefire_2025.shp')
roadlink_2019 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\roadlink_2019.shp')
roadlink_2026 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\road_shp_addedinfo.shp')
refcount_2019 = read.csv('20250410_capacity_recalculation\\Database\\hourlycount_reference_2019.csv')
refcount_2014 = read.csv('20250410_capacity_recalculation\\Database\\hourlycount_reference_2014.csv')

refstation_2014 = refcount_2014[which(!duplicated(refcount_2014$ID)),]
refstation_2019 = refcount_2019[which(!duplicated(refcount_2019$ID)),]

# convert csv data to shp
refstation_2014$Longitude = refstation_2014$Longitude/1e6
refstation_2014$Latitude = refstation_2014$Latitude/1e6
refstation_2019$Longitude = refstation_2019$Longitude/1e6
refstation_2019$Latitude = refstation_2019$Latitude/1e6

refstation_2014 = st_as_sf(refstation_2014, 
                                  coords = c("Longitude", "Latitude"), crs = 4326)
refstation_2019 = st_as_sf(refstation_2019, 
                           coords = c("Longitude", "Latitude"), crs = 4326)

##### for freeway, use sidefire and roadlink, the expected result is for each roadlink, match a sidefire detector ####
road_sidefire_match_2019 = rd_sf_match(roadlink_2019, sidefire_2025, buffer = 100)
road_refstation_match_2019 = rd_ref_match(roadlink_2019, refstation_2019, buffer = 100)


