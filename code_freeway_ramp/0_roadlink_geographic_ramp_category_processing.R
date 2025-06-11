library('arcgisbinding')
library('sf')
library('dplyr')

setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

# roadlink_intersect = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\road_areatype_table.csv', header = T)

#define the areatype of each road link
#basic logic: for each link, find the unique area type. If it intersects with several area types, use the link length to define (the area type that occupies the longest distance)

shapefile_path <- "20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\road_shp_addedinfo.shp"
roadlink <- st_read(shapefile_path)

# Check the coordinate reference system (CRS)
st_crs(roadlink)

# If in geographic (degrees), reproject to a projected CRS (e.g., UTM)
roadlink_project <- st_transform(roadlink, crs = 32633)  # Example: UTM zone 33N (EPSG:32633)

# Calculate lengths in meters
roadlink_project$length_m <- as.numeric(st_length(roadlink_project))


# check ramp connection: f2f, f2a, a2a. Freeway ramp FUNCL 6. 
# since the link layer does not have starting-ending notes, generate a node layer on top of the link layer
# Extract start and end node coordinates
node_coords <- lapply(st_geometry(roadlink), function(geom) {
  coords <- st_coordinates(geom)
  list(
    x_start = coords[1, "X"],
    y_start = coords[1, "Y"],
    x_end = coords[nrow(coords), "X"],
    y_end = coords[nrow(coords), "Y"]
  )
})

# Combine into a data frame
node_df <- do.call(rbind, lapply(node_coords, as.data.frame))
roadlink <- cbind(roadlink, node_df)

# Create unique nodes and assign node IDs
# Combine start and end node coordinates
all_nodes <- data.frame(
  x = c(roadlink$x_start, roadlink$x_end),
  y = c(roadlink$y_start, roadlink$y_end)
)

# Get unique nodes with IDs
unique_nodes <- all_nodes %>%
  distinct(x, y) %>%
  mutate(node_id = row_number())

# Join node IDs back to links
# Add from_node_id
roadlink <- roadlink %>%
  left_join(unique_nodes, by = c("x_start" = "x", "y_start" = "y")) %>%
  rename(from_node_id = node_id)

# Add to_node_id
roadlink <- roadlink %>%
  left_join(unique_nodes, by = c("x_end" = "x", "y_end" = "y")) %>%
  rename(to_node_id = node_id)

# remove manage lane, FUNCL 8: HOV, 9: Rail, 10: Managed lanes
# roadlink = roadlink[which(roadlink$FUNCL < 8),]

# after joining, check the links with FUNCL 6, if any links sharing the same starting nodes are freeway & ending nodes are freeway
# create a ramp linkfile
ramp = roadlink[which(roadlink$FUNCL == 6),]
ramp$connect_type = 0

# first define six types: if the two ends of a ramp connect to the same tyep of links: R-R, R-F, R-A, F-A, F-F, A-A
for (i in 1:nrow(ramp)) {
  # one end: according to the from node of the ramp, find links connecting to the from node, excluding the ramp itself
  end_1 = roadlink[which((roadlink$from_node_id == ramp$from_node_id[i] & roadlink$ID != ramp$ID[i]) | 
                           (roadlink$to_node_id == ramp$from_node_id[i])),]
  # another end: according to the to node of the ramp, find links connecting to the to node, excluding the ramp itself
  end_2 = roadlink[which((roadlink$to_node_id == ramp$to_node_id[i] & roadlink$ID != ramp$ID[i]) | 
                           (roadlink$from_node_id == ramp$to_node_id[i])),]
  # assign type: each of the two ends only connect to a single type of links
  if (length(unique(end_1$FUNCL)) == 1 & length(unique(end_2$FUNCL)) == 1) {
    # check the type of two ends
    if (unique(end_1$FUNCL) == 6 & unique(end_2$FUNCL) == 6) {
      ramp$connect_type[i] = 1
    }
    else if ((unique(end_1$FUNCL) == 6 & unique(end_2$FUNCL) == 1) | 
             (unique(end_1$FUNCL) == 1 & unique(end_2$FUNCL) == 6)) {
      ramp$connect_type[i] = 2
    }
    else if ((unique(end_1$FUNCL) == 6 & unique(end_2$FUNCL) != 1 & unique(end_2$FUNCL) != 6) | 
             (unique(end_1$FUNCL) != 1 & unique(end_1$FUNCL) != 6 & unique(end_2$FUNCL) == 6)) {
      ramp$connect_type[i] = 3
    }
    else if ((unique(end_1$FUNCL) == 1 & unique(end_2$FUNCL) != 1 & unique(end_2$FUNCL) != 6) | 
             (unique(end_1$FUNCL) != 1 & unique(end_1$FUNCL) != 6 & unique(end_2$FUNCL) == 1)) {
      ramp$connect_type[i] = 4
    }
    if (unique(end_1$FUNCL) == 1 & unique(end_2$FUNCL) == 1) {
      ramp$connect_type[i] = 5
    }
    else if ((unique(end_1$FUNCL) != 1 & unique(end_1$FUNCL) != 6 & unique(end_2$FUNCL) != 1 & unique(end_2$FUNCL) != 6)) {
      ramp$connect_type[i] = 6
    }
  }
}

# for ramps connecting to more than one type, if any type is F without A, it is F; if A without F, it is A; if no A and no F, it is R; if A and F, it is AF
# six types: R-R, R-F, R-A, F-A, F-F, A-A, plus F-FA, A-FA, R-FA. 
# In the further categorization, need to check if F-FA can be regarded as FF or FA, if A-FA can be AA or FA
for (i in 1:nrow(ramp)) {
  # only look at ramps with connect_type = 0 from the last round
  if (ramp$connect_type[i] == 0) {
    # one end: according to the from node of the ramp, find links connecting to the from node, excluding the ramp itself
    end_1 = roadlink[which((roadlink$from_node_id == ramp$from_node_id[i] & roadlink$ID != ramp$ID[i]) | 
                             (roadlink$to_node_id == ramp$from_node_id[i])),]
    end_1_unique = unique(end_1$FUNCL)
    # another end: according to the to node of the ramp, find links connecting to the to node, excluding the ramp itself
    end_2 = roadlink[which((roadlink$to_node_id == ramp$to_node_id[i] & roadlink$ID != ramp$ID[i]) | 
                             (roadlink$from_node_id == ramp$to_node_id[i])),]
    end_2_unique = unique(end_2$FUNCL)
    # assign type: each of the two ends connect to more than a single type of links
    if (length(end_1_unique) >=1 | length(end_2_unique) >=1) {
      # both end F without A, F-F
      if (length(which(end_1_unique == 1)) >= 1 & length(which(end_1_unique != 1 & end_1_unique != 6)) == 0 &
          length(which(end_2_unique == 1)) >= 1 & length(which(end_2_unique != 1 & end_2_unique != 6)) == 0) {
        ramp$connect_type[i] = 5
      }
      # both end A without F, A-A
      else if (length(which(end_1_unique != 1 & end_1_unique != 6)) >= 1 & length(which(end_1_unique == 1)) == 0 &
               length(which(end_2_unique != 1 & end_2_unique != 6)) >= 1 & length(which(end_2_unique == 1)) == 0) {
        ramp$connect_type[i] = 6
      }
      # one end F without A, another end A without F, F-A
      else if (length(which(end_1_unique == 1)) == 0 & length(which(end_1_unique != 1 & end_1_unique != 6)) >= 1 & 
               length(which(end_2_unique == 1)) >= 1 & length(which(end_2_unique != 1 & end_2_unique != 6)) == 0) {
        ramp$connect_type[i] = 4
      }
      else if (length(which(end_1_unique == 1)) >= 1 & length(which(end_1_unique != 1 & end_1_unique != 6)) == 0 & 
               length(which(end_2_unique == 1)) == 0 & length(which(end_2_unique != 1 & end_2_unique != 6)) >= 1) {
        ramp$connect_type[i] = 4
      }
      # one end connects F & A, another end also F & A, FA-FA
      else if (length(which(end_1_unique == 1)) >= 1 & length(which(end_1_unique != 1 & end_1_unique != 6)) >= 1 & 
               length(which(end_2_unique == 1)) >= 1 & length(which(end_2_unique != 1 & end_2_unique != 6)) >= 1) {
        ramp$connect_type[i] = 7
      }
      # one end connects to F & A, another end F without A, FA-F --> F-A
      else if (length(which(end_1_unique == 1)) >= 1 & length(which(end_1_unique != 1 & end_1_unique != 6)) >= 1 & 
               length(which(end_2_unique == 1)) >= 1 & length(which(end_2_unique != 1 & end_2_unique != 6)) == 0) {
        ramp$connect_type[i] = 4
      }
      else if (length(which(end_1_unique == 1)) >= 1 & length(which(end_1_unique != 1 & end_1_unique != 6)) == 0 & 
               length(which(end_2_unique == 1)) >= 1 & length(which(end_2_unique != 1 & end_2_unique != 6)) >= 1) {
        ramp$connect_type[i] = 4
      }
      # one end connects to F & A, another end A without F, FA-A --> F-A
      else if (length(which(end_1_unique == 1)) >= 1 & length(which(end_1_unique != 1 & end_1_unique != 6)) >= 1 & 
               length(which(end_2_unique == 1)) == 0 & length(which(end_2_unique != 1 & end_2_unique != 6)) >= 1) {
        ramp$connect_type[i] = 4
      }
      else if (length(which(end_1_unique == 1)) == 0 & length(which(end_1_unique != 1 & end_1_unique != 6)) >= 1 & 
               length(which(end_2_unique == 1)) >= 1 & length(which(end_2_unique != 1 & end_2_unique != 6)) >= 1) {
        ramp$connect_type[i] = 4
      }
      # one end connects to only R, another end connect F without A, R-F
      else if (length(which(end_1_unique == 6)) >= 1 & length(which(end_1_unique != 1 & end_1_unique != 6)) == 0 & length(which(end_1_unique == 1)) == 0 & 
               length(which(end_2_unique == 1)) >= 1 & length(which(end_2_unique != 1 & end_2_unique != 6)) == 0) {
        ramp$connect_type[i] = 2
      }
      else if (length(which(end_1_unique == 1)) >= 1 & length(which(end_1_unique != 1 & end_1_unique != 6)) == 0 & 
               length(which(end_2_unique == 6)) >= 1 & length(which(end_2_unique != 1 & end_2_unique != 6)) == 0 & length(which(end_2_unique == 1)) == 0) {
        ramp$connect_type[i] = 2
      }
      # one end connects to only R, another end connect A without F, R-A
      else if (length(which(end_1_unique == 6)) >= 1 & length(which(end_1_unique != 1 & end_1_unique != 6)) == 0 & length(which(end_1_unique == 1)) == 0 & 
               length(which(end_2_unique == 1)) == 0 & length(which(end_2_unique != 1 & end_2_unique != 6)) >= 1) {
        ramp$connect_type[i] = 3
      }
      else if (length(which(end_1_unique == 1)) == 0 & length(which(end_1_unique != 1 & end_1_unique != 6)) >= 1 & 
               length(which(end_2_unique == 6)) >= 1 & length(which(end_2_unique != 1 & end_2_unique != 6)) == 0 & length(which(end_2_unique == 1)) == 0) {
        ramp$connect_type[i] = 3
      }
      # one end connects to only R, another end connect A and F, R-FA
      else if (length(which(end_1_unique == 6)) >= 1 & length(which(end_1_unique != 1 & end_1_unique != 6)) == 0 & length(which(end_1_unique == 1)) == 0 & 
               length(which(end_2_unique == 1)) >= 1 & length(which(end_2_unique != 1 & end_2_unique != 6)) >= 1) {
        ramp$connect_type[i] = 8
      }
      else if (length(which(end_1_unique == 1)) >= 1 & length(which(end_1_unique != 1 & end_1_unique != 6)) >= 1 & 
               length(which(end_2_unique == 6)) >= 1 & length(which(end_2_unique != 1 & end_2_unique != 6)) == 0 & length(which(end_2_unique == 1)) == 0) {
        ramp$connect_type[i] = 8
      }
    }
  }
}

# finally, the last type is that either end does not connect to any non-managed link, assign type as 9: NA-any
# ramp$connect_type[which(ramp$connect_type == 0)] = 9

table(ramp$connect_type)

# export .shp file
st_write(ramp, "20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\ramp_withtype.shp")
# st_write(roadlink, "20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\road_shp_addedinfo.shp")



#################################Comment out sections##########################################################################################
################# Too much types if considering both starting & ending nodes and whether 2-dir or 1-dir########################################
################# so in the updated code in 04/21/2025, the type is simplified#################################################################
###############################################################################################################################################

# First, ramp connection check. If any ramp link (FUNCL = 6) connects to another ramp
ramp$connect_ramp = 0 
# if connect_ramp = 0, it means the ramp does not connect to another ramp
# if connect_ramp = 1, it means one of the end of the ramp connects to another ramp,  links
# (delete) # if connect_ramp = 2, it means the ramp connects between two ramps, links
# (delete) # if connect_ramp = 3, it means the ramp connects between arterial and ramp, links
for (i in 1:nrow(ramp)) {
  if (length(which(ramp$to_node_id == ramp$from_node_id[i])) > 0 |
      length(which(ramp$from_node_id == ramp$from_node_id[i])) > 1 |
      length(which(ramp$from_node_id == ramp$to_node_id[i])) > 0 |
      length(which(ramp$to_node_id == ramp$to_node_id[i])) > 1) {
    ramp$connect_ramp[i] = 1
  }
}

# Second, identify if the very start & very end of the ramp is freeway or arterial
# 1) first, assign connect type for ramps of which the connect_ramp = 0 
ramp$connect_type = 0
for (i in 1:nrow(ramp)) {
  if (ramp$connect_ramp[i] == 0) { 
    # both starting & ending connect to freeway, type 1, f2f
    if (length(which(roadlink$to_node_id[which(roadlink$FUNCL == 1)] == ramp$from_node_id[i]))>0 &
        length(which(roadlink$from_node_id[which(roadlink$FUNCL == 1)] == ramp$to_node_id[i]))>0) {
      ramp$connect_type[i] = 1
    } 
    # starting & ending connect to different types, type 2, f2a
    else if (length(which(roadlink$to_node_id[which(roadlink$FUNCL != 1 & roadlink$FUNCL != 6)] == ramp$from_node_id[i]))>0 &
             length(which(roadlink$from_node_id[which(roadlink$FUNCL == 1)] == ramp$to_node_id[i]))>0) {
      ramp$connect_type[i] = 2
    }
    else if (length(which(roadlink$to_node_id[which(roadlink$FUNCL == 1)] == ramp$from_node_id[i]))>0 &
             length(which(roadlink$from_node_id[which(roadlink$FUNCL != 1 & roadlink$FUNCL != 6)] == ramp$to_node_id[i]))>0) {
      ramp$connect_type[i] = 2
    }
    # both starting & ending conenct to arterial, type 3, a2a
    else if (length(which(roadlink$to_node_id[which(roadlink$FUNCL != 1 & roadlink$FUNCL != 6)] == ramp$from_node_id[i]))>0 &
             length(which(roadlink$from_node_id[which(roadlink$FUNCL != 1 & roadlink$FUNCL != 6)] == ramp$to_node_id[i]))>0) {
      ramp$connect_type[i] = 3
    }
  }
}

# 2) for ramps connecting to another ramp, change the starting & ending node. 
# Update 04/18/2025, Note that some links are bi-directional (DIR = 0), in this case, the starting & ending from topological aspect is not the real driving starting & ending 
ramp$from_node_id_1 = ramp$from_node_id
ramp$to_node_id_1 = ramp$to_node_id
for (i in 1:10){# nrow(ramp)) {
  if (ramp$connect_ramp[i] == 1) {
    index = which(ramp$from_node_id == ramp$to_node_id[i])
    # error: multiple links connected, end loop
    if (length(index) > 1) {
      print(paste("warnings: multiple links connect to type I, i =",i))
      next
    }
    print(i)
    skip_flag = F # indicate whether we hit an error in the while loop
    
    # for the ramp starting with the ending node of ramp i, start loop until the ramp connects to freeway or arterial
    repeat {
      if (ramp$connect_ramp[index] == 3) {
        ramp$from_node_id_1[index] = ramp$from_node_id[i]
        ramp$to_node_id_1[i] = ramp$to_node_id[index]
        break
      }
      ramp$from_node_id_1[index] = ramp$from_node_id[i]
      ramp$to_node_id_1[i] = ramp$to_node_id[index]
      
      index_pre = index
      index = which(ramp$from_node_id == ramp$to_node_id[index_pre])
      
      # error: multiple links connected, end loop
      if (length(index) > 1) {
        print(paste("errors: multiple links connect to the ramp, linkid",ramp$ID[index_pre]))
        skip_flag = T
        break
      }
    }
    if (skip_flag) next
  }
}

# 2) identify the from / end node connects to freeway, arterial, or ramp
# ramp$connect_type: 
# f2f: 1, f2a: 2, a2a: 3, 
# f2r:4, a2r:5, r2f:6, r2a: 7 
# [4-5: a ramp from f/a connects to another two ramps]
# [6-7: a ramp connects to f/a, and this ramp is one of the ramps branched from an original ramp]
ramp$connect_type = 0
for (i in 1:nrow(ramp)) {
  if (length(which(roadlink$to_node_id[which(roadlink$FUNCL == 1)] == ramp$from_node_id_1[i]))>0 &
      length(which(roadlink$from_node_id[which(roadlink$FUNCL == 1)] == ramp$to_node_id_1[i]))>0) {
    ramp$connect_type[i] = 1
  } 
  else if (length(which(roadlink$to_node_id[which(roadlink$FUNCL != 1 & roadlink$FUNCL != 6)] == ramp$from_node_id_1[i]))>0 &
           length(which(roadlink$from_node_id[which(roadlink$FUNCL == 1)] == ramp$to_node_id_1[i]))>0) {
    ramp$connect_type[i] = 2
  }
  else if (length(which(roadlink$to_node_id[which(roadlink$FUNCL == 1)] == ramp$from_node_id_1[i]))>0 &
           length(which(roadlink$from_node_id[which(roadlink$FUNCL != 1 & roadlink$FUNCL != 6)] == ramp$to_node_id_1[i]))>0) {
    ramp$connect_type[i] = 2
  }
  else if (length(which(roadlink$to_node_id[which(roadlink$FUNCL != 1 & roadlink$FUNCL != 6)] == ramp$from_node_id_1[i]))>0 &
           length(which(roadlink$from_node_id[which(roadlink$FUNCL != 1 & roadlink$FUNCL != 6)] == ramp$to_node_id_1[i]))>0) {
    ramp$connect_type[i] = 3
  }
  else if (length(which(roadlink$to_node_id[which(roadlink$FUNCL == 1)] == ramp$from_node_id_1[i]))>0 &
           length(which(roadlink$from_node_id[which(roadlink$FUNCL == 6)] == ramp$to_node_id_1[i]))>0) {
    ramp$connect_type[i] = 4 
  }
  else if (length(which(roadlink$to_node_id[which(roadlink$FUNCL != 1 & roadlink$FUNCL != 6)] == ramp$from_node_id_1[i]))>0 &
           length(which(roadlink$from_node_id[which(roadlink$FUNCL == 6)] == ramp$to_node_id_1[i]))>0) {
    ramp$connect_type[i] = 5
  }
  else if (length(which(roadlink$to_node_id[which(roadlink$FUNCL == 6)] == ramp$from_node_id_1[i]))>0 &
           length(which(roadlink$from_node_id[which(roadlink$FUNCL == 1)] == ramp$to_node_id_1[i]))>0) {
    ramp$connect_type[i] = 6
  }
  else if (length(which(roadlink$to_node_id[which(roadlink$FUNCL == 6)] == ramp$from_node_id_1[i]))>0 &
           length(which(roadlink$from_node_id[which(roadlink$FUNCL != 1 & roadlink$FUNCL != 6)] == ramp$to_node_id_1[i]))>0) {
    ramp$connect_type[i] = 7
  }
}

rm(i, index, index_pre, skip_flag)

################################################################Comment out section#######################################################
###############################the quoted section has logic errors, stop running##########################################################
##########################################################################################################################################

