library('arcgisbinding')
library('sf')
library('dplyr')

setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

# roadlink_intersect = read.csv('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\road_areatype_table.csv', header = T)

#define the areatype of each road link
#basic logic: for each link, find the unique area type. If it intersects with several area types, use the link length to define (the area type that occupies the longest distance)

shapefile_path <- "20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\road_shp_areatype.shp"
roadlink_intersect <- st_read(shapefile_path)

# Check the coordinate reference system (CRS)
st_crs(roadlink_intersect)

# If in geographic (degrees), reproject to a projected CRS (e.g., UTM)
roadlink_intersect_project <- st_transform(roadlink_intersect, crs = 32633)  # Example: UTM zone 33N (EPSG:32633)

# Calculate lengths in meters
roadlink_intersect_project$length_m <- as.numeric(st_length(roadlink_intersect_project))

# read roadlink without intersect
shapefile_path = "20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\road_shp.shp"
roadlink = st_read(shapefile_path)

# add a field "area type" to the road link
roadlink$areatype = 0

# define the area type in the initial roadlink shapefile with field areatype
for (i in 1:nrow(roadlink)) {
  areas = data.frame(roadlink_intersect_project[which(roadlink_intersect_project$ID == roadlink$ID[i]),])
  if (length(unique(areas$AREATYPE)) == 1) {
    roadlink$areatype[i] = unique(areas$AREATYPE)
  } else if (length(unique(areas$AREATYPE)) > 1) {
    roadlink$areatype[i] = areas$AREATYPE[which.max(areas$length_m)]
  }
}

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
roadlink <- bind_cols(roadlink, node_df)

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

# after joining, check the links with FUNCL 6, if any links sharing the same starting nodes are freeway & ending nodes are freeway
# roadlink$rampconnect = 0 #f2f: 1, f2a: 2, a2a: 3
# create a ramp linkfile
ramp = roadlink[which(roadlink$FUNCL == 6),]
# first define the upstream / downstream link if the link does not connect two ramp links (i.e., identify the beginning & ending session of a ramp)
ramp$connecttype = 0 # determine if the ramp connects with freeway, arterial, or ramp
for (i in 1:nrow(ramp)) {
  upstream = roadlink[which(roadlink$to_node_id == ramp$from_node_id[i]),]
  downstream = roadlink[which(roadlink$from_node_id == ramp$to_node_id[i]),]
  
}
