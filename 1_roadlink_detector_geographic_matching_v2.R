## updates in v2: 
## 1) select all the links that are within 100 ft of each sensor, and find the best match among them.
###  if several best matches found, save the link and check manually (possibly see if there is a method for automatic process)
## 2) delete FUNCL criterion because it is tagged with RDWY, not initially provided by sidefire (same as DIR. Use name to match direction instead)
## 3) Only look at freeway. In this version, we still use FUNCL in sidefire for simplicity. Need to transfer to name match


library('arcgisbinding')
library('sf')
library('dplyr')
library('openxlsx')


setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

sidefire2025 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\Sidefire_2025\\sidefire_2025.shp')
roadlink_2026 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\RDWY\\road_shp_addedinfo.shp')
taz_2026 = st_read('20250410_capacity_recalculation\\RoadNetwork_2026\\ArcGIS\\TAZ\\taz_shp.shp')

sidefire_2025 <- sidefire2025 %>%
  select(ID, LINKNAME, FUNCL, DIR, TMC, geometry)
colnames(sidefire_2025) = c('ID', 'LINKNAME', 'FUNCL', 'DIR', 'TMC', 'geometry')

# st_crs(sidefire_2025) == st_crs(roadlink_2026)  # should be TRUE, check if two layers are in the same coordinate
# st_crs(sidefire_2025) == st_crs(taz_2026) # should be TRUE, check if two layers are in the same coordinate

############################################# Geographic match #############################################
############################################# Geographic match #############################################
############################################# Geographic match #############################################
############################################# Geographic match #############################################
# note: process
############################################# intersect point buffer (100ft) with links to find links within 100ft of each detector ######################
############################################# intersect point buffer (100ft) with links to find links within 100ft of each detector ######################

# Ensure both are in the same projected CRS (must use feet as unit!)
# For example, NAD 1983 StatePlane Arizona East FIPS 0201 Feet
# Replace with your actual CRS
crs_feet <- 2223  # example EPSG code for feet
lines <- st_transform(roadlink_2026, crs_feet)
points <- st_transform(sidefire_2025, crs_feet)

# Buffer points by 100 feet
points_buffer <- st_buffer(points, dist = 100) # ease to 150? -- no, keep consistent with Francisco

# Spatial join: get all lines within 100 ft of any point
# This creates a row for each line-point pair
lines_within_100ft <- st_join(lines, points_buffer, join = st_intersects, left = FALSE)

############################################# Option 1: Find FUNCL = 1 ##########################################
############################################# Option 1: Find FUNCL = 1 ##########################################
# option 2 is to use street name. First use FUNCL 
sidefire_2025_frwy = sidefire_2025[which(sidefire_2025$FUNCL == 1),]
lines_within_100ft_frwy = lines_within_100ft[which(lines_within_100ft$FUNCL.x == 1 & # type freeway in rdwy
                                                     !is.na(lines_within_100ft$ID.y) & # there is point matched for each link
                                                     lines_within_100ft$FUNCL.y == 1),] # type freeway in sidefire
# for each sidefire ID that is tagged as FUNCL 1, find the best match from the 
sidefire_ID = unique(sidefire_2025_frwy$ID)
sidefire_linkmatch_frwy_2025 = data.frame(matrix(0, nrow = 0, ncol = 5))
# colnames(sidefire_linkmatch_frwy_2025) = c('sf_id','sf_name','rdwy_id','rdwy_name','distance')
a = 0 # record how many detectors are filtered out because of the distance buffer
b_typea = 0 # record how many links cannot pass direction check
b_typeb = 0
b_typec = 0
c_typea = 0 # record how many links cannot pass name check
c_typeb = 0
c_typec = 0
for (i in 1:length(sidefire_ID)) {
  link_i = lines_within_100ft_frwy[which(lines_within_100ft_frwy$ID.y == sidefire_ID[i]),]
  if (nrow(link_i) == 0) {
    # print(paste('sidefire sensor', sidefire_ID[i],'cannot find matched link within 100ft buffer'))
    a = a + 1
    next
  }
  match_i = matrix(0, nrow = 0, ncol = 2)
  for (j in 1:nrow(link_i)) {
    #initiate dir and name decision factor as 0. If it becomes 1 after the check, then it passes
    dir_j = 0; name_j = 0 
    ############################################# check dir by street name & sidefire name ##########################################
    if (isTRUE(grepl('NB', link_i$LINKNAME[j])) & isTRUE(grepl('NB', link_i$STREET[j])) | 
        isTRUE(grepl('SB', link_i$LINKNAME[j])) & isTRUE(grepl('SB', link_i$STREET[j])) | 
        isTRUE(grepl('WB', link_i$LINKNAME[j])) & isTRUE(grepl('WB', link_i$STREET[j])) | 
        isTRUE(grepl('EB', link_i$LINKNAME[j])) & isTRUE(grepl('EB', link_i$STREET[j]))) {
      dir_j = 1 # pass dir check
    } 

    ############################################# check check strict street name ##########################################
    if (strsplit(link_i$LINKNAME[j],"[-. ]+")[[1]][1] == strsplit(link_i$STREET[j], '[-. ]+')[[1]][1]) {
      name_j = 1
    } 
    ############################################# check nicknames of highway ##############################################
    #### SRT refers to Sam Rayburn tollway, SH121; PGBT refers to President George Bush Turnpike,  SH 190
    if (strsplit(link_i$LINKNAME[j],"[-. ]+")[[1]][1] == 'SH121' & strsplit(link_i$STREET[j], '[-. ]+')[[1]][1] == 'SRT' |
        strsplit(link_i$LINKNAME[j],"[-. ]+")[[1]][1] == 'SH190' & strsplit(link_i$STREET[j], '[-. ]+')[[1]][1] == 'PGBT') {
      name_j = 1
    } 
    #### IH can be I
    if (grepl("^I(?!H)",strsplit(link_i$LINKNAME[j],"[-. ]+")[[1]][1], perl = T) &
        sub("^I([^H])", "IH\\1", strsplit(link_i$LINKNAME[j],"[-. ]+")[[1]][1]) == strsplit(link_i$STREET[j], '[-. ]+')[[1]][1]) {
      name_j = 1
    }
    
    # check how many type a b c are remvoed by direction
    if (dir_j == 0) {
      if (any(link_i$WEAVE_T == 'A')) {
        b_typea = b_typea + length(which(link_i$WEAVE_T == 'A'))
      } 
      if (any(link_i$WEAVE_T == 'B')) {
        b_typeb = b_typeb + length(which(link_i$WEAVE_T == 'B'))
      } 
      if (any(link_i$WEAVE_T == 'C')) {
        b_typec = b_typec + length(which(link_i$WEAVE_T == 'C'))
      }
    } 
    # check how many type a b c are remvoed by name
    else if (name_j == 0) {
      if (any(link_i$WEAVE_T == 'A')) {
        c_typea = c_typea + length(which(link_i$WEAVE_T == 'A'))
      } 
      if (any(link_i$WEAVE_T == 'B')) {
        c_typeb = c_typeb + length(which(link_i$WEAVE_T == 'B'))
      } 
      if (any(link_i$WEAVE_T == 'C')) {
        c_typec = c_typec + length(which(link_i$WEAVE_T == 'C'))
      }
    }
    
     
    match_i = rbind(match_i, c(dir_j*name_j, st_length(st_nearest_points(points[which(points$ID == sidefire_ID[i]),], 
                                                           lines[which(lines$ID == link_i$ID.x[j]),])))) # combine the distance with two criteria check

  }
  choose_i = which(match_i[,1] != 0 & match_i[,2] == min(match_i[,2]))
  rowtocombine = c(link_i$ID.y[choose_i], link_i$LINKNAME[choose_i], link_i$ID.x[choose_i], link_i$STREET[choose_i], match_i[choose_i,2])
  sidefire_linkmatch_frwy_2025 = rbind(sidefire_linkmatch_frwy_2025, rowtocombine)
  rm(match_i, choose_i, dir_j, name_j, rowtocombine, link_i)
}
colnames(sidefire_linkmatch_frwy_2025) = c('sf_id','sf_name','rdwy_id','rdwy_name','distance')
sidefire_linkmatch_frwy_2025$distance = as.numeric(sidefire_linkmatch_frwy_2025$distance)

rm(a, i, j, crs_feet, points, lines, lines_within_100ft, lines_within_100ft_frwy)

############################################# find duplicated detector match, manual check (solved) ##########################################
sidefire_linkmatch_frwy_2025_sensordup <- sidefire_linkmatch_frwy_2025 %>%
  group_by(sf_id) %>%
  filter(n() > 1) %>%
  ungroup()

# write.csv(sidefire_linkmatch_frwy_2025_dup, 
#          paste('~/0_ModelDataDevelopment/20250410_capacity_recalculation/RoadNetwork_2026/Sensor_count/',
#                'sidefire_linkmatch_frwy_2025_v2_duplinks.csv', sep = ''), row.names = F)

##### Possibilities after manual check:
## 1) more than two links within 150 ft, in the same direction (two consecutive links), are matched to one sidefire
##    solution: choose the closest one --> adopt this one

## 2) simply delete dups (discard)
# sidefire_linkmatch_frwy_2025 = sidefire_linkmatch_frwy_2025[!duplicated(sidefire_linkmatch_frwy_2025$sf_id),]

############################################# find duplicated link match, manual check (do not run) ##########################################
# keep duplicated link match to keep as much records as possible, so do not delete dups
sidefire_link_dup = 
  unique(sidefire_linkmatch_frwy_2025$rdwy_id[duplicated(sidefire_linkmatch_frwy_2025$rdwy_id)])
# some links have multiple detectors attached. In this case, still choose the closest one -- this has to be done after the rdwy match, separately
for (i in 1:length(sidefire_link_dup)) {
  matched_i = which(sidefire_linkmatch_frwy_2025$rdwy_id == sidefire_link_dup[i])
  matched_rm = matched_i[which(sidefire_linkmatch_frwy_2025$distance[matched_i] > 
                                      min(sidefire_linkmatch_frwy_2025$distance[matched_i]))]
  if (length(matched_rm) > 0) {
    sidefire_linkmatch_frwy_2025 = sidefire_linkmatch_frwy_2025[-matched_rm,]
  }
}

# check if dup still exists
sidefire_link_dup = 
  unique(sidefire_linkmatch_frwy_2025$rdwy_id[duplicated(sidefire_linkmatch_frwy_2025$rdwy_id)])
# manual check, delete the first one of each dup because they are in the same link
sidefire_linkmatch_frwy_2025 = sidefire_linkmatch_frwy_2025[!duplicated(sidefire_linkmatch_frwy_2025$rdwy_id),]
# check if dup still exists
sidefire_link_dup = 
  unique(sidefire_linkmatch_frwy_2025$rdwy_id[duplicated(sidefire_linkmatch_frwy_2025$rdwy_id)])

rm(i,j,matched_i, matched_rm, sidefire_link_dup)


############################################# add weavetype & capacity & lanes to each matched record ##########################################
sidefire_linkmatch_frwy_2025$weavetype = 0
sidefire_linkmatch_frwy_2025$amhrcap = 0
sidefire_linkmatch_frwy_2025$ophrcap = 0
sidefire_linkmatch_frwy_2025$pmhrcap = 0
sidefire_linkmatch_frwy_2025$amlane = 0
sidefire_linkmatch_frwy_2025$oplane = 0
sidefire_linkmatch_frwy_2025$pmlane = 0
sidefire_linkmatch_frwy_2025$amffspd = 0
sidefire_linkmatch_frwy_2025$pmffspd = 0
sidefire_linkmatch_frwy_2025$opffspd = 0
sidefire_linkmatch_frwy_2025$areatype = 0
sidefire_linkmatch_frwy_2025$length = 0
sidefire_linkmatch_frwy_2025$amwvhrcap = 0
sidefire_linkmatch_frwy_2025$pmwvhrcap = 0
sidefire_linkmatch_frwy_2025$opwvhrcap = 0


for (i in 961:nrow(sidefire_linkmatch_frwy_2025)) {
  sidefire_linkmatch_frwy_2025$weavetype[i] = roadlink_2026$WEAVE_T[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])]
  
  sidefire_linkmatch_frwy_2025$length[i] = roadlink_2026$LENGTH[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])]
   
  sidefire_linkmatch_frwy_2025$amhrcap[i] = max(roadlink_2026$AMHRCAP_A[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                roadlink_2026$AMHRCAP_B[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
  sidefire_linkmatch_frwy_2025$pmhrcap[i] = max(roadlink_2026$PMHRCAP_A[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                 roadlink_2026$PMHRCAP_B[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
  sidefire_linkmatch_frwy_2025$ophrcap[i] = max(roadlink_2026$OPHRCAP_A[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                roadlink_2026$OPHRCAP_B[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
  
  sidefire_linkmatch_frwy_2025$amlane[i] = max(roadlink_2026$AMLN_AB[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                roadlink_2026$AMLN_BA[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
  sidefire_linkmatch_frwy_2025$pmlane[i] = max(roadlink_2026$PMLN_AB[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                roadlink_2026$PMLN_BA[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
  sidefire_linkmatch_frwy_2025$oplane[i] = max(roadlink_2026$OPLN_AB[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                roadlink_2026$OPLN_BA[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
  sidefire_linkmatch_frwy_2025$amffspd[i] = max(roadlink_2026$PKFRSPD_A[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                 roadlink_2026$PKFRSPD_B[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
  sidefire_linkmatch_frwy_2025$pmffspd[i] = max(roadlink_2026$PKFRSPD_A[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                 roadlink_2026$PKFRSPD_B[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
  sidefire_linkmatch_frwy_2025$opffspd[i] = max(roadlink_2026$OPFRSPD_A[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                roadlink_2026$OPFRSPD_A[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
  
  sidefire_linkmatch_frwy_2025$areatype[i] = roadlink_2026$AREATYP[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])]
  
  sidefire_linkmatch_frwy_2025$amwvhrcap[i] = max(roadlink_2026$AM_WEAVEHR[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                roadlink_2026$AM_WEAVEH1[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
  sidefire_linkmatch_frwy_2025$pmwvhrcap[i] = max(roadlink_2026$AM_WEAVEHR[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                roadlink_2026$PM_WEAVEH1[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
  sidefire_linkmatch_frwy_2025$opwvhrcap[i] = max(roadlink_2026$OP_WEAVEHR[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])],
                                                roadlink_2026$OP_WEAVEH1[which(roadlink_2026$ID == sidefire_linkmatch_frwy_2025$rdwy_id[i])], na.rm = T)
}


### check if lane numbers are the same
plot(as.numeric(sidefire_linkmatch_frwy_2025$amlane) - as.numeric(sidefire_linkmatch_frwy_2025$pmlane))
plot(as.numeric(sidefire_linkmatch_frwy_2025$amlane) - as.numeric(sidefire_linkmatch_frwy_2025$oplane))
## conclusion: lanes are the same, don't worry about the lanes

### check if ffspd are the same
plot(as.numeric(sidefire_linkmatch_frwy_2025$amffspd) - as.numeric(sidefire_linkmatch_frwy_2025$pmffspd))
plot(as.numeric(sidefire_linkmatch_frwy_2025$amffspd) - as.numeric(sidefire_linkmatch_frwy_2025$opffspd))
## conclusion: ffspd are the same, don't worry about the lanes

### check if hrcap are the same
plot(as.numeric(sidefire_linkmatch_frwy_2025$amhrcap) - as.numeric(sidefire_linkmatch_frwy_2025$pmhrcap))
plot(as.numeric(sidefire_linkmatch_frwy_2025$amhrcap) - as.numeric(sidefire_linkmatch_frwy_2025$ophrcap))
## conclusion: ffspd are the same, don't worry about the lanes

### check if weave hrcap are the same
plot(as.numeric(sidefire_linkmatch_frwy_2025$amwvhrcap) - as.numeric(sidefire_linkmatch_frwy_2025$pmwvhrcap), col = 'white')
plot(as.numeric(sidefire_linkmatch_frwy_2025$amwvhrcap) - as.numeric(sidefire_linkmatch_frwy_2025$opwvhrcap), col = 'white')

############################################# clean out working environment ##########################################

rm(link_i, lines_within_100ft, lines_within_100ft_frwy, points_buffer, lines, points,
   crs_feet, dir_j, distance_ij, name_j, rowtocombine, sidefire_ID, i, j)
