setwd("C:/Users/rtu/OneDrive - The North Central Texas Council of Governments/Documents/0_ModelDataDevelopment/")

# use leaflet
library('arcgisbinding')
library('leaflet')
library('sf')
library('dplyr')
library(RColorBrewer)
library(ggplot2)
library(htmlwidgets)
library('lwgeom')


########################################### data filter + weave type match #############################################
########################################### data filter + weave type match #############################################
########################################### data filter + weave type match #############################################
########################################### data filter + weave type match #############################################
# for AM
# delete records if vol per lane > 2500
sidefire_vol_spd_2022_am_filter = sidefire_vol_spd_2022_am[which(sidefire_vol_spd_2022_am$volboxupper/sidefire_vol_spd_2022_am$AMlane < 2500),]
sidefire_vol_spd_2022_am_filter$weavetype = 0
for (i in 1:nrow(sidefire_vol_spd_2022_am_filter)) {
  sidefire_vol_spd_2022_am_filter$weavetype[i] = roadlink_2026$WEAVE_T[which(roadlink_2026$ID == sidefire_vol_spd_2022_am_filter$ID_Road[i])]
}

# for PM
# delete records if vol per lane > 2500
sidefire_vol_spd_2022_pm_filter = sidefire_vol_spd_2022_pm[which(sidefire_vol_spd_2022_pm$volboxupper/sidefire_vol_spd_2022_pm$PMlane < 2500),]
sidefire_vol_spd_2022_pm_filter$weavetype = 0
for (i in 1:nrow(sidefire_vol_spd_2022_pm_filter)) {
  sidefire_vol_spd_2022_pm_filter$weavetype[i] = roadlink_2026$WEAVE_T[which(roadlink_2026$ID == sidefire_vol_spd_2022_pm_filter$ID_Road[i])]
}

# for OP
# delete records if vol per lane > 2500
sidefire_vol_spd_2022_op_filter = sidefire_vol_spd_2022_op[which(sidefire_vol_spd_2022_op$volboxupper/sidefire_vol_spd_2022_op$oplane < 2500),]
sidefire_vol_spd_2022_op_filter$weavetype = 0
for (i in 1:nrow(sidefire_vol_spd_2022_op_filter)) {
  sidefire_vol_spd_2022_op_filter$weavetype[i] = roadlink_2026$WEAVE_T[which(roadlink_2026$ID == sidefire_vol_spd_2022_op_filter$ID_Road[i])]
}

########################################### boxplot for FUNCL 1, op, pm, and am #############################################
########################################### boxplot for FUNCL 1, op, pm, and am #############################################
########################################### boxplot for FUNCL 1, op, pm, and am #############################################
########################################### boxplot for FUNCL 1, op, pm, and am #############################################
# combine op, pm, and am
sidefire_vol_spd_alltime = rbind(cbind(sidefire_vol_spd_2022_am_filter$ID_detector, sidefire_vol_spd_2022_am_filter$LinkName_detector,
                                       sidefire_vol_spd_2022_am_filter$ID_Road, sidefire_vol_spd_2022_am_filter$LinkName_Road, 
                                       sidefire_vol_spd_2022_am_filter$FUNCL, 'AM', sidefire_vol_spd_2022_am_filter$weavetype,
                                       sidefire_vol_spd_2022_am_filter$AMlane, 
                                       sidefire_vol_spd_2022_am_filter$volboxupper, sidefire_vol_spd_2022_am_filter$spdboxupper),
                                 
                                 cbind(sidefire_vol_spd_2022_pm_filter$ID_detector, sidefire_vol_spd_2022_pm_filter$LinkName_detector,
                                       sidefire_vol_spd_2022_pm_filter$ID_Road, sidefire_vol_spd_2022_pm_filter$LinkName_Road, 
                                       sidefire_vol_spd_2022_pm_filter$FUNCL, 'PM', sidefire_vol_spd_2022_pm_filter$weavetype,
                                       sidefire_vol_spd_2022_pm_filter$PMlane, 
                                       sidefire_vol_spd_2022_pm_filter$volboxupper, sidefire_vol_spd_2022_pm_filter$spdboxupper),
                                 
                                 cbind(sidefire_vol_spd_2022_op_filter$ID_detector, sidefire_vol_spd_2022_op_filter$LinkName_detector,
                                       sidefire_vol_spd_2022_op_filter$ID_Road, sidefire_vol_spd_2022_op_filter$LinkName_Road, 
                                       sidefire_vol_spd_2022_op_filter$FUNCL, 'OP', sidefire_vol_spd_2022_op_filter$weavetype,
                                       sidefire_vol_spd_2022_op_filter$oplane, 
                                       sidefire_vol_spd_2022_op_filter$volboxupper, sidefire_vol_spd_2022_op_filter$spdboxupper))

colnames(sidefire_vol_spd_alltime) = c('ID_detector','LinkName_detector','ID_Road','LinkName_Road','FUNCL', 'Time', 'weavetype', 'lane', 'vol_max','spd_at_max')
sidefire_vol_spd_alltime = data.frame(sidefire_vol_spd_alltime)

for (i in 8:10) {
  sidefire_vol_spd_alltime[,i] = as.numeric(sidefire_vol_spd_alltime[,i])
}

# remove outliers
sidefire_vol_spd_alltime_filter = sidefire_vol_spd_alltime[which(sidefire_vol_spd_alltime$vol_max/sidefire_vol_spd_alltime$lane < 2500),]

# Q3+1.5(Q3-Q1), max volume distribution by weaving types
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/freewayvol_by_weaving_type_alltime_boxupper.png", 
    width = 1200, height = 800)
ggplot(sidefire_vol_spd_alltime_filter[which(sidefire_vol_spd_alltime_filter$FUNCL == 1),], 
       aes(x = factor(weavetype, levels = c('FRWY_BASIC','BASIC','MD','A','B','C','LBS','LBE')), 
           y = vol_max/lane, fill = factor(Time))) + geom_boxplot() + 
  labs(title = 'Max Volume for different freeway weaving types') +
  scale_x_discrete(name = "Freeway Weaving Type") + scale_y_continuous(name = "Max Volume at speed < ff_speed") + 
  coord_cartesian(ylim = c(0, 2000)) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))
dev.off()
# corresponding speed distribution at max volume
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/freewayspd_by_weaving_type_alltime_boxupper.png", 
    width = 1200, height = 800)
ggplot(sidefire_vol_spd_alltime_filter[which(sidefire_vol_spd_alltime_filter$FUNCL == 1),], 
       aes(x = factor(weavetype, levels = c('FRWY_BASIC','BASIC','MD','A','B','C','LBS','LBE')), 
           y = spd_at_max, fill = factor(Time))) + geom_boxplot() + 
  labs(title = 'Corresponding speed distribution of max volume records for different freeway weaving types') +
  scale_x_discrete(name = "Freeway Weaving Type") + scale_y_continuous(name = "Speed at max volume (MPH)") + 
  coord_cartesian(ylim = c(0, 75)) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))
dev.off()


# Q3+1.5(Q3-Q1) volume distribution by weaving types for rows at IH
filtered_df_IH <- sidefire_vol_spd_alltime_filter %>%  filter(
  (FUNCL == 1 & grepl("IH", sidefire_vol_spd_alltime_filter$LinkName_Road) #| 
     # grepl("IH30", sidefire_vol_spd_alltime_filter$LinkName_Road) | 
     # grepl("IH635", sidefire_vol_spd_alltime_filter$LinkName_Road)
     ))

filtered_df_US <- sidefire_vol_spd_alltime_filter %>%  filter(
  (FUNCL == 1 & grepl("SH", sidefire_vol_spd_alltime_filter$LinkName_Road) | 
     grepl("US", sidefire_vol_spd_alltime_filter$LinkName_Road) #| 
     #grepl("IH635", sidefire_vol_spd_alltime_filter$LinkName_Road)
  ))

## IH 
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/freewayvol_by_weaving_type_alltime_boxupper_IH.png", 
     width = 1200, height = 800)
ggplot(filtered_df_IH, 
       aes(x = factor(weavetype, levels = c('FRWY_BASIC','BASIC','MD','A','B','C','LBS','LBE')), 
           y = vol_max/lane, fill = factor(Time))) + geom_boxplot() + 
  labs(title = 'Max Volume for different freeway weaving types on IH') +
  scale_x_discrete(name = "Freeway Weaving Type") + scale_y_continuous(name = "Max Volume at speed < ff_speed") + 
  coord_cartesian(ylim = c(0, 2000)) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))
dev.off()
# corresponding speed distribution at max volume
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/freewayspd_by_weaving_type_alltime_boxupper_IH.png", 
    width = 1200, height = 800)
ggplot(filtered_df_IH, 
       aes(x = factor(weavetype, levels = c('FRWY_BASIC','BASIC','MD','A','B','C','LBS','LBE')), 
           y = spd_at_max, fill = factor(Time))) + geom_boxplot() + 
  labs(title = 'Corresponding speed distribution of max volume records on IH') +
  scale_x_discrete(name = "Freeway Weaving Type") + scale_y_continuous(name = "Speed at max volume (MPH)") + 
  coord_cartesian(ylim = c(0, 75)) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))
dev.off()
# ggsave("20250410_capacity_recalculation\\RoadNetwork_2026\\Data_processing\\Plot\\freewayspd_by_weaving_type_alltime_boxupper_IH.jpg", width = 6, height = 4, dpi = 300)

## US & SH
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/freewayvol_by_weaving_type_alltime_boxupper_USSH.png", 
    width = 1200, height = 800)
ggplot(filtered_df_US, 
       aes(x = factor(weavetype, levels = c('FRWY_BASIC','BASIC','MD','A','B','C','LBS','LBE')), 
           y = vol_max/lane, fill = factor(Time))) + geom_boxplot() + 
  labs(title = 'Max Volume for different freeway weaving types on US & SH') +
  scale_x_discrete(name = "Freeway Weaving Type") + scale_y_continuous(name = "Max Volume at speed < ff_speed") + 
  coord_cartesian(ylim = c(0, 2000)) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))
dev.off()
# corresponding speed distribution at max volume
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/freewayspd_by_weaving_type_alltime_boxupper_USSH.png", 
    width = 1200, height = 800)
ggplot(filtered_df_US, 
       aes(x = factor(weavetype, levels = c('FRWY_BASIC','BASIC','MD','A','B','C','LBS','LBE')), 
           y = spd_at_max, fill = factor(Time))) + geom_boxplot() + 
  labs(title = 'Corresponding speed distribution of max volume records on US & SH') +
  scale_x_discrete(name = "Freeway Weaving Type") + scale_y_continuous(name = "Speed at max volume (MPH)") + 
  coord_cartesian(ylim = c(0, 75)) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))
dev.off()


########################################### box for ramp, three periods #############################################
########################################### box for ramp, three periods #############################################
########################################### box for ramp, three periods #############################################
########################################### box for ramp, three periods #############################################
filtered_df_ramp <- sidefire_vol_spd_alltime_filter %>%  filter(
  (FUNCL == 6
  ))

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/rampvol_all_alltime_boxupper.png", 
    width = 1200, height = 800)
ggplot(filtered_df_ramp, 
       aes(x = factor(Time), 
           y = vol_max/lane)) + geom_boxplot() + 
  labs(title = 'Max Volume on ramps') +
  scale_x_discrete(name = "Time period") + scale_y_continuous(name = "Max Volume at speed < ff_speed") + 
  coord_cartesian(ylim = c(0, 2000)) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))
dev.off()
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/rampspd_all_alltime_boxupper.png", 
    width = 1200, height = 800)
ggplot(filtered_df_ramp, 
       aes(x = factor(Time), 
           y = spd_at_max)) + geom_boxplot() + 
  labs(title = 'Corresponding speed distribution of max volume records on ramps') +
  scale_x_discrete(name = "Time period") + scale_y_continuous(name = "Speed at max volume (MPH)") + 
  coord_cartesian(ylim = c(0, 75)) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))
dev.off()

# add ramp type to filtered_df_ramp
filtered_df_ramp$connect_type = 0
for (i in 1:nrow(filtered_df_ramp)) {
  filtered_df_ramp$connect_type[i] = ramp$connect_type[which(ramp$ID == filtered_df_ramp$ID_Road[i])]
}

png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/rampvol_by_ramptype_alltime_boxupper.png", 
    width = 1200, height = 800)
ggplot(filtered_df_ramp, 
       aes(x = factor(connect_type, labels = c('ramp-ramp', 'ramp-freeway', 'ramp-arterial', 'freeway-arterial', 'freeway-freeway')), 
           y = vol_max/lane)) + geom_boxplot(fill = 'yellow') + 
  labs(title = 'Max Volume on ramps, by ramp type') +
  scale_x_discrete(name = "Ramp connection type") + scale_y_continuous(name = "Max Volume at speed < ff_speed") + 
  coord_cartesian(ylim = c(0, 2000)) +
  theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))
dev.off()
png("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/rampspd_by_ramptype_alltime_boxupper.png", 
    width = 1200, height = 800)
ggplot(filtered_df_ramp, 
       aes(x = factor(connect_type, labels = c('ramp-ramp', 'ramp-freeway', 'ramp-arterial', 'freeway-arterial', 'freeway-freeway')), 
           y = spd_at_max)) + geom_boxplot(fill = 'yellow') + 
  labs(title = 'Corresponding speed distribution of max volume records on ramps') +
  scale_x_discrete(name = "Ramp connection type") + scale_y_continuous(name = "Speed at max volume (MPH)") + 
  coord_cartesian(ylim = c(0, 75)) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))
dev.off()


########################################### map AM #############################################
########################################### map AM #############################################
########################################### map AM #############################################
########################################### map AM #############################################

# plot has bug - needs to join sidefire shape with sidefire vol spd
sidefire_vol_spd_2022_am_filter$ID_detector = as.numeric(sidefire_vol_spd_2022_am_filter$ID_detector)
sidefire_inTaz_2025_am = left_join(sidefire_inTaz_2025, sidefire_vol_spd_2022_am_filter,
                                    by = c('ID' = 'ID_detector'))
sidefire_inTaz_2025_am = sidefire_inTaz_2025_am[which(!is.na(sidefire_inTaz_2025_am$volmin)),]

sidefire_inTaz_2025_am$popup <- paste0(
  "<b>Sensor ID:</b> ", sidefire_inTaz_2025_am$ID, "<br>",
  "<b>Street Name:</b> ", sidefire_inTaz_2025_am$LINKNAME, "<br>",
  "<b>FUNCL:</b> ", sidefire_inTaz_2025_am$FUNCL, "<br>",
  "<b>Lane:</b> ", sidefire_inTaz_2025_am$AMlane, "<br>",
  "<b>Vol 95%:</b> ", round(sidefire_inTaz_2025_am$volboxupper/sidefire_inTaz_2025_am$AMlane,0), "<br>",
  "<b>Speed 95%:</b> ", sidefire_inTaz_2025_am$spdboxupper, "<br>")

# Define a color palette for volume
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_inTaz_2025_am$volboxupper/sidefire_inTaz_2025_am$AMlane, bins = 5)
mymap = 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025_am, crs = 4326), fillOpacity = 1,
                   color = ~pal(round(sidefire_inTaz_2025_am$volboxupper/sidefire_inTaz_2025_am$AMlane,0)), 
                   label = ~paste("Value:", round(sidefire_inTaz_2025_am$volboxupper/sidefire_inTaz_2025_am$AMlane,0)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_inTaz_2025_am$volboxupper/sidefire_inTaz_2025_am$AMlane,0),
    title = "AM, Max volume when speed < ff_speed", opacity = 1)
saveWidget(mymap, file = "20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/vol_am_boxupper.html", selfcontained = TRUE)
rm(mymap)
# Define a color palette for speed
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_inTaz_2025_am$spdboxupper, bins = 5)
mymap = 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025_am, crs = 4326), fillOpacity = 1,
                   color = ~pal(sidefire_inTaz_2025_am$spdboxupper), 
                   label = ~paste("Value:", round(sidefire_inTaz_2025_am$spdboxupper,2)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_inTaz_2025_am$spdboxupper,2),
            title = "AM, Max volume when speed < ff_speed", opacity = 1)
saveWidget(mymap, file = "20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/spd_am_boxupper.html", selfcontained = TRUE)
rm(mymap)

########################################### map PM #############################################
########################################### map PM #############################################
########################################### map PM #############################################
########################################### map PM #############################################

# plot has bug - needs to join sidefire shape with sidefire vol spd
sidefire_vol_spd_2022_pm_filter$ID_detector = as.numeric(sidefire_vol_spd_2022_pm_filter$ID_detector)
sidefire_inTaz_2025_pm = left_join(sidefire_inTaz_2025, sidefire_vol_spd_2022_pm_filter,
                                   by = c('ID' = 'ID_detector'))
sidefire_inTaz_2025_pm = sidefire_inTaz_2025_pm[which(!is.na(sidefire_inTaz_2025_pm$volmin)),]

sidefire_inTaz_2025_pm$popup <- paste0(
  "<b>Sensor ID:</b> ", sidefire_inTaz_2025_pm$ID, "<br>",
  "<b>Street Name:</b> ", sidefire_inTaz_2025_pm$LINKNAME, "<br>",
  "<b>FUNCL:</b> ", sidefire_inTaz_2025_pm$FUNCL, "<br>",
  "<b>Lane:</b> ", sidefire_inTaz_2025_pm$PMlane, "<br>",
  "<b>Vol 95%:</b> ", round(sidefire_inTaz_2025_pm$volboxupper/sidefire_inTaz_2025_pm$PMlane,0), "<br>",
  "<b>Speed 95%:</b> ", sidefire_inTaz_2025_pm$spdboxupper, "<br>")

# Define a color palette for volume
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_inTaz_2025_pm$volboxupper/sidefire_inTaz_2025_pm$PMlane, bins = 5)
mymap = 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025_pm, crs = 4326), fillOpacity = 1,
                   color = ~pal(round(sidefire_inTaz_2025_pm$volboxupper/sidefire_inTaz_2025_pm$PMlane,0)), 
                   label = ~paste("Value:", round(sidefire_inTaz_2025_pm$volboxupper/sidefire_inTaz_2025_pm$PMlane,0)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_inTaz_2025_pm$volboxupper/sidefire_inTaz_2025_pm$PMlane,0),
            title = "PM, Max volume when speed < ff_speed", opacity = 1)
saveWidget(mymap, file = "20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/vol_pm_boxupper.html", selfcontained = TRUE)
rm(mymap)
# Define a color palette for speed
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_inTaz_2025_pm$spdboxupper, bins = 5)
mymap = 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025_pm, crs = 4326), fillOpacity = 1,
                   color = ~pal(sidefire_inTaz_2025_pm$spdboxupper), 
                   label = ~paste("Value:", round(sidefire_inTaz_2025_pm$spdboxupper,2)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_inTaz_2025_pm$spdboxupper,2),
            title = "PM, Max volume when speed < ff_speed", opacity = 1)
saveWidget(mymap, file = "20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/spd_pm_boxupper.html", selfcontained = TRUE)
rm(mymap)



########################################### box & map OP #############################################
########################################### box & map OP #############################################
########################################### box & map OP #############################################
########################################### box & map OP #############################################

# boxupper volume distribution by weaving types
ggplot(sidefire_vol_spd_2022_op_filter[which(sidefire_vol_spd_2022_op_filter$FUNCL == 1),], 
       aes(x = factor(weavetype, levels = c('FRWY_BASIC','BASIC','MD','A','B','C','LBS','LBE')), 
           y = volboxupper/oplane)) + geom_boxplot(fill = 'yellow') + 
  labs(title = 'Max Volume for different freeway weaving types') +
  scale_x_discrete(name = "Freeway Weaving Type") + scale_y_continuous(name = "Max Volume at speed < ff_speed") + 
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))

# corresponding speed distribution at 95% volume
ggplot(sidefire_vol_spd_2022_op_filter[which(sidefire_vol_spd_2022_op_filter$FUNCL == 1),], 
       aes(x = factor(weavetype, levels = c('FRWY_BASIC','BASIC','MD','A','B','C','LBS','LBE')), 
           y = spdboxupper)) + geom_boxplot(fill = 'yellow') + 
  labs(title = 'Corresponding speed distribution of max volume records for different freeway weaving types') +
  scale_x_discrete(name = "Freeway Weaving Type") + scale_y_continuous(name = "Speed at max volume (MPH)") + 
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))

# plot has bug - needs to join sidefire shape with sidefire vol spd
sidefire_inTaz_2025$popup <- paste0(
  "<b>Sensor ID:</b> ", sidefire_inTaz_2025$ID, "<br>",
  "<b>Street Name:</b> ", sidefire_inTaz_2025$LINKNAME, "<br>",
  "<b>FUNCL:</b> ", sidefire_inTaz_2025$FUNCL, "<br>",
  "<b>Lane:</b> ", sidefire_vol_spd_2022_op_filter$oplane, "<br>",
  "<b>Vol 95%:</b> ", round(sidefire_vol_spd_2022_op_filter$vol95/sidefire_vol_spd_2022_op_filter$oplane,0), "<br>",
  "<b>Speed 95%:</b> ", sidefire_vol_spd_2022_op_filter$spd95, "<br>")

# Define a color palette
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_vol_spd_2022_op_filter$vol95/sidefire_vol_spd_2022_op_filter$oplane, bins = 5)

leaflet() %>%
  addTiles() %>%
  # addPolygons(data = st_transform(taz_2026, crs = 4326), color = 'blue', fillOpacity = 0.3, weight = 0.2) %>%
  #addPolylines(data = st_transform(roadlink_2026[which(roadlink_2026$FUNCL == 1 | roadlink_2026$FUNCL ==6),], crs = 4326), color = "red", weight = 1.5) %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025, crs = 4326), fillOpacity = 1,
                   color = ~pal(round(sidefire_vol_spd_2022_op_filter$vol95/sidefire_vol_spd_2022_op_filter$oplane,0)), 
                   label = ~paste("Value:", round(sidefire_vol_spd_2022_op_filter$vol95/sidefire_vol_spd_2022_op_filter$oplane,0)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_vol_spd_2022_op_filter$vol95/sidefire_vol_spd_2022_op_filter$oplane,0),
            title = "95% volume when speed < ff_speed", opacity = 1)


