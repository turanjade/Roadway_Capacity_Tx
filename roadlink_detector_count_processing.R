library('arcgisbinding')
library('sf')
library('dplyr')
library(ggplot2)

setwd("C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment")

# use leaflet
install.packages('leaflet')
library('leaflet')
library('sf')
library(RColorBrewer)

# plot has bug - needs to join sidefire shape with sidefire vol spd
# for AM
# delete records if vol per lane > 2500
sidefire_vol_spd_2022_am_filter = sidefire_vol_spd_2022_am[which(sidefire_vol_spd_2022_am$vol95/sidefire_vol_spd_2022_am$AMlane < 2500),]
sidefire_vol_spd_2022_am_filter$weavetype = 0
for (i in 1:nrow(sidefire_vol_spd_2022_am_filter)) {
  sidefire_vol_spd_2022_am_filter$weavetype[i] = roadlink_2026$WEAVE_T[which(roadlink_2026$ID == sidefire_vol_spd_2022_am_filter$ID_Road[i])]
}

# 95% volume distribution by weaving types
ggplot(sidefire_vol_spd_2022_am_filter[which(sidefire_vol_spd_2022_am_filter$FUNCL == 1),], 
       aes(x = factor(weavetype, levels = c('FRWY_BASIC','BASIC','MD','A','B','C','LBS','LBE')), 
                                            y = vol95/AMlane)) + geom_boxplot(fill = 'yellow') + 
  labs(title = '95% Volume for different freeway weaving types') +
  scale_x_discrete(name = "Freeway Weaving Type") + scale_y_continuous(name = "95% Volume at speed < ff_speed") + 
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))

# corresponding speed distribution at 95% volume
ggplot(sidefire_vol_spd_2022_am_filter[which(sidefire_vol_spd_2022_am_filter$FUNCL == 1),], 
       aes(x = factor(weavetype, levels = c('FRWY_BASIC','BASIC','MD','A','B','C','LBS','LBE')), 
           y = spd95)) + geom_boxplot(fill = 'yellow') + 
  labs(title = 'Corresponding speed distribution of 95% volume records for different freeway weaving types') +
  scale_x_discrete(name = "Freeway Weaving Type") + scale_y_continuous(name = "Speed at 95% volume (MPH)") + 
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 15))
  

sidefire_inTaz_2025$popup <- paste0(
  "<b>Sensor ID:</b> ", sidefire_inTaz_2025$ID, "<br>",
  "<b>Street Name:</b> ", sidefire_inTaz_2025$LINKNAME, "<br>",
  "<b>FUNCL:</b> ", sidefire_inTaz_2025$FUNCL, "<br>",
  "<b>Lane:</b> ", sidefire_vol_spd_2022_am_filter$AMlane, "<br>",
  "<b>Vol 95%:</b> ", round(sidefire_vol_spd_2022_am_filter$vol95/sidefire_vol_spd_2022_am_filter$AMlane,0), "<br>",
  "<b>Speed 95%:</b> ", sidefire_vol_spd_2022_am_filter$spd95, "<br>")

# Define a color palette
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_vol_spd_2022_am_filter$vol95/sidefire_vol_spd_2022_am_filter$AMlane, bins = 5)

leaflet() %>%
  addTiles() %>%
  # addPolygons(data = st_transform(taz_2026, crs = 4326), color = 'blue', fillOpacity = 0.3, weight = 0.2) %>%
  #addPolylines(data = st_transform(roadlink_2026[which(roadlink_2026$FUNCL == 1 | roadlink_2026$FUNCL ==6),], crs = 4326), color = "red", weight = 1.5) %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025, crs = 4326), fillOpacity = 1,
                   color = ~pal(round(sidefire_vol_spd_2022_am_filter$vol95/sidefire_vol_spd_2022_am_filter$AMlane,0)), 
                   label = ~paste("Value:", round(sidefire_vol_spd_2022_am_filter$vol95/sidefire_vol_spd_2022_am_filter$AMlane,0)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_vol_spd_2022_am_filter$vol95/sidefire_vol_spd_2022_am_filter$AMlane,0),
    title = "95% volume when speed < ff_speed", opacity = 1)


# for PM
# delete records if vol per lane > 2500
sidefire_vol_spd_2022_pm_filter = sidefire_vol_spd_2022_pm[which(sidefire_vol_spd_2022_pm$vol95/sidefire_vol_spd_2022_pm$PMlane < 2500),]

sidefire_inTaz_2025$popup <- paste0(
  "<b>Sensor ID:</b> ", sidefire_inTaz_2025$ID, "<br>",
  "<b>Street Name:</b> ", sidefire_inTaz_2025$LINKNAME, "<br>",
  "<b>FUNCL:</b> ", sidefire_inTaz_2025$FUNCL, "<br>",
  "<b>Lane:</b> ", sidefire_vol_spd_2022_pm_filter$PMlane, "<br>",
  "<b>Vol 95%:</b> ", round(sidefire_vol_spd_2022_pm_filter$vol95/sidefire_vol_spd_2022_pm_filter$PMlane,0), "<br>",
  "<b>Speed 95%:</b> ", sidefire_vol_spd_2022_pm_filter$spd95, "<br>")

# Define a color palette
pal <- colorBin(palette = colorRampPalette(c("red", "yellow", "green"))(5), 
                domain = sidefire_vol_spd_2022_pm_filter$vol95/sidefire_vol_spd_2022_pm_filter$PMlane, bins = 5)

leaflet() %>%
  addTiles() %>%
  # addPolygons(data = st_transform(taz_2026, crs = 4326), color = 'blue', fillOpacity = 0.3, weight = 0.2) %>%
  #addPolylines(data = st_transform(roadlink_2026[which(roadlink_2026$FUNCL == 1 | roadlink_2026$FUNCL ==6),], crs = 4326), color = "red", weight = 1.5) %>%
  addCircleMarkers(data = st_transform(sidefire_inTaz_2025, crs = 4326), fillOpacity = 1,
                   color = ~pal(round(sidefire_vol_spd_2022_pm_filter$vol95/sidefire_vol_spd_2022_pm_filter$PMlane,0)), 
                   label = ~paste("Value:", round(sidefire_vol_spd_2022_pm_filter$vol95/sidefire_vol_spd_2022_pm_filter$PMlane,0)),
                   radius = 5, popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = round(sidefire_vol_spd_2022_pm_filter$vol95/sidefire_vol_spd_2022_pm_filter$PMlane,0),
            title = "95% volume when speed < ff_speed", opacity = 1)
