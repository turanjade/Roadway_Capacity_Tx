sidefire_vol_spd_2022_plot$feet = as.numeric(sidefire_vol_spd_2022_plot$length) * 5280
for (i in 1:nrow(sidefire_vol_spd_2022_plot)) {
  if (sidefire_vol_spd_2022_plot$feet[i] <= 500) {
    sidefire_vol_spd_2022_plot$feet[i] = 500
  } 
  else if (sidefire_vol_spd_2022_plot$feet[i] <= 1000) {
    sidefire_vol_spd_2022_plot$feet[i] = 1000
  } 
  else if (sidefire_vol_spd_2022_plot$feet[i] <= 1500) {
    sidefire_vol_spd_2022_plot$feet[i] = 1500
  } 
  else if (sidefire_vol_spd_2022_plot$feet[i] <= 2000) {
    sidefire_vol_spd_2022_plot$feet[i] = 2000
  } 
  else if (sidefire_vol_spd_2022_plot$feet[i] <= 2500) {
    sidefire_vol_spd_2022_plot$feet[i] = 2500
  } 
  else {
    sidefire_vol_spd_2022_plot$feet[i] = 3000
  } 
  
}

AWCAP = matrix(0, nrow = 3, ncol = 5)
AWCAP[1,1] = 4300
AWCAP[1,2] = 4880
AWCAP[1,3] = 5240
AWCAP[1,4] = 5490
AWCAP[1,5] = 5690

AWCAP[2,1] = 5950
AWCAP[2,2] = 6740
AWCAP[2,3] = 7200
AWCAP[2,4] = 7530
AWCAP[2,5] = 7780

AWCAP[3,1] = 7980
AWCAP[3,2] = 9040
AWCAP[3,3] = 9610
AWCAP[3,4] = 10010
AWCAP[3,5] = 10280

colnames(AWCAP) = c('500', '1000', '1500', '2000', '2500')
rownames(AWCAP) = c('3', '4', '5')

table(sidefire_vol_spd_2022_plot$feet[which(sidefire_vol_spd_2022_plot$weavetype == 'A')])

sidefire_vol_spd_2022_plot$wvhrcapperlane[which(sidefire_vol_spd_2022_plot$feet == 1000 & sidefire_vol_spd_2022_plot$weavetype == 'A')]-
  sidefire_vol_spd_2022_plot$hrcapperlane[which(sidefire_vol_spd_2022_plot$feet == 1000 & sidefire_vol_spd_2022_plot$weavetype == 'A')]

# for BASIC and MD, wvhrcapperlane all equals 2300, hrcapperlane varies; For A, B (C not sure, no data), hrcapperlane = wvhrcapperlane

############################################# start from here, investigate HCM weave capacity, May 29 ###################################
############################################# start from here, investigate HCM weave capacity, May 29 ###################################
############################################# start from here, investigate HCM weave capacity, May 29 ###################################


############################################ count the occurrence ###########################################
sidefire_vol_spd_2022$feet = as.numeric(sidefire_vol_spd_2022$length) * 5280
for (i in 1:nrow(sidefire_vol_spd_2022)) {
  if (sidefire_vol_spd_2022$feet[i] <= 500) {
    sidefire_vol_spd_2022$feet[i] = 500
  } 
  else if (sidefire_vol_spd_2022$feet[i] <= 1000) {
    sidefire_vol_spd_2022$feet[i] = 1000
  } 
  else if (sidefire_vol_spd_2022$feet[i] <= 1500) {
    sidefire_vol_spd_2022$feet[i] = 1500
  } 
  else if (sidefire_vol_spd_2022$feet[i] <= 2000) {
    sidefire_vol_spd_2022$feet[i] = 2000
  } 
  else if (sidefire_vol_spd_2022$feet[i] <= 2500) {
    sidefire_vol_spd_2022$feet[i] = 2500
  } 
  else {
    sidefire_vol_spd_2022$feet[i] = 3000
  } 
  
}

table(sidefire_vol_spd_2022$lane[which(sidefire_vol_spd_2022$weavetype=='A')], sidefire_vol_spd_2022$ffspd[which(sidefire_vol_spd_2022$weavetype == 'A')])
table(sidefire_vol_spd_2022$lane[which(sidefire_vol_spd_2022$weavetype=='B')], sidefire_vol_spd_2022$ffspd[which(sidefire_vol_spd_2022$weavetype == 'B')])
table(sidefire_vol_spd_2022$lane[which(sidefire_vol_spd_2022$weavetype=='C')], sidefire_vol_spd_2022$ffspd[which(sidefire_vol_spd_2022$weavetype == 'C')])

table(roadlink_2026$AMLN_AB[which(roadlink_2026$WEAVE_T=='A')], roadlink_2026$PKFRSPD_A[which(roadlink_2026$WEAVE_T=='A')])
table(roadlink_2026$AMLN_AB[which(roadlink_2026$WEAVE_T=='B')], roadlink_2026$PKFRSPD_A[which(roadlink_2026$WEAVE_T=='B')])
table(roadlink_2026$AMLN_AB[which(roadlink_2026$WEAVE_T=='C')], roadlink_2026$PKFRSPD_A[which(roadlink_2026$WEAVE_T=='C')])

setwd("C:/Users/rtu/OneDrive - The North Central Texas Council of Governments/Documents/0_ModelDataDevelopment/")


library(dplyr)
library('tidyr')
library('ggplot2')

frwy_basic_capacity = data.frame(matrix(0, nrow = length(unique(sidefire_vol_spd_2022$ffspd[which(sidefire_vol_spd_2022$weavetype == 'FRWY_BASIC')])), 
                                        ncol = 2))
colnames(frwy_basic_capacity) = c('capacity','ffspd')
frwy_basic_capacity$ffspd = sort(unique(sidefire_vol_spd_2022$ffspd[which(sidefire_vol_spd_2022$weavetype == 'FRWY_BASIC')]))
for (i in 1:nrow(frwy_basic_capacity)) {
  frwy_basic_capacity$capacity[i] = unique(sidefire_vol_spd_2022$hrcapperlane[which(sidefire_vol_spd_2022$weavetype == 'FRWY_BASIC' &
                                                                                      sidefire_vol_spd_2022$ffspd == frwy_basic_capacity$ffspd[i])])
}

# get data from roadway_2026
roadlink_weave = roadlink_2026[which(roadlink_2026$WEAVE_T == 'A' |
                                     roadlink_2026$WEAVE_T == 'B' | 
                                     roadlink_2026$WEAVE_T == 'C'),]

roadlink_weave$hrcapperlane = roadlink_weave$AMHRCAP_A/roadlink_weave$AMLN_AB

new_cols <- t(apply(roadlink_weave[, c("WEAVE_T", "AMLN_AB")], 1, function(row) specs_type(row[1], row[2])))
colnames(new_cols) <- c("VR", "Nwl")
roadlink_weave <- data.frame(cbind(roadlink_weave, new_cols))
rm(new_cols)

roadlink_weave <- roadlink_weave %>%
  rowwise() %>%
  mutate(
    hrcapperlane_hcm = capacity_hcm2016_wv(
      frwy_basic_capacity = frwy_basic_capacity,
      ffspd = PKFRSPD_A,
      lanes = AMLN_AB,
      length = LENGTH * 5280,
      VR = VR,
      Nwl = Nwl# or another value if needed
    )
  )

roadlink_weave <- roadlink_weave %>%
  rowwise() %>%
  mutate(
    maxlength = max_weave_length(
      lanes = AMLN_AB,
      VR = VR,
      Nwl = Nwl
    )
  )


plot_black(roadlink_weave$hrcapperlane[which(roadlink_weave$WEAVE_T == 'A')]/roadlink_weave$LENGTH[which(roadlink_weave$WEAVE_T == 'A')], 
           roadlink_weave$hrcapperlane_hcm[which(roadlink_weave$WEAVE_T == 'A')]/roadlink_weave$LENGTH[which(roadlink_weave$WEAVE_T == 'A')],
           'Capacity TAFT per mi', 'Capacity HCM2016 per mi', 'Weave capacity comparison')

points(roadlink_weave$hrcapperlane[which(roadlink_weave$WEAVE_T == 'B')]/roadlink_weave$LENGTH[which(roadlink_weave$WEAVE_T == 'B')], 
       roadlink_weave$hrcapperlane_hcm[which(roadlink_weave$WEAVE_T == 'B')]/roadlink_weave$LENGTH[which(roadlink_weave$WEAVE_T == 'B')],
       col = 'red', pch = 16, cex = 2)

points(roadlink_weave$hrcapperlane[which(roadlink_weave$WEAVE_T == 'C')]/roadlink_weave$LENGTH[which(roadlink_weave$WEAVE_T == 'C')], 
       roadlink_weave$hrcapperlane_hcm[which(roadlink_weave$WEAVE_T == 'C')]/roadlink_weave$LENGTH[which(roadlink_weave$WEAVE_T == 'C')],
       col = 'blue', pch = 16, cex = 2)


plot_black(roadlink_weave$hrcapperlane[which(roadlink_weave$WEAVE_T == 'A')], 
           roadlink_weave$hrcapperlane_hcm[which(roadlink_weave$WEAVE_T == 'A')],
           'Capacity TAFT per mi', 'Capacity HCM2016 per mi', 'Weave capacity comparison')

points(roadlink_weave$hrcapperlane[which(roadlink_weave$WEAVE_T == 'B')], 
       roadlink_weave$hrcapperlane_hcm[which(roadlink_weave$WEAVE_T == 'B')],
       col = 'red', pch = 16, cex = 2)

points(roadlink_weave$hrcapperlane[which(roadlink_weave$WEAVE_T == 'C')], 
       roadlink_weave$hrcapperlane_hcm[which(roadlink_weave$WEAVE_T == 'C')],
       col = 'blue', pch = 16, cex = 2)

roadlink_A = roadlink_weave[which(roadlink_weave$WEAVE_T == 'A'),]
roadlink_B = roadlink_weave[which(roadlink_weave$WEAVE_T == 'B'),]
roadlink_C = roadlink_weave[which(roadlink_weave$WEAVE_T == 'C'),]

# save weavetype	length	ffspd	lane	hrcapperlane	hrcapperlane_A_CIW1
wb = createWorkbook()
addWorksheet(wb, 'roadlink_A')
addWorksheet(wb, 'roadlink_B')
addWorksheet(wb, 'roadlink_C')
writeData(wb, 'roadlink_A', cbind(roadlink_A$WEAVE_T, roadlink_A$LENGTH, roadlink_A$PKFRSPD_A, roadlink_A$AMLN_AB, roadlink_A$hrcapperlane, roadlink_A$hrcapperlane_hcm))
writeData(wb, 'roadlink_B', cbind(roadlink_B$WEAVE_T, roadlink_B$LENGTH, roadlink_B$PKFRSPD_A, roadlink_B$AMLN_AB, roadlink_B$hrcapperlane, roadlink_B$hrcapperlane_hcm))
writeData(wb, 'roadlink_C', cbind(roadlink_C$WEAVE_T, roadlink_C$LENGTH, roadlink_C$PKFRSPD_A, roadlink_C$AMLN_AB, roadlink_C$hrcapperlane, roadlink_C$hrcapperlane_hcm))
saveWorkbook(wb, file = 'roadlink_weave.xlsx', overwrite = T)


## find the anchor point by using the most frequently ocurring category (ffspd + lane)
roadlink_A %>%
  group_by(WEAVE_T, PKFRSPD_A, AMLN_AB) %>%
  summarise(count = n(), .groups = "drop")

# select ffspd = 70 & lane = 5 as the anchor point
# set different length of weave segment, create 5 tables respectively. chnage Nwl and VR. 
# create first, 500 ft
vr_val = seq(0.1, 0.9, by = 0.1)
nw_val = c(2,3)
len_val = seq(500, 2500, by = 500)
ffspd = 70; lane = 5; length = 500

vr_val = c(0.225, 0.45, 0.3)
ffspd_val = seq(55, 75, 5)
nw_val = 2

df = data.frame(matrix(0, nrow = length(vr_val)*length(nw_val)*length(len_val)*length(ffspd_val), ncol = 5))
m = 0
for (i in 1:length(vr_val)) {
  for (j in 1:length(nw_val)) {
    for (p in 1:length(len_val)) {
      for (q in 1:length(ffspd_val)) {
        m = m + 1
        df[m,1] = vr_val[i]
        df[m,2] = nw_val[j]
        df[m,3] = len_val[p]
        df[m,4] = ffspd_val[q]
        df[m,5] = capacity_hcm2016_wv(frwy_basic_capacity = frwy_basic_capacity, ffspd_val[q], lane, len_val[p], vr_val[i], nw_val[j])
      }
    }
  }
}
colnames(df) = c('VR', 'Nwl', 'length_ft', 'ffspd', 'capacity')


type = c('A', 'B', 'C')
ffspd = seq(55,75,5) 
#lane = seq(3,5,1)

df_cap = data.frame(matrix(0, nrow = length(type)*length(ffspd)*length(len_val), ncol = 4))
m = 0
for (i in 1:length(type)) {
  for (j in 1:length(ffspd)) {
    for (k in 1:length(len_val)) {
      m = m + 1
      df_cap[m,1] = type[i]
      df_cap[m,2] = ffspd[j]
      df_cap[m,3] = len_val[k]
      params = specs_type(type[i])
      df_cap[m,4] = capacity_hcm2016_wv(frwy_basic_capacity = frwy_basic_capacity, ffspd[j], 3, len_val[k], params[1], params[2])
    }
  }
}
colnames(df_cap) = c('weavetype','ffspd','length','capacity')

df = df[order(df$Nwl),]

png(paste0("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/Rdwy_weave_capacity/weaveA_ffspd_", ffspd, "_lane_", lane, ".png"),
    width = 1500, height = 800, bg = 'black')
ggplot(df, aes(x = VR, y = factor(Nwl), fill = capacity)) +
  geom_tile() +
  scale_fill_viridis_c(direction =-1) +
  facet_wrap(~length_ft) +
  labs(title = "Heatmap of D by A and B, Faceted by C",
       x = "VR",
       y = "Nwl") +  
  theme_minimal(base_size = 18) +  # larger base font
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black"),
    legend.title = element_text(color = "white", size = 18, face = "bold"),
    legend.text = element_text(color = "white", size = 18, face = "bold"),
    axis.title = element_text(color = "white", size = 18, face = "bold"),
    axis.text = element_text(color = "white", size = 18, face = "bold"),
    plot.title = element_text(color = "white", size = 18, face = "bold"),
    strip.background = element_rect(fill = "gray20"),  # if faceted
    strip.text = element_text(color = "white", size = 18, face = "bold")
  )
dev.off()


############################################### use the hcm2016 with determined VR and Nwl to calculate new cap for weave ABC #################
############################################### use the hcm2016 with determined VR and Nwl to calculate new cap for weave ABC #################
############################################### use the hcm2016 with determined VR and Nwl to calculate new cap for weave ABC #################
new_cols <- t(apply(roadlink_weave[, c("WEAVE_T")], 1, function(row) specs_type(row[1])))
colnames(new_cols) <- c("VR", "Nwl")
roadlink_weave <- data.frame(cbind(roadlink_weave, new_cols))
rm(new_cols)

roadlink_weave <- roadlink_weave %>%
  rowwise() %>%
  mutate(
    hrcapperlane_hcm = capacity_hcm2016_wv(
      frwy_basic_capacity = frwy_basic_capacity,
      ffspd = PKFRSPD_A,
      lanes = AMLN_AB,
      length = LENGTH * 5280,
      VR = VR,
      Nwl = Nwl# or another value if needed
    )
  )

roadlink_weave <- roadlink_weave %>%
  rowwise() %>%
  mutate(
    maxlength = max_weave_length(
      lanes = AMLN_AB,
      VR = VR,
      Nwl = Nwl
    )
  )

png(paste0("20250410_capacity_recalculation/RoadNetwork_2026/Data_processing/Plot/Rdwy_weave_capacity/weave_capacity_hcm2016_newVRNwl.png"),
    width = 800, height = 600, bg = 'black')
plot_black(roadlink_weave$hrcapperlane[which(roadlink_weave$WEAVE_T == 'A')], 
           roadlink_weave$hrcapperlane_hcm[which(roadlink_weave$WEAVE_T == 'A')],
           'Capacity TAFT', 'Capacity HCM2016', 'Weave capacity comparison')

points(roadlink_weave$hrcapperlane[which(roadlink_weave$WEAVE_T == 'B')], 
       roadlink_weave$hrcapperlane_hcm[which(roadlink_weave$WEAVE_T == 'B')],
       col = 'red', pch = 16, cex = 2)

points(roadlink_weave$hrcapperlane[which(roadlink_weave$WEAVE_T == 'C')], 
       roadlink_weave$hrcapperlane_hcm[which(roadlink_weave$WEAVE_T == 'C')],
       col = 'blue', pch = 16, cex = 2)
dev.off()

######################################################### functions #########################################################
######################################################### functions #########################################################
######################################################### functions #########################################################

##### calculate type A capacity based on HCM2016 chapter 16
# Ciwl = Cifl - [438.2*(1+VR)^1.6]+0.00765*Len + 119.8*Nwl
# Cifl is the base capacity for frwy_basic at the same ffspd; VR is weaving ratio, take the mid point for different lanes; len is length; Nwl for A is 2
capacity_hcm2016_wv = function(frwy_basic_capacity, ffspd, lanes, length, VR, Nwl) {
  
  # length should be ft !!! 
  # ffspd mph
  
  # in case that ffspd of the weave is out of the range of frwy_basic ffspd
  if (ffspd < min(frwy_basic_capacity$ffspd)) {
    ffspd = min(frwy_basic_capacity$ffspd)
  }
  if (ffspd > max(frwy_basic_capacity$ffspd)) {
    ffspd = max(frwy_basic_capacity$ffspd)
  }
  # use lookup table frwy_basic_capacity to get Cifl
  Cifl = frwy_basic_capacity$capacity[which(frwy_basic_capacity$ffspd == ffspd)]
  # refers to HCM 2016, Chapter 13
  Ciwl = Cifl - 438.2*(1+VR)^1.6+0.0765*length + 119.8*Nwl
  
  return (Ciwl)
}

#### Lmax
max_weave_length = function(lanes, VR, Nwl) {
  # refers to HCM 2016, Chapter 13
  Lmax = (5728*(1+VR)^1.6-1566*Nwl)/5280 # in mile
  return(Lmax)
  
}

#### determine VR and Nwl based on specs
specs_type = function(type) {
  if (type == 'A') {
    Nwl = 2
    VR = 0.25
  } 
  if (type == 'B') {
    Nwl = 2.3
    VR = 0.208
  }
  if (type == 'C') {
    Nwl = 1
    VR = 0.3125
  }
  return(c(VR, Nwl))
}
