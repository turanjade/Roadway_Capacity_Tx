## this file calls function from vdf_fitting_function
# define x as v/c ratio, y as (1/V-1/V0)/(1/V0)
# use average volume from SF and average speed from NPMRDS, data table: sf_2022_npmrds_2025_plot, weave type = FRWY_BASIC, source = npm
sf_2022_npmrds_2025_plot$vdf_x = (sf_2022_npmrds_2025_plot$avgvol)/sf_2022_npmrds_2025_plot$hrcap
sf_2022_npmrds_2025_plot$vdf_t = as.numeric(sf_2022_npmrds_2025_plot$length)/sf_2022_npmrds_2025_plot$avgspd
sf_2022_npmrds_2025_plot$vdf_t0 = as.numeric(sf_2022_npmrds_2025_plot$length)/sf_2022_npmrds_2025_plot$ffspd
sf_2022_npmrds_2025_plot$vsq = (as.numeric(sf_2022_npmrds_2025_plot$length)/sf_2022_npmrds_2025_plot$avgspd)^2
sf_2022_npmrds_2025_plot$volperlane = as.numeric(sf_2022_npmrds_2025_plot$avgvol)/as.numeric(sf_2022_npmrds_2025_plot$lane)

# Example data
sf_2022_npmrds_2025_vdf = data.frame(x = sf_2022_npmrds_2025_plot$vdf_x, 
                                     t = sf_2022_npmrds_2025_plot$vdf_t,
                                     t0 = sf_2022_npmrds_2025_plot$vdf_t0,
                                     spd = sf_2022_npmrds_2025_plot$avgspd,
                                     ffspd = sf_2022_npmrds_2025_plot$ffspd,
                                     length = sf_2022_npmrds_2025_plot$length,
                                     cap = sf_2022_npmrds_2025_plot$hrcap/sf_2022_npmrds_2025_plot$lane,
                                     weavetype = sf_2022_npmrds_2025_plot$weavetype,
                                     source = sf_2022_npmrds_2025_plot$source,
                                     time = sf_2022_npmrds_2025_plot$time)


vdf_fitting_sf_npmrds_frwybasic = VDF_fitting(sf_2022_npmrds_2025_vdf_frwybasic, 
                                              'FRWY_BASIC',
                                              c('am','pm'),
                                              'npm',
                                              bound_a = c(0, seq(3,10)),
                                              bound_e = c(-Inf, 0),
                                              taft_params = c(8, -0.15),
                                              fn = nll_vdf)


vdf_fitting_sf_npmrds_basic = VDF_fitting(sf_2022_npmrds_2025_vdf, 
                                              'BASIC',
                                              c('am','pm'),
                                              'npm',
                                              bound_a = c(0, seq(3,10)),
                                              bound_e = c(-Inf, 0),
                                              taft_params = c(8, -0.15),
                                              fn = nll_vdf)

vdf_fitting_sf_npmrds_md = VDF_fitting(sf_2022_npmrds_2025_vdf, 
                                          'MD',
                                          c('am','pm'),
                                          'npm',
                                          bound_a = c(0, seq(3,10)),
                                          bound_e = c(-Inf, 0),
                                          taft_params = c(8, -0.15),
                                          fn = nll_vdf)


vdf_fitting_sf_npmrds_A = VDF_fitting(sf_2022_npmrds_2025_vdf, 
                                       'A',
                                       c('am','pm'),
                                       'npm',
                                       bound_a = c(0, seq(3,10)),
                                       bound_e = c(-Inf, 0),
                                       taft_params = c(8, -0.15),
                                       fn = nll_vdf)


bpr_fitting_sf_npmrds_frwybasic = BPR_fitting(sf_2022_npmrds_2025_vdf_frwybasic, 
                                              'FRWY_BASIC',
                                              c('am','pm'),
                                              'npm',
                                              bound_a = c(0, seq(3,10)),
                                              bound_b = c(0, 10),
                                              taft_params = c(2, 4),
                                              fn = nll_bpr)


# only use TAFT t to calibrate frwy_basic data
sf_2022_npmrds_2025_vdf_frwybasic_bpr = sf_2022_npmrds_2025_vdf_frwybasic
for (i in 1:nrow(sf_2022_npmrds_2025_vdf_frwybasic_bpr)) {
  sf_2022_npmrds_2025_vdf_frwybasic_bpr$t[i] = VDF(8, -0.15, sf_2022_npmrds_2025_vdf_frwybasic_bpr$t0[i], sf_2022_npmrds_2025_vdf_frwybasic_bpr$x[i])
}
bpr_fitting_sf_npmrds_frwybasic_field = BPR_fitting(sf_2022_npmrds_2025_vdf_frwybasic, 
                                              'FRWY_BASIC',
                                              c('am','pm'),
                                              'npm',
                                              bound_a = c(0, seq(3,10)),
                                              bound_b = c(0, 10),
                                              taft_params = c(2, 4),
                                              fn = nll_bpr)

bpr_fitting_sf_npmrds_frwybasic_taft = BPR_fitting(sf_2022_npmrds_2025_vdf_frwybasic_bpr, 
                                                    'FRWY_BASIC',
                                                    c('am','pm'),
                                                    'npm',
                                                    bound_a = c(0, seq(3,10)),
                                                    bound_b = c(0, 10),
                                                    taft_params = c(2, 4),
                                                    fn = nll_bpr)

bpr_arc_fitting_sf_npmrds_frwybasic_field = BPRarc_fitting(sf_2022_npmrds_2025_vdf_frwybasic, 
                                                    'FRWY_BASIC',
                                                    c('am','pm'),
                                                    'npm',
                                                    bound_a = c(0.1, seq(0.2,1,0.1)),
                                                    bound_b = seq(0.1, 8, 1),
                                                    bound_d = seq(0.1, 2, 0.5),
                                                    bpr_params = c(0.1, 6, 0.6),
                                                    fn = nll_bpr_arc)
