find_closest_linkmatch = function(line_, point_, buffer = 100) {
  
  # Buffer points by 100 feet
  points_buffer <- st_buffer(point_, dist = buffer) # ease to 150? -- no, keep consistent with Francisco
  
  # Spatial join: get all lines within 100 ft of any point
  # This creates a row for each line-point pair
  lines_within_100ft <- st_join(line_, points_buffer, join = st_intersects, left = FALSE)
  
  # option 2 is to use street name. First use FUNCL 
  lines_within_100ft_ = lines_within_100ft[which(!is.na(lines_within_100ft$ID.y)),] # remove na
  
  points_id = unique(lines_within_100ft_$ID.y)
  
  point_linkmatch_ = data.frame(matrix(0, nrow = 0, ncol = ncol(lines_within_100ft_)))
  
  a = 0 # record how many reference stations cannot find links
  dir_a = 0 # how many links cannot find the matched station by dir
  funcl_a = 0 # how many links cannot find the matched station by funcl
  
  for (p in 1:length(points_id)) {
    link_i = lines_within_100ft_[which(lines_within_100ft_$ID.y == points_id[p]),]
    if (nrow(link_i) == 0) {
      print(paste('reference station', points_id[p],'cannot find matched NPMRDS link within 100ft buffer'))
      a = a + 1
      next
    }
    match_i = matrix(0, nrow = 0, ncol = 2)
    for (j in 1:nrow(link_i)) {
      #initiate dir and funcl decision factor as 0. If it becomes 1 after the check, then it passes
      dir_j = 0; funcl_j = 0 
      ############################################# check dir ##########################################
      if (link_i$Dir.x[j] == link_i$Dir.y[j])  {
        dir_j = 1 # pass dir check
      } 
      
      ############################################# check funcl ##########################################
      if (link_i$FUNCL.x[j] == link_i$FUNCL.y[j]) {
        funcl_j = 1
      } 
      
      if (dir_j == 0) {
        dir_a = dir_a + 1 # how many links cannot find the matched station by dir
      }
      if (funcl_j == 0) {
        funcl_a = funcl_a + 1 # how many links cannot find the matched station by funcl
      }
      
      
      match_i = rbind(match_i, c(dir_j*funcl_j, st_length(st_nearest_points(point_[which(point_$ID == points_id[p]),], 
                                                                            line_[which(line_$ID == link_i$ID.x[j]),])))) # combine the distance with two criteria check
      
    }
    choose_i = which(match_i[,1] != 0 & match_i[,2] == min(match_i[,2]))
    rowtocombine = link_i[choose_i,]
    
    point_linkmatch_ = rbind(point_linkmatch_, rowtocombine)
  }
  print(paste0(dir_a, ' links cannot find matched station by dir'))
  print(paste0(funcl_a, ' links cannot find matched station by funcl'))
  print(paste0(a, ' total links cannot find matched station by buffer'))
  return(point_linkmatch_)
}
