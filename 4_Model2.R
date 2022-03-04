#install.packages("os")
#library(os)
library(rgeos)
library(igraph)
library(ggplot2)
library(tidyverse)
library(maptools)
library(sf)
library(maptools)
library(cowplot)


#PATH <- "E://Files/HaHaHariss/22Winter/Policy Lab/Data"
 PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data"
df_shifts_avg <- st_read(file.path(PATH, "df_shifts_avg.shp"))
morning <- df_shifts_avg %>%
  filter(shift == "5-13") %>%
  select("region", "station", "sum", "geometry")
afternoon <- df_shifts_avg %>%
  filter(shift == "13-21") %>%
  select("region", "station", "sum", "geometry")
night <- df_shifts_avg %>%
  filter(shift == "21-5") %>%
  select("region", "station", "sum", "geometry")
shp2mtx <- function(df_shp){
  df_shp_sp <- as(df_shp, Class = "Spatial")
  matrix_shp <- 1*gTouches(df_shp_sp, byid = TRUE)
  for (i in 1:nrow(matrix_shp)) {
    for (j in 1:ncol(matrix_shp)) {
      if (i > j) {
        matrix_shp[i, j] <- 0
      }
    }
  }
  colnames(matrix_shp) <- df_shp_sp$region
  rownames(matrix_shp) <- df_shp_sp$region
  return(matrix_shp)
}
mtx2df_pairs <- function(mtx_shp){
  df_pairs <- igraph::as_data_frame(graph_from_adjacency_matrix(mtx_shp))
  return(df_pairs)
}
# matrix of all contiguous pairs
mtx_morn <- shp2mtx(morning)
# df of all contiguous pairs
pairs <- mtx2df_pairs(mtx_morn)
# function to filter only contiguous quads belonging to same station
get_stations <- function(df_pairs = pairs, df_stations = morning){
  df_shifts_stations <- df_stations %>%
    st_drop_geometry() %>%
    select(region, station)
  morn_pairs_test <- left_join(df_pairs, df_shifts_stations, by = c("from" = "region")) %>%
    rename(station_from = station) 
  morn_pairs_test <- left_join(morn_pairs_test, df_shifts_stations, by = c("to" = "region")) %>%
    rename(station_to = station)
  morn_pairs_test <- morn_pairs_test %>%
    filter(station_from == station_to)
}
# only quads within same station
df_pairs_stations <- get_stations()
# function to find sum of crimes by shift
## input df_timeofday = morning/afternoon/night and shift = 5-13/13-21/21-5
get_crime_sum <- function(df_pairs_stations = df_pairs_stations, df_timeofday, shift){
  df_quads_sums <- df_timeofday %>%
    st_drop_geometry() %>%
    select(region, sum)
  df_temp <- left_join(df_pairs_stations, df_quads_sums, by = c("from" = "region")) %>%
    rename(sum_from = sum)
  df_temp <- left_join(df_temp, df_quads_sums, by = c("to" = "region")) %>%
    rename(sum_to = sum)
  df_temp <- df_temp %>%
    mutate(sum_crimes = sum_from + sum_to,
           shift = shift) %>%
    select(c(-sum_from, - sum_to)) %>%
    arrange(sum_crimes) 
  df_temp <- df_temp %>%
    group_by(from) %>%
    filter(sum_crimes == min(sum_crimes),
           !duplicated(from)) %>%
    ungroup() %>%
    group_by(to) %>%
    filter(sum_crimes == min(sum_crimes),
           !duplicated(to)) %>%
    ungroup()
  
}
morning_pairs <- get_crime_sum(df_pairs_stations = df_pairs_stations, df_timeofday = morning, shift = "5-13")
afternoon_pairs <- get_crime_sum(df_pairs_stations = df_pairs_stations, df_timeofday = afternoon, shift = "13-21")
night_pairs <- get_crime_sum(df_pairs_stations = df_pairs_stations, df_timeofday = night, shift = "21-5")

# function that saves value on "from" column and drops any row with that repeated value in "to" column
elimin_repeats <- function(df, index){
  value <- as.character(df[index, 1])
  df <- df %>%
    filter(to != value)
  value_to <- as.character(df[index, 2])
  df <- df %>%
    filter(from != value_to)
  return(df)
}
for (i in 1:nrow(morning_pairs)) {
  ifelse(i <= nrow(morning_pairs), 
         morning_pairs <- elimin_repeats(df = morning_pairs, index = i),
         break)
}
for (i in 1:nrow(afternoon_pairs)) {
  ifelse(i <= nrow(afternoon_pairs), 
         afternoon_pairs <- elimin_repeats(df = afternoon_pairs, index = i),
         break)
}
for (i in 1:nrow(night_pairs)) {
  ifelse(i <= nrow(night_pairs), 
         night_pairs <- elimin_repeats(df = night_pairs, index = i),
         break)
}
# function to make final df with all contiguous pairs of quads within same station across all shifts whose sum of crimes is below a given critical value
into_final_df <- function(crit_value){
  critical_value <- crit_value
  df_all <- morning_pairs %>%
    rbind(afternoon_pairs) %>%
    rbind(night_pairs) %>%
    filter(sum_crimes < critical_value)
  df_geoms <- df_shifts_avg %>%
    filter(shift == "5-13") %>%
    select(region, geometry)
  df_all <- left_join(df_all, df_geoms, by = c("from" = "region")) %>%
    rename(geometry_from = geometry)
  df_all <- left_join(df_all, df_geoms, by = c("to" = "region")) %>%
    rename(geometry_to = geometry)
}

# choose 55 as optimal cutoff based on minimization of variance of crimes per officer
df_all <- into_final_df(crit_value = 55)

# function to indicate which quads to merge together
which_to_merge <- function(){
  to_merge <- df_all %>%
    rename(merge_with = to) %>%
    select(from, shift, merge_with, geometry_to)
  df_final <- left_join(df_shifts_avg, to_merge, by = c("region" = "from", "shift"))
}

df_final <- which_to_merge()

#PART2####
rm(list= ls()[!(ls() %in% c('df_final','PATH'))])

##fxns####
join_by_group <- function(df_shp,cname){
  
  group <- st_drop_geometry(df_shp)[,cname]
  sp <- as(df_shp, Class = "Spatial")
  reg4 <- unionSpatialPolygons(sp, group)
  df_temp1 <- as(sp, "data.frame")
  
  # sapply(df_temp1, class)
  df_temp2 <- aggregate(df_temp1[,'sum'], list(group), sum)
  row.names(df_temp2) <- as.character(df_temp2$Group.1)
  sp2 <- SpatialPolygonsDataFrame(reg4, df_temp2)
  
  # reformat
  sp2 <- st_as_sf(sp2)
  colnames(sp2) <- c('region', 'sum', 'geometry')
  rownames(sp2) <- 1:nrow(sp2)
  
  # filter merged ones
  # sp2 <- sp2[which(sp2$region %in% unique(df_shp$merge_with)),]
  
  return(sp2) 
} 

redistribute <- function(df, colname){
  df$temp <- 1 + (((522 - (nrow(df))) * st_drop_geometry(df)[, colname] / sum(st_drop_geometry(df)[, colname])))
  df$temp <- round(df$temp)
  return(df$temp)
}

crime_per_police <- function(df, crime_type, n_of_police = ''){
  df <- st_drop_geometry(df)
  if (n_of_police == '') {
    df$temp <- df[, crime_type] / 2
  }
  else {
    df$temp <- df[, crime_type] / df[, n_of_police]
  }
  return(df$temp)
}

plot_cpp <- function(df_temp, df_m_temp, shift){
  p_cpp <- ggplot() +
    geom_sf(data = df_temp, aes(fill = rcpp))+
    geom_sf(data = df_m_temp, colour = 'red', fill = NA)+
    labs(fill = "Crimes per Officer",
         color = "Crimes per Officer") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
    scale_color_viridis_c(option = "inferno", limits = c(0, 80)) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
  
  return(p_cpp)
}

plot_nofp <- function(df_temp, df_m_temp, shift){
  ltitle <- ifelse(shift=='5-13',"Morning shift (5:00 - 13:00)",
                   ifelse(shift=='13-21',"Afternoon shift (13:00 - 21:00)",
                          "Night shift (21:00 - 5:00)"))
  p_nofp <- ggplot() +
    geom_sf(data = df_temp, aes(fill = rn_f_pl))+
    geom_sf(data = df_m_temp, colour = 'red', fill=NA)+
    labs(fill = "Number of Officers",
         color = "Number of Officers") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    scale_fill_viridis_c(option = "inferno", limits = c(1, 6)) +
    scale_color_viridis_c(option = "inferno", limits = c(1, 6)) 
  
  return(p_nofp)
}

part2 <- function(df_final, shift) {
  
  # sample the shift
  df_temp <- df_final[which(df_final$shift == shift),]
  df_temp$group <- ifelse(is.na(df_temp$merge_with) == TRUE, df_temp$region, df_temp$merge_with)
  
  # join by group
  df_temp_m <- join_by_group(df_temp, 'group')
  df_m_temp <- df_temp_m[which(df_temp_m$region %in% unique(df_temp$merge_with)),]
  
  # redistribute
  df_temp_m$n_f_plc <- 2
  df_temp_m$rn_f_pl <- redistribute(df_temp_m, 'sum')
  
  # crime per police
  df_temp_m$cpp <- crime_per_police(df_temp_m, 'sum','n_f_plc')
  df_temp_m$rcpp <- crime_per_police(df_temp_m, 'sum', 'rn_f_pl')
  
 
  # plotting cpp
  p_status_quo <- ggplot() +
    geom_sf(data = df_temp[df_temp$shift == shift,],
            aes(fill = cpp)) +
    geom_sf(data = df_m_temp, colour = 'red', fill = NA)+
    labs(fill = "Crimes per Officer",
         color = "Crimes per Officer") +
    scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
    scale_color_viridis_c(option = "inferno", limits = c(0, 80)) +
    labs(subtitle = "Status Quo") +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 15),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
  p_cpp_before <- plot_cpp(df_temp, df_m_temp, shift) +
    labs(title = "Simple Redistribution") +
    theme(plot.title = element_text(hjust = 0.5, size = 15))
  p_cpp_after <- plot_cpp(df_temp_m, df_m_temp, shift) +
    labs(subtitle = "Dynamic Quadrant Redistribution") +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 15))
  
   p_cpp <- plot_grid(p_status_quo, p_cpp_before, p_cpp_after,
            ncol = 3)
  
  # plotting n_of_police
  p_nofp_before <- plot_nofp(df_temp, df_m_temp, shift)
  p_nofp_after <- plot_nofp(df_temp_m, df_m_temp, shift)
  
  ptitle <- ggplot() + 
    labs(title = paste('Number of Officers in ',ltitle, sep = ''), 
         subtitle = "before (left) and after (right) clustering")
  p_nofp <- plot_grid(ptitle, plot_grid(p_nofp_before, p_nofp_after),
                      ncol = 1, rel_heights = c(0.1, 1))
 
  return(p_cpp)
}

compare_redis <- function(df_final, shift) {
  
  plot_cpp <- function(df_temp, df_m_temp, shift){
    p_cpp <- ggplot() +
      geom_sf(data = df_temp, aes(fill = rcpp))+
      geom_sf(data = df_m_temp, colour = 'red', fill = NA, lty = 1)+
      labs(fill = "Crimes per Officer",
           color = "Crimes per Officer") +
      theme(plot.title = element_text(hjust = 0.5, size = 10)) +
      scale_fill_viridis_c(option = "inferno", limits = c(0, 30)) +
      scale_color_viridis_c(option = "inferno", limits = c(0, 30)) +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank())
    
    return(p_cpp)
  }
  
    # sample the shift
  df_temp <- df_final[which(df_final$shift == shift),]
  df_temp$group <- ifelse(is.na(df_temp$merge_with) == TRUE, df_temp$region, df_temp$merge_with)
  
  # join by group
  df_temp_m <- join_by_group(df_temp, 'group')
  df_m_temp <- df_temp_m[which(df_temp_m$region %in% unique(df_temp$merge_with)),]
  
  # redistribute
  df_temp_m$n_f_plc <- 2
  df_temp_m$rn_f_pl <- redistribute(df_temp_m, 'sum')
  
  # crime per police
  df_temp_m$cpp <- crime_per_police(df_temp_m, 'sum','n_f_plc')
  df_temp_m$rcpp <- crime_per_police(df_temp_m, 'sum', 'rn_f_pl')
  
  p_cpp_before <- plot_cpp(df_temp, df_m_temp, shift) +
    labs(title = "Simple Redistribution") +
    theme(plot.title = element_text(hjust = 0.5, size = 15)) 
  p_cpp_after <- plot_cpp(df_temp_m, df_m_temp, shift) +
    labs(subtitle = "Dynamic Quadrant Redistribution") +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 15))
  
  p_cpp <- plot_grid(p_cpp_before, p_cpp_after,
                     ncol = 2)
  
  return(p_cpp)
  
}

part2_nofp <- function(df_final, shift) {
  
  # sample the shift
  df_temp <- df_final[which(df_final$shift == shift),]
  df_temp$group <- ifelse(is.na(df_temp$merge_with) == TRUE, df_temp$region, df_temp$merge_with)
  
  # join by group
  df_temp_m <- join_by_group(df_temp, 'group')
  df_m_temp <- df_temp_m[which(df_temp_m$region %in% unique(df_temp$merge_with)),]
  
  # redistribute
  df_temp_m$n_f_plc <- 2
  df_temp_m$rn_f_pl <- redistribute(df_temp_m, 'sum')
  
  p_nofp_before <- plot_nofp(df_temp, df_m_temp, shift)
  p_nofp_after <- plot_nofp(df_temp_m, df_m_temp, shift)
  
  ptitle <- ggplot() + 
    labs(title = paste('Number of Officers in ',ltitle, sep = ''), 
         subtitle = "before (left) and after (right) clustering")
  p_nofp <- plot_grid(ptitle, plot_grid(p_nofp_before, p_nofp_after),
                      ncol = 1, rel_heights = c(0.1, 1))
  return(p_nofp)
}


### final maps ####

# final plots for morning
p_morning_cpp <- part2(df_final,'5-13') 
ggsave(filename = "p_morning_cpp.png",
       plot = p_morning_cpp,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
p_morning_redis <- compare_redis(df_final, "5-13")
ggsave(filename = "p_morning_redis.png",
       plot = p_morning_redis,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
p_morning_pol_58 <- part2_nofp(df_final, "5-13")
ggsave(filename = "p_morning_pol_58.png",
       plot = p_morning_pol_58,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

# final plots for afternoon
p_afternoon_cpp <- part2(df_final,'13-21') 
ggsave(filename = "p_afternoon_cpp.png",
       plot = p_afternoon_cpp,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
p_afternoon_redis <- compare_redis(df_final, "13-21")
ggsave(filename = "p_afternoon_redis.png",
       plot = p_afternoon_redis,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
p_afternoon_pol_58 <- part2_nofp(df_final, "13-21")
ggsave(filename = "p_afternoon_pol_58.png",
       plot = p_afternoon_pol_58,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

# final plots for night
p_night_cpp <- part2(df_final,'21-5') 
ggsave(filename = "p_night_cpp.png",
       plot = p_night_cpp,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
p_night_redis <- compare_redis(df_final, "21-5")
ggsave(filename = "p_night_redis.png",
       plot = p_night_redis,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
p_night_pol_58 <- part2_nofp(df_final, "21-5")
ggsave(filename = "p_night_pol_58.png",
       plot = p_night_pol_58,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")







## think about ideal crit value cutoffs
morn_deciles <- quantile(morning_pairs$sum_crimes, probs = seq(0, 1, 0.1))
aftn_deciles <- quantile(afternoon_pairs$sum_crimes, probs = seq(0, 1, 0.1))
nght_deciles <- quantile(night_pairs$sum_crimes, probs = seq(0, 1, 0.1))  
 







