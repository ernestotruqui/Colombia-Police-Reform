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
library(hrbrthemes)


PATH <- "E://Files/HaHaHariss/22Winter/Policy Lab/Data"
# PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data"

#PART1####

##fxns#####
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

elimin_repeats <- function(df, index){
  value <- as.character(df[index, 1])
  df <- df %>%
    filter(to != value)
  value_to <- as.character(df[index, 2])
  df <- df %>%
    filter(from != value_to)
  return(df)
}

get_df_all_raw <- function(morning_pairs,afternoon_pairs,night_pairs){
  df_all_raw <- morning_pairs %>%
    rbind(afternoon_pairs) %>%
    rbind(night_pairs)
  return(df_all_raw)
}

##run####

#read df
df_shifts_avg <- st_read(file.path(PATH, "df_shifts_avg.shp"))

#sep into 3 shifts
morning <- df_shifts_avg %>%
  filter(shift == "5-13") %>%
  select("region", "station", "sum", "geometry")
afternoon <- df_shifts_avg %>%
  filter(shift == "13-21") %>%
  select("region", "station", "sum", "geometry")
night <- df_shifts_avg %>%
  filter(shift == "21-5") %>%
  select("region", "station", "sum", "geometry")

# matrix of all contiguous pairs
mtx_morn <- shp2mtx(morning)

# df of all contiguous pairs
pairs <- mtx2df_pairs(mtx_morn)

# only quads within same station
df_pairs_stations <- get_stations()

## input df_timeofday = morning/afternoon/night and shift = 5-13/13-21/21-5
morning_pairs <- get_crime_sum(df_pairs_stations = df_pairs_stations, df_timeofday = morning, shift = "5-13")
afternoon_pairs <- get_crime_sum(df_pairs_stations = df_pairs_stations, df_timeofday = afternoon, shift = "13-21")
night_pairs <- get_crime_sum(df_pairs_stations = df_pairs_stations, df_timeofday = night, shift = "21-5")

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

df_all_raw <- get_df_all_raw(morning_pairs,afternoon_pairs,night_pairs)
#PART2####
lst <- c('df_shifts_avg','PATH','df_all_raw')
rm(list= ls()[!(ls() %in% lst)])

##fxns####
into_final_df <- function(df_all_raw,crit_value){
  df_all <- df_all_raw%>%
    filter(sum_crimes < crit_value)
  df_geoms <- df_shifts_avg %>%
    filter(shift == "5-13") %>%
    select(region, geometry)
  df_all <- left_join(df_all, df_geoms, by = c("from" = "region")) %>%
    rename(geometry_from = geometry)
  df_all <- left_join(df_all, df_geoms, by = c("to" = "region")) %>%
    rename(geometry_to = geometry)
  return(df_all)
}

which_to_merge <- function(df_all){
  to_merge <- df_all %>%
    rename(merge_with = to) %>%
    select(from, shift, merge_with, geometry_to)
  df_final <- left_join(df_shifts_avg, to_merge, by = c("region" = "from", "shift"))
  return(df_final)
}

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

get_df_shift_dy <- function(df_all_raw, crit_value){
  df_shift_dy <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df_shift_dy) <- c('region', 'cpp', 'shift')
  
  # get df_all
  df_all <- into_final_df(df_all_raw, crit_value)
  
  # function to indicate which quads to merge together
  df_final <- which_to_merge(df_all)
  
  for (shift in unique(df_all_raw$shift)) {
    
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
    df_temp_m$cpp <- crime_per_police(df_temp_m, 'sum','rn_f_pl')
    
    # get df_out
    df_out <- st_drop_geometry(df_temp_m)[,c('region','cpp')]
    df_out$shift <- shift
    
    # row bind
    df_shift_dy <- rbind(df_shift_dy,df_out)
  }
  
  return(df_shift_dy)
}

df_shift_dy <- get_df_shift_dy(df_all_raw,55)

plot_cpp_new <- function(df_shift_dy,df_shifts_avg){
  df_plot <- data.frame(cpp = c(df_shifts_avg$cpp,df_shifts_avg$rcpp,df_shift_dy$cpp),
                        group = c(rep('Status Quo',nrow(df_shifts_avg)),
                                  rep('Proportional Distribution',nrow(df_shifts_avg)),
                                  rep('Dynamic Quadrant',nrow(df_shift_dy))))
  p <- ggplot(df_plot, aes(x = cpp, color = group, fill = group)) +
    geom_histogram(aes(y=..density..), alpha = 0.05, position = "identity", binwidth = 1)+
    geom_density(alpha = .2)+
    ggtitle('Crimes per officer') +
    xlab(label = "Crimes per officer by quadrant-shift") +
    theme(plot.title = element_text(hjust = 0.5, size = 15)) +
    ylim(0, 0.1)+xlim(0,85)+
    theme(plot.subtitle = element_text(hjust = 0.5, size = 10))
  return(p)
}

###run: plot_cpp_new####
p_cpp_new <- plot_cpp_new(df_shift_dy,df_shifts_avg)
p_cpp_new
###(cont)####
c_v_m <- function(df_all_raw, crit_value, shift) {
  # get df_all
  df_all <- into_final_df(df_all_raw, crit_value)
  
  # function to indicate which quads to merge together
  df_final <- which_to_merge(df_all)
  
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
  
  # stats
  var_temp <- var(df_temp_m$rcpp)
  mean_temp <- mean(df_temp_m$rcpp)
  max_temp <- max(df_temp_m$rcpp)
  min_temp <- min(df_temp_m$rcpp)
  median_temp <- median(df_temp_m$rcpp)

  return(c(crit_value, mean_temp, min_temp, median_temp, max_temp, var_temp))
}

sstats <- function(df_shifts_avg,shift){
  rcpp <- df_shifts_avg[which(df_shifts_avg$shift==shift),10]
  mea <- mean(rcpp)
  min <- min(rcpp)
  med <- median(rcpp)
  max <- max(rcpp)
  var <- var(rcpp)
  return(c(mea,min,med,max,var))
}






get_df_sens <- function(nmin,nmax,df_all_raw,shift){
  df_temp <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(df_temp) <- c('crit_value','mean', 'min', 'median', 'max', 'variance')
  for (n in c(nmin:nmax)) {
    df_temp[nrow(df_temp) + 1,] = c_v_m(df_all_raw,n,shift)
  }
  return(df_temp)
}

## run ####
df_sens_morning <- get_df_sens(1,100,df_all_raw,'5-13')
df_sens_afternoon <- get_df_sens(1,100,df_all_raw,'13-21')
df_sens_night <- get_df_sens(1,100,df_all_raw,'21-5')

temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

p_n <- ggplot(df_sens_night, aes(x=crit_value)) +
  geom_line(aes(y=mean), size=1,colour = "black") + 
  geom_line(aes(y=variance/2),size=1,colour = "darkred") +
  scale_y_continuous(name = "Mean",sec.axis = sec_axis(~.*2, name="Variance")) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = "black",size=14),
    axis.title.y.right = element_text(color = "darkred",size=14)
  )+
  scale_fill_viridis_c(option = "inferno", limits = c(1, 8)) +
  ggtitle('Night')

p_m
p_a
p_n

plot_grid(p_m, p_a, p_n, ncol = 3)
