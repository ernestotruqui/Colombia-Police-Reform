library(rgeos)
library(igraph)
library(ggplot2)
library(tidyverse)
library(sf)


PATH <- "E://Files/HaHaHariss/22Winter/Policy Lab/Data"
# PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data"
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

df_all <- into_final_df(crit_value = 10)

# function to indicate which quads to merge together
which_to_merge <- function(){
  to_merge <- df_all %>%
    rename(merge_with = to) %>%
    select(from, shift, merge_with, geometry_to)
  df_final <- left_join(df_shifts_avg, to_merge, by = c("region" = "from", "shift"))
}

df_final <- which_to_merge()


#MERGE POLYGONS####
#test on afternoon shift
df_aftn <- df_final[which(df_final$shift=="13-21"),]
df_aftn$group <- ifelse(is.na(df_aftn$merge_with)==TRUE,
                        df_aftn$region,df_aftn$merge_with)

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
  colnames(sp2) <- c('region','sum','geometry')
  rownames(sp2) <- 1:nrow(sp2)
  return(sp2) 
}  

df_aftn_m <- join_by_group(df_aftn,'group')
plot(df_aftn_m)
