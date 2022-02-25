library(rgeos)
library(igraph)
library(ggplot2)
library(tidyverse)
library(sf)

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
  df_pairs <- as_data_frame(graph_from_adjacency_matrix(mtx_shp))
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


#find sum of crimes of pairs
df_quads_sums <- morning %>%
  st_drop_geometry() %>%
  select(region, sum)
morn_pairs_test <- left_join(morn_pairs_test, df_quads_sums, by = c("from" = "region")) %>%
  rename(sum_from = sum)
morn_pairs_test <- left_join(morn_pairs_test, df_quads_sums, by = c("to" = "region")) %>%
  rename(sum_to = sum)
morn_pairs_test <- morn_pairs_test %>%
  mutate(sum_crimes = sum_from + sum_to) %>%
  select(c(-sum_from, - sum_to)) %>%
  arrange(sum_crimes)








manrique <- df_shifts_avg %>%
  filter(station == "MANRIQUE")

manrique_morn <- manrique %>%
  filter(shift == "5-13")
manrique_aftn <- manrique %>%
  filter(shift == "13-21")
manrique_nght <- manrique %>%
  filter(shift == "21-5")

manrique_morn %>% 
  st_drop_geometry() %>%
  group_by(region) %>%
  dplyr::summarise(crime_sum = sum(sum))

# source: https://stackoverflow.com/questions/45497742/generate-all-possible-combinations-of-rows-in-r
matrix <- expand.grid(quad_1 = manrique_morn$region, quad_2 = manrique_morn$region)

matrix[, paste("teacher", c("height", "smart"), sep="_")] <- 
  teachers[match(res$teacher_name, teachers$name), c("height","smart")]













filter_pairs <- function(df_pairs, scode){
  df_temp <- df_pairs %>% filter(grepl(scode, from))
  df_temp <- df_temp %>% filter(grepl(scode, to))
  return(df_temp)
}

df_d1e1 <- filter_pairs(df_pairs,'D01E01')

sum_pairs <- function(df_pairs,df_shift){
  df_temp <- df_pairs %>%
    left_join(st_drop_geometry(df_shift[,c('region','sum')]),by = c("from" = "region"))
  colnames(df_temp)[3] = 'sum0'
  df_temp <- df_temp %>%
    left_join(st_drop_geometry(df_shift[,c('region','sum')]),by = c("to" = "region"))
  colnames(df_temp)[4] = 'sum1'
  df_temp$sum <- df_temp$sum0 + df_temp$sum1
  return(df_temp)
}

manrique_morn_pairs <- sum_pairs(df_d1e1, manrique_morn) %>%
  select("from", "to", "sum") %>%
  arrange(sum)
manrique_aftn_pairs <- sum_pairs(df_d1e1, manrique_aftn) %>%
  select("from", "to", "sum") %>%
  arrange(sum)
manrique_nght_pairs <- sum_pairs(df_d1e1, manrique_nght) %>%
  select("from", "to", "sum") %>%
  arrange(sum)

ggplot() +
  geom_sf(data = manrique_morn,
          aes(fill = sum))
ggplot() +
  geom_sf(data = manrique_aftn,
          aes(fill = sum))
ggplot() +
  geom_sf(data = manrique_nght,
          aes(fill = sum))


