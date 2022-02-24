## run this after running 1.2_Preprocessing
library(rgeos)
library(igraph)


manrique <- df_shifts_avg %>%
  filter(station == "MANRIQUE")

manrique_morn <- manrique %>%
  filter(shift == "5-13")
  
manrique_morn %>% 
  st_drop_geometry() %>%
  group_by(region, shift) %>%
  dplyr::summarise(crime_sum = sum(sum))

# source: https://stackoverflow.com/questions/45497742/generate-all-possible-combinations-of-rows-in-r
matrix <- expand.grid(quad_1 = manrique_morn$region, quad_2 = manrique_morn$region)

matrix[, paste("teacher", c("height", "smart"), sep="_")] <- 
  teachers[match(res$teacher_name, teachers$name), c("height","smart")]











shp2mtx <- function(df_shp){
  df_shp_sp <- as(df_shp, Class = "Spatial")
  matrix_shp <- 1*gTouches(df_shp_sp, byid=TRUE)
  colnames(matrix_shp) <- df_shp_sp$region
  rownames(matrix_shp) <- df_shp_sp$region
  return(matrix_shp)
}

mtx2df_pairs <- function(mtx_shp){
  df_pairs <- as_data_frame(graph_from_adjacency_matrix(mtx_shp))
  return(df_pairs)
}

mtx_shp <- shp2mtx(df_shp)
df_pairs <- mtx2df_pairs(mtx_shp)


filter_pairs <- function(df_pairs,scode){
  df_temp <- df_pairs %>% filter(grepl(scode, from))
  df_temp <- df_temp %>% filter(grepl(scode, to))
  return(df_temp)
}

df_d1e1 <- filter_pairs(df_pairs,'D01E01')



