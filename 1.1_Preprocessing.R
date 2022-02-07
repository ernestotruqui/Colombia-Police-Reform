# Set Environment####
## set wd and path####

#PATH <- "E://Files/HaHaHariss/22Winter/Policy Lab/Data"
 PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data" 

## load libraries####
library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggeasy)

# Pre-processing Crime Data ####
clean_crime_data <- function(df_crime){
  df_crime$lat <- str_replace(df_crime$lat, ',' , '.')
  df_crime$lon <- str_replace(df_crime$lon, ',' , '.')
  return(df_crime)
}

read_crime_data <- function(fname, sname, path = PATH){
  df <- read_xlsx(file.path(PATH, fname), sheet = sname)
  if ('cod_dane' %in% colnames(df)) {
    df <- filter(df, as.numeric(cod_dane) == 5001000)
    df <- mutate(df, time = as.numeric(hour(time)))
  }
  else {
    df <- filter(df, zone == "05001000")   
  }
  df <- clean_crime_data(df) %>%
    mutate(crime_type = sname) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    select(c(crime_type, date, time, geometry))  
}

merge_crime_data <- function(fname, path = PATH){
  names <- excel_sheets(file.path(path, fname))[c(2,3,4,5)]
  df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(df) <- c('crime_type', 'date', 'time', 'geometry')
  for (name in names) {
    df_temp <- read_crime_data(fname, name)
    df <- rbind(df, df_temp)
  }
  # rename crime types
  crime_types <- c('vehicle theft', 'theft', 'homicide', 'burglary')
  df$crime_type[df$crime_type %in% names] <- crime_types[match(df$crime_type, names, nomatch = 0)]
  
  # turn time into shifts
  df$shift <- ifelse(df$time %in% c(5,6,7,8,9,10,11,12),'5-13',
                             ifelse(df$time %in% c(13,14,15,16,17,18,19,20),
                                    '13-21','21-5'))
  return(df)
}

df_crime19 <- merge_crime_data('CrimesV2.xlsx')

# Pre-processing Shapefile ####
clean_shp <- function(df_shp){
  df_shp <- df_shp[!substr(as.character(df_shp$NRO_CUADRA), 13, 13) %in% c('6', '7'),]
  return(df_shp)
}

p2p <- function(df_crime,df_shp){
  pnts <- data.frame('x' = unlist(map(df_crime$geometry, 1)),
                     'y' = unlist(map(df_crime$geometry, 2)))
  
  # create a points collection
  pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(pnts), 
                                       function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 
  
  # apply transformation to pnts sf
  pnts_trans <- st_transform(pnts_sf, 2163) 
  tt1_trans <- st_transform(df_shp, 2163)
  
  
  pnts$region <- apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                       function(col) { 
                         tt1_trans[which(col), ]$NRO_CUADRA
                       })
  
  df_crime$NRO_CUADRA <- as.character(pnts$region)
  return(df_crime)
}

# Clean Data
df_shp <- st_read(file.path(PATH, '07_Cuadrantes'))
df_shp <- clean_shp(st_read(file.path(PATH, '07_Cuadrantes')))
df_crime19 <- p2p(df_crime19, df_shp)

# Summarise to Quadrants Shift Level ####
change_to_shift <- function(df_crime, shp = df_shp){
  df_quad <- data.frame(NRO_CUADRA = rep(shp$NRO_CUADRA, 3),
                        shift = rep(c("21-5", "5-13", "13-21"), each = 286))
  df_temp <- df_crime %>% group_by(NRO_CUADRA, shift) %>% 
    summarise(homicide = sum(crime_type == 'homicide'),
              theft = sum(crime_type == 'theft'),
              vehicle_theft= sum(crime_type == 'vehicle theft'),
              burglary = sum(crime_type == 'burglary'),
              sum = n())
  df_quad <- merge(df_quad, df_temp, by = c('NRO_CUADRA','shift'), all.x = T)
  df_quad[is.na(df_quad)] <- 0
  df_quad <- merge(df_quad[,c(1:7)],shp[,c(2,13)], all = T)
  df_quad <- st_as_sf(df_quad)
  return(df_quad)
}

df_shift <- change_to_shift(df_crime19)
  

# Visualizations --------------------------------------------------------------------------------
p_count_crime <- function(df, colname){
  p <- ggplot(st_drop_geometry(df), aes(x = st_drop_geometry(df)[,colname])) + 
    geom_histogram(binwidth = 1) +
    theme_classic() +
    ggtitle(paste('Number of Crimes per Quad Shift(2019):', colname))
  print(p)
  return(p)
}

#### Histograms - Crime distribution----------------------------------------------------

#p_homicide <- p_count_crime(df_shift, 'homicide')
#p_sum <- p_count_crime(df_shift, 'sum')

hist_crimes <- ggplot(data = df_shift,
       aes(x = sum)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribution of crimes per quadrant-shift (2019)",
       x = "Total number of crimes",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 300, 25)) +
  annotate("text", label = c("286 quadrants", "    x 3 shifts", "= 858 quadrant-shifts"),
           x = c(250, 250, 250), y = c(80, 76, 72))
hist_crimes
ggsave(filename = "hist_crimes.png",
       plot = hist_crimes,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

hist_homicides <- ggplot(data = df_shift,
                         aes(x = homicide)) +
  geom_histogram(binwidth = 1, color = "lightgrey") +
  labs(title = "Distribution of homicides per quadrant-shift (2019)",
       x = "Total number of homicides",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", label = c("286 quadrants", "    x 3 shifts", "= 858 quadrant-shifts"),
           x = c(7.5, 7.5, 7.5), y = c(500, 460, 420))
hist_homicides
ggsave(filename = "hist_homicides.png",
       plot = hist_homicides,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

## Maps ----------------------------------------------------------------------------

map_crimes_quad_2019 <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = sum))
map_crimes_quad_2019
ggsave(filename = "map_crimes_quad_2019.png",
       plot = map_crimes_quad_2019,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
