# Set Environment####
## set path and options####
options(scipen = 999)
#PATH <- "E://Files/HaHaHariss/22Winter/Policy Lab/Data"
PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data" 

## load libraries####
library(readxl)
library(sf)
library(ggplot2)
#install.packages("reshape2")
library(reshape2)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(leaflet)
library(ggeasy)

# Data Cleaning ####
## Pre-processing Crime Data ####
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

### run ####
df_crime19 <- merge_crime_data('CrimesV2.xlsx')

## Pre-processing Shapefile ####
clean_shp <- function(df_shp){
  df_shp <- df_shp[!substr(as.character(df_shp$NRO_CUADRA), 13, 13) %in% c('6', '7'),]
  df_shp <- df_shp[is.na(df_shp$SUBESTACIO),]
  df_shp <- df_shp[which(df_shp$ESTACION!='SAN ANTONIO DE PRADO'),]
  # df_shp <- df_shp[which(df_shp$CAI!='CAI LAS PALMAS'),]
  colnames(df_shp)[which(names(df_shp) == "NRO_CUADRA")] <- 'region'
  return(df_shp)
}

clean_shp_old <- function(df_shp){
  df_shp <- df_shp[,'COD_DANE_A']
  colnames(df_shp)[which(names(df_shp) == "COD_DANE_A")] <- 'region'
  return(df_shp)
}

p2p <- function(df_crime, df_shp){
  pnts <- data.frame('x' = unlist(map(df_crime$geometry, 1)),
                     'y' = unlist(map(df_crime$geometry, 2)))
  
  # create a points collection
  pnts_sf <- do.call("st_sfc", c(lapply(1:nrow(pnts), 
                                       function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 
  
  # apply transformation to pnts sf
  pnts_trans <- st_transform(pnts_sf, 2163) 
  tt1_trans <- st_transform(df_shp, 2163)
  
  
  pnts$region <- apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                       function(col) { 
                         tt1_trans[which(col), ]$region
                       })
  
  df_crime$region <- as.character(pnts$region)
  return(df_crime)
}

### run ####

# Get block level data
# df_shp_old <- clean_shp_old(st_read(file.path(PATH, 'manzanas_MEVAL.shp')))
# df_crime19_old <- p2p(df_crime19, df_shp_old)
# df_crime19_old$region <- as.numeric(df_crime19_old$region)
# df_crim19_old <- na.omit(df_crime19_old)

df_shp <- clean_shp(st_read(file.path(PATH, '07_Cuadrantes')))
df_crime19 <- p2p(df_crime19, df_shp)

## Summarise to Quadrants Shift Level ####
change_to_shift <- function(df_crime, shp = df_shp){
  df_quad <- data.frame(region = rep(shp$region, 3),
                        shift = rep(c("21-5", "5-13", "13-21"), each = nrow(shp)))
  df_temp <- df_crime %>% group_by(region, shift) %>% 
    dplyr::summarize(homicide = sum(crime_type == 'homicide'),
              theft = sum(crime_type == 'theft'),
              vehicle_theft= sum(crime_type == 'vehicle theft'),
              burglary = sum(crime_type == 'burglary'),
              sum = n())
  df_quad <- merge(df_quad, df_temp, by = c('region','shift'), all.x = T)
  df_quad[is.na(df_quad)] <- 0
  df_quad <- merge(df_quad[,c(1:7)],shp[,c(2,13)], all = T)
  df_quad <- st_as_sf(df_quad)
  return(df_quad)
}

### run ####
df_shift <- change_to_shift(df_crime19)

# Redistribution####
## redistribution algo####
redistribute <- function(df,colname){
  df$temp <- (nrow(df) * st_drop_geometry(df)[,colname] / sum(st_drop_geometry(df)[,colname])) + 1
  df$temp <- round(df$temp)
  return(df$temp)
  }

crime_per_police <- function(df, crime_type, n_of_police = ''){
  df <- st_drop_geometry(df)
  if (n_of_police == '') {
    df$temp <- df[,crime_type] / 2
  }
  else {
    df$temp <- df[,crime_type] / df[,n_of_police]
  }
  return(df$temp)
}

### run ####
# number of police in the quad shift
df_shift$n_of_police <- 2
df_shift$rn_of_police <- redistribute(df_shift, 'sum')

# number of crimes per police
df_shift$cpp <- crime_per_police(df_shift, 'sum')
df_shift$rcpp <- crime_per_police(df_shift, 'sum', 'rn_of_police')

df_shift_dataframe <- fortify(df_shift)
write.csv(df_shift_dataframe, 
          file = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data/df_shift_dataframe.csv")


