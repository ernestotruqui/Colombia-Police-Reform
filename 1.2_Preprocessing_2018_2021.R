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
#install.packages("leaflet")
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
  df <- clean_crime_data(df) %>%
    mutate(crime_type = sname) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    select(c(crime_type, fecha, hora_24, geometry))  
}


merge_crime_data <- function(fname, path = PATH){
  names <- excel_sheets(file.path(path, fname))[c(2,3,4)]
  df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(df) <- c('crime_type', 'fecha', 'hora_24', 'geometry')
  for (name in names) {
    df_temp <- read_crime_data(fname, name)
    df <- rbind(df, df_temp)
  }
  # rename crime types
  df$crime_type <- case_when(df$crime_type == "Homicidios" ~ "homicide",
                             df$crime_type == "H.Automotores" ~ "car theft",
                             df$crime_type == "H.Personas" ~ "robbery")
  
  
  # turn time into shifts
  df$shift <- ifelse(df$hora_24 %in% c(5,6,7,8,9,10,11,12),'5-13',
                     ifelse(df$hora_24 %in% c(13,14,15,16,17,18,19,20),
                            '13-21','21-5'))
  
  #rename columns into English
  df <- df %>%
    dplyr::rename(date = fecha,
                  hour = hora_24)
  
  #get years
  df <- df %>%
    mutate(year = year(date),
           month = month(date))
  
  return(df)
}

df_crime_yrs <- merge_crime_data('Crimes_MDE_V3.xlsx')

df_crime_18 <- df_crime_yrs %>%
  filter(year == 2018)
df_crime_19 <- df_crime_yrs %>%
  filter(year == 2019)
df_crime_20 <- df_crime_yrs %>%
  filter(year == 2020)
df_crime_21 <- df_crime_yrs %>%
  filter(year == 2021)
df_crime_22 <- df_crime_yrs %>%
  filter(year == 2022)








