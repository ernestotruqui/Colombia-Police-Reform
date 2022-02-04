# Set Environment####
## set wd and path####
#setwd("E:/Files/HaHaHariss/22Winter/Policy Lab/Police Reform Policy Lab/G2 -Patrolling strategies and complementary social interventions")
#PATH <- "E:/Files/HaHaHariss/22Winter/Policy Lab"

PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data"

## load libraries####
library(tidyverse)
library(ggplot2)
#install.packages("maps")
#install.packages("mapdata")
library(maps)
library(mapdata)
#install.packages("sf")
library(sf)
library(readr)
library(tidyr)
#install.packages("vtable")
library(vtable)
library(dplyr)
library(haven)
library(readxl)
library(lubridate)
library(readr)
library(sf)
#install.packages("cowplot")
library(cowplot)
#install.packages("expss")
library(expss)

# Read Data ####

## crime data ####
df_crime <- read_xlsx(file.path(PATH, "Data_Manzana_MDE.xlsx"))[, c(4, 37:68)]

## shape file ####
#df_shp <- st_read(file.path(PATH, "MGN_ANM_MANZANA.shp"))
df_shp <- st_read(file.path(PATH, "manzanas_MEVAL.shp"))[, 1:106]

# Clean Data ####
## add labels ####
# create function
add_labels <- function(df, dict_filename, path = PATH){
  
  # load library
  library(labelled)
  
  # read dictionary
  dict <- read.csv(file.path(PATH, dict_filename))
  colnames(dict) <- c('colname','label')
  # create named list
  nlist <- dict$label
  
  names(nlist) <- dict$colname
  if ('geometry' %in% colnames(df)) {
    nlist <- c(nlist, geometry = 'Geometry')
  }
  
  # label variables
  var_label(df) <- nlist
  
  return(df)
}

# run function
df_shp <- add_labels(df_shp, "shapefile_labels.csv")
df_crime <- add_labels(df_crime,'crime_labels.csv')

## merge data####

# drop useless cols
select_cols <- function(df, dict_filename, path = PATH){
  dict_colname <- read_excel(file.path(path, dict_filename))
  df <- df[, dict_colname$Variable]
}
df_shp <- select_cols(df_shp,'Selected data dictionary.xlsx')

# rename cols
colnames(df_crime)[1] <- colnames(df_shp)[1]

# df with all important socio-demographic info and crime, by block, geocoded
df_main <- merge(df_shp, df_crime, by = colnames(df_shp)[1])

# df with only crime info, by block, geocoded
df_crime_geo <- merge(df_shp %>% select(COD_DANE_A, geometry), df_crime, by = "COD_DANE_A")
df_crime_geo <- df_crime_geo %>%
  pivot_longer(cols = c(hom2012:hmot2019),
               names_to = "crime_year",
               values_to = "crime_count") %>%
  separate(col = crime_year,
           into = c("crime_type", "crime_year"),
           sep = "20") %>%
  mutate(crime_year = as.numeric(crime_year) + 2000,
         crime_type = case_when(crime_type == "hom" ~ "homicide",
                                crime_type == "hper" ~ "robbery",
                                crime_type == "hveh" ~ "motor vehicle theft",
                                crime_type == "hmot" ~ "motorcycle theft",
                                TRUE ~ as.character(crime_type)))
