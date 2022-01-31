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

# Read Data####

## crime data####
df_crime <- read_xlsx(file.path(path, "Data_Manzana_MDE.xlsx"))[, c(4, 37:68)]

## shape file####
#df_shp <- st_read(file.path(PATH, 'E:/Files/HaHaHariss/22Winter/Policy Lab'))
#df_shp <- st_read(file.path(PATH, "Shapefiles/08_Manzanas"))[,1:106]
df_shp <- st_read(file.path(path, "manzanas_MEVAL.shp"))[, 1:106]

# Clean Data####
## add labels####
# create function
add_labels <- function(df, dict_filename, path=PATH){
  
  # load library
  library(labelled)
  
  # read dictionary
  dict <- read.csv(file.path(path, dict_filename))
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
df_shp <- add_labels(df_shp,'shapefile_labels.csv')
df_crime <- add_labels(df_crime,'crime_labels.csv')

## merge data####

# drop useless cols
select_cols <- function(df,dict_filename,path=PATH){
  dict_colname <- read_excel(file.path(path,dict_filename))
  df <- df[,dict_colname$Variable]
}
df_shp <- select_cols(df_shp,'Selected data dictionary.xlsx')

# rename cols
colnames(df_crime)[1] <- colnames(df_shp)[1]

# merge
df_main <- merge(df_shp, df_crime, by = colnames(df_shp)[1])
df_main_all <- merge(df_shp, df_crime, by = colnames(df_shp)[1],all=T)

## check for NAs####
sum(is.na(df_main_all$hom2012)) - sum(is.na(df_main$hom2012))
# missing crime data for 4128 blocks!
# distribution of blocks without crime data
ggplot(df_main_all)+
  geom_sf(aes(fill = is.na(hom2012))) +
  ggtitle('Blocks Without Crime Data')
