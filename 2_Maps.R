# Set Environment####
## path ####
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
select_cols <- function(df, dict_filename, path=PATH){
  dict_colname <- read_excel(file.path(path, dict_filename))
  df <- df[, dict_colname$Variable]
}
df_shp <- select_cols(df_shp,'Selected data dictionary.xlsx')

# rename cols
colnames(df_crime)[1] <- colnames(df_shp)[1]

# merge
df_main <- merge(df_shp, df_crime, by = colnames(df_shp)[1])

# DEMOGRAPHIC DFs

## Economic strata df
df_strata <- df_main %>%
  filter(TVIVIENDA != 0,
         TP19_EE_E9 == 0) %>%
  mutate(stratum1_perc = TP19_EE_E1/TVIVIENDA, 
         stratum2_perc = TP19_EE_E2/TVIVIENDA,
         stratum3_perc = TP19_EE_E3/TVIVIENDA,
         stratum4_perc = TP19_EE_E4/TVIVIENDA,
         stratum5_perc = TP19_EE_E5/TVIVIENDA,
         stratum6_perc = TP19_EE_E6/TVIVIENDA,
         poor = as.factor(ifelse(stratum1_perc > stratum2_perc & stratum1_perc > stratum3_perc & stratum1_perc > stratum4_perc & stratum1_perc > stratum5_perc & stratum1_perc > stratum6_perc | stratum2_perc > stratum1_perc & stratum2_perc > stratum3_perc & stratum2_perc > stratum4_perc & stratum2_perc > stratum5_perc & stratum2_perc > stratum6_perc, 1, 0)))
# Education df
df_educ <- df_main %>%
  filter(TP27_PERSO != 0) %>%
  mutate(primary_school   = TP51PRIMAR/TP27_PERSO,
         secondary_school = TP51SECUND/TP27_PERSO,
         college          = TP51SUPERI/TP27_PERSO,
         graduate_school  = TP51POSTGR/TP27_PERSO,
         no_school        = TP51_13_ED/TP27_PERSO)


