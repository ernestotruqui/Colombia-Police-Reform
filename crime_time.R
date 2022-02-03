library(tidyverse)
library(sf)

path <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data"

df_crime19 <- st_read(file.path(path, "CIEPS_MEVAL.shp"))
quadrants <- st_read(file.path(path, "lines.shp"))


map_primaryschool +
  geom_sf(data = quadrants)
