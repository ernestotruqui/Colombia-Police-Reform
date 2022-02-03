library(tidyverse)
library(sf)
library(readxl)

path <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data"

df_homicides19 <- read_xlsx(file.path(path, "crimes_times.xlsx"), sheet = 4)
df_homicides19 <- df_homicides19 %>%
  mutate(lat = ifelse(str_detect(lat, ","), str_replace(lat, pattern = ",", replacement = "."), lat),
         lat = as.numeric(lat),
         lon = ifelse(str_detect(lon, ","), str_replace(lon, pattern = ",", replacement = "."), lon),
         lon = as.numeric(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

quadrants <- st_read(file.path(path, "lines.shp"))




ggplot() +
  #geom_sf(data = quadrants) +
  geom_sf(data = df_homicides19)
