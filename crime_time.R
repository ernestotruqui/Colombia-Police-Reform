library(tidyverse)
library(sf)
library(readxl)

path <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data"

quadrants <- st_read(file.path(path, "lines.shp"))

df_homicides19 <- read_csv(file.path(path, "homicides_2019.csv"))
df_homicides19 <- df_homicides19 %>%
  mutate(lat = ifelse(str_detect(lat, ","), str_replace(lat, pattern = ",", replacement = "."), lat),
         lat = as.numeric(lat),
         lon = gsub(pattern = "^(.{3})(.*)$", replacement = "\\1.\\2", x = lon),
         lon = as.numeric(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

df_theft19 <- read_csv(file.path(path, "theft_2019.csv"))
df_theft19 <- df_theft19 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
df_burglary19 <- read_csv(file.path(path, "burglary_2019.csv"))
df_burglary19 <- df_burglary19 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
df_vtheft19 <- read_csv(file.path(path, "vehicle_theft_2019.csv"))
df_vtheft19 <- df_vtheft19 %>%
  mutate(lat = ifelse(str_detect(lat, ","), str_replace(lat, pattern = ",", replacement = "."), lat),
         lat = as.numeric(lat),
         lon = gsub(pattern = "^(.{3})(.*)$", replacement = "\\1.\\2", x = lon),
         lon = as.numeric(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)




ggplot() +
  #geom_sf(data = quadrants) #+
  geom_sf(data = df_homicides19)
