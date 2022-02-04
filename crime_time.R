library(tidyverse)
library(sf)
library(readxl)

path <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data"

quadrants <- st_read(file.path(path, "lines.shp"))

df_homicides19_raw <- read_csv(file.path(path, "homicides_2019.csv"))
df_homicides19 <- df_homicides19_raw %>%
  mutate(lat = ifelse(str_detect(lat, ","), str_replace(lat, pattern = ",", replacement = "."), lat),
         lat = as.numeric(lat),
         lon = gsub(pattern = "^(.{3})(.*)$", replacement = "\\1.\\2", x = lon),
         lon = as.numeric(lon),
         crime_type = "homicide") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  select(c(crime_type, date, time, geometry))

df_theft19_raw <- read_csv(file.path(path, "theft_2019.csv"))
df_theft19 <- df_theft19_raw %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(crime_type = "theft") %>%
  select(c(crime_type, date, time, geometry))

#limpiar formato de hora - separar hora y minutos
#repetir lo de arriba abajo, filtar df y hacer rbind con todas juntas
df_burglary19_raw <- read_csv(file.path(path, "burglary_2019.csv"))
df_burglary19 <- df_burglary19_raw %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
df_vtheft19_raw <- read_csv(file.path(path, "vehicle_theft_2019.csv"))
df_vtheft19 <- df_vtheft19_raw %>%
  mutate(lat = ifelse(str_detect(lat, ","), str_replace(lat, pattern = ",", replacement = "."), lat),
         lat = as.numeric(lat),
         lon = gsub(pattern = "^(.{3})(.*)$", replacement = "\\1.\\2", x = lon),
         lon = as.numeric(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

df_crime19 <- df_homicides19 %>%
  select(c(crime_type, date, time, geometry)) %>%
  rbi


ggplot() +
  #geom_sf(data = quadrants) #+
  geom_sf(data = df_homicides19)
