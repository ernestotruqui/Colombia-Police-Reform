library(tidyverse)
library(sf)
library(readxl)
library(lubridate)
library(stringr)
path <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data"

quadrants <- st_read(file.path(path, "lines.shp"))

df_homicides19_raw <- read_csv(file.path(path, "homicides_2019.csv"))
df_homicides19 <- df_homicides19_raw %>%
  filter(zone == "05001000") %>%
  mutate(lat = ifelse(str_detect(lat, ","), str_replace(lat, pattern = ",", replacement = "."), lat),
         lat = as.numeric(lat),
         lon = gsub(pattern = "^(.{3})(.*)$", replacement = "\\1.\\2", x = lon),
         lon = as.numeric(lon),
         crime_type = "homicide") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  select(c(crime_type, date, time, zone, geometry)) %>%
  rename(hour = time,
         cod_dane = zone)

df_theft19_raw <- read_csv(file.path(path, "theft_2019.csv"))
df_theft19 <- df_theft19_raw %>%
  mutate(cod_dane = as.character(cod_dane),
         cod_dane = ifelse(is.na(cod_dane), "NA", cod_dane)) %>%
  #filter(str_detect(df_theft19$cod_dane, "5001")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(crime_type = "theft") %>%
  select(c(crime_type, date, time, cod_dane, geometry)) %>%
  mutate(time = hour(time)) %>%
  rename(hour = time)

df_burglary19_raw <- read_csv(file.path(path, "burglary_2019.csv"))
df_burglary19 <- df_burglary19_raw %>%
  mutate(cod_dane = as.character(cod_dane)) %>%
  #filter(str_detect(df_burglary19$cod_dane, "5001")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(crime_type = "burglary",
         time = hour(time)) %>%
  select(c(crime_type, date, time, cod_dane, geometry)) %>%
  rename(hour = time)

df_vtheft19_raw <- read_csv(file.path(path, "vehicle_theft_2019.csv"))
df_vtheft19 <- df_vtheft19_raw %>%
  #filter(str_detect(cod_dane, "050011")) %>%
  mutate(lat = ifelse(str_detect(lat, ","), str_replace(lat, pattern = ",", replacement = "."), lat),
         lat = as.numeric(lat),
         lon = gsub(pattern = "^(.{3})(.*)$", replacement = "\\1.\\2", x = lon),
         lon = as.numeric(lon),
         crime_type = "vehicle theft",
         time = hour(time)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  select(c(crime_type, date, time, cod_dane,geometry)) %>%
  rename(hour = time)

df_crime19 <- rbind(df_homicides19, df_theft19, df_burglary19, df_vtheft19)


ggplot() +
  #geom_sf(data = quadrants) #+
  geom_sf(data = df_homicides19)
