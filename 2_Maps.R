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
st_crs(df_shp)
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
df_main <- df_main %>%
  mutate(total_homicides = hom2012 + hom2013 + hom2014 + hom2015 + hom2016 + hom2017 + hom2018 + hom2019,
         total_crimes_2019 = hom2019 + hmot2019 + hveh2019 + hper2019,
         total_crimes_2018 = hom2018 + hmot2018 + hveh2018 + hper2018,
         total_crimes_2017 = hom2017 + hmot2017 + hveh2017 + hper2017,
         total_crimes_2016 = hom2016 + hmot2016 + hveh2016 + hper2016,
         total_crimes_2015 = hom2015 + hmot2015 + hveh2015 + hper2015)

# Crime maps -------
map_homicides_2019 <- ggplot() +
  geom_sf(data = df_main,
          aes(fill = hom2019, color = hom2019)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  #theme_void() +
  labs(fill = "Number of homicides",
       color = "Number of homicides")
map_homicides_2019
ggsave(filename = "map_homs_block_2019.png",
       plot = map_homicides_2019,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

map_total_homicides <- ggplot() +
  geom_sf(data = df_main,
          aes(fill = total_homicides, color = total_homicides)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  labs(fill = "Number of homicides",
       color = "Number of homicides") +
  ylim(min = 6.17, max = 6.32) +
  xlim(min = -75.67, max = -75.48)
map_total_homicides
ggsave(filename = "map_homs_block_2012_19.png",
       plot = map_total_homicides,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

for (year in c(2015:2019)) {
  map_total_crimes_year <- ggplot() +
    geom_sf(data = df_main,
            aes_string(fill = total_crimes_year, color = total_crimes_year)) +
    scale_fill_viridis_c() +
    scale_color_viridis_c() +
    labs(fill = "Number of crimes",
         color = "Number of crimes") +
    ylim(min = 6.17, max = 6.32) +
    xlim(min = -75.67, max = -75.48)
  ggsave(filename = "map_total_crimes_year.png",
         plot = map_total_crimes_year,
         path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
}

map_total_crimes_2019 <- ggplot() +
  geom_sf(data = df_main,
          aes(fill = total_crimes_2019, color = total_crimes_2019)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  labs(fill = "Number of crimes",
       color = "Number of crimes") +
  ylim(min = 6.17, max = 6.32) +
  xlim(min = -75.67, max = -75.48)
map_total_crimes_2019
ggsave(filename = "map_total_crimes_2019.png",
       plot = map_total_crimes_2019,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
map_total_crimes_2018 <- ggplot() +
  geom_sf(data = df_main,
          aes(fill = total_crimes_2018, color = total_crimes_2018)) +
  scale_fill_viridis_c(limits = c(0, 16)) +
  scale_color_viridis_c(limits = c(0, 16)) +
  labs(fill = "Number of crimes",
       color = "Number of crimes") +
  ylim(min = 6.17, max = 6.32) +
  xlim(min = -75.67, max = -75.48)
map_total_crimes_2018
ggsave(filename = "map_total_crimes_2018.png",
       plot = map_total_crimes_2018,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
map_total_crimes_2017 <- ggplot() +
  geom_sf(data = df_main,
          aes(fill = total_crimes_2017, color = total_crimes_2017)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  labs(fill = "Number of crimes",
       color = "Number of crimes") +
  ylim(min = 6.17, max = 6.32) +
  xlim(min = -75.67, max = -75.48)
map_total_crimes_2017
ggsave(filename = "map_total_crimes_2017.png",
       plot = map_total_crimes_2017,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")


# DEMOGRAPHIC DFs -----

# Education -----
df_educ <- df_main %>%
  filter(TP27_PERSO != 0) %>%
  mutate(primary_school   = TP51PRIMAR/TP27_PERSO,
         secondary_school = TP51SECUND/TP27_PERSO,
         college          = TP51SUPERI/TP27_PERSO,
         graduate_school  = TP51POSTGR/TP27_PERSO,
         no_school        = TP51_13_ED/TP27_PERSO)

# Education maps
map_primaryschool <- ggplot() +
  geom_sf(data = df_educ,
          aes(fill = primary_school, color = primary_school)) +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c() 
map_secondaryschool <- ggplot() +
  geom_sf(
    data = df_educ,
    aes(fill = secondary_school, color = secondary_school)) +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c()
map_college <- ggplot() +
  geom_sf(
    data = df_educ,
    aes(fill = college, color = college)) +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c()
map_graduate  <- ggplot() +
  geom_sf(
    data = df_educ,
    aes(fill = graduate_school, color = graduate_school)) +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c()
# source: http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
plot_grid(map_primaryschool, map_secondaryschool, map_college, map_graduate,
          labels = c("Primary School", "Secondary School", "College", "Graduate School"),
          ncol = 2)


## Economic strata  -----
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

# Poor households map
ggplot(data = df_strata ,
       aes(fill = poor, color = poor)) +
  geom_sf() +
  theme_void() +
  labs(title = "Poverty in Medellin",
       subtitle = "Street block as a unit of analysis",
       caption = "Poverty = 1 when block has most households belonging to Stratum 1 or 2")
# Plots by stratum
map_stratum1 <- ggplot(data = df_strata ,
                       aes(fill = stratum1_perc, color = stratum1_perc)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  labs(title = "% of Housing Units in Stratum 1")
map_stratum2 <- ggplot(data = df_strata ,
                       aes(fill = stratum2_perc, color = stratum2_perc)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  labs(title = "% of Housing Units in Stratum 2")
map_stratum3 <- ggplot(data = df_strata ,
                       aes(fill = stratum3_perc, color = stratum3_perc)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  labs(title = "% of Housing Units in Stratum 3")
map_stratum4 <- ggplot(data = df_strata ,
                       aes(fill = stratum4_perc, color = stratum4_perc)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  labs(title = "% of Housing Units in Stratum 4")
map_stratum5 <- ggplot(data = df_strata ,
                       aes(fill = stratum5_perc, color = stratum5_perc)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  labs(title = "% of Housing Units in Stratum 5")
map_stratum6 <- ggplot(data = df_strata ,
                       aes(fill = stratum6_perc, color = stratum6_perc)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  labs(title = "% of Housing Units in Stratum 6")

plot_grid(map_stratum1, map_stratum2, map_stratum3, map_stratum4, map_stratum5, map_stratum6,
          ncol = 3)
