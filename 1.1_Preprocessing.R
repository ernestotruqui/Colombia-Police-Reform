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
  if ('cod_dane' %in% colnames(df)) {
    df <- filter(df, as.numeric(cod_dane) == 5001000)
    df <- mutate(df, time = as.numeric(hour(time)))
  }
  else {
    df <- filter(df, zone == "05001000")   
  }
  df <- clean_crime_data(df) %>%
    mutate(crime_type = sname) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    select(c(crime_type, date, time, geometry))  
}

merge_crime_data <- function(fname, path = PATH){
  names <- excel_sheets(file.path(path, fname))[c(2,3,4,5)]
  df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(df) <- c('crime_type', 'date', 'time', 'geometry')
  for (name in names) {
    df_temp <- read_crime_data(fname, name)
    df <- rbind(df, df_temp)
  }
  # rename crime types
  crime_types <- c('vehicle theft', 'theft', 'homicide', 'burglary')
  df$crime_type[df$crime_type %in% names] <- crime_types[match(df$crime_type, names, nomatch = 0)]
  
  # turn time into shifts
  df$shift <- ifelse(df$time %in% c(5,6,7,8,9,10,11,12),'5-13',
                             ifelse(df$time %in% c(13,14,15,16,17,18,19,20),
                                    '13-21','21-5'))
  return(df)
}

### run ####
df_crime19 <- merge_crime_data('CrimesV2.xlsx')

## Pre-processing Shapefile ####
clean_shp <- function(df_shp){
  df_shp <- df_shp[!substr(as.character(df_shp$NRO_CUADRA), 13, 13) %in% c('6', '7'),]
  colnames(df_shp)[which(names(df_shp) == "NRO_CUADRA")] <- 'region'
  return(df_shp)
}

clean_shp_old <- function(df_shp){
  df_shp <- df_shp[,'COD_DANE_A']
  colnames(df_shp)[which(names(df_shp) == "COD_DANE_A")] <- 'region'
  return(df_shp)
}

p2p <- function(df_crime, df_shp){
  pnts <- data.frame('x' = unlist(map(df_crime$geometry, 1)),
                     'y' = unlist(map(df_crime$geometry, 2)))
  
  # create a points collection
  pnts_sf <- do.call("st_sfc", c(lapply(1:nrow(pnts), 
                                       function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 
  
  # apply transformation to pnts sf
  pnts_trans <- st_transform(pnts_sf, 2163) 
  tt1_trans <- st_transform(df_shp, 2163)
  
  
  pnts$region <- apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                       function(col) { 
                         tt1_trans[which(col), ]$region
                       })
  
  df_crime$region <- as.character(pnts$region)
  return(df_crime)
}

### run ####

# Get block level data
# df_shp_old <- clean_shp_old(st_read(file.path(PATH, 'manzanas_MEVAL.shp')))
# df_crime19_old <- p2p(df_crime19, df_shp_old)
# df_crime19_old$region <- as.numeric(df_crime19_old$region)
# df_crim19_old <- na.omit(df_crime19_old)

df_shp <- clean_shp(st_read(file.path(PATH, '07_Cuadrantes')))
df_crime19 <- p2p(df_crime19, df_shp)

## Summarise to Quadrants Shift Level ####
change_to_shift <- function(df_crime, shp = df_shp){
  df_quad <- data.frame(region = rep(shp$region, 3),
                        shift = rep(c("21-5", "5-13", "13-21"), each = nrow(shp)))
  df_temp <- df_crime %>% group_by(region, shift) %>% 
    dplyr::summarize(homicide = sum(crime_type == 'homicide'),
              theft = sum(crime_type == 'theft'),
              vehicle_theft= sum(crime_type == 'vehicle theft'),
              burglary = sum(crime_type == 'burglary'),
              sum = n())
  df_quad <- merge(df_quad, df_temp, by = c('region','shift'), all.x = T)
  df_quad[is.na(df_quad)] <- 0
  df_quad <- merge(df_quad[,c(1:7)],shp[,c(2,13)], all = T)
  df_quad <- st_as_sf(df_quad)
  return(df_quad)
}

### run ####
df_shift <- change_to_shift(df_crime19)

# Redistribution####
## redistribution algo####
redistribute <- function(df, colname){
  df$temp <- (nrow(df) * st_drop_geometry(df)[,colname] / sum(st_drop_geometry(df)[,colname])) + 1
  df$temp <- round(df$temp)
  #colnames(df)[which(names(df) == "temp")] <- paste('rn_', colname, sep = '')
  return(df$temp)
  }

crime_per_police <- function(df, crime_type, n_of_police = ''){
  df <- st_drop_geometry(df)
  if (n_of_police == '') {
    df$temp <- df[,crime_type] / 2
  }
  else {
    df$temp <- df[,crime_type] / df[,n_of_police]
  }
  return(df$temp)
}

### run ####
# number of police in the quad shift
df_shift$n_of_police <- 2
df_shift$rn_of_police <- redistribute(df_shift,'sum')

# number of crimes per police
df_shift$cpp <- crime_per_police(df_shift, 'sum')
df_shift$rcpp <- crime_per_police(df_shift, 'sum', 'rn_of_police')

# Visualizations ####

## Histograms ####

### crime per police ####
plot_cpp <- function(df, cpp, rcpp){
  # prepare df for plotting
  df <- st_drop_geometry(df)
  df_temp <- as.data.frame(df[,c('region', 'shift', cpp, rcpp)])
  df_long <- melt(df, id.vars = c("region", "shift"),
                  measure.vars = c(cpp, rcpp),
                  variable.name = "distribution",
                  value.name = "crime_per_police")
  levels(df_long$distribution) <- c('before','after')
  group_mean <- ddply(df_long, "distribution", summarise, 
                      grp.mean=mean(crime_per_police))
  
  
  # plot
  p <- ggplot(df_long, aes(x=crime_per_police, color=distribution, fill = distribution)) +
    geom_histogram(aes(y=..density..), alpha = 0.05, position = "identity", binwidth = 1)+
    geom_vline(data = group_mean, aes(xintercept = grp.mean, color = distribution),
               size = 1.25)+
    geom_density(alpha = .2)+
    ggtitle('Crime per officer: before and after redistribution') +
    xlab(label = "Crimes per officer by quadrant-shift") +
    theme(plot.title = element_text(hjust = 0.5, size = 15))
  #print(p)
  #return(p)
}
p_cpp <- plot_cpp(df_shift, 'cpp', 'rcpp')
p_cpp
ggsave(filename = "hist_crimes_p_officer.png",
       plot = p_cpp,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")


### crime per quad shift ####
#  hist_crimes <- ggplot(data = df_shift,
#         aes(x = sum,
#             y = ..density..)) +
#    geom_histogram(binwidth = 5) +
#    labs(title = "Distribution of crimes per quadrant-shift (2019)",
#         x = "Total number of crimes",
#         y = "Density") +
#    theme(plot.title = element_text(hjust = 0.5,
#                                    size = 20),
#          axis.title = element_text(size = 16)) +
#    scale_x_continuous(breaks = seq(0, 300, 25)) #+
#    #annotate("text", label = c("286 quadrants", "    x 3 shifts", "= 858 quadrant-shifts"),
#     #        x = c(250, 250, 250), y = c(80, 76, 72))
#  hist_crimes
#  ggsave(filename = "hist_crimes.png",
#         plot = hist_crimes,
#         path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
#  
#  hist_homicides <- ggplot(data = df_shift,
#                           aes(x = homicide)) +
#    geom_histogram(binwidth = 1, color = "lightgrey") +
#    labs(title = "Distribution of homicides per quadrant-shift (2019)",
#         x = "Total number of homicides",
#         y = "Count") +
#    theme(plot.title = element_text(hjust = 0.5)) +
#    annotate("text", label = c("286 quadrants", "    x 3 shifts", "= 858 quadrant-shifts"),
#             x = c(7.5, 7.5, 7.5), y = c(500, 460, 420))
#  hist_homicides
#  ggsave(filename = "hist_homicides.png",
#         plot = hist_homicides,
#         path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
# 

# table to export for data task ------
table_crimes <- df_shift %>%
  st_drop_geometry() %>%
  group_by(shift) %>%
  summarise(`Total homicides` = sum(homicide),
            `Total car thefts` = sum(vehicle_theft),
            `Total robberies` = sum(theft),
            `Total burglaries` = sum(burglary),
            `Total crimes` = sum(sum)) %>%
  as.data.frame()
write.csv(x = table_crimes,
            file = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Midterm deliverables/.csv",
            row.names = F)

## Maps ----------------------------------------------------------------------------

map_crimes_quad_2019_morn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = sum)) +
  labs(title = "Morning shift (5:00 - 13:00)",
       fill = "Crimes",
       color = "Crimes") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 300)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 300)) 
map_crimes_quad_2019_morn
ggsave(filename = "map_crimes_quad_2019_morn.png",
       plot = map_crimes_quad_2019_morn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

map_crimes_quad_2019_aftn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='13-21',],
          aes(fill = sum)) +
  labs(title = "Afternoon shift (13:00 - 21:00)",
       fill = "Crimes",
       color = "Crimes") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 300)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 300)) 
map_crimes_quad_2019_aftn
ggsave(filename = "map_crimes_quad_2019_aftn.png",
       plot = map_crimes_quad_2019_aftn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

map_crimes_quad_2019_nght <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
          aes(fill = sum)) +
  labs(title = "Night shift (21:00 - 5:00)",
       fill = "Crimes",
       color = "Crimes") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 300)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 300)) 
map_crimes_quad_2019_nght
ggsave(filename = "map_crimes_quad_2019_nght.png",
       plot = map_crimes_quad_2019_nght,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
plot_grid(map_crimes_quad_2019_morn, map_crimes_quad_2019_aftn, map_crimes_quad_2019_nght,
          labels = c("Morning Shift", "Afternoon Shift", "Night Shift"),
          ncol = 3)


## Maps of crimes per officer by quadrant --------------
## Status quo
map_crimes_officer_quad_2019_morn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = cpp)) +
  labs(title = "Morning shift (5:00 - 13:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 162)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 162)) 
map_crimes_officer_quad_2019_morn
ggsave(filename = "map_crimes_officer_quad_2019_morn.png",
       plot = map_crimes_officer_quad_2019_morn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
map_crimes_officer_quad_2019_aftn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='13-21',],
          aes(fill = cpp)) +
  labs(title = "Afternoon shift (13:00 - 21:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 162)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 162)) 
map_crimes_officer_quad_2019_aftn
ggsave(filename = "map_crimes_officer_quad_2019_aftn.png",
       plot = map_crimes_officer_quad_2019_aftn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
map_crimes_officer_quad_2019_nght <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
          aes(fill = cpp)) +
  labs(title = "Night shift (21:00 - 5:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 162)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 162)) 
map_crimes_officer_quad_2019_nght
ggsave(filename = "map_crimes_officer_quad_2019_nght.png",
       plot = map_crimes_officer_quad_2019_nght,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

## Redistributed
map_redis_crimes_officer_quad_2019_morn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = rcpp)) +
  labs(title = "Morning shift (5:00 - 13:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 162)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 162)) 
map_redis_crimes_officer_quad_2019_morn
ggsave(filename = "map_redis_crimes_officer_quad_2019_morn.png",
       plot = map_redis_crimes_officer_quad_2019_morn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
map_redis_crimes_officer_quad_2019_aftn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='13-21',],
          aes(fill = rcpp)) +
  labs(title = "Afternoon shift (13:00 - 21:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 162)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 162)) 
map_redis_crimes_officer_quad_2019_aftn
ggsave(filename = "map_redis_crimes_officer_quad_2019_aftn.png",
       plot = map_redis_crimes_officer_quad_2019_aftn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
map_redis_crimes_officer_quad_2019_nght <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
          aes(fill = rcpp)) +
  labs(title = "Night shift (21:00 - 5:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 162)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 162)) 
map_redis_crimes_officer_quad_2019_nght
ggsave(filename = "map_redis_crimes_officer_quad_2019_nght.png",
       plot = map_redis_crimes_officer_quad_2019_nght,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

### Officers per quadrant
## status quo
map_officers_quad_2019 <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
          aes(fill = n_of_police)) +
  labs(title = "Officers per Quadrant",
       subtitle = "Uniform distribution - 2 per quadrant",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8))
map_officers_quad_2019
ggsave(filename = "map_officers_quad_2019.png",
       plot = map_officers_quad_2019,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

### redistribution
map_redis_officers_quad_2019_morn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = rn_of_police)) +
  labs(title = "Officers per Quadrant - Redistribution",
       subtitle = "Morning Shift",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8)) 
map_redis_officers_quad_2019_morn
ggsave(filename = "map_redis_officers_quad_2019_morn.png",
       plot = map_redis_officers_quad_2019_morn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

map_redis_officers_quad_2019_aftn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='13-21',],
          aes(fill = rn_of_police)) +
  labs(title = "Officers per Quadrant - Redistribution",
       subtitle = "Afternoon Shift",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8)) 
map_redis_officers_quad_2019_aftn
ggsave(filename = "map_redis_officers_quad_2019_aftn.png",
       plot = map_redis_officers_quad_2019_aftn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

map_redis_officers_quad_2019_nght <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
          aes(fill = rn_of_police)) +
  labs(title = "Officers per Quadrant - Redistribution",
       subtitle = "Night Shift",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8)) 
map_redis_officers_quad_2019_nght
ggsave(filename = "map_redis_officers_quad_2019_nght.png",
       plot = map_redis_officers_quad_2019_nght,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")



## Analysis - crimes per officer ----------
crimes_p_officer <- df_shift %>%
  st_drop_geometry() %>%
  group_by(shift) %>%
  summarise(`Mean Crimes` = mean(cpp),
            `Median Crimes` = median(cpp),
            `Max Crimes` = max(cpp),
            `Min Crimes` = min(cpp)) %>%
  arrange(`Max Crimes`)
write.csv(x = crimes_p_officer,
          file = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform/crimes_p_officer.csv",
          row.names = F)
crimes_p_officer_redis <- df_shift %>%
  st_drop_geometry() %>%
  group_by(shift) %>%
  dplyr::summarise(`Mean Crimes` = mean(rcpp),
            `Median Crimes` = median(rcpp),
            `Max Crimes` = max(rcpp),
            `Min Crimes` = min(rcpp))
write.csv(x = crimes_p_officer_redis,
          file = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform/crimes_p_officer_redis.csv",
          row.names = F)
