# Visualizations ####

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
#install.packages("leaflet")
library(leaflet)
library(ggeasy)

#PATH <- "E://Files/HaHaHariss/22Winter/Policy Lab/Data"
PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Data" 
# using 2019 data
#df_shift <- st_read(file.path(PATH, "df_shift.shp")) 
# using 2018-2021 averaged data
df_shift <- st_read(file.path(PATH, "df_shifts_avg.shp")) 
## Histograms ####

### crime per police function ####
plot_cpp <- function(df, cpp, rcpp){
  # prepare df for plotting
  df <- st_drop_geometry(df)
  df_temp <- as.data.frame(df[,c('region', 'shift', cpp, rcpp)])
  df_long <- melt(df, id.vars = c("region", "shift"),
                  measure.vars = c(cpp, rcpp),
                  variable.name = "distribution",
                  value.name = "crime_per_police")
  levels(df_long$distribution) <- c('status quo','re-allocation')
  group_mean <- ddply(df_long, "distribution", summarise, 
                      grp.mean=mean(crime_per_police, na.rm = T))
  
  
  # plot
  p <- ggplot(df_long, aes(x = crime_per_police, color = distribution, fill = distribution)) +
    geom_histogram(aes(y=..density..), alpha = 0.05, position = "identity", binwidth = 1)+
    geom_vline(data = group_mean, aes(xintercept = grp.mean, color = distribution),
               size = 1.25)+
    geom_density(alpha = .2)+
    ggtitle('Crime per officer: before and after redistribution') +
    xlab(label = "Crimes per officer by quadrant-shift") +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          legend.text =  element_text(size = 12),
          legend.title = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13)) +
    ylim(0, 0.07)
  #print(p)
  #return(p)
}

## Optimal reallocation - proportional -----------
p_cpp <- plot_cpp(df_shift, 'cpp', 'rcpp') 
p_cpp
ggsave(filename = "hist_crimes_p_officer.png",
       plot = p_cpp,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")


## Different re-allocation strategies: Quintiles ----------

# Send one officer from lowest 20% to highest 20% quad-shifts
quantile(df_shift$cpp, probs = seq(.1, .9, by = .1))
df_shift$cpp_rank <- case_when(df_shift$cpp < 3.875 ~ 1,
                               df_shift$cpp >= 3.875 & df_shift$cpp < 8.750 ~ 2,
                               df_shift$cpp >= 8.750 & df_shift$cpp < 14.775 ~ 3,
                               df_shift$cpp >= 14.775 & df_shift$cpp < 24.700 ~ 4,
                               df_shift$cpp > 24.700 ~ 5)
df_shift$rn_police_quintile <- case_when(df_shift$cpp_rank == 1 ~ df_shift$n_f_plc - 1,
                                         df_shift$cpp_rank > 1 & df_shift$cpp_rank < 5  ~ df_shift$n_f_plc,
                                         df_shift$cpp_rank == 5 ~ df_shift$n_f_plc + 1)
df_shift$rcpp_quintile <- crime_per_police(df_shift, 'sum', 'rn_police_quintile')
p_cpp_quintile <- plot_cpp(df_shift, 'cpp', 'rcpp_quintile') +
  labs(subtitle = "One officer from 20% lowest crime quadrants to 20% highest crime quadrants") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 10))
p_cpp_quintile
ggsave(filename = "p_cpp_quintile.png",
       plot = p_cpp_quintile,
       path = PATH)

# Send one officer from lowest 40% to highest 40% quad-shifts
df_shift$rn_police_2quintile <- case_when(df_shift$cpp_rank < 3 ~ df_shift$n_f_plc - 1,
                                          df_shift$cpp_rank == 3  ~ df_shift$n_f_plc,
                                          df_shift$cpp_rank > 3 ~ df_shift$n_f_plc + 1)
df_shift$rcpp_2quintile <- crime_per_police(df_shift, 'sum', 'rn_police_2quintile')
p_cpp_2quintile <- plot_cpp(df_shift, 'cpp', 'rcpp_2quintile') +
  labs(subtitle = "One officer from 40% lowest crime quadrants to 40% highest crime quadrants") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 10))
p_cpp_2quintile
ggsave(filename = "p_cpp_2quintile.png",
       plot = p_cpp_2quintile,
       path = PATH)

# Send one officer from bottom 40% to top 20%
df_shift$rn_police_3quintile <- case_when(df_shift$cpp_rank < 3 ~ df_shift$n_f_plc - 1,
                                          df_shift$cpp_rank == 3 | df_shift$cpp_rank == 4  ~ df_shift$n_f_plc,
                                          df_shift$cpp_rank > 4 ~ df_shift$n_f_plc + 2)
df_shift$rcpp_3quintile <- crime_per_police(df_shift, 'sum', 'rn_police_3quintile')
p_cpp_3quintile <- plot_cpp(df_shift, 'cpp', 'rcpp_3quintile') +
  labs(subtitle = "One officer from 40% lowest crime quadrants to 20% highest crime quadrants") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 10))
p_cpp_3quintile
ggsave(filename = "p_cpp_3quintile.png",
       plot = p_cpp_3quintile,
       path = PATH)





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
#    scale_x_continuous(breaks = seq(0, 170, 25)) #+
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

# table with crime sum stats by shift ------
table_crimes <- df_shift %>%
  st_drop_geometry() %>%
  group_by(shift) %>%
  summarise(`Total homicides` = sum(homicid),
            `Total car thefts` = sum(vhcl_th),
            `Total robberies` = sum(theft),
            `Total crimes` = sum(sum)) %>%
  as.data.frame()
write.csv(x = table_crimes,
          file = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Midterm deliverables/.csv",
          row.names = F)

## Maps ----------------------------------------------------------------------------
map_simple <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = sum)) +
  labs(fill = "Crimes",
       color = "Crimes",
       title = "Crimes per quadrant in Medellin",
       subtitle = "Average over 2018-2021") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 170)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 170)) 
ggsave(filename = "map_simple.png",
       plot = map_simple,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

map_crimes_quad_2019_morn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = sum)) +
  labs(title = "Morning shift (5:00 - 13:00)",
       fill = "Crimes",
       color = "Crimes") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 170)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 170)) 
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
  scale_fill_viridis_c(option = "inferno", limits = c(0, 170)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 170)) 
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
  scale_fill_viridis_c(option = "inferno", limits = c(0, 170)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 170)) 
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
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 15)) +
  # if using the averaged data limits are 0, 80. If using only 2019 data, limits are 0, 162.
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
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
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 15)) +
  # if using the averaged data limits are 0, 80. If using only 2019 data, limits are 0, 162.
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
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
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 15)) +
  # if using the averaged data limits are 0, 80. If using only 2019 data, limits are 0, 162.
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
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
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 15)) +
  # if using the averaged data limits are 0, 80. If using only 2019 data, limits are 0, 162.
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
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
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 15)) +
  # if using the averaged data limits are 0, 80. If using only 2019 data, limits are 0, 162.
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
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
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 15)) +
  # if using the averaged data limits are 0, 80. If using only 2019 data, limits are 0, 162.
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80))
map_redis_crimes_officer_quad_2019_nght
ggsave(filename = "map_redis_crimes_officer_quad_2019_nght.png",
       plot = map_redis_crimes_officer_quad_2019_nght,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")


### Officers per quadrant -----------------

## status quo
map_officers_quad_2019 <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
         #aes(fill = n_of_police)) +
         aes(fill = n_f_plc)) +
  labs(title = "Officers per Quadrant",
       subtitle = "Uniform distribution - 2 per quadrant",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8))
map_officers_quad_2019
ggsave(filename = "map_officers_quad_2019.png",
       plot = map_officers_quad_2019,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

### redistribution
map_redis_officers_quad_2019_morn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          #aes(fill = rn_of_police)) +
          aes(fill = rn_f_pl)) +
  labs(title = "Officers per Quadrant - Redistribution",
       subtitle = "Morning Shift",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8)) 
map_redis_officers_quad_2019_morn
ggsave(filename = "map_redis_officers_quad_2019_morn.png",
       plot = map_redis_officers_quad_2019_morn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

map_redis_officers_quad_2019_aftn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='13-21',],
          #aes(fill = rn_of_police)) +
          aes(fill = rn_f_pl)) +
  labs(title = "Officers per Quadrant - Redistribution",
       subtitle = "Afternoon Shift",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8)) 
map_redis_officers_quad_2019_aftn
ggsave(filename = "map_redis_officers_quad_2019_aftn.png",
       plot = map_redis_officers_quad_2019_aftn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")

map_redis_officers_quad_2019_nght <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
          #aes(fill = rn_of_police)) +
          aes(fill = rn_f_pl)) +
  labs(title = "Officers per Quadrant - Redistribution",
       subtitle = "Night Shift",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8)) 
map_redis_officers_quad_2019_nght
ggsave(filename = "map_redis_officers_quad_2019_nght.png",
       plot = map_redis_officers_quad_2019_nght,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")



## Analysis - crimes per officer ----------
crimes_p_officer <- df_shift %>%
  st_drop_geometry() %>%
  group_by(shift) %>%
  dplyr::summarise(`Mean Crimes` = mean(cpp),
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


df_shift %>%
  mutate(less_five = ifelse(sum <= 5, 1, 0),
         more_fifty = ifelse(sum >= 100, 1, 0)) %>%
  summarise(sum_five = sum(less_five),
            sum_fifty = sum(more_fifty))
