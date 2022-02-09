#use only after loading 2_Maps dataframes


# fix loop ---------------
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

# maps for crimes by year ---------------

## 2019 total crime map
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
## 2018 total crime map with fill limits
map_total_crimes_2018_lim <- ggplot() +
  geom_sf(data = df_main %>% mutate(total_crimes_2018 = ifelse(total_crimes_2018 > 16, 16, total_crimes_2018)),
          aes(fill = total_crimes_2018, color = total_crimes_2018)) +
  scale_fill_viridis_c(limits = c(0, 16)) +
  scale_color_viridis_c(limits = c(0, 16)) +
  labs(fill = "Number of crimes",
       color = "Number of crimes") +
  ylim(min = 6.17, max = 6.32) +
  xlim(min = -75.67, max = -75.48)
map_total_crimes_2018_lim
ggsave(filename = "map_total_crimes_2018_lim.png",
       plot = map_total_crimes_2018_lim,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Policy Lab/Data/Colombia-Police-Reform")
## 2018 total crime map with no limits
map_total_crimes_2018 <- ggplot() +
  geom_sf(data = df_main,
          aes(fill = total_crimes_2018, color = total_crimes_2018)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
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

