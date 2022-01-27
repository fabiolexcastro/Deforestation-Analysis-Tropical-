

# -------------------------------------------------------------------------
# Cut the countries - Tropical
# Author: Fabio Castro
# December 16th - 2021
# -------------------------------------------------------------------------

source('00 00 load libraries.R')

# Load data ---------------------------------------------------------------
wrld <- shapefile('./1.Data/shp/base/all_countries.shp')
extent(wrld)
trpc <- c(-180, 180, -23.5, 23.5)

# Cropping ----------------------------------------------------------------
wrld.crop <- raster::crop(x = wrld, y = trpc)
plot(wrld.crop)

wrld.cntr <- wrld[wrld@data$COUNTRY %in% wrld.crop$COUNTRY,]

# Write these files -------------------------------------------------------
shapefile(wrld.cntr, './1.Data/shp/base/all_countries_tropical.shp')
shapefile(wrld.crop, './1.Data/shp/base/all_countries_crop.shp')

# Counting the countries --------------------------------------------------
smmr <- wrld.cntr %>% 
  as.data.frame %>% 
  as_tibble() %>% 
  dplyr::select(COUNTRY, ENGLISH, CONTINENT)

freq <- smmr %>% 
  group_by(CONTINENT) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>%
  filter(count > 1) %>% 
  mutate(CONTINENT = factor(CONTINENT, levels = CONTINENT))

gbar <- ggplot(data = freq, aes(x = CONTINENT, y = count)) + 
  geom_col() +
  coord_flip() + 
  theme_ipsum_es() + 
  labs(x = '', y = 'Country frequency') + 
  ggtitle(label = 'Country frequency by Continent - Intertropical') + 
  theme(plot.title = element_text(size = 14, hjust = 0.5))

ggsave(plot = gbar, filename = './7.Png/graphs/general/frequency_continent_studyzone_v1.png', 
       units = 'in', width = 8, height = 6, dpi = 300)

# Make a map of the studyzone
gmap <- ggplot() + 
  geom_sf(data = st_as_sf(wrld), fill = NA) + 
  geom_sf(data = st_as_sf(wrld.cntr), fill = '#A9B6A8', alpha = 0.5) +
  geom_sf(data = st_as_sf(wrld.crop), fill = '#954242', alpha = 0.5) +
  theme_bw() + 
  theme() + 
  coord_sf() +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), # 0.2 # 0.3
                         style = north_arrow_fancy_orienteering)

ggsave(plot = gmap,
       filename = './7.png/maps/gral/zone_v1.jpg', 
       units = 'in', width = 9, height = 5, dpi = 300)

