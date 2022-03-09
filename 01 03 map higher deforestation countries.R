

# -------------------------------------------------------------------------
# Cut the countries - Tropical
# Author: Fabio Castro
# December 28th - 2022 - 
# -------------------------------------------------------------------------
setwd('//catalogue/CL02_UCOD_FAO')
source('./4.Scripts/v1/00 00 load libraries.R')
options(scipen = 999)

# Function ----------------------------------------------------------------
get_zones <- function(cnt){
  
  cnt <- 'Madagascar'
  
  cat('Start ', cnt, '\n')
  cou <- filter(topc, country == cnt)
  cou <- pull(cou, country)
  ctn <- filter(topc, country == cnt)
  ctn <- pull(ctn, continent)
  shp <- shpf[shpf$ENGLISH == cou,]
  
  cat('To make the extract by mask\n')
  fle <- grep(ctn, fles, value = TRUE)
  fle <- as.character(fle)
  trr <- terra::rast(fle)
  trr <- raster::crop(trr, shp)
  trr <- raster::mask(trr, shp)
  trr <- trr * 1
  pnt <- raster::as.data.frame(trr, xy = TRUE)
  pnt <- as_tibble(pnt)
  colnames(pnt) <- c('x', 'y', 'value')
  pnt <- filter(pnt, value != 0)
  pnt <- filter(pnt, value != 1)
  yrs <- unique(pnt$value)
  yrs <- sort(yrs)
  pnt <- mutate(pnt, value = factor(value, levels = yrs))
  unique(pnt$value)
  
  cat('To make the map\n')
  ggp <- ggplot() + 
    geom_tile(data = pnt, aes(x = x, y = y, fill = value, col = value)) + 
    scale_fill_manual(values = cols) + 
    scale_color_manual(values = cols) +
    geom_sf(data = st_as_sf(shp), fill = NA) + 
    coord_sf() + 
    ggtitle(label = glue('Deforestation {cou}')) +
    theme_pander() + 
    labs(x = 'Longitude', y = 'Latitude', fill = 'Deforestation\nYear', caption = 'Source: Terra-i') +
    theme(axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5),
          axis.text.x = element_text(size = 8), 
          legend.position = 'bottom', 
          legend.title = element_text(size = 12, face = 'bold'), 
          plot.title = element_text(size = 16, face = 'bold', hjust = 0.5)) + 
    annotation_north_arrow(location = 'tr', which_north = 'true', style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = 'bl', bar_cols = c("grey60", "white"), text_family = "ArcherPro Book") + 
    guides(color = 'none', 
           fill = guide_legend(ncol = 5, nrow = 4))
  cat('Done!\n')
  return(ggp)
  
}

# Load data ---------------------------------------------------------------
tble <- read_csv('./1.Data/tbl/terra_i/count_pixels_deforestation_alerts/tb_allerts_deforestation_by_year_continent.csv')
shpf <- terra::vect('./1.Data/shp/base/all_countries_tropical.shp')

# Get the countries
topc <- tble %>% distinct(continent, country)

# Raster data
fles <- dir_ls('./1.Data/raster/terra-i/classified', regexp = '.tif$')
trra <- map(fles, raster)

# Colors cptcity
find_cpt('earth')
image(matrix(1:100),col = cpt(pal = "gist_earth"),axes = FALSE)
image(matrix(1:100),col = cpt(pal = "pj_4_earth"),axes = FALSE)
image(matrix(1:100),col = cpt(pal = "pj_6_sky_earth_water"),axes = FALSE)
cols <- cpt('gist_earth', n = 18)
dout <- glue('./7.Png/maps/deforestation')

# -------------------------------------------------------------------------
# Make the maps -----------------------------------------------------------
# -------------------------------------------------------------------------

# Africa maps -------------------------------------------------------------
cngo <- get_zones(cnt = 'Democratic Republic of the Congo')
mdgs <- get_zones(cnt = 'Madagascar')
safr <- get_zones(cnt = 'South Africa')

# Save Africa maps
ggsave(plot = cngo, filename = glue('{dout}/dfr_Democratic Republic of the Congo.jpg'), units = 'in', width = 7, height = 7, dpi = 300)
ggsave(plot = mdgs, filename = glue('{dout}/dfr_Madagascar.jpg'), units = 'in', width = 6, height = 12, dpi = 300)
ggsave(plot = safr, filename = glue('{dout}/dfr_South Africa.jpg'), units = 'in', width = 7, height = 7, dpi = 300)

# Asia maps
chna <- get_zones(cnt = 'China')
indn <- get_zones(cnt = 'Indonesia')
mlsy <- get_zones(cnt = 'Malaysia')

ggsave(plot = chna, filename = glue('{dout}/dfr_China.jpg'), units = 'in', width = 7, height = 7, dpi = 300)
ggsave(plot = indn, filename = glue('{dout}/dfr_Indonesia.jpg'), units = 'in', width = 7, height = 7, dpi = 300)
ggsave(plot = mlsy, filename = glue('{dout}/dfr_Malaysia.jpg'), units = 'in', width = 10, height = 6, dpi = 300)

# Latin
blva <- get_zones(cnt = 'Bolivia')
brzl <- get_zones(cnt = 'Brazil')
prgy <- get_zones(cnt = 'Paraguay')

ggsave(plot = blva, filename = glue('{dout}/dfr_Bolivia.jpg'), units = 'in', width = 7, height = 9, dpi = 300)
ggsave(plot = brzl, filename = glue('{dout}/dfr_Brazil.jpg'), units = 'in', width = 7, height = 7, dpi = 300)
ggsave(plot = prgy, filename = glue('{dout}/dfr_Paraguay.jpg'), units = 'in', width = 5, height = 9, dpi = 300)

# Oceania
astr <- get_zones(cnt = 'Australia')
fiji <- get_zones(cnt = 'Fiji')
fiji <- fiji + coord_sf(xlim = c(177, 181), ylim = c(-20, -16))
newc <- get_zones(cnt = 'New Caledonia')
newc <- newc + coord_sf(xlim = c(163, 168.5))
ggsave(plot = astr, filename = glue('{dout}/dfr_Australia.jpg'), units = 'in', width = 7, height = 9, dpi = 300)
ggsave(plot = fiji, filename = glue('{dout}/dfr_Fiji.jpg'), units = 'in', width = 7, height = 8, dpi = 300)
ggsave(plot = newc, filename = glue('{dout}/dfr_NewCaicedonia.jpg'), units = 'in', width = 8, height = 7, dpi = 300)

