
# -------------------------------------------------------------------------
# Cut the countries - Tropical
# Author: Fabio Castro
# December 16th - 2021 - Update: 1st - 2022
# -------------------------------------------------------------------------
setwd('//catalogue/CL02_UCOD_FAO')
source('./4.Scripts/v1/00 00 load libraries.R')
options(scipen = 999)
library(colorspace)
library(ghibli)

# Import fonts ------------------------------------------------------------
font_import()

# Functions ---------------------------------------------------------------
extract_mask <- function(rst){
  
  # rst <- rstr[[1]]
  
  cat('Start\n')
  zne <- raster::crop(zone, extent(rst))
  cnt <- zne@data$ENGLISH
  nme <- names(rst) %>% str_split(., pattern = '_') %>% unlist() %>% .[1]
  
  tbl <- list()
  
  cat('To process the pixels\n')
  for(i in 1:length(cnt)){
    
    cat('Processing country: ', cnt[i], '\n')
    lim <- zne[zne@data$ENGLISH == cnt[i],]
    rsl <- raster::crop(rst, lim) %>% raster::mask(., lim)
    tbl[[i]] <- rsl %>% rasterToPoints() %>% as_tibble() %>% mutate(country = cnt[i])
    
  }
  
  cat('To save the RDS file\n')
  tbl <- bind_rows(tbl)
  saveRDS(object = tbl, file = glue('./8.RDS/terra_i/count_raw_{nme}.rds'))
  
  cat('To make the summarise\n')
  smm <- tbl %>% 
    setNames(c('x', 'y', 'value', 'country')) %>% 
    group_by(value, country) %>% 
    summarise(count = n()) %>% 
    ungroup()
  
  smm <- smm %>% 
    spread(value, count) %>% 
    setNames(c('country', 'No_frst', 'No_info', glue('y{2004:2021}')))
  smm[is.na(smm)] <- 0
  smm <- mutate(smm, continent = nme)
  smm <- dplyr::select(smm, continent, everything())
  saveRDS(object = smm, file = glue('../8.RDS/terra_i/count_smm_{nme}.rds'))
  cat('Done!\n')
  return(smm)
  
}
make_graph <- function(tbl){
  
  cat("Start\n")
  tbl <- tbl %>% 
    gather(period, value, -continent, -country) %>% 
    filter(!period %in% c('No_frst', 'No_info')) %>% 
    mutate(year = parse_number(period)) %>% 
    dplyr::select(continent, country, year, period, value)
  
  smm <- tbl %>% 
    group_by(country) %>% 
    summarise(value = sum(value)) %>% 
    ungroup()
  
  sub <- top_n(x = smm, n = 3, wt = value) %>% pull(1)
  
  top <- tbl %>% filter(country %in% sub)
  
  cat('To make the graph\n')
  ggp <- ggplot(data = top, aes(x = year, y = value, col = country)) +
    geom_smooth(method = 'loess', se = FALSE) + 
    scale_color_discrete_qualitative(palette = 'Dark 2') +
    scale_y_continuous(limits = c(0, max(top$value))) +
    theme_ipsum_es() +
    theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          legend.position = 'bottom') +
    labs(x = 'Year', y = 'Count (pix) deforestation alerts', col = '')
  
  return(list(ggp, top))
  
}

# Load data ---------------------------------------------------------------
fles <- dir_ls('./1.Data/raster/terra-i/classified') %>% 
  grep('.tif$', ., value = TRUE) %>% 
  as.character()
rstr <- map(fles, raster)
zone <- raster::shapefile('../1.Data/shp/base/all_countries_tropical.shp')

# A simple plot -----------------------------------------------------------
plot(rstr[[1]])

# Extract by mask ---------------------------------------------------------
afrc <- extract_mask(rst = rstr[[1]])
asia <- extract_mask(rst = rstr[[2]])
ltin <- extract_mask(rst = rstr[[3]])
ocna <- extract_mask(rst = rstr[[4]])

# Load these results ------------------------------------------------------
fles <- as.character(dir_ls('../8.RDS/terra_i', regexp = 'smm'))
afrc <- grep('africa', fles, value = TRUE) %>% readRDS()
asia <- grep('asia', fles, value = TRUE) %>% readRDS()
ltin <- grep('latin', fles, value = TRUE) %>% readRDS()
ocna <- grep('ocean', fles, value = TRUE) %>% readRDS()

# Analyzing these data ----------------------------------------------------
gg_afrc <- make_graph(tbl = afrc)
gg_asia <- make_graph(tbl = asia)
gg_ltin <- make_graph(tbl = ltin)
gg_ocna <- make_graph(tbl = ocna)

dir.create(path = './7.Png/graphs/deforestation/terra_i', recursive = TRUE)
dout <- './7.Png/graphs/deforestation/terra_i'
ggsave(plot = gg_afrc, filename = glue('{dout}/top_03_terra_i_africa.png'), units = 'in', width = 9, height = 7, dpi = 300)
ggsave(plot = gg_asia, filename = glue('{dout}/top_03_terra_i_asia.png'), units = 'in', width = 9, height = 7, dpi = 300)
ggsave(plot = gg_ltin, filename = glue('{dout}/top_03_terra_i_latin.png'), units = 'in', width = 9, height = 7, dpi = 300)
ggsave(plot = gg_ocna, filename = glue('{dout}/top_03_terra_i_oceania.png'), units = 'in', width = 9, height = 7, dpi = 300)

# Get the tables ----------------------------------------------------------
tb_afrc <- gg_afrc[[2]] %>% dplyr::select(continent, country, period, value) %>% spread(period, value)
tb_asia <- gg_asia[[2]] %>% dplyr::select(continent, country, period, value) %>% spread(period, value)
tb_ltin <- gg_ltin[[2]] %>% dplyr::select(continent, country, period, value) %>% spread(period, value)
tb_ocna <- gg_ocna[[2]] %>% dplyr::select(continent, country, period, value) %>% spread(period, value)

tb_allr <- rbind(tb_afrc, tb_asia, tb_ltin, tb_ocna)
write.csv(tb_allr, './1.Data/tbl/terra_i/count_pixels_deforestation_alerts/tb_allerts_deforestation_by_year_continent.csv', row.names = FALSE)


