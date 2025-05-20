### Figures for PNAS Manuscript ###

library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(readr)
library(terra)
library(tidyterra)
library(raster)
library(sp)
library(sf)
library(ggthemes)
library(extrafont)
library(lubridate)
library(paletteer)
library(tidyr)
library(tibble)
library(patchwork)
library(magick)
library(ggnewscale)
library(mgcv)
library(ggspatial)
library(scales)
library(forecast)
library(plotrix)
library(ggbreak)

#### AGB and BGB Stocks Maps ####
#basemap
ga_coast <- vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/gshhg_coastline_clipped2.shp")
ga_coast <- project(ga_coast, "epsg:32617")
#states
state_borders <- vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/states/cb_2018_us_state_500k_GA_borders.shp")
state_borders <- project(state_borders, "epsg:32617")

state_borders2 <- vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/states/cb_2018_us_state_500k.shp")
state_borders2 <- project(state_borders2, "epsg:4326")

#map theme
map_theme <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 15, hjust = 0.5),
      axis.text = element_text(family = font, size = 10), 
      axis.title = element_blank(), 
      panel.background = element_rect(fill = "white", color = "white"),
      panel.border = element_rect(fill = NA, color = NA),
      legend.title = element_text(size = 16, family = font),
      legend.text = element_text(size = 10, family = font),
      legend.margin=margin(1,1,1,1),
      legend.box.margin=margin(1,1,1,1),
      legend.key.width = unit(20, 'pt'),
      legend.key.height = unit(30, 'pt'),
      legend.position = c(0.3,0.7),
      legend.background = element_rect(fill = "transparent"),
      text = element_text(family = font),
      #margin
      #plot.margin = margin(-100,-100,-100,-100),
    )
} 
map_theme_legends_left <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 14, hjust = 0.5),
      axis.text = element_text(family = font, size = 10), 
      axis.title = element_blank(), 
      panel.background = element_rect(fill = "white", color = "white"),
      legend.title = element_text(size = 16, family = font),
      legend.text = element_text(size = 10, family = font),
      legend.margin=margin(1,1,1,1),
      legend.box.margin=margin(1,1,1,1),
      legend.key.height = unit(50, 'pt'),
      legend.position = "left",
      legend.direction = "vertical",
      text = element_text(family = font),
      #margin
      plot.margin = margin(1,1,1,1),
    )
} 
map_theme_legends_right <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 14, hjust = 0, vjust = -2.5),
      plot.subtitle = element_text(family = font, size = 11, hjust = 1, vjust = 3),
      axis.text = element_text(family = font, size = 10), 
      axis.title = element_blank(), 
      panel.background = element_rect(fill = "white", color = "white"),
      panel.border = element_rect(fill = NA, color = NA),
      legend.title = element_text(size = 16, family = font),
      legend.text = element_text(size = 10, family = font),
      legend.margin=margin(1,1,1,1),
      legend.box.margin=margin(1,1,1,1),
      legend.key.height = unit(50, 'pt'),
      legend.position = "right",
      legend.direction = "vertical",
      text = element_text(family = font),
      #margin
      plot.margin = margin(1,1,1,1),
    )
} 

## Coast Wide ##

# Pixel AGB Stock
ag_pix_stock <- rast("output/maps/predag_mean.tif")
# Pixel BGB Stock
bg_pix_stock <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/predbg_mean.tif")

# Axis Values
vals_ag_stock <- c(round_any(minmax(ag_pix_stock)[1], 10, f = floor), round_any(minmax(ag_pix_stock)[2], 10, f = ceiling))
vals_bg_stock <- c(round_any(minmax(bg_pix_stock)[1], 10, f = floor), round_any(minmax(bg_pix_stock)[2], 100, f = ceiling))

# Values
scale_bar_text_cex <- 0.5
bg_color <- "whitesmoke"
scalebar_pad_x <- 5
scalebar_pad_y <- 20
annotation_size <- 4
h <- 0
#leg_dir <- "horizontal"
ag_breaks <- c(seq(150, 300, 50))
ag_labs <- c(seq(150, 250, 50), parse(text=paste("300", "~g~m^-~2")))
bg_breaks <- c(seq(300, 2000, 425))
bg_labs <- c(seq(300, 1575, 425), parse(text=paste("2000", "~g~m^-~2")))

# Maps
# AGB
ext <- ext(ag_pix_stock)
unit <- paste("g~m^-~2")
agb_pix_stock_map <- ggplot(data = ag_pix_stock) +
  geom_sf(data = ga_coast, fill = "white") +
  geom_sf(data = state_borders, color = "black", fill = "transparent") +
  geom_raster(aes(x = x, y = y, fill = predag.allom.l8)) + 
  coord_sf() + 
  scale_x_continuous(breaks = c(-81.5, -81), limits = c(434265, 514485)) +
  scale_y_continuous(breaks = c(31, 32), limits = c(3398085, 3554895)) +
  scale_fill_gradientn(colors = c(rev(terrain.colors(7)), "#007000", "#005600"), na.value = "transparent", limits = vals_ag_stock, breaks = ag_breaks, labels = ag_labs) +
  labs(title = "Aboveground", x = "", y = "", fill = "") +
  map_theme() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  annotate(geom = 'text', label = paste0("bar(x) == ", format(round(global(ag_pix_stock, mean, na.rm = T),0), nsmall =0)),
           x = ext[1], y = ext[4], vjust = -0.2, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = 'text', label = paste0("sigma == ", format(round(global(ag_pix_stock,sd,na.rm = T),0),nsmall = 0)),
           x = ext[1], y = ext[4], vjust = 1.7, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 1.1, hjust = h-1.25, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 0.1, hjust = h-1.35, family = "Helvetica", size = annotation_size, parse = T)

# BGB
ext <- ext(bg_pix_stock)
bgb_pix_stock_map <- ggplot(data = bg_pix_stock) +
  geom_sf(data = ga_coast, fill = "white") +
  geom_sf(data = state_borders, color = "black", fill = "transparent") +
  geom_raster(aes(x = x, y = y, fill = predbg.xgb)) + 
  coord_sf() + 
  annotation_scale(location = "br", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), width_hint = 0.3, height = unit(0.1, "in"), text_cex = 1, text_family = "Helvetica") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"),  pad_y = unit(0.4, "in"), height = unit(0.4, "in"), width = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_x_continuous(breaks = c(-81.5, -81), limits = c(434265, 514485)) +
  scale_y_continuous(breaks = c(31, 32), limits = c(3398085, 3554895)) +
  scale_fill_gradientn(colors = c(rev(terrain.colors(7)), "#007000", "#005600"), na.value = "transparent", limits = vals_bg_stock, breaks = bg_breaks, labels = bg_labs) +
  labs(title ="Belowground", x = "", y = "", fill = "") +
  map_theme() +
  theme(panel.background = element_rect(fill = "lightblue"), 
        axis.text.y = element_blank()) +
  annotate(geom = 'text', label = paste0("bar(x) ", "  == ", format(round(global(bg_pix_stock, mean, na.rm = T),0), nsmall =0)),
           x = ext[1], y = ext[4], vjust = -0.2, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = 'text', label = paste0("sigma == ", format(round(global(bg_pix_stock,sd,na.rm = T),0),nsmall = 0)),
           x = ext[1], y = ext[4], vjust = 1.7, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 0.1, hjust = h-1.35, family = "Helvetica", size = annotation_size, parse = T) + # top
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 1.1, hjust = h-1.35, family = "Helvetica", size = annotation_size, parse = T)  # bottom

# Full Page Maps for Supplement
# AGB
ext <- ext(ag_pix_stock)
unit <- paste("g~m^-~2")
agb_pix_stock_map_full <- ggplot(data = ag_pix_stock) +
  geom_sf(data = ga_coast, fill = "white") +
  geom_raster(aes(x = x, y = y, fill = predag.allom.l8)) + 
  coord_sf() + 
  scale_x_continuous(breaks = c(-81.5, -81), limits = c(434265, 514485)) +
  scale_y_continuous(breaks = c(31, 32), limits = c(3398085, 3554895)) +
  scale_fill_gradientn(colors = c(rev(terrain.colors(7)), "#007000", "#005600"), na.value = "transparent", limits = vals_ag_stock, breaks = ag_breaks, labels = ag_labs) +
  annotation_scale(location = "br", pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"), width_hint = 0.4, height = unit(0.2, "in"), text_cex = 1.4, text_family = "Helvetica") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.2, "in"),  pad_y = unit(1.0, "in"), height = unit(0.75, "in"), width = unit(0.75, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(title = "Aboveground Biomass Stock", x = "", y = "", fill = "", subtitle = "2014 - 2023") +
  map_theme_legends_right() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  annotate(geom = 'text', label = paste0("bar(x) == ", format(round(global(ag_pix_stock, mean, na.rm = T),0), nsmall =0)),
           x = ext[1], y = ext[4], vjust = -0.2, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = 'text', label = paste0("sigma == ", format(round(global(ag_pix_stock,sd,na.rm = T),0),nsmall = 0)),
           x = ext[1], y = ext[4], vjust = 1.7, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 1.1, hjust = h-1.25, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 0.1, hjust = h-1.35, family = "Helvetica", size = annotation_size, parse = T)
ggsave("figures/figure_s1.pdf", device = pdf, agb_pix_stock_map_full, width = 18, height = 22, units = "cm", dpi = 900)
ggsave("figures/figure_s1.png", agb_pix_stock_map_full, width = 18, height = 22, units = "cm", dpi = 300)

# BGB
ext <- ext(bg_pix_stock)
unit <- paste("g~m^-~2")
bgb_pix_stock_map_full <- ggplot(data = bg_pix_stock) +
  geom_sf(data = ga_coast, fill = "white") +
  geom_raster(aes(x = x, y = y, fill = predbg.xgb)) + 
  coord_sf() + 
  scale_x_continuous(breaks = c(-81.5, -81), limits = c(434265, 514485)) +
  scale_y_continuous(breaks = c(31, 32), limits = c(3398085, 3554895)) +
  scale_fill_gradientn(colors = c(rev(terrain.colors(7)), "#007000", "#005600"), na.value = "transparent", limits = vals_bg_stock, breaks = bg_breaks, labels = bg_labs) +
  annotation_scale(location = "br", pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"), width_hint = 0.4, height = unit(0.2, "in"), text_cex = 1.4, text_family = "Helvetica") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.2, "in"),  pad_y = unit(1.0, "in"), height = unit(0.75, "in"), width = unit(0.75, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(title = "Belowground Biomass Stock", x = "", y = "", fill = "", subtitle = "2014 - 2023") +
  map_theme_legends_right() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  annotate(geom = 'text', label = paste0("bar(x) ", "  == ", format(round(global(bg_pix_stock, mean, na.rm = T),0), nsmall =0)),
           x = ext[1], y = ext[4], vjust = -0.2, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = 'text', label = paste0("sigma == ", format(round(global(bg_pix_stock,sd,na.rm = T),0),nsmall = 0)),
           x = ext[1], y = ext[4], vjust = 1.7, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 0.1, hjust = h-1.35, family = "Helvetica", size = annotation_size, parse = T) + # top
annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 1.1, hjust = h-1.35, family = "Helvetica", size = annotation_size, parse = T)  # bottom
ggsave("figures/figure_s2.pdf", device = pdf, bgb_pix_stock_map_full, width = 18, height = 22, units = "cm", dpi = 900)
ggsave("figures/figure_s2.png", bgb_pix_stock_map_full, width = 18, height = 22, units = "cm", dpi = 300)

## Sapelo and Skidaway ##

# Basemap
ga_rivers <- vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/Rivers_Streams_GA.shp")
ga_rivers <- project(ga_rivers, "epsg:32617")

# Sapelo Marsh Area
flux_marsh <- vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/FluxOutline.shp")
flux_marsh <- project(flux_marsh, "epsg:32617")
flux_radius <- round(max(c(ext(flux_marsh)[2] - ext(flux_marsh)[1], ext(flux_marsh)[4] - ext(flux_marsh)[3]))/2,0)
flux_centroid <- centroids(flux_marsh)
flux_sq <- ext(ext(flux_centroid)[1] - flux_radius, ext(flux_centroid)[2] + flux_radius, ext(flux_centroid)[3] - flux_radius, ext(flux_centroid)[4] + flux_radius)

# Skidaway Marsh Area
skida_marsh <-  vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/skidaway_outline.shp")
skida_marsh <- project(skida_marsh, "epsg:32617")
skida_radius <- round(max(c(ext(skida_marsh)[2] - ext(skida_marsh)[1], ext(skida_marsh)[4] - ext(skida_marsh)[3]))/2,0)
skida_centroid <- centroids(skida_marsh)
skida_sq <- ext(ext(skida_centroid)[1] - skida_radius, ext(skida_centroid)[2] + skida_radius, ext(skida_centroid)[3] - skida_radius, ext(skida_centroid)[4] + skida_radius)

# Pixel AGB Stock
ag_pix_stock <- rast("output/maps/predag_mean.tif")
# Pixel BGB Stock
bg_pix_stock <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/predbg_mean.tif")

# Sapelo Data
# Data Crops 
flux_bg_pix_stock <- crop(bg_pix_stock, flux_sq)
flux_ag_pix_stock <- crop(ag_pix_stock, flux_sq)
# Data masks
flux_bg_pix_stock <- terra::mask(flux_bg_pix_stock, flux_marsh)
flux_ag_pix_stock <- terra::mask(flux_ag_pix_stock, flux_marsh)
# Base Crop
flux_ext <- ext(flux_sq)
flux_coast <- terra::crop(ga_coast, flux_ext)
flux_rivers <- terra::crop(ga_rivers,flux_ext)

# Skidaway Data
# Data Crops 
skida_bg_pix_stock <- crop(bg_pix_stock, skida_sq)
skida_ag_pix_stock <- crop(ag_pix_stock, skida_sq)
# Data Masks
skida_bg_pix_stock <- terra::mask(skida_bg_pix_stock, skida_marsh)
skida_ag_pix_stock <- terra::mask(skida_ag_pix_stock, skida_marsh)
# Base Crop
skida_ext <- ext(skida_sq)
skida_coast <- terra::crop(ga_coast, skida_ext)
skida_rivers <- terra::crop(ga_rivers,skida_ext)

# Values
scale_bar_text_cex <- 0.5
#bg_color <- "whitesmoke"
bg_color <- "white"
scalebar_pad_x <- 2
scalebar_pad_y <- 4
annotation_size <- 3.5
leg_dir <- "horizontal"
marsh_name_size <- 3

map_theme <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #panel.background = element_blank(),
      panel.background = element_rect(fill = "transparent",color = NA), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      #text elements
      plot.title = element_text(size = 11, family = font, hjust = 0.5),
      axis.text = element_blank(), 
      axis.title = element_blank(), 
      axis.ticks = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      legend.position = "none",
      text = element_text(family = font),
      plot.margin = unit(c(0,0,40,0), "pt"),
      #plot.tag.position = "bottomleft",
      #plot.tag = element_text(family = font, size = 10)
    )
} 


# Sapelo
ext <- ext(flux_sq)
# AGB
flux_agb_pix_stock_map <- ggplot(data = flux_ag_pix_stock) +
  #geom_sf(data = flux_coast, fill = bg_color, lwd = 0) +
  geom_sf(data = flux_rivers, fill = "lightblue", color = "lightblue", linewidth = 0) +
  geom_raster(aes(x = x, y = y, fill = predag.allom.l8 )) + 
  coord_sf(xlim = c(xmin(ext), xmax(ext)), ylim = c(ymin(ext), ymax(ext))) + 
  #scale_x_continuous(limits = c(xmin(ext), xmax(ext))) +
  #scale_y_continuous(limits = c(ymin(ext), ymax(ext))) +
  scale_fill_gradientn(colors = c(rev(terrain.colors(7)), "#007000", "#005600"), na.value = "transparent", limits = vals_ag_stock) +
  labs(fill = "", x = "", y = "") +
  annotate(geom = 'text', label = "Sapelo", x = ext[1], y = ext[3], size = marsh_name_size, hjust = 0, vjust = -0.5, family = "Helvetica") +
  map_theme()   +
  guides(fill = guide_colorbar(direction = leg_dir)) + 
  annotate(geom = 'text', label = paste0("bar(x) == ", format(round(global(flux_ag_pix_stock, mean, na.rm = T),0), nsmall =0)),
           x = ext[1], y = ext[4], vjust = 1.4, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 1, hjust = h-1.3, family = "Helvetica", size = annotation_size, parse = T) 
# BGB
flux_bgb_pix_stock_map <- ggplot(data = flux_bg_pix_stock) +
  #geom_sf(data = flux_coast, fill = bg_color, lwd = 0) +
  geom_sf(data = flux_rivers, fill = "lightblue", color = "lightblue", linewidth = 0) +
  geom_raster(aes(x = x, y = y, fill = predbg.xgb )) + 
  coord_sf(xlim = c(xmin(ext), xmax(ext)), ylim = c(ymin(ext), ymax(ext))) + 
  #scale_x_continuous(limits = c(xmin(ext), xmax(ext))) +
  #scale_y_continuous(limits = c(ymin(ext), ymax(ext))) +
  scale_fill_gradientn(colors = c(rev(terrain.colors(7)), "#007000", "#005600"), na.value = "transparent", limits = vals_bg_stock) +
  labs(fill = "", x = "", y = "") +
  annotate(geom = 'text', label = "Sapelo", x = ext[1], y = ext[3], size = marsh_name_size, hjust = 0, vjust = -0.5, family = "Helvetica") +
  map_theme()   +
  guides(fill = guide_colorbar(direction = leg_dir)) +
  annotation_scale(height = unit(10, "pt"), location = "br", pad_x = unit(scalebar_pad_x, "pt"), pad_y = unit(scalebar_pad_y, "pt"), text_cex = 0.7, text_family = "Helvetica") + 
  annotate(geom = 'text', label = paste0("bar(x) == ", format(round(global(flux_bg_pix_stock, mean, na.rm = T),0), nsmall =0)),
           x = ext[1], y = ext[4], vjust = 1.4, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 1, hjust = h-1.5, family = "Helvetica", size = annotation_size, parse = T)
  
# Skidaway
ext <- ext(skida_sq)
#AGB
skida_agb_pix_stock_map <- ggplot(data = skida_ag_pix_stock) +
  #geom_sf(data = skida_coast, fill = bg_color, lwd = 0) +
  geom_sf(data = skida_rivers, fill = "lightblue", color = "lightblue", linewidth = 0) +
  geom_raster(aes(x = x, y = y, fill = predag.allom.l8 )) + 
  coord_sf(xlim = c(xmin(ext), xmax(ext)), ylim = c(ymin(ext), ymax(ext))) + 
  #scale_x_continuous(limits = c(xmin(ext), xmax(ext))) +
  #scale_y_continuous(limits = c(ymin(ext), ymax(ext))) +
  scale_fill_gradientn(colors = c(rev(terrain.colors(7)), "#007000", "#005600"), na.value = "transparent", limits = vals_ag_stock) +
  labs(fill = "", x = "", y = "") +
  annotate(geom = 'text', label = "Skidaway", x = ext[1], y = ext[3], size = marsh_name_size, hjust = 0, vjust = -0.5, family = "Helvetica") +
  map_theme()   +
  annotate(geom = 'text', label = paste0("bar(x) == ", format(round(global(skida_ag_pix_stock, mean, na.rm = T),0), nsmall =0)),
           x = ext[1], y = ext[4], vjust = 1.4, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 1, hjust = h-1.3, family = "Helvetica", size = annotation_size, parse = T)
#BGB
skida_bgb_pix_stock_map <- ggplot(data = skida_bg_pix_stock) +
  #geom_sf(data = skida_coast, fill = bg_color, lwd = 0) +
  geom_sf(data = skida_rivers, fill = "lightblue", color = "lightblue", linewidth = 0) +
  geom_raster(aes(x = x, y = y, fill = predbg.xgb )) + 
  coord_sf(xlim = c(xmin(ext), xmax(ext)), ylim = c(ymin(ext), ymax(ext))) + 
  #scale_x_continuous(limits = c(xmin(ext), xmax(ext))) +
  #scale_y_continuous(limits = c(ymin(ext), ymax(ext))) +
  scale_fill_gradientn(colors = c(rev(terrain.colors(7)), "#007000", "#005600"), na.value = "transparent", limits = vals_bg_stock) +
  labs(fill = "", x = "", y = "") +
  annotate(geom = 'text', label = "Skidaway", x = ext[1], y = ext[3], size = marsh_name_size, hjust = 0, vjust = -0.5, family = "Helvetica") +
  map_theme()   +
  annotation_scale(height = unit(10, "pt"), location = "br", pad_x = unit(scalebar_pad_x, "pt"), pad_y = unit(scalebar_pad_y, "pt"), text_cex = 0.7, text_family = "Helvetica") + 
  annotate(geom = 'text', label = paste0("bar(x) == ", format(round(global(skida_bg_pix_stock, mean, na.rm = T),0), nsmall =0)),
           x = ext[1], y = ext[4], vjust = 1.4, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 1, hjust = h-1.3, family = "Helvetica", size = annotation_size, parse = T)

# Marsh Unit Stats
# Biomass
flux_agb_min <- (global(flux_ag_pix_stock, min, na.rm = T))
flux_agb_max <- global(flux_ag_pix_stock, max, na.rm = T)
flux_agb_mean <- global(flux_ag_pix_stock, mean, na.rm = T)
flux_agb_sd <- global(flux_ag_pix_stock, sd, na.rm = T)

flux_bgb_min <- global(flux_bg_pix_stock, min, na.rm = T)
flux_bgb_max <- global(flux_bg_pix_stock, max, na.rm = T)
flux_bgb_mean <- global(flux_bg_pix_stock, mean, na.rm = T)
flux_bgb_sd <- global(flux_bg_pix_stock, sd, na.rm = T)

skida_agb_min <- global(skida_ag_pix_stock, min, na.rm = T)
skida_agb_max <- global(skida_ag_pix_stock, max, na.rm = T)
skida_agb_mean <- global(skida_ag_pix_stock, mean, na.rm = T)
skida_agb_sd <- global(skida_ag_pix_stock, sd, na.rm = T)

skida_bgb_min <- global(skida_bg_pix_stock, min, na.rm = T)
skida_bgb_max <- global(skida_bg_pix_stock, max, na.rm = T)
skida_bgb_mean <- global(skida_bg_pix_stock, mean, na.rm = T)
skida_bgb_sd <- global(skida_bg_pix_stock, sd, na.rm = T)


layout <- "
AABB
AABB
AABB
CDEF"

# overview map
locations <- data.frame()
locations[1,1] <- ext(flux_centroid)[1]
locations[1,2] <- ext(flux_centroid)[3]
locations[1,3] <- "Flux Tower"
locations[2,1] <- ext(skida_centroid)[1]
locations[2,2] <- ext(skida_centroid)[3]
locations[2,3] <- "Skidaway"


#map theme
map_theme <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 15, hjust = 0.5),
      axis.text = element_text(family = font, size = 10), 
      axis.title = element_blank(), 
      panel.background = element_rect(fill = "white", color = "white"),
      panel.border = element_rect(fill = NA, color = NA),
      legend.title = element_text(size = 16, family = font),
      legend.text = element_text(size = 10, family = font),
      legend.margin=margin(1,1,1,1),
      legend.box.margin=margin(1,1,1,1),
      legend.key.width = unit(20, 'pt'),
      legend.key.height = unit(30, 'pt'),
      legend.position = c(0.3,0.7),
      legend.background = element_rect(fill = "transparent"),
      text = element_text(family = font),
      #margin
      #plot.margin = margin(-100,-100,-100,-100),
    )
} 

annotation_size <- 4

# AGB
ext <- ext(ag_pix_stock)
unit <- paste("g~m^-~2")
agb_pix_stock_map <- ggplot(data = ag_pix_stock) +
  geom_sf(data = ga_coast, fill = "white") +
  geom_sf(data = state_borders, color = "black", fill = "transparent") +
  geom_raster(aes(x = x, y = y, fill = predag.allom.l8)) + 
  coord_sf() + 
  scale_x_continuous(breaks = c(-81.5, -81), limits = c(434265, 514485)) +
  scale_y_continuous(breaks = c(31, 32), limits = c(3398085, 3554895)) +
  scale_fill_gradientn(colors = c(rev(terrain.colors(7)), "#007000", "#005600"), na.value = "transparent", limits = vals_ag_stock, breaks = ag_breaks, labels = ag_labs) +
  labs(title = "Aboveground", x = "", y = "", fill = "") +
  map_theme() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  annotate(geom = 'text', label = paste0("bar(x) == ", format(round(global(ag_pix_stock, mean, na.rm = T),0), nsmall =0)),
           x = ext[1], y = ext[4], vjust = -0.2, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = 'text', label = paste0("sigma == ", format(round(global(ag_pix_stock,sd,na.rm = T),0),nsmall = 0)),
           x = ext[1], y = ext[4], vjust = 1.7, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 1.1, hjust = h-1.25, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 0.1, hjust = h-1.35, family = "Helvetica", size = annotation_size, parse = T) +
geom_point(data = locations, aes(x = V1, y  = V2), color = "black", shape = 5, size = 2, stroke = 1.25) +
  annotate("segment", x=locations[1,1] + 10000, y=locations[1,2], xend=locations[1,1] + 3000, yend=locations[1,2], color = "black", size = 0.5, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x=locations[1,1] +19000, y=locations[1,2], color = "black", size = 4, label = "Sapelo", family = "Helvetica") + 
annotate("segment", x=locations[2,1], y=locations[2,2] - 20000, xend=locations[2,1], yend=locations[2,2] -3000, color = "black", size = 0.5, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x=locations[2,1] + 10000 , y=locations[2,2]- 23000, color = "black", size = 4, label = "Skidaway", family = "Helvetica")

# BGB
ext <- ext(bg_pix_stock)
unit <- paste("g~m^-~2")
bgb_pix_stock_map <- ggplot(data = bg_pix_stock) +
  geom_sf(data = ga_coast, fill = "white") +
  geom_sf(data = state_borders, color = "black", fill = "transparent") +
  geom_raster(aes(x = x, y = y, fill = predbg.xgb)) + 
  coord_sf() + 
  scale_x_continuous(breaks = c(-81.5, -81), limits = c(434265, 514485)) +
  scale_y_continuous(breaks = c(31, 32), limits = c(3398085, 3554895)) +
  scale_fill_gradientn(colors = c(rev(terrain.colors(7)), "#007000", "#005600"), na.value = "transparent", limits = vals_bg_stock, breaks = bg_breaks, labels = bg_labs) +
  labs(title = "Belowground", x = "", y = "", fill = "") +
  map_theme() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  annotate(geom = 'text', label = paste0("bar(x) == ", format(round(global(bg_pix_stock, mean, na.rm = T),0), nsmall =0)),
           x = ext[1], y = ext[4], vjust = -0.2, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = 'text', label = paste0("sigma == ", format(round(global(bg_pix_stock,sd,na.rm = T),0),nsmall = 0)),
           x = ext[1], y = ext[4], vjust = 1.7, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 1.1, hjust = h-1.35, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = "text", label = unit,
           x = ext[1], y = ext[4], vjust = 0.1, hjust = h-1.35, family = "Helvetica", size = annotation_size, parse = T) 

biomass_stock_pix <-  agb_pix_stock_map + bgb_pix_stock_map + flux_agb_pix_stock_map + skida_agb_pix_stock_map + flux_bgb_pix_stock_map + skida_bgb_pix_stock_map +
  plot_layout(design = layout) +
  plot_annotation(title = "Biomass Stocks", theme = theme(plot.title = element_text(hjust = 0.5)), tag_levels = list(c('a', 'b', 'c', '', 'd', ''))) & 
  theme(text = element_text(family = "Helvetica", size = 16), plot.margin = margin(10,0,-10,-10), plot.tag = element_text(size = 12), plot.tag.location = "panel", plot.tag.position = c(0,1)) 
ggsave("figures/figure_2.pdf", biomass_stock_pix, width = 18, height = 22, units = "cm", dpi = 900)



# map to add as inset
# AGB
x1 <- -84
x2 <- -75
y1 <- 25
y2 <- 39
inset_map <- ggplot(data = state_borders2) +
  geom_sf(data = state_borders2, color = "black", fill = "white") +
  annotate(geom = "rect", xmin = -81.8, xmax = -80.9, ymin = 30.7, ymax = 32, color = "red", fill = NA, size = 0.5)  +
  coord_sf(xlim = c(x1, x2), ylim = c(y1,y2), clip = "on", expand = T) +
  theme(panel.background = element_rect(fill = "lightblue", color = NA, size = 0), axis.text = element_blank(), axis.ticks.length=unit(0, "pt"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )


inset_map
#ggsave("figures/inset.tiff", inset_map, dpi = 600)
ggsave("figures/figure_2_inset.pdf", inset_map, dpi = 900, width = 3, height = 6, units = "in")

#### Summary Stats by Height Zone ####

zone <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/spal_ht_zone.csv")
pix_attr <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/pix_attr/pix_attr.csv")
vul_pix <- read_csv("results/vul_pix.csv")
col_pix <- read_csv("results/col_pix.csv")

filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/pix_summ", full.names = T)
pix_summ <- lapply(filenames, read_csv, lazy = T)
pix_summ <- do.call(rbind, pix_summ)
pix_summ <- pix_summ %>%
  dplyr::select(pix, predag.allom.l8, predbg.xgb, coef_ag, coef_bg)
pix_summ$ag_trend <- pix_summ$coef_ag / pix_summ$predag.allom.l8 *12*100
pix_summ$bg_trend <- pix_summ$coef_bg / pix_summ$predbg.xgb *12*100

pix_summ <- merge(pix_summ, zone, by = "pix")
pix_summ <- merge(pix_summ, pix_attr, by = "pix")
pix_summ <- merge(pix_summ, vul_pix, by = "pix")
pix_summ <- merge(pix_summ, col_pix, by = "pix")

zone_counts <- as.data.frame(table(pix_summ$zone))
zone_counts <- spread(zone_counts, key = Var1, value = Freq)

write_csv(zone_counts, file = "results/zone_counts.csv")

zone_counts2 <- zone_counts %>%
  t() %>%
  as.data.frame() %>%
  mutate(zone = rownames(.)) %>%
  rename(count = V1)

pix_summ2 <- pix_summ %>%
  dplyr::select(zone, predag.allom.l8, predbg.xgb, ag_trend, bg_trend, inund_inten) %>%
  pivot_longer(cols= 2:6, names_to =c("metric"), values_to = "value") %>%
  group_by(zone, metric) %>%
  summarize_if(is.numeric, funs(mean, sd, std.error), na.rm = T) 

pix_summ3 <- pix_summ %>%
  mutate(location = ifelse(zone == "t", "creekbank", "platform")) %>%
  dplyr::select(location, predag.allom.l8, predbg.xgb, ag_trend, bg_trend, inund_inten) %>%
  pivot_longer(cols= 2:6, names_to =c("metric"), values_to = "value") %>%
  group_by(location, metric) %>%
  summarize_if(is.numeric, funs(mean, sd, std.error), na.rm = T) 


#pix_summ3 <- merge(pix_summ2, zone_counts2, by = "zone")

write_csv(pix_summ2, file = "results/height_forms_stats.csv")
write_csv(pix_summ3, file = "results/marsh_location_stats.csv")


loc_stats <- read_csv("results/marsh_location_stats.csv")
loc_stats$loc <- factor(loc_stats$location, levels = c("platform", "creekbank"))

plot_theme <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 12),
      axis.text.x = element_text(family = font, size = 10, angle = 0, color = "black"), 
      axis.text.y = element_text(family = font, size = 10, color = "black"), 
      axis.title.x = element_blank(), 
      axis.title.y = element_text(family = font, size = 12, color = "black"), 
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "none",
      legend.text = element_text(family = font, size = 12),
      text = element_text(family = font),
      plot.margin = margin(5,10,5,5)
      #ggside.panel.grid = element_blank(),
      #ggside.panel.background = element_blank(),
      #ggside.axis.text = element_blank(),
      #ggside.axis.ticks = element_blank()
    )
} 

library(palettetown)
pokedex(1,3)
#ichooseyou("wartortle", 3)

color_1 <- "#FDD262"
color_2 <- "#446455"

loc_agb_stock <- ggplot(data = loc_stats[loc_stats$metric == "predag.allom.l8",], aes(x = loc, fill = loc)) +
  geom_bar(aes(y = mean), stat = "identity") +
  geom_errorbar(aes(x = loc, ymin = mean - sd, ymax = mean + sd), width = 0.1, lwd = 0.3) +
  scale_fill_manual(values = c(color_1, color_2)) +
  scale_x_discrete(labels = c('Platform','Creekbank')) +
  scale_y_continuous(limits = c(0,250))+
  labs(y = expression(paste("Stock (g " ~ m^-~2 ~ ")")), title = "Aboveground Biomass") +
  plot_theme()
loc_agb_stock

loc_bgb_stock <- ggplot(data = loc_stats[loc_stats$metric == "predbg.xgb",], aes(x = loc, fill = loc)) +
  geom_bar(aes(y = mean), stat = "identity") +
  geom_errorbar(aes(x = loc, ymin = mean - sd, ymax = mean + sd), width = 0.1, lwd = 0.3) +
  scale_fill_manual(values = c(color_1, color_2)) +
  scale_x_discrete(labels = c('Platform','Creekbank')) +
  scale_y_continuous(limits = c(0,1200))+
  labs(y = expression(paste("Stock (g " ~ m^-~2 ~ ")")), title = "Belowground Biomass") +
  plot_theme()

loc_agb_trend <- ggplot(data = loc_stats[loc_stats$metric == "ag_trend",], aes(x = loc, fill = loc)) +
  geom_bar(aes(y = mean/100), stat = "identity") +
  geom_errorbar(aes(x = loc, ymin = mean/100 - sd/100, ymax = mean/100 + sd/100), width = 0.1, lwd = 0.3) +
  scale_fill_manual(values = c(color_1, color_2)) +
  scale_x_discrete(labels = c('Platform','Creekbank')) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(-0.015,0.015)) +
  labs(y = "Annual Trend") +
  plot_theme()

loc_bgb_trend<-  ggplot(data = loc_stats[loc_stats$metric == "bg_trend",], aes(x = loc, fill = loc)) +
  geom_bar(aes(y = mean/100), stat = "identity") +
  geom_errorbar(aes(x = loc, ymin = mean/100 - sd/100, ymax = mean/100 + sd/100), width = 0.1, lwd = 0.3) +
  scale_fill_manual(values = c(color_1, color_2)) +
  scale_x_discrete(labels = c('Platform','Creekbank')) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(-0.035,0.035)) +
  labs(y = "Annual Trend") +
  plot_theme()

loc_plots <- loc_agb_stock + loc_bgb_stock + loc_agb_trend + loc_bgb_trend + plot_layout(ncol = 2, nrow = 2) + plot_annotation(tag_levels = 'a')
ggsave("figures/figure_s3.pdf", loc_plots, width = 6, height = 6, dpi = 900)



#### AGB and BGB Trends Maps ####

#basemap
ga_coast <- vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/gshhg_coastline_clipped2.shp")
ga_coast <- project(ga_coast, "epsg:32617")

# Pixel AGB Trend
ag_pix_trend <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/ag_trend_mean.tif")
ag_pix_trend <- ag_pix_trend*12*100 ## trend is actually monthly, so multiply by 12 to get annual and 100 to get %
ag_pix_trend[ag_pix_trend$annual_trend < -10] <- NA #subsetting a couple outliers

# Pixel BGB Trend
bg_pix_trend <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/bg_trend_mean.tif")
bg_pix_trend <- bg_pix_trend*12*100 ## trend is actually monthly, so multiply by 12 to get annual and 100 to get %
bg_pix_trend[bg_pix_trend$annual_trend < -10] <- NA #subsetting a couple outliers

# Creating color palette
#library(devtools)
#install_github("jmw86069/colorjam")
#my.pal <- colorjam::make_jam_divergent("#ac3933", "#2c7360", n = 19)
#my.pal <- my.pal$`#ac3933_#2c7360`
#my.pal <- my.pal[c(3:7, 13:17)]

# Map Theme
map_theme_legends_right <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 14, hjust = 0, vjust = -2.5),
      plot.subtitle = element_text(family = font, size = 11, hjust = 1, vjust = 3),
      axis.text = element_text(family = font, size = 10), 
      axis.title = element_blank(), 
      panel.background = element_rect(fill = "white", color = "white"),
      panel.border = element_rect(fill = NA, color = NA),
      legend.title = element_text(size = 16, family = font),
      legend.text = element_text(size = 10, family = font),
      legend.margin=margin(1,1,1,1),
      legend.box.margin=margin(1,1,1,1),
      legend.key.height = unit(50, 'pt'),
      legend.position = "right",
      legend.direction = "vertical",
      text = element_text(family = font),
      #margin
      plot.margin = margin(1,1,1,1),
    )
} 


# Values
scale_bar_text_cex <- 0.5
scalebar_pad_x <- 5
scalebar_pad_y <- 20
annotation_size <- 4
h <- 0


# Maps
# AGB
ext <- ext(ag_pix_trend)
agb_pix_trend_map <- ggplot(data = ag_pix_trend) +
  geom_sf(data = ga_coast, fill = "white") +
  geom_raster(aes(x = x, y = y, fill = annual_trend)) + 
  coord_sf() + 
  annotation_scale(location = "br", pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"), width_hint = 0.4, height = unit(0.2, "in"), text_cex = 1.4, text_family = "Helvetica") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.2, "in"),  pad_y = unit(1.0, "in"), height = unit(0.75, "in"), width = unit(0.75, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_x_continuous(breaks = c(-81.5, -81), limits = c(434265, 514485)) +
  scale_y_continuous(breaks = c(31, 32), limits = c(3398085, 3554895)) +
  scale_fill_gradientn(colors = c(paletteer_c("ggthemes::Red-Blue Diverging", 30)[1:13], paletteer_c("ggthemes::Red-Blue Diverging", 30)[18:30]) , na.value = "transparent", limits = c(-10,10), labels = percent(seq(-0.1,0.1,0.05))) +
  labs(title = "Aboveground Biomass\nAnnual Trend", x = "", y = "", fill = "", subtitle = "2014 - 2023") + #"Annual AGB Trend", x = "", y = "", fill = "") +
  annotate(geom = 'text', label = paste0("bar(x) == ",  format(round(global(ag_pix_trend, mean, na.rm = T),2), nsmall =2), "*\'%'"),
           x = ext[1], y = ext[4], vjust = -0.2, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = 'text', label = paste0("sigma == ",  format(round(global(ag_pix_trend,sd,na.rm = T),2),nsmall = 2), "*\'%'"),
           x = ext[1], y = ext[4], vjust = 1.7, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
map_theme_legends_right() +
  theme(panel.background = element_rect(fill = "lightgrey"), plot.margin = margin(1,1,1,1,"pt"))
ggsave("figures/figure_s4.pdf", agb_pix_trend_map, width = 18, height = 22, units = "cm", dpi = 900)

# BGB
bgb_pix_trend_map <- ggplot(data = bg_pix_trend) +
  geom_sf(data = ga_coast, fill = "white") +
  geom_raster(aes(x = x, y = y, fill = annual_trend)) + 
  coord_sf() + 
  annotation_scale(location = "br", pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"), width_hint = 0.4, height = unit(0.2, "in"), text_cex = 1.4, text_family = "Helvetica") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.2, "in"),  pad_y = unit(1.0, "in"), height = unit(0.75, "in"), width = unit(0.75, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_x_continuous(breaks = c(-81.5, -81), limits = c(434265, 514485)) +
  scale_y_continuous(breaks = c(31, 32), limits = c(3398085, 3554895)) +
  scale_fill_gradientn(colors = c(paletteer_c("ggthemes::Red-Blue Diverging", 30)[1:13], paletteer_c("ggthemes::Red-Blue Diverging", 30)[18:30]) , na.value = "transparent", limits = c(-10,10), labels = percent(seq(-0.1,0.1,0.05))) +
  labs(title = "Belowground Biomass\nAnnual Trend", x = "", y = "", fill = "", subtitle = "2014 - 2023") + #"Annual Belowground Biomass Trend", x = "", y = "", fill = "") +
  annotate(geom = 'text', label = paste0("bar(x) == ",  format(round(global(bg_pix_trend, mean, na.rm = T),2), nsmall =2), "*\'%'"),
           x = ext[1], y = ext[4], vjust = -0.2, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  annotate(geom = 'text', label = paste0("sigma == ",  format(round(global(bg_pix_trend,sd,na.rm = T),2),nsmall = 2), "*\'%'"),
           x = ext[1], y = ext[4], vjust = 1.7, hjust = h, family = "Helvetica", size = annotation_size, parse = T) +
  map_theme_legends_right() +
  theme(panel.background = element_rect(fill = "lightgrey"))
ggsave("figures/figure_s5.pdf", bgb_pix_trend_map, width = 18, height = 22, units = "cm", dpi = 900)

# Marsh Unit Case Studies
# Sapelo 
# Data crops 
flux_bg_pix_trend <- crop(bg_pix_trend, flux_sq)
flux_ag_pix_trend <- crop(ag_pix_trend, flux_sq)
# Data masks
flux_bg_pix_trend <- terra::mask(flux_bg_pix_trend, flux_marsh)
flux_ag_pix_trend <- terra::mask(flux_ag_pix_trend, flux_marsh)
# Skidaway
# Data crops 
skida_bg_pix_trend <- crop(bg_pix_trend, skida_sq)
skida_ag_pix_trend <- crop(ag_pix_trend, skida_sq)
# Data masks
skida_bg_pix_trend <- terra::mask(skida_bg_pix_trend, skida_marsh)
skida_ag_pix_trend <- terra::mask(skida_ag_pix_trend, skida_marsh)

flux_agb_trend_mean <- global(flux_ag_pix_trend, mean, na.rm = T)
flux_bgb_trend_mean <- global(flux_bg_pix_trend, mean, na.rm = T)
skida_agb_trend_mean <- global(skida_ag_pix_trend, mean, na.rm = T)
skida_bgb_trend_mean <- global(skida_bg_pix_trend, mean, na.rm = T)

marsh_unit_stats <- cbind(flux_agb_min, flux_agb_max, flux_agb_mean, flux_agb_sd, 
                          flux_bgb_min, flux_bgb_max, flux_bgb_mean, flux_bgb_sd, 
                          skida_agb_min, skida_agb_max, skida_agb_mean, skida_agb_sd, 
                          skida_bgb_min, skida_bgb_max, skida_bgb_mean, skida_bgb_sd,
                          flux_agb_trend_mean, flux_bgb_trend_mean, 
                          skida_agb_trend_mean, skida_bgb_trend_mean)
colnames(marsh_unit_stats) <- c("famin", "famax", "famean", "fasd", "fbmin", "fbmax", "fbmean", "fbsd", "samin", "samax", "samean", "sasd", "sbmin", "sbmax", "sbmean", "sbsd", "fta", "ftb", "sta", "stb")
write_csv(marsh_unit_stats, file = "results/marsh_unit_stats.csv")

# Map Theme
map_theme <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 8, hjust = 0.5),
      plot.subtitle = element_text(family = font, size = 7, hjust = 0.5),
      axis.text = element_text(family = font, size = 6), 
      axis.title = element_blank(), 
      plot.tag = element_text(family = font, size = 8),
      panel.background = element_rect(fill = "transparent",color = NA), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.title = element_text(size = 6, family = font),
      legend.text = element_text(size = 8, family = font),
      legend.margin=margin(1,1,1,1),
      legend.box.margin=margin(1,1,1,1),
      legend.key.width = unit(40, 'pt'),
      legend.position = c(0.2,0.8),
      text = element_text(family = font),
      #margin
      plot.margin = margin(0,0,0,0),
    )
} 

annotation_size <- 3.5

# Sapelo
ext <- ext(flux_sq)
# AGB
flux_agb_pix_trend_map <- ggplot(data = flux_ag_pix_trend) +
  #geom_sf(data = flux_coast, fill = bg_color, linewidth = 0) +
  geom_sf(data = flux_rivers, fill = "lightgray", linewidth = 0, color = "lightgray") +
  geom_raster(aes(x = x, y = y, fill = annual_trend )) + 
  coord_sf(xlim = c(xmin(ext), xmax(ext)), ylim = c(ymin(ext), ymax(ext))) + 
  scale_x_continuous(breaks = c(-81.29, -81.27),) +
  scale_y_continuous(breaks = c(31.44, 31.46)) +
  scale_fill_gradientn(colors = c(paletteer_c("ggthemes::Red-Blue Diverging", 30)[1:13], paletteer_c("ggthemes::Red-Blue Diverging", 30)[18:30]) , na.value = "transparent", limits = c(-10,10), labels = percent(seq(-0.1,0.1,0.05))) +
  labs(fill = "", x = "", y = "", subtitle = "Sapelo", title = "Aboveground Biomass\nAnnual Trend") +
  annotate(geom = 'text', label = paste0("bar(x) == ",  format(round(global(flux_ag_pix_trend, mean, na.rm = T),2), nsmall =2), "*\'%'"),
           x = ext[1], y = ext[4] - 40, vjust = 1, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  map_theme()   
# BGB
flux_bgb_pix_trend_map <- ggplot(data = flux_bg_pix_trend) +
  #geom_sf(data = flux_coast, fill = bg_color, lwd = 0) +
  geom_sf(data = flux_rivers, fill = "lightgray", linewidth = 0, color = "lightgray") +
  geom_raster(aes(x = x, y = y, fill = annual_trend )) + 
  coord_sf(xlim = c(xmin(ext), xmax(ext)), ylim = c(ymin(ext), ymax(ext))) + 
  scale_x_continuous(breaks = c(-81.29, -81.27),) +
  scale_y_continuous(breaks = c(31.44, 31.46)) +
  #scale_x_continuous(limits = c(xmin(ext), xmax(ext))) +
  #scale_y_continuous(limits = c(ymin(ext), ymax(ext))) +
  scale_fill_gradientn(colors = c(paletteer_c("ggthemes::Red-Blue Diverging", 30)[1:13], paletteer_c("ggthemes::Red-Blue Diverging", 30)[18:30]) , na.value = "transparent", limits = c(-10,10), labels = percent(seq(-0.1,0.1,0.05))) +
  labs(fill = "", x = "", y = "", subtitle = "Sapelo", title = "Belowground Biomass\nAnnual Trend") +
  annotate(geom = 'text', label = paste0("bar(x) == ",  format(round(global(flux_bg_pix_trend, mean, na.rm = T),2), nsmall =2), "*\'%'"),
           x = ext[1], y = ext[4] - 40, vjust = 1, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  map_theme()   
# Skidaway
ext <- ext(skida_sq)
# AGB
skida_agb_pix_trend_map <- ggplot(data = skida_ag_pix_trend) +
  #geom_sf(data = skida_coast, fill = bg_color, lwd = 0) +
  geom_sf(data = skida_rivers, fill = "lightgray", linewidth = 0, color = "lightgray") +
  geom_raster(aes(x = x, y = y, fill = annual_trend )) + 
  coord_sf(xlim = c(xmin(ext), xmax(ext)), ylim = c(ymin(ext), ymax(ext))) + 
  scale_x_continuous(breaks = c(-81.04, -81.03),) +
  scale_y_continuous(breaks = c(31.97, 31.98)) +
  scale_fill_gradientn(colors = c(paletteer_c("ggthemes::Red-Blue Diverging", 30)[1:13], paletteer_c("ggthemes::Red-Blue Diverging", 30)[18:30]) , na.value = "transparent", limits = c(-10,10), labels = percent(seq(-0.1,0.1,0.05))) +
  labs(fill = "", x = "", y = "", subtitle = "Skidaway") +
  annotate(geom = 'text', label = paste0("bar(x) == ",  format(round(global(skida_ag_pix_trend, mean, na.rm = T),2), nsmall =2), "*\'%'"),
           x = ext[1], y = ext[4] - 40, vjust = 1, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  map_theme()   
# BGB
skida_bgb_pix_trend_map <- ggplot(data = skida_bg_pix_trend) +
  #geom_sf(data = skida_coast, fill = bg_color, lwd = 0) +
  geom_sf(data = skida_rivers, fill = "lightgray", linewidth = 0, color = "lightgray") +
  geom_raster(aes(x = x, y = y, fill = annual_trend )) + 
  coord_sf(xlim = c(xmin(ext), xmax(ext)), ylim = c(ymin(ext), ymax(ext))) + 
  scale_x_continuous(breaks = c(-81.04, -81.03),) +
  scale_y_continuous(breaks = c(31.97, 31.98)) +
  scale_fill_gradientn(colors = c(paletteer_c("ggthemes::Red-Blue Diverging", 30)[1:13], paletteer_c("ggthemes::Red-Blue Diverging", 30)[18:30]) , na.value = "transparent", limits = c(-10,10), labels = percent(seq(-0.1,0.1,0.05))) +
  labs(fill = "", x = "", y = "", subtitle = "Skidaway",) +
  annotate(geom = 'text', label = paste0("bar(x) == ",  format(round(global(skida_bg_pix_trend, mean, na.rm = T),2), nsmall =2), "*\'%'"),
           x = ext[1], y = ext[4] - 40, vjust = 1, hjust = h - 0.02, family = "Helvetica", size = annotation_size, parse = T) +
  map_theme()   

library(patchwork)
marsh_unit_trends <- flux_agb_pix_trend_map + flux_bgb_pix_trend_map + skida_agb_pix_trend_map + skida_bgb_pix_trend_map +
  plot_layout(ncol = 2, guides = 'collect')  + plot_annotation(tag_levels = "a") & theme(legend.position = "bottom", plot.tag.location = "plot")
ggsave("figures/figure_s6.pdf", marsh_unit_trends, units = "cm", height = 13, width = 13)

#### Field AGB and BGB ####


#df <- read_csv("~/git/berm2/output/core_vegplot_combined.csv")
df <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/core_vegplot_combined.csv")
#df <- df %>%
#  filter(site == "fluxa")

plot_theme <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      axis.text = element_text(family = font, size = 6), 
      axis.title = element_text(family = font, size = 8), 
      axis.text.x = element_text(family = font, size = 6, angle = -45),
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "none",
      legend.text = element_text(family = font, size = 8),
      text = element_text(family = font),
      plot.margin = margin(1,10,1,10),
      #ggside.panel.grid = element_blank(),
      #ggside.panel.background = element_blank(),
      #ggside.axis.text = element_blank(),
      #ggside.axis.ticks = element_blank()
    )
} 

counts <- df %>%
  mutate(year = year(date)) %>%
  filter(year > 2013)
colSums(!is.na(counts))

field_trend <- df %>%
  mutate(year = year(date)) %>%
  filter(year > 2013) %>%
  mutate(mo_num = time_length(interval(as.Date("2013-12-15"), date),  unit = "month"))
ag_trend <- lm(agm2.allom ~ mo_num, data = field_trend)
ag_annual_trend <- ag_trend$coefficients[[2]] * 12 * 100 / mean(field_trend$agm2.allom, na.rm = T)
bg_trend <- lm(bgm2.core.stemscaled ~ mo_num, data = field_trend)
bg_annual_trend <- bg_trend$coefficients[[2]] * 12 * 100 / mean(field_trend$bgm2.core.stemscaled, na.rm = T)

annotation_size = 2.5
line_size = 2
point_size = 0.1
alp = 0.9

p_agb <- ggplot(data = df, aes(x = date, y = agm2.allom)) +
  geom_point(size = point_size) +
  geom_smooth(method = "lm", size = line_size,  col = alpha("#97ad3d", alpha = alp), se = F) +
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2023-07-31"))) +
  labs(x = "", y = parse(text=paste("Aboveground~Biomass~(g~m^-~2)"))) +
  annotate(geom = 'text', label = paste0("Annual Trend: ", format(round(ag_annual_trend, 2), nsmall = 2), "%"), x = as.Date("2016-12-31"), y = 1800,  family = "Georgia", size = annotation_size, hjust = 0.45) +
  plot_theme()

p_bgb <- ggplot(data = df, aes(x = date, y = bgm2.core.stemscaled)) +
  geom_point(size = point_size) +
  geom_smooth(method = "lm", size = line_size, col = alpha("#86486f", alpha = alp), se = F) +
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2023-07-31"))) +
  labs(x = "", y = parse(text=paste("Belowground~Biomass~(g~m^-~2)"))) +
  annotate(geom = 'text', label = paste0("Annual Trend: ", format(round(bg_annual_trend, 2), nsmall = 2), "%"), x = as.Date("2016-12-31"), y = 6200,  family = "Georgia", size = annotation_size, hjust = 0.42) +
  plot_theme()

skc <- df %>%
  filter(plot %in% c("sk7", "sk8", "sk9")) %>%
  group_by(date) %>%
  summarise(agb = mean(agm2.allom, na.rm = T), bgb = mean(bgm2.core.stemscaled, na.rm = T))

field_trend <- df %>%
  mutate(year = year(date)) %>%
  filter(year > 2013 & plot %in% c("m1", "m2", "m3")) %>%
  mutate(mo_num = time_length(interval(as.Date("2013-12-15"), date),  unit = "month"))
ag_trend <- lm(agm2.allom ~ mo_num, data = field_trend)
ag_annual_trend <- ag_trend$coefficients[[2]] * 12 * 100 / mean(field_trend$agm2.allom, na.rm = T)
bg_trend <- lm(bgm2.core.stemscaled ~ mo_num, data = field_trend)
bg_annual_trend <- bg_trend$coefficients[[2]] * 12 * 100 / mean(field_trend$bgm2.core.stemscaled, na.rm = T)


ag_trend <- lm(agb ~ mo_num, data = field_trend)
ag_annual_trend <- ag_trend$coefficients[[2]] * 12 * 100 / mean(fluxa$agb, na.rm = T)
bg_trend <- lm(bgb ~ mo_num, data = df)
bg_annual_trend <- bg_trend$coefficients[[2]] * 12 * 100 / mean(fluxa$bgb, na.rm = T)


df <- read_csv("data/fort_pulaski_monthly_mean_sea_level_NAVD_CO-OPS_8670870_wl_2013_2023.csv")
#df <- read_csv("data/CO-OPS_8670870_met_1984_2014.csv")
slr <- df %>%
  mutate(year = year(Date)) %>%
  filter(year > 2013) %>%
  mutate(mo_num = time_length(interval(as.Date("2013-12-15"), Date),  unit = "month"))
slr_trend <- lm(`MSL (m)` ~ mo_num, data = slr)
slr_annual_trend <- slr_trend$coefficients[[2]] * 12 * 1000 # from m/mo to mm/yr
slr_annual_trend <- format(round(slr_annual_trend, 2), nsmall = 2)
lb1 <- paste("mm~yr~^{-1}")

p_slr <- ggplot(data = df, aes(x = Date, y = `MSL (m)`)) +
  geom_point(size = point_size) +
  geom_smooth(method = "lm", size = line_size,  col = alpha("deepskyblue4", alpha = alp), se = F) +
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2023-07-31"))) +
  labs(x = "", y = "MSL (m NAVD88)") +
  annotate(geom = 'text', label = "SLR~rate~'='~12.43~mm~yr^-~1", parse = T, x = as.Date("2015-06-30"), y = 0.23,  family = "Georgia", size = annotation_size) +
  plot_theme()
#p_slr

p_field_biomass <-  (p_agb | p_bgb) / p_slr +
  plot_layout(ncol = 1, heights = c(2,1)) +
  plot_annotation(tag_levels = "a")  &   theme(plot.tag = element_text(size = 12), plot.tag.position  = c(.04, .98), legend.position = 'bottom')
ggsave("figures/figure_s7.tiff", p_field_biomass, width = 11, height = 11, units = "cm", dpi = 600)

fluxa_field_trends <- data.frame(ag_annual_trend, bg_annual_trend)
write_csv(fluxa_field_trends, file = "results/fluxa_field_trends.csv")

# Just Flux A
df <- read_csv("results/plot_level_calculations.csv")

counts <- df %>%
  mutate(year = year(date)) %>%
  filter(year > 2013)
colSums(!is.na(counts))

df <- df %>%
  filter(site == "fluxa")

plot_theme <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      axis.text = element_text(family = font, size = 10), 
      axis.title = element_text(family = font, size = 12), 
      axis.text.x = element_text(family = font, size = 10, angle = -45),
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "none",
      legend.text = element_text(family = font, size = 8),
      text = element_text(family = font),
      plot.margin = margin(1,10,1,10),
      #ggside.panel.grid = element_blank(),
      #ggside.panel.background = element_blank(),
      #ggside.axis.text = element_blank(),
      #ggside.axis.ticks = element_blank()
    )
} 

counts <- df %>%
  mutate(year = year(date)) %>%
  filter(year > 2013)
colSums(!is.na(counts))

field_trend <- df %>%
  mutate(year = year(date)) %>%
  filter(year > 2013) %>%
  mutate(mo_num = time_length(interval(as.Date("2013-12-15"), date),  unit = "month"))
ag_trend <- lm(agm2.allom ~ mo_num, data = field_trend)
ag_annual_trend <- ag_trend$coefficients[[2]] * 12 * 100 / mean(field_trend$agm2.allom, na.rm = T)
bg_trend <- lm(bgm2.core.stemscaled ~ mo_num, data = field_trend)
bg_annual_trend <- bg_trend$coefficients[[2]] * 12 * 100 / mean(field_trend$bgm2.core.stemscaled, na.rm = T)

annotation_size = 4.5
line_size = 1
point_size = 0.02
alp = 0.9

p_agb <- ggplot(data = df, aes(x = date, y = agm2.allom)) +
  geom_point(size = point_size, color = "grey60") +
  geom_smooth(method = "lm", size = line_size,  col = alpha("#7D904A", alpha = alp), se = T, fill = "#c1cd9f", lineend = "square") +
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2023-07-31"))) +
  labs(x = "", y = parse(text=paste("Aboveground~Biomass~(g~m^-~2)"))) +
  annotate(geom = 'text', label = paste0("Annual Trend: ", format(round(ag_annual_trend, 2), nsmall = 2), "%"), x = as.Date("2017-03-31"), y = 1657,  family = "Helvetica", size = annotation_size, hjust = 0.45) +
  plot_theme()

point_size <-  0.1

p_bgb <- ggplot(data = df, aes(x = date, y = bgm2.core.stemscaled)) +
  geom_point(size = point_size, color = "grey60") +
  geom_smooth(method = "lm", size = line_size, col = alpha("#6f4a2d", alpha = alp), se = T, fill = "#cca281", lineend = "square") +
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2023-07-31"))) +
  labs(x = "", y = parse(text=paste("Belowground~Biomass~(g~m^-~2)"))) +
  annotate(geom = 'text', label = paste0("Annual Trend: ", format(round(bg_annual_trend, 2), nsmall = 2), "%"), x = as.Date("2017-03-31"), y = 5700,  family = "Helvetica", size = annotation_size, hjust = 0.42) +
  plot_theme()

slr <- read_csv("data/fort_pulaski_monthly_mean_sea_level_NAVD_CO-OPS_8670870_wl_2013_2023.csv")
#df <- read_csv("data/CO-OPS_8670870_met_1984_2014.csv")
slr <- slr %>%
  mutate(year = year(Date)) %>%
  filter(year > 2013) %>%
  mutate(mo_num = time_length(interval(as.Date("2013-12-15"), Date),  unit = "month"))
slr_trend <- lm(`MSL (m)` ~ mo_num, data = slr)
slr_annual_trend <- slr_trend$coefficients[[2]] * 12 * 1000 # from m/mo to mm/yr
slr_annual_trend <- format(round(slr_annual_trend, 2), nsmall = 2)
lb1 <- paste("mm~yr~^{-1}")
point_size <- 0.75

p_slr <- ggplot(data = slr, aes(x = Date, y = `MSL (m)`)) +
  geom_point(size = point_size, color = "grey60") +
  geom_smooth(method = "lm", size = line_size,  col = alpha("#00688b", alpha = alp), se = T, fill = "#45d0ff", lineend = "square") +
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2023-07-31"))) +
  labs(x = "", y = "MSL (m NAVD88)") +
  annotate(geom = 'text', label = "SLR~rate~'='~12.43~mm~yr^-~1", parse = T, x = as.Date("2015-09-30"), y = 0.23,  family = "Helvetica", size = annotation_size) +
  plot_theme() +
  theme(axis.text.x = element_text(family = "Helvetica", size = 10, angle = 0))
#_slr

p_field_biomass <-  (p_agb | p_bgb) / p_slr +
  plot_layout(ncol = 1, heights = c(2,1)) +
  plot_annotation(tag_levels = "a")  &   theme(plot.tag = element_text(size = 12), plot.tag.position  = c(.04, .98), legend.position = 'bottom')
ggsave("figures/figure_s7.pdf", p_field_biomass, width = 18, height = 18, units = "cm", dpi = 900)

fluxa_field_trends <- data.frame(ag_annual_trend, bg_annual_trend)
write_csv(fluxa_field_trends, file = "results/fluxa_field_trends.csv")

## AGB and BGB measurements

df <- read_csv("~/git/berm2/output/core_vegplot_combined.csv")

plot_theme <- function(){ 
  font <- "Georgia"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 12),
      axis.text = element_text(family = font, size = 8, color = "black"),
      axis.title = element_text(family = font, size = 10), 
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "none",
      text = element_text(family = font),
    )
} 


line_size = 2
point_size = 0.1
alp = 0.5

p_agb <- ggplot(data = df, aes(x = date, y = agm2.allom)) +
  geom_point(size = point_size, col = "#7D904A", alpha = alp) +
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2023-07-31"))) +
  labs(x = "", y = parse(text=paste("Biomass~(g~m^-~2)")), title = "Aboveground") +
  plot_theme() +
  theme(plot.title = element_text(family = "Georgia", size = 12, color = "#7D904A", hjust = 0.5, face = "bold"))

p_bgb <- ggplot(data = df, aes(x = date, y = bgm2.core.stemscaled)) +
  geom_point(size = point_size, col = "#6f4a2d") +
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2023-07-31"))) +
  labs(x = "", y = "", title = "Belowground") +
  plot_theme() +
  theme(plot.title = element_text(family = "Georgia", size = 12, color = "#6f4a2d", hjust = 0.5, face = "bold"))

field_biomass_measurements <- p_agb + p_bgb + plot_layout(axes = "collect", ncol = 2)
ggsave("results/field_biomass_measurements.png", field_biomass_measurements, units = "in", width = 6, height = 3)
## SLR for presentation

plot_theme <- function(){ 
  font <- "Arial"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      axis.text = element_text(family = font, size = 6), 
      axis.title = element_text(family = font, size = 8), 
      axis.text.x = element_text(family = font, size = 6, angle = 0),
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "none",
      legend.text = element_text(family = font, size = 8),
      text = element_text(family = font),
      plot.margin = margin(1,10,1,10),
      #ggside.panel.grid = element_blank(),
      #ggside.panel.background = element_blank(),
      #ggside.axis.text = element_blank(),
      #ggside.axis.ticks = element_blank()
    )
} 

p_slr <- ggplot(data = df, aes(x = Date, y = `MSL (m)`)) +
  geom_point(size = point_size) +
  geom_smooth(method = "lm", size = line_size,  col = alpha("deepskyblue4", alpha = alp), se = F) +
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2023-07-31"))) +
  labs(x = "", y = "MSL (m NAVD88)") +
  annotate(geom = 'text', label = "SLR~rate~'='~12.43~mm~yr^-~1", parse = T, x = as.Date("2016-06-30"), y = 0.22,  family = "Arial", size = annotation_size) +
  plot_theme()
p_slr
ggsave("results/plot_slr.png", p_slr, width = 3, height = 2, dpi = 300, units = "in")

#### AGB and BGB TS and GAMs with Inundation Intensity ####


# RS, AGB, and BGB by month
#df1 <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/rs/rs_by_month_mean.csv")
df1 <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/date_mean_sd/df.csv")

# Decompose TS
#AGB
ts_agb<-ts(df1$agb_mean, frequency=12, start=c(2014, 1))
ts_agb
plot.ts(ts_agb, ylim = c(200, 250))
#decompose
dcom_agb <- decompose(ts_agb)
dcom_agb
plot(dcom_agb)
agb_seasonallyadjusted <- ts_agb - dcom_agb$seasonal
plot.ts(agb_seasonallyadjusted, ylim = c(200, 250))
agb_mean_seasonallyadjusted_df <- data.frame(Y=as.matrix(agb_seasonallyadjusted), date= time(agb_seasonallyadjusted))
agb_mean_seasonallyadjusted_lm <- lm(Y ~ date, data = agb_mean_seasonallyadjusted_df)
summary(agb_mean_seasonallyadjusted_lm)
#SD
ts_agb<-ts(df1$agb_sd, frequency=12, start=c(2014, 1))
ts_agb
plot.ts(ts_agb, ylim = c(0,30))
#decompose
dcom_agb <- decompose(ts_agb)
dcom_agb
plot(dcom_agb)
agb_seasonallyadjusted <- ts_agb - dcom_agb$seasonal
plot.ts(agb_seasonallyadjusted, ylim = c(0, 30))
agb_sd_seasonallyadjusted_df <- data.frame(Y=as.matrix(agb_seasonallyadjusted), date=time(agb_seasonallyadjusted))
agb_df_seasonallyadjusted_lm <- lm(Y ~ date, data = agb_sd_seasonallyadjusted_df)
summary(agb_df_seasonallyadjusted_lm)

#BGB
ts_bgb<-ts(df1$bgb_mean, frequency=12, start=c(2014, 1))
ts_bgb
plot.ts(ts_bgb, ylim = c(700, 1000))
#decompose
dcom_bgb <- decompose(ts_bgb)
dcom_bgb
plot(dcom_bgb)
bgb_seasonallyadjusted <- ts_bgb - dcom_bgb$seasonal
plot.ts(bgb_seasonallyadjusted, ylim = c(700, 1000))
bgb_mean_seasonallyadjusted_df <- data.frame(Y=as.matrix(bgb_seasonallyadjusted), date= time(bgb_seasonallyadjusted))
bgb_mean_seasonallyadjusted_lm <- lm(Y ~ date, data = bgb_mean_seasonallyadjusted_df)
summary(bgb_mean_seasonallyadjusted_lm)
#SD
ts_bgb<-ts(df1$bgb_sd, frequency=12, start=c(2014, 1))
ts_bgb
plot.ts(ts_bgb, ylim = c(100,400))
#decompose
dcom_bgb <- decompose(ts_bgb)
dcom_bgb
plot(dcom_bgb)
bgb_seasonallyadjusted <- ts_bgb - dcom_bgb$seasonal
plot.ts(bgb_seasonallyadjusted, ylim = c(100, 400))
bgb_sd_seasonallyadjusted_df <- data.frame(Y=as.matrix(bgb_seasonallyadjusted), date=time(bgb_seasonallyadjusted))
bgb_df_seasonallyadjusted_lm <- lm(Y ~ date, data = bgb_sd_seasonallyadjusted_df)
summary(bgb_df_seasonallyadjusted_lm)


agb_time_lm <- lm(agb_mean ~ date, data = df1)
summary(agb_time_lm)

bgb_time_lm <- lm(bgb_mean ~ date, data = df1)
summary(bgb_time_lm)


agb_mean_seasonallyadjusted_df$date  <- as.Date(format(date_decimal(as.numeric(agb_mean_seasonallyadjusted_df$date) + 0.01), "%Y-%m-15")) # +0.01 fixes an issue where it thought Feb was Jan
agb_mean_seasonallyadjusted_df$ymin <- agb_mean_seasonallyadjusted_df$Y - agb_sd_seasonallyadjusted_df$Y
agb_mean_seasonallyadjusted_df$ymax <- agb_mean_seasonallyadjusted_df$Y + agb_sd_seasonallyadjusted_df$Y

bgb_mean_seasonallyadjusted_df$date  <- as.Date(format(date_decimal(as.numeric(bgb_mean_seasonallyadjusted_df$date) + 0.01), "%Y-%m-15")) # +0.01 fixes an issue where it thought Feb was Jan
bgb_mean_seasonallyadjusted_df$ymin <- bgb_mean_seasonallyadjusted_df$Y - bgb_sd_seasonallyadjusted_df$Y
bgb_mean_seasonallyadjusted_df$ymax <- bgb_mean_seasonallyadjusted_df$Y + bgb_sd_seasonallyadjusted_df$Y

agb_mean_seasonallyadjusted_lm$coefficients

agb_sa_lm_confidence_intervals <- as.data.frame(predict(agb_mean_seasonallyadjusted_lm, interval = "confidence", level = 0.95))
max(abs(max(agb_sa_lm_confidence_intervals$fit - agb_sa_lm_confidence_intervals$lwr, agb_sa_lm_confidence_intervals$fit - agb_sa_lm_confidence_intervals$upr)))

bgb_sa_lm_confidence_intervals <- as.data.frame(predict(bgb_mean_seasonallyadjusted_lm, interval = "confidence", level = 0.95))
max(abs(max(bgb_sa_lm_confidence_intervals$fit - bgb_sa_lm_confidence_intervals$lwr, bgb_sa_lm_confidence_intervals$fit - bgb_sa_lm_confidence_intervals$upr)))


plot_theme <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 11, hjust = 0.5),
      axis.text = element_text(family = font, size = 7, color = "black"), 
      axis.text.x = element_text(family = font, size = 7, hjust = 0.5, angle = 0), 
      axis.title = element_text(family = font, size = 9), 
      axis.title.x = element_text(size = 0),
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "top",
      legend.text = element_text(family = font, size = 7),
      legend.box.margin = margin(1,1,1,1, unit = "pt"),
      text = element_text(family = font),
      plot.margin = margin(10,10,10,10),
      #ggside.panel.grid = element_blank(),
      #ggside.panel.background = element_blank(),
      #ggside.axis.text = element_blank(),
      #ggside.axis.ticks = element_blank()
    )
} 


agb_time <- ggplot(data = agb_mean_seasonallyadjusted_df, aes(x = date, y = Y)) +
  geom_ribbon(aes(x = date, ymin = ymin, ymax = ymax), fill = "lightgray", alpha = 0.5) +
  geom_line(linewidth = 0.25, color = "black") +
  geom_smooth(data = agb_mean_seasonallyadjusted_df, aes(x = date, y = Y, color = "#7D904A"), method = "lm", se = F, size = 0.75, fill = "#c1cd9f", show.legend = F) +
  geom_text(label = parse(text=paste("1.3~g~m^-~2~yr^-~1")), x = as.Date("2021-03-15"), y = 185, family = "Helvetica", size = 3, check_overlap = TRUE) +
  scale_x_date(breaks= seq(min(agb_mean_seasonallyadjusted_df$date), max(agb_mean_seasonallyadjusted_df$date), length=3),  date_labels="%Y") +
  scale_y_continuous(limits = c(175,275), breaks = c(175, 225, 275)) +
  labs(x = "", y = parse(text=paste("Biomass~(g~m^-~2)")), color = "", title = "Aboveground") +
  plot_theme() + 
  scale_color_manual(values =  "#7D904A", labels = "Aboveground") 
agb_time

bgb_time <- ggplot(data = bgb_mean_seasonallyadjusted_df, aes(x = date, y = Y)) +
  geom_ribbon(aes(x = date, ymin = ymin, ymax = ymax), fill = "lightgray", alpha = 0.5) +
  geom_line(linewidth = 0.25, color = "black") +
  geom_smooth(data = bgb_mean_seasonallyadjusted_df, aes(x = date, y = Y, color = "#6f4a2d"), method = "lm", se = F, size = 0.75, fill = "#cca281", show.legend = F) +
  geom_text(label = parse(text=paste("-~`5.0`~g~m^-~2~yr^-~1")), x = as.Date("2021-01-15"), y = 600, family = "Helvetica", size = 3, check_overlap = TRUE) +
  scale_x_date(breaks= seq(min(bgb_mean_seasonallyadjusted_df$date), max(bgb_mean_seasonallyadjusted_df$date), length=3),  date_labels="%Y", date_minor_breaks = '1 year') +
  scale_y_continuous(limits = c(460,1300), breaks = c(500,750,1000,1250)) +
  labs(x = "", y = parse(text=paste("Biomass~(g~m^-~2)")), color = "", title = "Belowground") +
  scale_color_manual(values =  "#6f4a2d", labels = "Belowground") +
  plot_theme () +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))
bgb_time

biomass <- (agb_time / plot_spacer() / bgb_time) + plot_layout(heights = c(5,0.5,5)) +
  plot_annotation(tag_levels = "a") & #, caption = "Seasonal Trend Removed") & 
  theme(plot.margin = margin(1,3,0,1), legend.margin = margin(0,0,-10,0), plot.tag = element_text(size = 10), plot.tag.location = "plot")#, plot.caption = element_text(family = "Georgia", size = 6, hjust = 0.5))
ggsave("figures/figure_3.pdf", biomass, height = 9, width = 6, units = "cm", dpi = 900)

## poster
plot_theme <- function(){ 
  font <- "Arial"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 0),
      axis.text = element_text(family = font, size = 7, color = "black"), 
      axis.text.x = element_text(family = font, size = 7, hjust = 0.5, angle = 0), 
      axis.title = element_text(family = font, size = 8), 
      axis.title.x = element_text(size = 0),
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "top",
      legend.text = element_text(family = font, size = 7),
      legend.box.margin = margin(1,1,1,1, unit = "pt"),
      text = element_text(family = font),
      plot.margin = margin(10,10,10,10),
    )
} 

agb_time <- ggplot(data = agb_seasonallyadjusted_df, aes(x = date, y = Y)) +
  #geom_ribbon(aes(x = date, ymin = agb_mean - agb_sd, ymax = agb_mean + agb_sd), fill = "lightgray", alpha = 0.5) +
  geom_line(linewidth = 0.25, color = "black") +
  geom_smooth(data = agb_seasonallyadjusted_df, aes(x = date, y = Y, color = "#7D904A"), method = "lm", se = F, size = 1.5) +
  geom_text(label = parse(text=paste("1.3~g~m^-~2~yr^-~1")), x = as.Date("2021-03-15"), y = 160, family = "Arial", size = 3, fontface = "plain") +
  scale_x_date(breaks= seq(min(agb_seasonallyadjusted_df$date), max(agb_seasonallyadjusted_df$date), length=3),  date_labels="%Y") +
  scale_y_continuous(limits = c(150,250), breaks = c(150, 200, 250)) +
  labs(x = "", y = parse(text=paste("Biomass~(g~m^-~2)")), color = "", title = "") +
  plot_theme() + 
  scale_color_manual(values =  "#7D904A", labels = "Aboveground") 
agb_time
ggsave("results/agb_time_poster.png", agb_time, width = 3, height = 2, units = "in", dpi = 300)
bgb_time <- ggplot(data = bgb_seasonallyadjusted_df, aes(x = date, y = Y)) +
  #geom_ribbon(aes(x = date, ymin = bgb_mean - bgb_sd, ymax = bgb_mean + bgb_sd), fill = "lightgray", alpha = 0.5) +
  geom_line(linewidth = 0.25, color = "black") +
  geom_smooth(data = bgb_seasonallyadjusted_df, aes(x = date, y = Y, color = "#6f4a2d"), method = "lm", se = F, size = 1.5) +
  geom_text(label = parse(text=paste("-~`5.0`~g~m^-~2~yr^-~1")), x = as.Date("2021-01-15"), y = 550, family = "Arial", size = 3, fontface = "plain") +
  scale_x_date(breaks= seq(min(bgb_seasonallyadjusted_df$date), max(bgb_seasonallyadjusted_df$date), length=3),  date_labels="%Y", date_minor_breaks = '1 year') +
  scale_y_continuous(limits = c(500,1000), breaks = c(500,750,1000)) +
  labs(x = "", y = parse(text=paste("Biomass~(g~m^-~2)")), color = "", title = "") +
  scale_color_manual(values =  "#6f4a2d", labels = "Belowground") +
  plot_theme ()
bgb_time
ggsave("results/bgb_time_poster.png", bgb_time, width = 3, height = 2, units = "in", dpi = 300)



#pix_summ
filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/pix_summ/", pattern = ".csv", full.names = T)
tables <- lapply(filenames, read.csv, header = TRUE)
pix_summ<- do.call(rbind , tables)
pix_summ <- pix_summ %>%
  mutate(ag_trend = coef_ag/predag.allom.l8*12*100, bg_trend = coef_bg/predbg.xgb*12*100)

#pix_elevation_flood
pix_elevation_flood <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/pix_attr/pix_attr.csv")

df <- merge(pix_summ, pix_elevation_flood, by = "pix")
df <- df %>%
  dplyr::select(pix, predbg.xgb, bg_trend, predag.allom.l8, ag_trend, elevation, flood_time, inund_inten)

# Inundation Intensity, Stocks
# AGB
gam_mean_ag <- gam(predag.allom.l8 ~ s(inund_inten, bs ="cs", k = 10), data = df)
summary(gam_mean_ag)
plot.gam(gam_mean_ag)
abline(v=0.0653, col="black")
abline(v=0.0776, col="red")
abline(v=0.0899, col="blue")

#gam.check(gam_mean_ag)
agb_r.sq <- summary(gam_mean_ag)$r.sq
new_data <- data.frame(inund_inten = seq(0, 1.608986, 0.001))
pred <- predict(gam_mean_ag, new_data, type = "response", se.fit = T)
new_data$predag.allom.l8 <- pred$fit
# BGB
gam_mean_bg <- gam(predbg.xgb ~ s(inund_inten, bs = "cs", k = 10), data = df)
summary(gam_mean_bg)
bgb_r.sq <- summary(gam_mean_bg)$r.sq
pred <- predict(gam_mean_bg, new_data, type = "response", se.fit = T)
new_data$predbg.xgb <- pred$fit

class(new_data$predag.allom.l8)
new_data$predag.allom.l8 <- as.numeric(new_data$predag.allom.l8)
class(new_data$predbg.xgb)
new_data$predbg.xgb <- as.numeric(new_data$predbg.xgb)

write_csv(new_data, "/media/kyle/Seagate Expansion Drive/data/results/gam_agb_bgb_ii.csv")
write_csv(df, "/media/kyle/Seagate Expansion Drive/data/results/ii_mean.csv")


line_size <- 0.4
ii_med_2014 <- 0.06537992
ii_med_2023 <- 0.07765544
ii_med_plus <- ii_med_2023 + (ii_med_2023 - ii_med_2014)

plot_theme <- function(){
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%	#replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),	#strip major gridlines
      panel.grid.minor = element_blank(),	#strip minor gridlines
      #text elements
      axis.text = element_text(family = font, size = 7, color = "black"),
      axis.title = element_text(family = font, size = 9, color = "black"),
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "none",
      text = element_text(family = font),
      plot.margin = margin(1,1,5,1),
      #ggside.panel.grid = element_blank(),
      #ggside.panel.background = element_blank(),
      #ggside.axis.text = element_blank(),
      #ggside.axis.ticks = element_blank()
    )
}

annotation <- data.frame(
  x = c(ii_med_2014, ii_med_2023, ii_med_plus),
  y = c(1125,1125, 1125),
  label = c("2014", "2023", ""),
  color = c("2014 Median II", "2023 Median II", "Further Increase"),
  lty = c("solid","dashed","dotted")
)
line_size_2 <- 0.39

# Biomass II plot
agb_stocks_ii <- ggplot(data = df, aes(x = inund_inten)) +
  geom_line(data = new_data, aes(x = inund_inten, y = predag.allom.l8), col =  "#7D904A", lwd = 2) +
  #geom_col(data = df_hist_ii, aes(x = bins, y = (hist_ii.counts.sum.hist_ii.counts.)/coef), col = "blue") +
  scale_x_continuous(limits = c(-0.0, 0.3)) +
  scale_y_continuous(breaks = c(210, 225, 240), limits = c(210, 240)) +
  #scale_y_continuous(breaks = c(200, 210, 220, 230, 240, 250, 500, 750, 1000, 1250, 1500), limits = c(200, 1500), sec.axis = sec_axis(~ .x * coef -0.2, labels = scales::percent)) +
  #scale_y_continuous(limits = c(0,1250)) +
  #scale_y_cut(breaks = c(249), scales = c(3,1), which = c(1,2), space = 0) +
  scale_color_manual(name = "", values = c("2014 Median II" = "black", "2023 Median II" = "red", "Further Increase" = "red")) +
  scale_linetype_manual(name = "", values = c("2014 Median II" = "solid", "2023 Median II" = "dashed", "Further Increase" = "dotted")) +
  scale_size_manual(name = "", values = c("2014 Median II" = line_size_2, "2023 Median II" = line_size_2, "Further Increase" = line_size_2)) +
  geom_vline(data = annotation, aes(xintercept = x, color = color, linetype = color, size = color)) +
  labs(x = "Inundation Intensity (II)", y = "") +#, y = parse(text=paste("Biomass~(g~m^-~2)"))) +
  plot_theme() +
  theme(
        plot.margin = margin(0,5.5,5.5,5.5), legend.position = "bottom", legend.text = element_text(family = "Helvetica", size = 8),  legend.key.width = unit(0.3, 'cm'), legend.spacing.x = unit(1, 'cm'), legend.justification = "left") +
  annotate(geom = "text", x = 0.28, y = 221, label = paste("R^2: ", sprintf("'%0.2f'", round(agb_r.sq, digits = 2))), parse=T, family = "Helvetica", size = 2.5) +
  guides(color = guide_legend(nrow = 1, ncol = 3))
agb_stocks_ii

annotation
head(new_data)

bgb_stocks_ii <- ggplot(data = df, aes(x = inund_inten)) +
  geom_line(data = new_data, aes(x = inund_inten, y = predag.allom.l8, col = "Aboveground"), lwd = 2) +
  geom_line(data = new_data, aes(x = inund_inten, y = predbg.xgb, col = "Belowground"), lwd = 2) +
  annotate("segment", x=ii_med_2014 + 0.001, y=1080, xend=ii_med_2023 - 0.001, yend=1080, color = "red", size = 0.5, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x= 0.02, xend=annotation$x[1], y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[1],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[1],3)], color = "black", linetype = 2) +
  annotate("segment", x= 0.02, xend=annotation$x[2], y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)], color = "black", linetype = 2) +
  annotate("segment", x= 0.022, xend=0.022, y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[1],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)], color = "black", linetype = 2) +
  annotate("text", label = "8% decline", x= 0.015, y= 875, color = "black", family = "Helvetica") +
  annotate("segment", x= annotation$x[2], xend=0.15, y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)], color = "red", linetype = 2) +
  annotate("segment", x= annotation$x[3], xend=0.15, y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[3],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[3],3)], color = "red", linetype = 2) +
  annotate("segment", x= 0.148, xend=0.148, y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[3],3)], color = "red", linetype = 2) +
  annotate("text", label = "9% decline", x= 0.165, y= 675, color = "red", family = "Helvetica") +
  scale_x_continuous(limits = c(-0.0, 0.3)) +
  scale_y_continuous(breaks = c(500, 750, 1000, 1250), limits = c(250, 1250)) +
  labs(x = "", y = parse(text=paste("Biomass~(g~m^-~2)"))) +
  scale_color_manual(name = "", values = c("Aboveground" = "#7D904A", "Belowground" = "#6f4a2d")) +
  geom_vline(data = annotation, aes(xintercept = x), color = c("black", "red", "red"), linetype = c(1,2,3), size = line_size_2) +
  plot_theme() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.length.x=unit(0,'lines'),
        plot.margin = margin(5.5,5.5,0,5.5),
        axis.title.y=element_text(hjust=0.2),
        legend.position = "top") +
  annotate(geom = "text", x = 0.28, y = 450, label = paste("R^2: ", round(bgb_r.sq,2),sep=""), parse=T, family = "Helvetica", size = 2.5)
bgb_stocks_ii

###

bgb_stocks_ii <- ggplot(data = df, aes(x = inund_inten)) +
  geom_line(data = new_data, aes(x = inund_inten, y = predag.allom.l8, col = "Aboveground"), lwd = 2) +
  geom_line(data = new_data, aes(x = inund_inten, y = predbg.xgb, col = "Belowground"), lwd = 2) +
  annotate("segment", x=ii_med_2014 + 0.001, y=1080, xend=ii_med_2023 - 0.001, yend=1080, color = "red", size = 0.5, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x= 0.00, xend=annotation$x[1], y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[1],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[1],3)], color = "black", linetype = 1) +
  annotate("segment", x= 0.00, xend=annotation$x[2], y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)], color = "red", linetype = 2) +
  annotate("text", label = "8% decline", x= 0.025, y= 807, color = "black", family = "Helvetica", size = 2.5) +
  annotate("segment", x = 0, xend= annotation$x[3], y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[3],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[3],3)], color = "red", linetype = 3) +
  annotate("text", label = "9% decline", x= 0.025, y= 742, color = "red", family = "Helvetica", size = 2.5) +
  scale_x_continuous(limits = c(-0.0, 0.3)) +
  scale_y_continuous(breaks = c(500, 750, 1000, 1250), limits = c(250, 1250)) +
  labs(x = "", y = parse(text=paste("Biomass~(g~m^-~2)"))) +
  scale_color_manual(name = "", values = c("Aboveground" = "#7D904A", "Belowground" = "#6f4a2d")) +
  geom_vline(data = annotation, aes(xintercept = x), color = c("black", "red", "red"), linetype = c(1,2,3), size = line_size_2) +
  plot_theme() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.length.x=unit(0,'lines'),
        plot.margin = margin(5.5,5.5,0,5.5),
        axis.title.y=element_text(hjust=0.2),
        legend.position = "top") +
  annotate(geom = "text", x = 0.28, y = 450, label = paste("R^2: ", round(bgb_r.sq,2),sep=""), parse=T, family = "Helvetica", size = 2.5)
bgb_stocks_ii
  

ii_biomass <- bgb_stocks_ii / agb_stocks_ii  + 
  plot_layout(heights = c(4,1)) +
  plot_annotation() & theme(plot.margin = margin(0,0,0,0), legend.margin = margin(0,0,1,0), plot.tag = element_text(size = 8), plot.tag.location = "plot")
ggsave("figures/figure_4.pdf", ii_biomass, height = 11, width = 11, units = "cm", dpi = 900)

## poster

line_size <- 0.4
ii_med_2014 <- 0.06537992
ii_med_2023 <- 0.07765544
ii_med_plus <- ii_med_2023 + (ii_med_2023 - ii_med_2014)

plot_theme <- function(){
  font <- "Arial"   #assign font family up front
  theme_hc() %+replace%	#replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),	#strip major gridlines
      panel.grid.minor = element_blank(),	#strip minor gridlines
      #text elements
      axis.text = element_text(family = font, size = 7, color = "black"),
      axis.title = element_text(family = font, size = 9, color = "black"),
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "none",
      text = element_text(family = font),
      plot.margin = margin(1,1,5,1),
      #ggside.panel.grid = element_blank(),
      #ggside.panel.background = element_blank(),
      #ggside.axis.text = element_blank(),
      #ggside.axis.ticks = element_blank()
    )
}

annotation <- data.frame(
  x = c(ii_med_2014, ii_med_2023, ii_med_plus),
  y = c(1125,1125, 1125),
  label = c("2014", "2023", ""),
  color = c("2014 Median", "2023 Median", "Further Increase"),
  lty = c("solid","dashed","dotted")
)
line_size_2 <- 0.39

# Biomass II plot
agb_stocks_ii <- ggplot(data = df, aes(x = inund_inten)) +
  geom_line(data = new_data, aes(x = inund_inten, y = predag.allom.l8), col =  "#7D904A", lwd = 2) +
  #geom_col(data = df_hist_ii, aes(x = bins, y = (hist_ii.counts.sum.hist_ii.counts.)/coef), col = "blue") +
  scale_x_continuous(limits = c(-0.0, 0.3)) +
  scale_y_continuous(breaks = c(210, 225, 240), limits = c(210, 240)) +
  #scale_y_continuous(breaks = c(200, 210, 220, 230, 240, 250, 500, 750, 1000, 1250, 1500), limits = c(200, 1500), sec.axis = sec_axis(~ .x * coef -0.2, labels = scales::percent)) +
  #scale_y_continuous(limits = c(0,1250)) +
  #scale_y_cut(breaks = c(249), scales = c(3,1), which = c(1,2), space = 0) +
  scale_color_manual(name = "", values = c("2014 Median" = "black", "2023 Median" = "red", "Further Increase" = "red")) +
  scale_linetype_manual(name = "", values = c("2014 Median" = "solid", "2023 Median" = "dashed", "Further Increase" = "dotted")) +
  scale_size_manual(name = "", values = c("2014 Median" = line_size_2, "2023 Median" = line_size_2, "Further Increase" = line_size_2)) +
  geom_vline(data = annotation, aes(xintercept = x, color = color, linetype = color, size = color)) +
  labs(x = "Inundation Intensity", y = "") +#, y = parse(text=paste("Biomass~(g~m^-~2)"))) +
  plot_theme() +
  theme(
    plot.margin = margin(0,5.5,5.5,5.5), legend.position = "bottom", legend.text = element_text(family = "Arial", size = 8),  legend.key.width = unit(0.3, 'cm'), legend.spacing.x = unit(1, 'cm'), legend.justification = "left") +
  annotate(geom = "text", x = 0.28, y = 221, label = paste("R^2: ", sprintf("'%0.2f'", round(agb_r.sq, digits = 2))), parse=T, family = "Arial", size = 2.5) +
  guides(color = guide_legend(nrow = 1, ncol = 3))
agb_stocks_ii

annotation
head(new_data)

bgb_stocks_ii <- ggplot(data = df, aes(x = inund_inten)) +
  geom_line(data = new_data, aes(x = inund_inten, y = predag.allom.l8, col = "Aboveground"), lwd = 2) +
  geom_line(data = new_data, aes(x = inund_inten, y = predbg.xgb, col = "Belowground"), lwd = 2) +
  #geom_segment(aes(x=0.095, y=1125, xend=0.105, yend=1125), arrow = arrow(length=unit(.5, 'cm')), color='red', lwd=1) +
  annotate("segment", x=ii_med_2014 + 0.001, y=1080, xend=ii_med_2023 - 0.001, yend=1080, color = "red", size = 0.5, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x= 0.00, xend=annotation$x[1], y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[1],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[1],3)], color = "black", linetype = 1) +
  annotate("segment", x= 0.00, xend=annotation$x[2], y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)], color = "red", linetype = 2) +
  annotate("text", label = "8% decline", x= 0.025, y= 807, color = "black", family = "Arial", size = 2) +
  #annotate("segment", x = 0, xend= annotation$x[2], y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[2],3)], color = "red", linetype = 2) +
  annotate("segment", x = 0, xend= annotation$x[3], y= new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[3],3)],  yend=new_data$predbg.xgb[new_data$inund_inten == round(annotation$x[3],3)], color = "red", linetype = 3) +
  annotate("text", label = "9% decline", x= 0.025, y= 742, color = "red", family = "Arial", size = 2) +
  #geom_text(data=annotation, aes( x=x, y=y, label=label), color=c("black", "red"), size=1.25 , angle=0, fontface="bold", family = "Georgia" ) +  
  scale_x_continuous(limits = c(-0.0, 0.3)) +
  scale_y_continuous(breaks = c(500, 750, 1000, 1250), limits = c(250, 1250)) +
  #scale_y_continuous(breaks = c(200, 210, 220, 230, 240, 250, 500, 750, 1000, 1250, 1500), limits = c(200, 1500), sec.axis = sec_axis(~ .x * coef -0.2, labels = scales::percent)) +
  #scale_y_continuous(limits = c(0,1250)) +
  #scale_y_cut(breaks = c(249), scales = c(3,1), which = c(1,2), space = 0) +
  labs(x = "", y = parse(text=paste("Biomass~(g~m^-~2)"))) +
  #new_scale_color() + 
  scale_color_manual(name = "", values = c("Aboveground" = "#7D904A", "Belowground" = "#6f4a2d")) +
  geom_vline(data = annotation, aes(xintercept = x), color = c("black", "red", "red"), linetype = c(1,2,3), size = line_size_2) +
  #scale_linetype_manual(values = c(1,3,2)) +
  plot_theme() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.length.x=unit(0,'lines'),
        plot.margin = margin(5.5,5.5,0,5.5),
        axis.title.y=element_text(hjust=0.2),
        legend.position = "top") +
  annotate(geom = "text", x = 0.28, y = 450, label = paste("R^2: ", round(bgb_r.sq,2),sep=""), parse=T, family = "Arial", size = 2.5)
bgb_stocks_ii


ii_biomass <- bgb_stocks_ii / agb_stocks_ii  + 
  plot_layout(heights = c(4,1)) +
  plot_annotation() & theme(plot.margin = margin(0,0,0,0), legend.margin = margin(0,0,1,0), plot.tag = element_text(size = 8), plot.tag.location = "plot")
ggsave("results/agb_bgb_ii.png", ii_biomass, height = 11, width = 11, units = "cm", dpi = 600)


#cumulative ii

df <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/ii_mean.csv")

c_ii <- ggplot(data = df, aes(x = inund_inten)) +
  geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.01, col = "white") +
  stat_bin(aes(y = after_stat(cumsum(count/sum(count)/4))), geom = "line", col = "blue", size = 2) +
  #scale_x_continuous(labels = scales::percent(seq(0,1,0.1)), breaks = seq(0,1,0.1)) +
  scale_y_continuous(limits = c(0,0.25), sec.axis = sec_axis(~ .x * 4, name = "Cumulative Proportion")) +
  labs(x = "Inundation Intesity", y = "Proportion") +
  theme(
    #grid elements
    panel.grid.major = element_blank(),    #strip gridlines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linetype = 1, color = "black", size = 1),
    axis.line.y.right = element_line(color = "blue"),            
    axis.ticks.y.right = element_line(color = "blue"),
    #text elements
    axis.title = element_text(family = "Georgia", size = 16),
    axis.title.y.right = element_text(family = "Georgia", size = 16, color = "blue"),
    axis.text = element_text(family = "Georgia", size = 12),
    axis.text.y.right = element_text(family = "Georgia", size = 12, color = "blue"),
    panel.background = element_rect(fill = "white", color = "white"),
    text = element_text(family = "Georgia"),
    plot.margin = margin(1,1,1,1)
  )
c_ii
#ggsave("results/c_flood.jpg", c_flood, width = 7.5, height = 5)

#### Predictor Time Series ####

dat <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/pix_attr/predictor_attr_ts2.csv")

plot_theme <- function(){
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%	#replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),	#strip major gridlines
      panel.grid.minor = element_blank(),	#strip minor gridlines
      #text elements
      plot.title = element_text(family = font, hjust = 0.5, size = 10),
      axis.text = element_text(family = font, size = 8),
      axis.title = element_text(family = font, size = 9),
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "none",
      text = element_text(family = font),
      plot.margin = margin(1,1,5,1),
      #ggside.panel.grid = element_blank(),
      #ggside.panel.background = element_blank(),
      #ggside.axis.text = element_blank(),
      #ggside.axis.ticks = element_blank()
    )
}

line_color <- "black"

ts_ii <- ggplot(data = dat, aes(x = date, y = local_hiwater_mean_mean)) +
  geom_smooth(aes(ymin = local_hiwater_mean_mean - 100, ymax = local_hiwater_mean_mean + local_hiwater_var_mean), color = line_color, 
              method = "gam", formula = y ~ s(x, bs = "cs", k = 8)) +
  labs(x = "", y = "Inundation Intensity") +
  plot_theme()
ts_di <- ggplot(data = dat, aes(x = date, y = local_lowater_mean_mean)) +
  geom_smooth(aes(ymin = local_lowater_mean_mean - local_lowater_var_mean, ymax = local_lowater_mean_mean + local_lowater_var_mean), color = line_color,
              method = "gam", formula = y ~ s(x, bs = "cs", k = 8)) +
  labs(x = "", y = "Dry Intensity") +
  plot_theme()
ts_gdoy <- ggplot(data = dat, aes(x = date, y = greendoy_mean_mean)) +
  geom_smooth(aes(ymin = greendoy_mean_mean - greendoy_var_mean, ymax = greendoy_mean_mean + greendoy_var_mean), color = line_color, 
              method = "gam", formula = y ~ s(x, bs = "cs", k = 4)) +
  labs(x = "", y = "Greenup Day of Year") +
  plot_theme()
ts_lai <- ggplot(data = dat, aes(x = date, y = predlai.l8_mean_mean)) +
  geom_smooth(aes(ymin = predlai.l8_mean_mean - predlai.l8_var_mean, ymax = predlai.l8_mean_mean + predlai.l8_var_mean), color = line_color, 
              method = "gam", formula = y ~ s(x, bs = "cs", k = 8)) +
  labs(x = "", y = "Pred. Leaf Area Index") +
  plot_theme()
ts_ndvi <- ggplot(data = dat, aes(x = date, y = ndvi_mean_mean)) +
  geom_smooth(aes(ymin = ndvi_mean_mean - ndvi_var_mean, ymax = ndvi_mean_mean + ndvi_var_mean), color = line_color, 
              method = "gam", formula = y ~ s(x, bs = "cs", k = 8)) +
  labs(x = "", y = "NDVI") +
  plot_theme()
ts_n <- ggplot(data = dat, aes(x = date, y = predn.l8_mean_mean)) +
  geom_smooth(aes(ymin = predn.l8_mean_mean - predn.l8_var_mean, ymax = predn.l8_mean_mean + predn.l8_var_mean), color = line_color, 
              method = "gam", formula = y ~ s(x, bs = "cs", k = 8)) +
  labs(x = "", y = "Foliar N (%)") +
  plot_theme()
ts_agb <- ggplot(data = dat, aes(x = date, y = predag.allom.l8_mean_mean)) +
  geom_smooth(aes(ymin = predag.allom.l8_mean_mean - predag.allom.l8_var_mean, ymax = predag.allom.l8_mean_mean + predag.allom.l8_var_mean), color = line_color, 
              method = "gam", formula = y ~ s(x, bs = "cs", k = 10)) +
  labs(x = "", y = parse(text=paste("Pred.~Aboveground~Biomass~(g~m^-~2)"))) +
  plot_theme()
ts_chl <- ggplot(data = dat, aes(x = date, y = predchl.l8_mean_mean)) +
  geom_smooth(aes(ymin = predchl.l8_mean_mean - predchl.l8_var_mean, ymax = predchl.l8_mean_mean + predchl.l8_var_mean), color = line_color, 
              method = "gam", formula = y ~ s(x, bs = "cs", k = 10)) +
  labs(x = "", y = parse(text=paste("Pred.~Foliar~Chlorophyll~(mg~g^-~1)"))) +
  plot_theme()
ts_lst <- ggplot(data = dat, aes(x = date, y = lst_mean_mean)) +
  geom_smooth(aes(ymin = lst_mean_mean - lst_var_mean, ymax = lst_mean_mean + lst_var_mean), color = line_color, 
              method = "gam", formula = y ~ s(x, bs = "cs", k = 8)) +
  labs(x = "", y = "Land Surface Temperature (\u00B0C)") +
  plot_theme()
ts_lst
ts_plots <- ts_ii + ts_di + ts_gdoy + ts_lai + ts_ndvi + ts_n + ts_agb + ts_chl + ts_lst + plot_layout(ncol = 3)

ggsave("figures/figure_s9.pdf", ts_plots, units = "cm", width = 18, height = 18, dpi = 900)

#### AGB Thinned Areas ####

area <- c(rep(c("AGB Decline Areas", "Remaining Coast"), 4))
int <- c(rep("Interval 1", 4), rep("Interval 2", 4))
trend <- c(rep(c(rep("Aboveground", 2), rep("Belowground", 2)), 2))
value <- c(0.60, 0.67, -2.40, -1.02, -0.63, 0.86, 0.66, -0.06)

df <- data.frame(area, int, trend, value)
df$area <- factor(df$area, levels = c("AGB Decline Areas", "Remaining Coast"))

df2 <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/col_pix_metrics.csv")
df2 <- df2 %>%
  rename(area = col) %>%
  mutate(area = factor(ifelse(area == 0, "Remaining Coast", "AGB Decline Areas"), levels = c("Remaining Coast", "AGB Decline Areas"))) %>%
  mutate(interval = factor(interval, levels = c("1", "2")))

color_1 <- "#5ab4ac"
color_2 <- "#E1BD6D"

plot_theme2 <- function(){
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%	#replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),	#strip major gridlines
      panel.grid.minor = element_blank(),	#strip minor gridlines
      axis.ticks = element_blank(),      	#strip axis ticks
      #text elements
      plot.title = element_text(family = font, size = 10, hjust = 0.5),
      strip.text = element_text(family = font, size = 10),
      axis.title = element_text(family = font, size = 8),          	 
      axis.text = element_text(family = font, size = 7, color = "black"),
      legend.text = element_text(family = font, size = 9, color = "black"),
      legend.direction="vertical",
      legend.margin = margin(0.1,0.1,0.1,0.1, unit = "cm"),
      legend.background = element_rect(fill = "transparent", color = "transparent"),
      panel.background = element_rect(fill = "white", color = "white"),
      strip.background =element_rect(fill="white"),
      plot.margin = unit(c(0.1,0,0.1,0.1), "cm")
    )
}

p_thin_bgb <- ggplot(data = df2[df2$metric == "bg_trend",], aes(x = interval, group = area, fill = area)) +
  geom_col(aes(y = mean/100),position = "dodge", color = NA) +
  geom_errorbar(aes(ymin = mean/100 - std.error/100, ymax = mean/100 + std.error/100), position = position_dodge(0.9), width = 0.1, lwd = 0.1) +
  scale_y_continuous(limits = c(-0.025,0.025), labels = scales::percent, breaks = c(-0.025, 0, 0.025)) +
  scale_x_discrete(labels = c("Interval 1", "Interval 2")) +
  scale_fill_manual(values = c(color_1, color_2)) +
  labs(y = "Annual Trend", x = "", fill = "", title = "Belowground") +
  plot_theme2()
p_thin_bgb


p_thin_agb <- ggplot(data = df2[df2$metric == "ag_trend",], aes(x = interval, group = area, fill = area)) +
  geom_col(aes(y = mean/100),position = "dodge", color = NA) +
  geom_errorbar(aes(ymin = mean/100 - std.error/100, ymax = mean/100 + std.error/100), position = position_dodge(0.9), width = 0.1, lwd = 0.1) +
  scale_x_discrete(labels = c("Interval 1", "Interval 2")) +
  scale_y_continuous(limits = c(-0.01,0.01), labels = scales::percent, breaks = c(-0.01, 0, 0.01)) +
  scale_fill_manual(values = c(color_1, color_2)) +
  labs(y = "Annual Trend", x = "", fill = "", title = "Aboveground") +
  plot_theme2()
p_thin_agb

p_thin_ii <- ggplot(data = df2[df2$metric == "inund_inten",], aes(x = interval, group = area, fill = area)) +
  geom_col(aes(y = mean),position = "dodge", color = NA) +
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), position = position_dodge(0.9), width = 0.1, lwd = 0.1) +
  scale_x_discrete(labels = c("Interval 1", "Interval 2")) +
  scale_y_continuous(limits = c(0,0.15)) +
  scale_fill_manual(values = c(color_1, color_2)) +
  labs(y = "Inundation Intensity", x = "", fill = "", title = "Inundation") +
  plot_theme2()
p_thin_ii

plots <- p_thin_agb / p_thin_bgb / p_thin_ii / guide_area() + plot_layout(ncol = 1, nrow = 4, heights = c(rep(1,3), 0.5)) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a")  &   
  theme(plot.tag = element_text(size = 12), plot.tag.position  = c(-.02,  1.02), legend.position = 'bottom', legend.box="vertical", legend.box.margin = margin(b = 0.5, unit = "cm"))

ggsave("figures/figure_5.pdf", units = "cm", height = 11, width = 6, dpi = 900)




#### GA Coast SPAL Cover and Vulnerable Pixels ####


#spal cover
spal <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/predag_mean.tif")
spal[spal > 0] <- 1
#maps
#map theme
map_theme <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 14, hjust = 0.5),
      axis.text = element_text(family = font, size = 10), 
      axis.title = element_blank(), 
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "none",
      text = element_text(family = font),
      #margin
      plot.margin = margin(20,0,0,0),
    )
} 
#spal
spal_cover_map <- ggplot(data = spal) +
  geom_sf(data = ga_coast, fill = "white") +
  geom_raster(aes(x = x, y = y, fill = as.factor(predag.allom.l8))) + 
  coord_sf() + 
  scale_x_continuous(breaks = c(-81.5, -81), limits = c(434265, 514485)) +
  scale_y_continuous(breaks = c(31, 32), limits = c(3398085, 3554895)) +
  scale_fill_manual(na.value = "transparent", values = "black") +
  labs(title = expression(paste("U.S. Georgia Coast ", italic("Spartina alterniflora"), " Cover")), x = "", y = "", fill = "") +
  map_theme() +
  theme(panel.background = element_rect(fill = "lightblue"))
ggsave("figures/figure_s13.pdf", spal_cover_map, units = "cm", width = 18, height = 22, dpi = 900)

#vulnerable pixels

bg_color <- "white"#whitesmoke
scalebar_pad_x <- 10
scalebar_pad_y <- 85
annotation_size <- 3.5
#vul_pal <- c("grey80", "red")
vul_pal <- c("#3b9ab2", "#f21a00")
label_vul <- "Vulnerable Pixels"
leg_dir <- "horizontal"

# vul_pix
vulnerable <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/bgb_max_change.tif")
vulnerable[vulnerable >= -344] <- 0
vulnerable[vulnerable < -344] <- 1

map_theme <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 14, hjust = 0.5),
      axis.text = element_text(family = font, size = 10), 
      axis.title = element_blank(), 
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "none",
      text = element_text(family = font),
      #margin
      plot.margin = margin(20,0,0,0),
    )
} 

map_theme_vul <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 14, hjust = 0.5),
      axis.text = element_text(family = font, size = 10), 
      axis.title = element_blank(), 
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 11, family = font),
      #legend.key = element_rect(size = 1),
      #legend.spacing = unit(1, "cm"),
      text = element_text(family = font),
      panel.background = element_rect(fill = "lightgrey")
    )
} 

#vul
vul_pix_map <- ggplot(data = vulnerable) +
  geom_sf(data = ga_coast, fill = "white") +
  geom_raster(aes(x = x, y = y, fill = as.factor(bgb_raw_change))) + 
  coord_sf() + 
  scale_x_continuous(breaks = c(-81.5, -81), limits = c(434265, 514485)) +
  scale_y_continuous(breaks = c(31, 32), limits = c(3398085, 3554895)) +
  #scale_fill_manual(na.value = "transparent", values = c("#3b9ab2", "#f21a00")) +
  labs(title = "Vulnerable Marsh", x = "", y = "", fill = "") +
  scale_fill_manual(values = vul_pal, labels = c("Non-Vulnerable", "Vulnerable"), na.value = "transparent", na.translate = F) +
  map_theme_vul()
ggsave("figures/figure_s11.pdf", vul_pix_map, units = "cm", width = 18, height = 22, dpi = 900)

# marsh units
# vul_pix
vulnerable <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/bgb_max_change.tif")
vulnerable[vulnerable >= -344] <- 0
vulnerable[vulnerable < -344] <- 1

ga_coast <- vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/gshhg_coastline_clipped2.shp")
ga_coast <- project(ga_coast, "epsg:32617")
# Basemap
ga_rivers <- vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/Rivers_Streams_GA.shp")
ga_rivers <- project(ga_rivers, "epsg:32617")

# Sapelo Marsh Area
flux_marsh <- vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/FluxOutline.shp")
flux_marsh <- project(flux_marsh, "epsg:32617")
flux_radius <- round(max(c(ext(flux_marsh)[2] - ext(flux_marsh)[1], ext(flux_marsh)[4] - ext(flux_marsh)[3]))/2,0)
flux_centroid <- centroids(flux_marsh)
flux_sq <- ext(ext(flux_centroid)[1] - flux_radius, ext(flux_centroid)[2] + flux_radius, ext(flux_centroid)[3] - flux_radius, ext(flux_centroid)[4] + flux_radius)

flux_ext <- ext(flux_sq)
flux_coast <- terra::crop(ga_coast, flux_ext)
flux_rivers <- terra::crop(ga_rivers,flux_ext)


# Skidaway Marsh Area
skida_marsh <-  vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/skidaway_outline.shp")
skida_marsh <- project(skida_marsh, "epsg:32617")
skida_radius <- round(max(c(ext(skida_marsh)[2] - ext(skida_marsh)[1], ext(skida_marsh)[4] - ext(skida_marsh)[3]))/2,0)
skida_centroid <- centroids(skida_marsh)
skida_sq <- ext(ext(skida_centroid)[1] - skida_radius, ext(skida_centroid)[2] + skida_radius, ext(skida_centroid)[3] - skida_radius, ext(skida_centroid)[4] + skida_radius)

skida_ext <- ext(skida_sq)
skida_coast <- terra::crop(ga_coast, skida_ext)
skida_rivers <- terra::crop(ga_rivers,skida_ext)

flux_vul <- crop(vulnerable, flux_sq)
flux_vul <- terra::mask(flux_vul, flux_marsh)

skida_vul <- crop(vulnerable, skida_sq)
skida_vul <- terra::mask(skida_vul, skida_marsh)

map_theme_vul <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.subtitle = element_text(size = 10, family = font, hjust = 0.5),
      axis.title = element_blank(), 
      axis.text = element_text(size = 7),
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 9, family = font),
      legend.key.height = unit(10, "pt"),
      legend.key.width = unit(2, "pt"),
      legend.box.margin = unit(0, "pt"),
      legend.justification = "top",
      legend.box.spacing = unit(-50, "pt"),
      text = element_text(family = font),
      plot.margin = unit(c(0,-40,-10,-40), "pt"),
      panel.border = element_rect(color = "black", fill = NA)
    )
} 

ext <- ext(flux_sq)
flux_vul_map <- ggplot(data = flux_vul) +
  geom_sf(data = flux_rivers, fill = "lightgrey", lwd = 0, color = "lightgrey") +
  geom_raster(aes(x = x, y = y, fill = as.factor(bgb_raw_change))) + 
  coord_sf(xlim = c(xmin(ext), xmax(ext)), ylim = c(ymin(ext), ymax(ext))) + 
  scale_x_continuous(breaks = c(-81.29, -81.27),) +
  scale_y_continuous(breaks = c(31.44, 31.46)) +
  scale_fill_manual(values = vul_pal, labels = c("Non-Vulnerable", "Vulnerable"), na.value = "transparent", na.translate = F) +
  labs(fill = "", x = "", y = "", subtitle = "Sapelo") +
  map_theme_vul()   +
  theme(legend.key.size = unit(5, 'pt')) +
  guides(fill = guide_legend(direction = leg_dir, label.position = "bottom")) + 
  annotate(geom = 'text', label = paste0("bar(x) == ", format(round(global(flux_vul, mean, na.rm = T)*100,0), nsmall = 0), "*\'%'"), 
           x = ext[1], y = ext[4], vjust = 1, hjust = 0, family = "Helvetica", size = annotation_size, parse = T)
flux_vul_map

ext <- ext(skida_sq)
skida_vul_map <- ggplot(data = skida_vul) +
  geom_sf(data = skida_rivers, fill = "lightgray", lwd = 0, color = "lightgrey") +
  geom_raster(aes(x = x, y = y, fill = as.factor(bgb_raw_change))) + 
  coord_sf(xlim = c(xmin(ext), xmax(ext)), ylim = c(ymin(ext), ymax(ext))) + 
  scale_x_continuous(breaks = c(-81.04, -81.03),) +
  scale_y_continuous(breaks = c(31.97, 31.98)) +
  scale_fill_manual(values = vul_pal, labels = c("Non-Vulnerable", "Vulnerable"), na.value = "transparent", na.translate = F) +
  labs(fill = "", x = "", y = "", subtitle = "Skidaway") +
  map_theme_vul()   +
  theme(legend.key.size = unit(5, 'pt')) +
  guides(fill = guide_legend(direction = leg_dir, label.position = "bottom")) + 
  annotate(geom = 'text', label = paste0("bar(x) == ", format(round(global(skida_vul, mean, na.rm = T)*100,0), nsmall = 0), "*\'%'"), 
           x = ext[1], y = ext[4], vjust = 1, hjust = 0, family = "Helvetica", size = annotation_size, parse = T)
skida_vul_map

vul_unit <- flux_vul_map + skida_vul_map + plot_annotation(tag_levels = "a", title = "Vulnerable Marsh", ) + plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom", plot.tag.location = "plot", legend.margin = margin(0,1,1,1, unit = "pt"), legend.box.margin=margin(-10,-10,-10,-10), plot.title = element_text(hjust = 0.5, size = 11))
ggsave("figures/figure_s12.pdf", vul_unit, dpi = 900, units = 'cm', width = 11, height = 11)

#### Cumulative Inundation Intensity ####

df_ii_plot <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/ii_mean.csv")

df_ii_plot <- df
df_ii_plot$inund_inten[df_ii_plot$inund_inten > 0.5] <- 0.51
c_ii <- ggplot(data = df_ii_plot, aes(x = inund_inten)) +
  geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.025, color = "white", fill = "grey30", boundary = 1, closed = "right") +
  stat_bin(aes(y = after_stat(cumsum(count/sum(count)/4))), geom = "line", col = "deepskyblue4", size = 2) +#, position=position_nudge(x=0.025)) +
  scale_x_continuous(limits = c(0,0.525), breaks = seq(0,0.5,0.1), labels = c(seq(0,0.4,0.1), "> 0.5")) +
  scale_y_continuous(labels = scales::percent,  limits = c(0,0.25), sec.axis = sec_axis(~ .x * 4, name = "Cumulative Proportion", labels = scales::percent, breaks = c(0,0.25,0.50,0.75,1))) +
  labs(x = "Inundation Intensity", y = "Proportion") +
  theme(
    #grid elements
    panel.grid.major = element_blank(),	#strip gridlines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linetype = 1, color = "black", size = 1),
    axis.line.y.left = element_line(color = "grey30"),       	 
    axis.ticks.y.left = element_line(color = "grey30"),
    axis.line.y.right = element_line(color = "deepskyblue4"),       	 
    axis.ticks.y.right = element_line(color = "deepskyblue4"),
    #text elements
    axis.title = element_text(family = "Helvetica", size = 12),
    axis.title.y.left = element_text(family = "Helvetica", size = 12, color = "grey30"),
    axis.title.y.right = element_text(family = "Helvetica", size = 12, color = "deepskyblue4"),
    axis.text = element_text(family = "Helvetica", size = 10, color = "black"),
    axis.text.y.left = element_text(family = "Helvetica", size = 10, color = "grey30"),
    axis.text.y.right = element_text(family = "Helvetica", size = 10, color = "deepskyblue4"),
    panel.background = element_rect(fill = "white", color = "white"),
    text = element_text(family = "Helvetica"),
    plot.margin = margin(15,5,5,5)
  )
c_ii
ggsave("figures/figure_s8.pdf", c_ii, width = 11, height = 11, units = "cm", dpi = 900)
#### Range Plot ####

# range of calibration and application data

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(svglite)
library(grid)

## datafile to create bermv2.0
data <- read_csv("data/processed_bg2.0.csv")
## important features
feat <- read_csv("data/xgb_mean_import_features.csv")
feat <- feat %>%
  dplyr::select(-c(3:5))

## ranges of calibration data
feat_list <- feat$features
data_feat <- data[names(data) %in% feat_list]
data_feat_long <- data_feat %>%
  pivot_longer(names_to = "variable", values_to = "value", cols = 1:length(data_feat)) %>%
  na.omit(value)
data_den <- data_feat_long %>%
  group_by(variable) %>%
  mutate(value_norm = (value - min(value)) / (max(value) - min(value)) )
data <- merge(data_den, feat, by.y = "features", by.x = "variable")
data <- data[order(data$importance_mean, decreasing = T),]
data$features_full_f <- factor(data$features_full, levels = unique(data$features_full))

ggplot(data = data, aes(x = value_norm)) +
  geom_density(aes(fill = category), color = NA, alpha = 0.7) +
  facet_grid(rows = vars(features_full_f),  switch="y", scales = "free") +
  scale_fill_manual(values = c("Hydrologic" = "#4080C0", "Physical" = "#E0B860", "Biological" = "#209058", "Climatic" = "#D84840")) +
  scale_x_continuous(limits = c(-1,2)) +
  geom_vline(aes(xintercept = 0), color = "grey") +
  geom_vline(aes(xintercept = 1), color = "grey") +
  theme(
    text = element_text(size = 30, family = "Helvetica"),
    axis.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_blank(),
    strip.placement='outside',
    strip.text.y.left = element_text(angle = 0),
    strip.text = element_text(size = 6),
    panel.spacing = unit(0.2, "lines"),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = "white"))

## ranges of application data normalized to calibration data
feats <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/xgb_mean_import_features.csv")
feats_list <- feats$features
cal_ranges <- read_csv("data/calibration_data_ranges.csv")

watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10

out2 <- data.frame()

for (w in 1:length(watersheds)){
  #filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  #bgb_list <- lapply(filenames, fread, select = c(1,2,47)) # add in pix
  out <- data.frame(matrix(ncol = 3, nrow = 0))
  x <- c("variable", "norm_value", "Freq")
  colnames(out) <- x
  out$variable <- as.factor(out$variable)
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    features <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/process_newpixels_for_xgb/", watersheds[w], "/xgb_processed_", watersheds[w], "_cutno", c, ".csv"), select = c(feats_list))
    features <- melt(features)
    features <- features %>%
      drop_na(value)
    features <- merge(features, cal_ranges, by = "variable")  
    features <- features %>%
      mutate(norm_value = round((value - min_value) / (max_value - min_value),2))
    features <- features %>%
      dplyr::select(variable, norm_value) #%>%
    #group_by(variable, norm_value) %>%
    #summarise(count = nrow())
    feature_count <- as.data.frame(table(features))
    feature_count$norm_value <- as.numeric(as.character(feature_count$norm_value))
    
    out <- bind_rows(out,feature_count) %>%
      group_by(variable, norm_value) %>%
      summarise_all((funs(sum(., na.rm = T))))
  }
  write_csv(out, paste0("results/range_comp/", watersheds[w], ".csv"))
}
out2 <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("variable", "norm_value", "Freq")
colnames(out2) <- x
out2$variable <- as.factor(out2$variable)

for (w in 1:length(watersheds)){
  ranges <- fread(paste0("results/range_comp/", watersheds[w], ".csv"))
  out2 <- bind_rows(out2,ranges) %>%
    group_by(variable, norm_value) %>%
    summarise_all((funs(sum(., na.rm = T))))
}
write_csv(out2, "results/range_comp/total.csv")

feats <- read.csv("/media/kyle/Seagate Expansion Drive/data/results/xgb2.0_all_import_features.csv")
feats2 <- feats %>%
  group_by(features) %>%
  pivot_longer(cols = c("train.1", "train.2", "train.3", "train.4", "train.5"), names_to = "train", values_to = "mean_importance")
feats3 <- feats2 %>%
  group_by(features) %>%
  summarise(importance_mean = mean(mean_importance), importance_sd = sd(mean_importance), importance_min = min(mean_importance), importance_max = max(mean_importance))
feats3 <- feats3[order(feats3$importance_mean, decreasing = T),]
feats_list <- feats3$features

## important features
feat <- read_csv("data/xgb_mean_import_features.csv")
feat <- feat %>%
  dplyr::select(-c(3:5))

data <- read_csv("results/range_comp/total.csv")
data <- merge(data, feat, by.x = "variable", by.y = "features")
data <- data[order(data$importance_mean, decreasing = T),]
data$features_full_f <- factor(data$features_full, levels = unique(data$features_full))
data <- data %>%
  group_by(variable) %>%
  mutate(Freq_weighted = Freq/sum(Freq))
data$category[data$category == "Physical"] <- "Elevation"

#remotes::install_github("wilkelab/ggridges", force = T)
#library(ggridges)

range_plot <- ggplot(data = data, aes(x = norm_value, weight = Freq_weighted)) +
  geom_density(aes( fill = category), color = NA, alpha = 0.7, bw = 0.05) +
  facet_grid(rows = vars(features_full_f),  switch="y", scales = "free") +
  scale_fill_manual(values = c("Hydrologic" = "#4080C0", "Elevation" = "#E0B860", "Biological" = "#209058", "Climatic" = "#D84840")) +
  scale_x_continuous(limits = c(-1,2), breaks = -1:2, labels = paste0(c("-1 Range", "Minimum of\nCalibration Data", "Maximum of\nCalibration Data", "+1 Range"))) +
  geom_vline(aes(xintercept = 0), color = "grey") +
  geom_vline(aes(xintercept = 1), color = "grey") +
  labs(x = "Distribution of Application Data,\nNormalized to Range of Calibration Data", y = "Predictor Metric", fill = "Category of Predictor Metric") +
  theme(
    text = element_text(size = 10, family = "Helvetica"),
    axis.text = element_text(size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    strip.placement='outside',
    strip.text.y.left = element_text(hjust = 0, angle = 0,),
    strip.text = element_text(size = 8, margin = unit(c(0,0.5,0,0), 'cm')),
    panel.spacing = unit(0.1, "lines"),
    legend.position = "bottom",
    legend.justification = "right",
    legend.text = element_text(size=8),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.margin = unit(c(1,1,0.5,0.25), "cm"),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
range_plot
ggsave("figures/figure_s15.pdf", range_plot, units = "cm", width = 18, height = 22, dpi = 900)




#### Field Biomass vs Inundation Intensity ####

df <- read_csv("/home/kyle/Documents/Documents/UT/Sapelo/berm_output/processed_bg2.0.csv")

#GAM AGB
gam_mean_ag <- gam(agm2.allom ~ s(local_hiwater, bs ="cs", k = 8), data = df)
summary(gam_mean_ag)
plot.gam(gam_mean_ag)
abline(v=0.0653, col="black")
abline(v=0.0776, col="red")
abline(v=0.0899, col="blue")
gam.check(gam_mean_ag)
k.check(gam_mean_ag)
agb_r.sq <- summary(gam_mean_ag)$r.sq
new_data <- data.frame(local_hiwater = seq(0, 1.608986, 0.001))
pred <- predict(gam_mean_ag, new_data, type = "response", se.fit = T)
new_data$agm2.allom <- pred$fit
# GAM BGB
gam_mean_bg <- gam(bgm2.core.stemscaled ~ s(local_hiwater, bs = "cs", k = 10), data = df)
summary(gam_mean_bg)
plot.gam(gam_mean_bg)
abline(v=0.0653, col="black")
abline(v=0.0776, col="red")
abline(v=0.0899, col="blue")
bgb_r.sq <- summary(gam_mean_bg)$r.sq
pred <- predict(gam_mean_bg, new_data, type = "response", se.fit = T)
new_data$bgm2.core.stemscaled <- pred$fit

class(new_data$agm2.allom)
new_data$agm2.allom <- as.numeric(new_data$agm2.allom)
new_data$bgm2.core.stemscaled <- as.numeric(new_data$bgm2.core.stemscaled)

write_csv(new_data, "/media/kyle/Seagate Expansion Drive/data/results/gam_agb_bgb_ii.csv")
write_csv(df, "/media/kyle/Seagate Expansion Drive/data/results/ii_mean.csv")


data <- read_csv("/home/kyle/Documents/Documents/UT/Sapelo/berm_output/processed_bg2.0.csv")

i <- "local_hiwater"
df <- data[,c(i,"bgm2.core.stemscaled", "agm2.allom")]
df <- df[complete.cases(df),]
colnames(df)
df <- df %>%
  pivot_longer(cols = c("agm2.allom", "bgm2.core.stemscaled"), names_to = "component", values_to = "value") %>%
  mutate(component = replace(component, component=="agm2.allom", "Aboveground"), component = replace(component, component=="bgm2.core.stemscaled", "Belowground"))

line_size <- 0.4
ii_med_2014 <- 0.06537992
ii_med_2023 <- 0.07765544
ii_med_plus <- ii_med_2023 + (ii_med_2023 - ii_med_2014)

annotation <- data.frame(
  x = c(ii_med_2014, ii_med_2023, ii_med_plus),
  y = c(1125,1125, 1125),
  label = c("2014", "2023", ""),
  color = c("2014 Median", "2023 Median", "Further Increase"),
  lty = c("solid","dashed","dotted")
)
line_size_2 <- 0.39

plot_theme <- function(){ 
  font <- "Arial"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #text elements
      plot.title = element_text(family = font, size = 0),
      axis.text = element_text(family = font, size = 10, color = "black"), 
      axis.title = element_text(family = font, size = 12), 
      panel.background = element_rect(fill = "white", color = "white"),
      legend.position = "bottom",
      legend.text = element_text(family = font, size = 8),
      legend.box.margin = margin(0,0,0,0, unit = "pt"),
      legend.key.width = unit(0, 'cm'), 
      legend.spacing.x = unit(0, 'cm'), 
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.spacing.y = unit(0, 'cm'),
      text = element_text(family = font),
      plot.margin = margin(2,2,2,2),
    )
} 

# Biomass II plot
bio_field_ii <-  ggplot() +
  geom_point(data = df, aes(x = local_hiwater, y = value, color = component), size = 0.25, alpha = 0.35) +
  geom_line(data = new_data, aes(x = local_hiwater, y = agm2.allom), col =  "#7D904A", lwd = 2) +
  geom_line(data = new_data, aes(x = local_hiwater, y = bgm2.core.stemscaled), col =  "#6f4a2d", lwd = 2) +
  scale_color_manual(name = "", values = c("Aboveground" = "#7D904A", "Belowground" = "#6f4a2d", "2014 Median" = "black", "2023 Median" = "red", "Further Increase" = "red"), breaks = c("Aboveground", "Belowground")) +
  labs(x = "Inundation Intensity", y = parse(text=paste("Biomass~(g~m^-~2)")), color  = "Legend", linetype = "Legend", size = "Legend") +
  coord_cartesian() +
  scale_x_continuous(limits = c(-0.0, 0.3)) +
  scale_y_continuous(limits = c(0,2000)) +
  scale_linetype_manual(name = "", values = c("2014 Median" = "solid", "2023 Median" = "dashed", "Further Increase" = "dotted")) +
  #scale_size_manual(name = "", values = c("2014 Median" = line_size_2, "2023 Median" = line_size_2, "Further Increase" = line_size_2)) +
  geom_vline(data = annotation, aes(xintercept = x, color = color, linetype = color), size = 0.5) +
  plot_theme() +
  guides(size = "none", color = guide_legend(override.aes=list(shape = 15, size = 5, alpha = 1), ncol = 2), linetype = guide_legend(override.aes=list(color = c("black", "red", "red")), ncol = 3))
bio_field_ii
ggsave("results/field_bio_ii.png", bio_field_ii, width = 4, height = 4, units = "in", dpi = 300)

# plotting bare gam line
new_data <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/gam_agb_bgb_ii.csv")

bgb_ii_resp <- ggplot(data = new_data, aes(x = inund_inten, y = predbg.xgb, col = "Belowground")) +
  geom_line(lwd = 2, color = "#6f4a2d") +
  scale_x_continuous(limits = c(0,0.3)) +
  theme_void()  +
  theme(panel.background = element_rect(fill='transparent', linewidth = 0))
bgb_ii_resp
ggsave("results/bgb_ii_resp_curve.png", bgb_ii_resp, width = 4, height = 3, units = "in", dpi = 150)

x <- -100:100
dat <- data.frame(x,y=-x^2)
f <- function(x) -x^2
organic_parabola <- ggplot(dat, aes(x,y)) + 
  stat_function(fun=f, colour="#0E8136", size = 4) +
  theme_void() +
  scale_y_continuous(limits = c(-11000, 3000)) +
  theme(panel.background = element_rect(fill='transparent', linewidth = 0),
        plot.margin =  unit(c(0,0,0,0), "cm"))
organic_parabola
ggsave("results/organic_parabola_curve.png", organic_parabola, width = 4, height = 3, units = "in", dpi = 150)
ggsave("results/organic_parabola_curve_small.png", organic_parabola, width = 1, height = 0.25, units = "in", dpi = 150)
ggsave("results/organic_parabola_curve_thick.png", organic_parabola, width = 4, height = 3, units = "in", dpi = 150)


x <- 0:100
dat <- data.frame(x, y=exp(-0.03*x))
f <- function(x) exp(-0.03*x)
decay <- ggplot(dat, aes(x,y)) + 
  stat_function(fun=f, colour="#F2300F", size = 5) +
  theme_void() +
  theme(panel.background = element_rect(fill='transparent', linewidth = 0))
decay
ggsave("results/decay_curve.png", decay, width = 4, height = 3, units = "in", dpi = 150)
ggsave("results/decay_curve_thick.png", decay, width = 4, height = 3, units = "in", dpi = 150)


#### BERM2.0 Performance ####

## AGB model performance
df<- read_csv("/media/kyle/Seagate Expansion Drive/data/results/xgb2_predicted_biophysical_seagrant_landsat8.csv")
sort(colnames(df))

df <- df %>%
  filter(train.1 == 0) %>%
  dplyr:: select(agm2.allom, predag.allom.l8) %>%
  drop_na()

library(ggthemes)
plot_theme <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_hc() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      #text elements
      plot.title = element_text(family = font, size = 16, hjust = 0.5),
      plot.subtitle = element_text(family = font, size = 12, hjust = 0.5),
      axis.title = element_text(family = font, size = 12),               
      axis.text = element_text(family = font, size = 10, color = "black"), 
      legend.position = "none",
      panel.background = element_rect(fill = "white", color = "white"),
      aspect.ratio = 1
    )
}


ag_test <- ggplot(data = df, aes(x = agm2.allom, y = predag.allom.l8)) +
  geom_point(size = 0.75) +
  geom_abline(slope = 1) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,250)) +
  scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,250)) +
  labs(x =  expression("Observed Biomass (g " ~ m^-~~2 ~ ")"), y =  expression("Predicted Biomass (g " ~ m^-~~2 ~ ")"), title = "Aboveground Biomass") +
  plot_theme()
ag_test

df<- read_csv("/media/kyle/Seagate Expansion Drive/data/results/bg_model_output.csv")
sort(colnames(df))

df <- df %>%
  rowwise() %>%
  mutate(predbgb_train_mean = mean(c(ifelse(train.1 == 1, pred.1, NA), ifelse(train.2 == 1, pred.2, NA), ifelse(train.3 == 1, pred.3, NA), ifelse(train.4 == 1, pred.4, NA),  ifelse(train.5 == 1, pred.5, NA)), na.rm = T),
         predbgb_test_mean  = mean(c(ifelse(train.1 == 0, pred.1, NA), ifelse(train.2 == 0, pred.2, NA), ifelse(train.3 == 0, pred.3, NA), ifelse(train.4 == 0, pred.4, NA),  ifelse(train.5 == 0, pred.5, NA)), na.rm = T)) %>%
  dplyr:: select(bgm2.core.stemscaled, predbgb_test_mean) %>%
  drop_na()

bg_test <- ggplot(data = df, aes(x = bgm2.core.stemscaled, y = predbgb_test_mean)) +
  geom_point(size = 0.75) +
  geom_abline(slope = 1) +
  scale_x_continuous(limits = c(0,3000), breaks = seq(0,3000,1000)) +
  scale_y_continuous(limits = c(0,3000), breaks = seq(0,3000,1000)) +
  labs(x =  expression("Observed Biomass (g " ~ m^-~~2 ~ ")"), y =  expression("Predicted Biomass (g " ~ m^-~~2 ~ ")"), title = "Belowground Biomass") +
  plot_theme()
bg_test

berm_perf <- ag_test + bg_test + plot_layout(nrow = 1) + plot_annotation(tag_levels = "a") & theme(plot.tag.position = c(0.02, 0.98))
ggsave("figures/figure_s14.pdf", berm_perf, width = 18, height = 9, units = "cm")
