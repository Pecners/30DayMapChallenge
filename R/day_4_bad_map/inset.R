library(tigris)
library(rnaturalearth)
library(ggfx)
library(sf)
library(tidyverse)
library(colorspace)
library(glue)
source("R/day_4_bad_map/make_ellipse.R")

# Load `header` list with needed data
header <- readRDS("R/day_4_bad_map/header.rds")
colors <- natparks.pals("Acadia")
swatchplot(colors)

text_color <- colors[1]

park <- st_read("data/nps_boundary/nps_boundary.shp") |> 
  filter(str_detect(PARKNAME, "Death Valley"))

s <- states()

skinny_s <- s |> 
  filter(NAME %in% c("California", "Nevada"))

skinny_s |> 
  ggplot() +
  geom_sf()

land <- ne_download(type = "land", category = "physical", scale = "large")  %>%
  st_as_sf() %>%
  st_transform(., crs = st_crs(skinny_s)) |> 
  st_union()

skinny_s <- st_intersection(skinny_s, land)

coords <- c(36.23470006140297, -116.81570526664568)

data <- make_ellipse(coords, r = 40000, height_factor = 1.5, width_factor = .95, 
                     npc = 360, tilt = 75, crs = 3310) 


loc_plot <- skinny_s |> 
  ggplot() +
  geom_sf(fill = NA, color = colors[4],
          linewidth = .5) +
  geom_sf_text(data = skinny_s[1,],
               aes(label = NAME), 
               color = alpha(colors[4], .5),
               size = 8,
               family = "Poller One",
               nudge_x = -170000,
               nudge_y = 400000) +
  geom_sf_text(data = skinny_s[2,],
               aes(label = NAME), 
               color = alpha(colors[4], .5),
               size = 8,
               family = "Poller One",
               nudge_x = -30000,
               nudge_y = 250000) +
  geom_sf(data = park, 
          fill = alpha(colors[2], .1),
          color = colors[2]) +
  geom_sf(data = data,
          fill = alpha(colors[8], .5),
          color = colors[8],
          linewidth = .5) +
  geom_sf_text(data = data,
               label = "Area shown\nat left",
               family = "Poller One",
               size = 10,
               nudge_y = 60000,
               nudge_x = 110000,
               color = colors[8]) +
  # annotate(geom = "curve", yend = 180000, xend = 64306,
  #          y = 167500, x = 90000, 
  #          curvature = -.5,
  #          angle = 50,
  #          linewidth = 1,
  #          color = colors[6], arrow = arrow(type = "closed")) +
  geom_sf_text(data = park,
               label = "Death Valley\nNational Park",
               family = "Poller One",
               size = 8,
               nudge_y = -5000,
               nudge_x = -170000,
               color = alpha(colors[1], .5)) +
  coord_sf(crs = st_crs(data), clip = "off") +
  theme_void()

loc_plot

ggsave(loc_plot, 
       filename = glue("plots/day_4_bad_map/inset.png"),
       bg = "transparent", width = 20, height = 15)
