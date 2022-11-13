library(tidyverse)
library(osmdata)
library(NatParksPalettes)
library(tigris)
library(sf)

il_places <- places(state = "Illinois")
alton <- il_places |> 
  filter(NAME == "Alton")

data <- opq("Alton, Illinois") |> 
  add_osm_feature(key = "highway") |> 
  osmdata_sf()

colors <- natparks.pals("Acadia")
colorspace::swatchplot(colors)

data$osm_lines |> 
  st_transform(crs = st_crs(alton)) |> 
  st_intersection(alton) |> 
  ggplot() +
  geom_sf(size = .2, color = colors[9]) +
  theme_void() +
  theme(text = element_text(family = "Aclonica",
                            size = 30),
        plot.background = element_rect(fill = colors[1], color = NA),
        panel.background = element_rect(fill = colors[1], color = NA),
        plot.title = element_text(color= colors[9],
                                  hjust = .1),
        plot.caption = element_text(color = alpha(colors[9], .5),
                                    size = 10,
                                    hjust = .9)) +
  labs(title = "Alton, Illinois",
       caption = "Graphic by Spencer Schien (@MrPecners) | Data from OpenStreetMap")

ggsave("plots/day_13/alton.png", bg = colors[1])
