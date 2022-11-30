library(tidyverse)
library(magick)
library(ggfx)

header <- read_rds("R/day_30_remix/header.rds")

colors <- header$colors

utah <- tigris::states() |> 
  filter(NAME == "Utah")

utah |> 
  ggplot() + 
  with_shadow(
    geom_sf(fill = colors[1], color = NA),
    sigma = 50, colour = "grey50",
    x_offset = 30, y_offset = 50
  ) +
  geom_sf(data = all, fill = colors[6], size = 0) +
  geom_sf_text(aes(label = NAME), family = "Denk One",
               size = 12, color = colors[4]) +
  coord_sf(clip = "off") +
  theme_void() +
  theme(text = element_text(family = "Denk One"),
        plot.margin = margin(rep(50, 4)))

ggsave("plots/day_30/inset.png")


inset <- image_read("plots/day_30/inset.png")

scaled_inset <- image_scale(inset, "50%")

img <- image_read("plots/day_30/upheaval_dome_glacier_arches2_z14.png")

img |> 
  image_crop(geometry = "4750x3500", gravity = "west") |> 
  image_annotate(text = "Upheaval Dome", gravity = "north",
                 location = "-1600+700", font = "Denk One",
                 color = colors[4],
                 size = 175, weight = 700) |> 
  image_annotate(text = "at Canyonlands National Park", gravity = "north",
                 location = "-1600+1000", font = "Denk One",
                 color = colors[4],
                 size = 90, weight = 700) |> 
  image_annotate(text = "Data from AWS Terrain Tiles via {elevatr}", gravity = "north",
                 location = "-1600+2600", font = "Denk One",
                 color = alpha(colors[4], .5),
                 size = 50, weight = 700) |> 
  image_annotate(text = "Graphic by Spencer Schien (@MrPecners)", gravity = "north",
                 location = "-1600+2700", font = "Denk One",
                 color = alpha(colors[4], .5),
                 size = 50, weight = 700) |> 
  image_composite(scaled_inset, gravity = "north", 
                  offset = "-1600+1250") |> 
  image_write("plots/day_30/titled_upheaval_dome.png")
