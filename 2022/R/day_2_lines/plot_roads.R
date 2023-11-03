# Load data first with `load_data.R` scipt
#
# This script might not make sense at first because it's a bit of a
# hacky solution to get what I wanted. OSM returns data that is within
# a bounding box, which does not match out specified boundaries
# (i.e. here that's Colorado state borders). I tried using st_intersection(),
# but that didn't work, I think because there are so many elements to the roads.
# Easier solution is to take the difference of the bounding box returned by
# OSM and the boundaries you want. Then, it's like you're applying a matte to a frame.

library(tidyverse)
library(sf)
library(tigris)
library(magick)

states <- states()

# Get state boundaries

co <- states |> 
  filter(NAME == "Colorado") |> 
  st_transform(crs = st_crs(roads$service))

# get bounding box for roads

bb <- map(roads, function(r) {
  st_bbox(r)%>%
    st_as_sfc() %>%
    st_transform(., crs = st_crs(co))
})

# create the outline polygon that will cover roads outside
# state borders
outline <- st_difference(bb$not_service, co)

font_add_google("Stick No Bills", "f")
showtext_auto()

# plot baby plot

r_only <- roads$service |> 
  ggplot() +
  geom_sf(color = "white", size = .02) +
  geom_sf(data = roads$not_service, color = "white",
          size = .04) +
  geom_sf(data = outline, fill = "black", color = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        text = element_text(family = "f"),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = .5, color = "grey70",
                                    margin = margin(b = 10), size = 30),
        plot.title = element_text(color = "grey95", hjust = .5,
                                  size = 60)) 

ggsave("plots/day_2/co_roads.png", plot = r_only, device = "png", bg = "black")


# I wanted greater control over the annotations, so I'm using imagemagick
# instead of normal ggplot

img <- image_read("plots/day_2/co_roads.png")

# add annotations using different fonts
titled <- image_annotate(img, text = "COLORADO", gravity = "north",
                         location = "-600+50", color = "white",
                         size = 200, font = "Zeyada") |> 
  image_annotate(text = "ROADS", gravity = "north",
                 location = "+700+100", color = "white",
                 size = 150, font = "Turret Road") |> 
  image_annotate(text = "Graphic by Spencer Schien (@MrPecners) | Data from OpenStreetMap", 
                 gravity = "south",
                 location = "+0+50", color = "grey70",
                 size = 40, font = "Turret Road")

titled

image_write(titled, "plots/day_2/co_roads_titled.png")
