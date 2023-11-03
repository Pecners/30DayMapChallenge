library(tidyverse)
library(sf)
library(tigris)
library(rnaturalearth)
library(MetBrewer)
library(glue)

# Get states sf objects

states <- states()

# Remove states we won't be plotting

not_states <- c("Commonwealth of the Northern Mariana Islands",
                "Alaska",
                "Puerto Rico",
                "Hawaii",
                "American Samoa",
                "Guam",
                "United States Virgin Islands")

states_skinny <- states %>%
  filter(!NAME %in% not_states) 

# Get Great Lakes and oceans so we can erase the water from map

gl <- ne_download(type = "lakes", category = "physical", scale = "large")  %>%
  st_as_sf() %>%
  st_transform(., crs = st_crs(states_skinny))

l <- ne_download(type = "land", category = "physical", scale = "large")  %>%
  st_as_sf() %>%
  st_transform(., crs = st_crs(states_skinny))

# gl now has a lot of lakes, need to specify our 
# Great Lakes

lakes <- c("Lake Erie",
           "Lake Michigan",
           "Lake Superior",
           "Lake Huron",
           "Lake Ontario")

gl <- gl %>%
  filter(name %in% lakes) %>%
  st_transform(crs = st_crs(states)) |> 
  st_union()

no_lakes <- st_difference(states_skinny, gl)
landed <- st_intersection(no_lakes, l)

colors <- met.brewer("Hiroshige")
colors

bg <- colors[8]

p <- landed |> 
  ggplot() +
  geom_sf(size = .1, color = colors[1], fill = colors[5]) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        text = element_text(family = "Julee", color = colors[5]),
        panel.background = element_rect(fill = bg,
                                        color = NA),
        plot.background = element_rect(fill = bg,
                                       color = NA),
        panel.grid = element_line(color = colors[6]),
        plot.subtitle = element_text(hjust = .5,
                                     size = 24,
                                     margin = margin(b = 10)),
        plot.title = element_text(hjust = .5,
                                  size = 26,
                                  family = "Frijole",
                                  margin = margin(b = 20)),
        plot.caption = element_text(hjust = .5,
                                    size = 12, 
                                    color = alpha(colors[5], .75))) +
  coord_sf(crs = 3112, expand = 0, clip = "off") +
  labs(title = "The Australian United States",
       subtitle = glue("This map projects the United States using a ",
                       "Coordinate Reference System for Australia.") |> 
         str_wrap(50),
       caption = "Graphic by Spencer Schien (@MrPecners)")

ggsave(p, filename = "plots/day_10/bad_map.png", bg = bg,
       width = 8.5, h = 11)
