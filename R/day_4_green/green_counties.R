library(tidyverse)
library(tigris)
library(sf)
library(ggfx)
library(colorspace)
library(rnaturalearth)

# load states

s <- states()

# filter for only contiguous states
skip <- c(
  "Puerto Rico",
  "Alaska",
  "Hawaii",
  "United States Virgin Islands",
  "Commonwealth of the Northern Mariana Islands",
  "American Samoa",
  "Guam"
)

skinny_s <- s |> 
  filter(!NAME %in% skip)

# load counties

counties <- map_df(1:nrow(skinny_s), function(x) {
  counties(state = skinny_s[[x, "NAME"]])
})

# join counties with states

c <- counties |>
  left_join(s |> 
              as_tibble() |> 
              select(STATEFP,
                     state = NAME)) |> 
  mutate(gr = ifelse(str_detect(NAME, regex("green", ignore_case = TRUE)),
                     "darkgreen", lighten("darkgreen", .9)))

# load land and great lakes to make map more detailed

l <- ne_download(type = "lakes", category = "physical", scale = "large")  %>%
  st_as_sf(., crs = st_crs(states))

lakes <- c("Lake Erie",
           "Lake Michigan",
           "Lake Superior",
           "Lake Huron",
           "Lake Ontario")
gl <- l %>%
  filter(name %in% lakes) %>%
  st_transform(crs = st_crs(skinny_s)) |> 
  st_union()

land <- ne_download(type = "land", category = "physical", scale = "large")  %>%
  st_as_sf() %>%
  st_transform(., crs = st_crs(skinny_s)) |> 
  st_union()


skinny_s <- st_difference(skinny_s, gl)
c <- st_difference(c, gl)

skinny_s <- st_intersection(skinny_s, land)
c <- st_intersection(c, land)

bg <- lighten("darkgreen", .95)

swatchplot(bg)

# pull green county names

c_names <- c |> 
  as_tibble() |> 
  filter(gr == "darkgreen") |>
  select(NAME) |>
  arrange(NAME) |> 
  unique() |> 
  pull(1)

# make the green plot!
p <- c |> 
  ggplot(aes(fill = gr)) +
  geom_sf(data = c |> filter(gr != "darkgreen"),
          color = NA) +
  with_shadow(
    geom_sf(data = c |> 
              filter(gr == "darkgreen"),
            size = .01, color = NA), 
    x_offset = 0, y_offset = 0,
    sigma = 10, colour = "darkgreen"
  ) +
  geom_sf(data = skinny_s, 
          color = "darkgreen", 
          size = .1,
          fill = NA) +
  scale_fill_identity() +
  coord_sf(crs = 2163, expand = TRUE) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = bg, color = NA),
        plot.background = element_rect(fill = bg, color = NA),
        panel.grid = element_line(color = "white"),
        text = element_text(family = "Aclonica"),
        plot.title = element_text(size = 32, margin = margin(b = 20, t = 2),
                                  color = darken("darkgreen", .5)),
        plot.subtitle = element_text(color = darken("darkgreen", .25),
                                     lineheight = 1.1,
                                     size = 16),
        plot.caption = element_text(color = lighten("darkgreen", .5))) +
  labs(title = "Green Counties of America",
       subtitle = sprintf("Counties in the contiguous United States with names that contain 'green' include %s",
                          paste0(paste(c_names[1:length(c_names)-1], collapse = ", "),
                                 paste0(c(", and ", c_names[length(c_names)], " counties."),
                                        collapse = ""),
                                 collapse = "")) |> 
         str_wrap(75),
       caption = "Graphic by Spencer Schien (@MrPecners) | Data from US Census Bureau via the {tigris} R package")

ggsave("plots/day_4/green_counties.png", bg = bg)
