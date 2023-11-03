library(tidyverse)
library(sf) 
library(glue)
library(patchwork)
library(NatParksPalettes)
library(magick)

nps <- st_read("data/nps_boundary/nps_boundary.shp")

np <- nps |> 
  filter(UNIT_TYPE == "National Park" | str_detect(UNIT_NAME, "New River Gorge")) |> 
  select(name = UNIT_NAME,
         area = Shape_Area,
         state = STATE,
         geometry) |> 
  st_transform(crs = 5070)

np_max <- map_df(1:nrow(np), function(i) {
  this <- np[i,]
  pts <- st_bbox(this) |> 
    st_as_sfc() |> 
    st_cast("POINT") |> 
    st_as_sf()
  w <- st_distance(pts[1,], pts[2,])
  h <- st_distance(pts[2,], pts[3,])
  if (w > h) {
    m <- w
  } else {
    m <- h
  }
  
  this |> 
    mutate(max = m)
}) |> 
  arrange(max)

np_area <- np_max |> 
  arrange(area)

radius <- np_max |> 
  tail(1) |> 
  pull(max) / 2

pal <- natparks.pals("Acadia", n = 11)

fac <- 4

plots <- map(1:nrow(np_area), function(i) {
  this_one <- np_area[i,]
  
  if (this_one$state == "AK") {
    this_one <- st_transform(this_one, 3338)
  }
  
  buff <- st_buffer(st_centroid(this_one), radius) |> 
    st_bbox()

  t <- this_one |> 
    mutate(name = str_remove(name, "National Park$|National Park and Preserve$")) |> 
    mutate(t = glue("{name}\n({state})")) |> 
    pull(t)
  
  if (i / 6 == 1) {
    margin <- -100
  } else {
    margin <- 0
  }
  this_one |> 
    ggplot() +
    # geom_sf(data = st_as_sfc(buff), fill = "red") +
    geom_sf(linewidth = .1, fill = pal[8],
            color = NA) +
    theme_void() +
    labs(caption = t) + 
    theme(plot.caption = element_text(hjust = .5, vjust = 10,
                                      color = pal[8],
                                      size = rel(fac),
                                      family = "El Messiri"),
          plot.margin = margin(b = margin)) +
    coord_sf(xlim = c(buff[1], buff[3]),
             ylim = c(buff[2], buff[4]), 
             clip = "off", expand = 0)
})

layout <- c(

  area(2, 1), area(2, 2), area(2, 3), area(2, 4), area(2, 5), area(2, 6), area(2, 7),
  area(3, 1), area(3, 2), area(3, 3), area(3, 4), area(3, 5), area(3, 6), area(3, 7),
  area(4, 1), area(4, 2), area(4, 3), area(4, 4), area(4, 5), area(4, 6), area(4, 7),
  area(5, 1), area(5, 2), area(5, 3), area(5, 4), area(5, 5), area(5, 6), area(5, 7),
  area(6, 1), area(6, 2), area(6, 3), area(6, 4), area(6, 5), area(6, 6), area(6, 7),
  area(7, 1), area(7, 2), area(7, 3), area(7, 4), area(7, 5), area(7, 6), area(7, 7),
  area(8, 1), area(8, 2), area(8, 3), area(8, 4), area(8, 5), area(8, 6), area(8, 7),
  area(9, 1), area(9, 2), area(9, 3), area(9, 4), area(9, 5), area(9, 6), area(9, 7),
  area(10, 2), area(10, 3), area(10, 4), area(10, 5), area(10, 6),
  area(11, 3), area(11, 5)

)

patch <- wrap_plots(
  plots,
  design = layout, 
  width = 10
) +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = NA, color = NA)
  ))

ggsave("plots/day_3_polygons/final.png", patch,
       w = 15 * fac, h = 25 * fac, bg = pal[1], limitsize = FALSE)

img <- image_read("plots/day_3_polygons/final.png")

img |> 
  image_annotate(text = "US NATIONAL PARKS", 
                 gravity = "north", 
                 location = "+0+1000",
                 font = "Poller One",
                 size = 750,
                 kerning = 300,
                 color = pal[8]) |> 
  image_annotate(text = glue("Graphic by Spencer Schien (@MrPecners) | ",
                             "Data from the National Park Service"),
                 gravity = "north",
                 location = "+0+1800",
                 font = "El Messiri",
                 size = 175,
                 kerning = 112,
                 color = pal[7]) |> 
  image_write("plots/day_3_polygons/final_titled.png")


img <- image_read("plots/day_3_polygons/final_titled.png")

img |> 
  image_scale(geometry = "x1900") |> 
  image_write("plots/day_3_polygons/final_titled_small.png")
