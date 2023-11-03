library(tidyverse)
library(sf)
library(rayshader)
library(elevatr)
library(PrettyCols)
library(ggfx)

# set up Milwaukee boundaries
wi <- tigris::places(state = "Wisconsin")

mke <- wi |> 
  filter(NAME == "Milwaukee") |> 
  st_transform(8159)

# make hex grid
gridded <- st_make_grid(mke, square = FALSE, cellsize = 500)

# limit hex grid to mke
mke_grid <- gridded[mke] 

mke_grid |> 
  ggplot() +
  geom_sf()

# set up random index to populate grid
ind <- sample(1:length(mke_grid), size = length(mke_grid), replace = FALSE)

# not totally necessary here, but it's easier with dataframe
df <- tibble(mke_grid) |> 
  rename(geometry = mke_grid) |> 
  st_as_sf()

# Set up bounding box used to keep plot boundaries constant
bb <- st_bbox(df)

# do this to speed things up, populate multiple hexes at once
# instead of one at a time
ss <- seq(from = 1, to = length(ind), by = 3)

# filling in background

walk2(ss, 1:length(ss), function(x, y) {
  
  # this is a buffer around the hex, used to simulate glowing
  big <- st_buffer(df[ind[c(x:(x+3))],], 200)
  
  # map baby map
  df[ind[c(x:(x+3))],] |> 
    ggplot() +
    # this geom is the background, all hexes mapped so far
    geom_sf(data = df[ind[1:x],], color = "#E4C601",
            alpha = .25, size = .1, fill = "#E4C601") +
    # this geom is the glow for currently blinking
    geom_sf(data = big, size = .1, color = NA, fill = "white",
            alpha = .25) +
    # this geom is the main for the currently blinking
    geom_sf(size = .1, color = "white", fill = "white") +
    annotate(geom = "text", label = "Milwaukee en Tour Eiffel",
             x = 1.08 * bb[["xmin"]],
             y = 1.38 * bb[["ymin"]],
             family = "Aclonica",
             color = alpha("#E4C601", .75),
             size = 6) +
    annotate(geom = "text", label = "Réalisation: Spencer Schien (@MrPecners)",
             x = 1.025 * bb[["xmin"]],
             y = 1.005 * bb[["ymin"]],
             family = "Aclonica",
             size = 3,
             color = alpha("#E4C601", .5)) +
    scale_fill_identity() +
    coord_sf(xlim = c(bb[["xmax"]], bb[["xmin"]]),
             ylim = c(bb[["ymin"]], bb[["ymax"]]),
             clip = "off") +
    theme_void()
  
  ggsave(sprintf("plots/day_14/tmp2/%04i.png", y), bg = "#092E59",
         width = 6, height = 9)
  cat(crayon::cyan("Finished ", x), "\n")
})

# with complete background

walk2(ss, length(ss) + 1:length(ss), function(x, y) {
  
  # this is a buffer around the hex, used to simulate glowing
  big <- st_buffer(df[ind[c(x:(x+3))],], 200)
  
  # map baby map
  df[ind[c(x:(x+3))],] |> 
    ggplot() +
    # this geom is the background, all hexes mapped so far
    geom_sf(data = df, color = "#E4C601",
            alpha = .25, size = .1, fill = "#E4C601") +
    # this geom is the glow for currently blinking
    geom_sf(data = big, size = .1, color = NA, fill = "white",
            alpha = .25) +
    # this geom is the main for the currently blinking
    geom_sf(size = .1, color = "white", fill = "white") +
    annotate(geom = "text", label = "Milwaukee en Tour Eiffel",
             x = 1.08 * bb[["xmin"]],
             y = 1.38 * bb[["ymin"]],
             family = "Aclonica",
             color = alpha("#E4C601", .75),
             size = 6) +
    annotate(geom = "text", label = "Réalisation: Spencer Schien (@MrPecners)",
             x = 1.025 * bb[["xmin"]],
             y = 1.005 * bb[["ymin"]],
             family = "Aclonica",
             size = 3,
             color = alpha("#E4C601", .5)) +
    scale_fill_identity() +
    coord_sf(xlim = c(bb[["xmax"]], bb[["xmin"]]),
             ylim = c(bb[["ymin"]], bb[["ymax"]]),
             clip = "off") +
    theme_void()
  
  ggsave(sprintf("plots/day_14/tmp2/%04i.png", y), bg = "#092E59",
         width = 6, height = 9)
  cat(crayon::cyan("Finished ", x), "\n")
})

# make video

files <- list.files("plots/day_14/tmp2")

ff <- paste0("plots/day_14/tmp2/", files)
fff <- c(ff, rev(ff))

av::av_encode_video(fff, output = "plots/day_14/output2.mp4")
