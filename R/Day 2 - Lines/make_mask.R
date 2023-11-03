library(tidyverse)
library(magick)
library(glue)
library(foreach)
library(doParallel)

mke_bb <- st_bbox(mke) |> 
  st_as_sfc()

mke_neg <- st_difference(mke_bb, mke)

mke_neg |> 
  ggplot() +
  geom_sf(fill = c_pal[12], color = NA) +
  theme_void()

ggsave("temp/helper/mke_roads_cutter.png", background = "transparent",
       w = 1650, h = 2000, units = "px")

mask <- image_read("temp/helper/mke_roads_cutter.png")

orig_files <- list.files("temp/roads")

mask_orig <- function(f) {

  orig <- glue("temp/roads/{f}")
  img <- image_read(orig)
  
  image_composite(img, mask) |> 
    image_write(glue("temp/roads_new/{f}"))
}

# Set up cores for parallel rendering of plots
registerDoParallel(10)

# foreach(i = 1:(nrow(done_roads) - 1)) %dopar% do_it(p = i)
foreach(i = orig_files) %dopar% mask_orig(f = i)

