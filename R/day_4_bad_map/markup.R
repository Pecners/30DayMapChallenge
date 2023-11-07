library(tidyverse)
library(magick)
library(glue)
library(NatParksPalettes)

img <- image_read("plots/day_4_bad_map/acadia_z12.png")
inset <- image_read("plots/day_4_bad_map/inset.png")

colors <- natparks.pals("Acadia")

# Load blank plot to use as tile
# I rendered this seperately
blank <- "plots/day_4_bad_map/blank.png"
b_img <- image_read(blank)

# Create a strip 1000 px high to add padding
strip <- image_crop(b_img, geometry = "1000x4000")
pad <- image_append(rep(strip, 2), stack = TRUE)
wider <- image_append(c(img, pad))


wider |> 
  image_crop(gravity = "east", geometry = "7500x5500") |> 
  image_annotate(text = "BADWATER",
                 gravity = "center",
                 location = "+1900-1700",
                 font = "Poller One",
                 size = 400,
                 kerning = 10,
                 color = colors[1]) |> 
  image_annotate(text = "BASIN",
                 gravity = "center",
                 location = "+1900-1200",
                 font = "Poller One",
                 size = 400,
                 kerning = 200,
                 color = colors[3]) |> 
  image_annotate(text = glue("Graphic by Spencer Schien (@MrPecners) | ",
                             "Data from National Park Service"),
                 gravity = "center",
                 location = "+1900+2150",
                 font = "El Messiri",
                 size = 60,
                 kerning = 10,
                 color = colors[4]) |> 
  image_composite(image_scale(inset, geometry = "60%x"),
                  gravity = "south",
                  offset = "+1900+800") |> 
  image_write("plots/day_4_bad_map/final.png")

image_read("temp/day_4_bad_map/final.png") |> 
  image_scale(geometry = "45%x") |> 
  image_write("plots/day_4_bad_map/final_small.png")
