library(tidyverse)
library(magick)
library(glue)
library(colorspace)
library(MetBrewer)

colors <- met.brewer("Hiroshige", 12)
swatchplot(colors)

text_color <- colors[5]

img <- image_read("temp/day_6_asia/render.png")
image_info(img)

# s <- darken("#9a9397", .1)
# 
# shadow <- "#9a9397"
# inset <- image_read("images/georgia_country/tam_inset.png")


img |> 
  image_crop(geometry = "7000x6000+0+0", gravity = "center") |> 
  image_annotate(text = "WISCONSIN", 
                 gravity = "north",
                 location = "-500+400", font = "Poller One",
                 color = text_color, kerning = 150,
                 size = 400, weight = 700) |> 
  image_annotate(text = "HMONG POPULATION",
                 gravity = "northwest",
                 location = "+3100+1000", font = "Amarante",
                 color = colors[8], kerning = 50,
                 weight = 700,
                 size = 300) |>
  image_annotate(text = glue("Graphic by Spencer Schien (@MrPecners) | ",
                             "Data from US Census Bureau"),
                 gravity = "northwest",
                 location = "+3110+1375", font = "Amarante",
                 color = colors[8],
                 kerning = 23,
                 size = 60) |>
  # image_composite(image_scale(inset, geometry = "75%x"),
  #                 gravity = "southwest",
  #                 offset = "+500+750") |> 
  image_write("temp/day_6_asia/titled.png")
