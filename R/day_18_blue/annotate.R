library(magick)
library(NatParksPalettes)

colors <- natparks.pals("Denali")

img <- image_read("plots/day_18/crater_pink_greens.png")

img |> 
  image_crop(geometry = "4500x3500-250+0", gravity = "center") |> 
  image_annotate(text = "Crater Lake", gravity = "north",
                 location = "-1600+200", font = "Denk One",
                 color = colors[1],
                 size = 175, weight = 700) |> 
  # image_annotate(text = "at Canyonlands National Park", gravity = "north",
  #                location = "-1600+1000", font = "Denk One",
  #                color = colors[4],
  #                size = 90, weight = 700) |> 
  image_annotate(text = "Graphic by Spencer Schien (@MrPecners)" , gravity = "north",
                 location = "-1600+500", font = "Denk One",
                 color = alpha(colors[1], .5),
                 size = 50, weight = 700) |> 
  image_annotate(text = "Data from Oregon Explorer", gravity = "north",
                 location = "-1600+600", font = "Denk One",
                 color = alpha(colors[1], .5),
                 size = 50, weight = 700) |> 
  image_write("plots/day_18/titled_crater_lake.png")
