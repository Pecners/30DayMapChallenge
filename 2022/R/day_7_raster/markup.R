library(magick)


img <- image_read("plots/day_7/michigan_glacier_arches2.png")

img |> 
  image_crop(geometry = "3500x4500", gravity = "center") |> 
  image_annotate(text = "Lake Michigan", gravity = "north",
                 location = "+900+1750", font = "IM Fell English SC",
                 color = colors[1],
                 size = 200, weight = 700) |> 
  # image_annotate(text = "at Canyonlands National Park", gravity = "north",
  #                location = "-1600+1000", font = "Denk One",
  #                color = colors[4],
  #                size = 90, weight = 700) |> 
  image_annotate(text = "Data from NOAA", gravity = "north",
                 location = "+900+2000", font = "IM Fell English SC",
                 color = alpha(colors[1], .75),
                 size = 75) |>
  image_annotate(text = "Graphic by Spencer Schien (@MrPecners)", gravity = "north",
                 location = "+900+2100", font = "IM Fell English SC",
                 color = alpha(colors[1], .75),
                 size = 75) |>
  image_write("plots/day_7/titled_lake_michigan.png")

