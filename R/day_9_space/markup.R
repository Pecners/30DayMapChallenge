library(magick)


img <- image_read("plots/day_9/merging_galaxies/merging_galaxies_img_z1000.png")

img |> 
  image_annotate(text = "Merging Galaxies", gravity = "north",
                 location = "-1750+300", font = "IM Fell English SC",
                 size = 250, weight = 700) |> 
  # image_annotate(text = "at Canyonlands National Park", gravity = "north",
  #                location = "-1600+1000", font = "Denk One",
  #                color = colors[4],
  #                size = 90, weight = 700) |> 
  # image_annotate(text = "Data from NOAA", gravity = "north",
  #                location = "+900+2000", font = "IM Fell English SC",
  #                color = alpha(colors[1], .75),
  #                size = 75) |>
  image_annotate(text = "Graphic by Spencer Schien (@MrPecners) | Data from NASA", 
                 gravity = "southeast",
                 location = "+150+150", font = "IM Fell English SC",
                 size = 75) |>
  image_write("plots/day_9/merging_galaxies/titled_merging_galaxies.png")

