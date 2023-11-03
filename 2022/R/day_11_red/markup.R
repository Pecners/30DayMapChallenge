library(magick)

header <- read_rds("R/day_11_red/header.rds")
colors <- header$colors

img <- image_read("plots/day_11/eye_acadia_z12.png")

img |> 
  image_crop(geometry = "4500x4250+500+0", gravity = "west") |> 
  image_annotate(text = "(Red) Eye", gravity = "north",
                 location = "-1400+400", font = "Yanone Kaffeesatz",
                 color = colors[7],
                 size = 300, weight = 700) |> 
  image_annotate(text = "of the", gravity = "north",
                 location = "-1400+775", font = "Yanone Kaffeesatz",
                 color = colors[7],
                 size = 150) |> 
  image_annotate(text = "SAHARA", gravity = "north",
                 location = "-1400+1000", font = "Yanone Kaffeesatz",
                 color = colors[7],
                 size = 300, weight = 700) |> 
  image_annotate(text = "Graphic by Spencer Schien (@MrPecners) | Data from AWS Terrain Tiles via {elevatr}", 
                 gravity = "southeast",
                 location = "+100+50", font = "Yanone Kaffeesatz",
                 color = ggplot2::alpha(colors[7], .5),
                 size = 75) |>
  image_write("plots/day_11/titled_red_eye.png")
