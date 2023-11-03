library(magick)
library(glue)
library(av)

dir <- "temp/city_bounds"

# first frame

first <- glue("{dir}/first.png")
img <- image_read(glue("{dir}/0000.png"))
cap <- "#30DayMapChallenge\nDay 1 - Points"
img |> 
  image_annotate(text = cap,
                 gravity = "center",
                 location = "+0-500",
                 font = "El Messiri", 
                 boxcolor = alpha("white", .75),
                 weight = 900,
                 size = 100) |> 
  image_write(first)

# second frame
second <- glue("{dir}/second.png")
cap <- "Drawing city bounds\npoint by point..."
img |> 
  image_annotate(text = cap,
                 gravity = "center",
                 location = "+0-500",
                 font = "El Messiri", 
                 boxcolor = alpha("white", .75),
                 weight = 900,
                 size = 100) |> 
  image_write(second)

# final frame
final <- glue("{dir}/final.png")
img <- image_read(glue("{dir}/4903.png"))
cap <- "Milwaukee, Wisconsin"
img |> 
  image_annotate(text = cap,
                 gravity = "center",
                 location = "+0-500",
                 font = "El Messiri", 
                 boxcolor = alpha("white", .75),
                 weight = 900,
                 size = 100) |> 
  image_write(final)
  

# make the video

this_vec <- list.files("temp/city_bounds")
these <- paste("temp/city_bounds/", this_vec, sep = "")
these <- these[which(!str_detect(these, "first|second|final|0000"))]

fps <- 200

intro <- c(
  rep(first),
  rep(second)
)

av_encode_video(input = intro, framerate = .5, 
                output = "R/Day 1 - Points/intro.mp4")

av_encode_video(input = these, framerate = 200, 
                output = "R/Day 1 - Points/main.mp4")

av_encode_video(input = rep(final, 5), framerate = 1, 
                output = "R/Day 1 - Points/end.mp4")

system(
  glue("ffmpeg -f concat -safe 0 -i mylist.txt -c copy 'R/Day 1 - Points/out.mp4'")
)

