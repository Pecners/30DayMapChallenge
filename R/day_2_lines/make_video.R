library(magick)
library(glue)
library(av)
library(tigris)
library(MetBrewer)

wi_places <- places("wi")
mke <- wi_places |> 
  filter(NAME == "Milwaukee")

dir <- "temp/roads_new"

# first frame

c_pal <- met.brewer("Hiroshige", n = 14)


first_p <- mke |> 
  ggplot() +
  # base mke map
  geom_sf(fill = NA, color = NA) +
  theme_void()

ggsave(filename = "temp/roads_new/0000.png", 
       plot = first_p, bg = c_pal[12], 
       w = 1650, h = 2000, units = "px")

first <- glue("{dir}/first.png")
img <- image_read(glue("{dir}/0000.png"))
cap <- "#30DayMapChallenge\nDay 2 - Lines"
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
cap <- "Drawing city roads\npoint by point..."
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
img <- image_read(glue("{dir}/76107.png"))
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

this_vec <- list.files("temp/roads_new")
these <- paste("temp/roads_new/", this_vec, sep = "")
these <- these[which(!str_detect(these, "first|second|final|00000"))]

fps <- 200

intro <- c(
  rep(first),
  rep(second)
)

av_encode_video(input = intro, framerate = .5, 
                output = "R/Day 1 - Points/intro.mp4")

av_encode_video(input = these, framerate = 1000, 
                output = "R/Day 2 - Lines/main.mp4")

av_encode_video(input = rep(final, 5), framerate = 1, 
                output = "R/Day 1 - Points/end.mp4")

system(
  glue("ffmpeg -f concat -safe 0 -i mylist.txt -c copy 'R/Day 1 - Points/out.mp4'")
)

