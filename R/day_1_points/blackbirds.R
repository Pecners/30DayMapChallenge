library(tidyverse)
library(auk)
library(lubridate)
library(glue)
library(tigris)
library(sf)
library(magick)

# set up base map -------------------------------------------

wi <- counties(state = "WI") %>%
  st_transform(., crs = st_crs(4326))

l <- rnaturalearth::ne_download(type = "lakes", category = "physical", scale = "large")  %>%
  st_as_sf(., crs = st_crs(4326))

gl <- l %>% 
  filter(name %in% c("Lake Michigan", "Lake Superior")) %>%
  st_union()

wi_trim <- st_difference(wi, gl)

# read in data -------------------------------------------
# Data downloaded from eBird, then I pre-processed it to 
# facilitate using it here. 

ebird_data <- read_rds("data/red-winged_blackbirds.rda")

# clean data, create datetime var, convert to sf

f <- ebird_data |> 
  mutate(lat = latitude,
         long = longitude, 
         observation_count = as.numeric(observation_count),
         time_observations_started = ifelse(is.na(time_observations_started),
                                            "12:00:00", time_observations_started),
         dt = ymd_hms(paste(observation_date, time_observations_started)),
         wk = week(dt)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(crs = 3071) |> 
  arrange(dt)

# create vector of dates, needed because not all dates are present
# in the red-winged blackbird sightings

dates <- seq.Date(from = ymd("2021-10-01"), to = ymd("2022-09-30"), by = 7)

wks <- week(dates)

# read in Sibley image of Red-winged Blackbird
# source: https://cdn.birdwatchingdaily.com/2015/09/Sibley-Red-winged-Blackbird-Spring_1500-600x400.jpg

rwb_img <- image_read("R/day_1_points/Sibley-Red-winged-Blackbird-Spring_1500-600x400.jpeg") |> 
  image_scale(geometry = "75%")

# NOTE: I'm not using gganimate because I wanted greater control over
# the resulting graphic, and I found it easier to create my own.

# This function creates a plot for each week, then combines the 
# plots in a video using the {av} package

make_bird_plot <- function(species, fr = 2) {
  tmp_df <- f |> 
    arrange(dt)
  
  tmp_dir <- tempdir()
  
  walk2(wks, 1:length(dates), function(w, i) {
    lab <- format(dates[i], "%B %d, %Y") 
    
    p <- tmp_df |> 
      filter(wk == w) |> 
      ggplot() +
      geom_sf(data = wi_trim, size = .15,
              color = "#F7DD49", fill = "grey10") +
      geom_sf(alpha = .75, 
              size = .25, 
              color = "#BC2A11") +
      theme_void() +
      theme(text = element_text(family = "Aclonica", color = "grey10"),
            plot.title = element_text(hjust = 1, size = 18,
                                      vjust = -1),
            plot.subtitle = element_text(hjust = 1, vjust = -5),
            plot.caption = element_text(color = "grey70", size = 8,
                                        margin = margin(b = 10))) +
      labs(title = glue("Wisconsin {species} Sightings"),
           subtitle = glue("Week of {lab}"),
           caption = "Graphic by Spencer Schien (@MrPecners) | Data from eBird") +
      coord_sf(crs = 3071) +
      lims(x = c(296474.0, 768906.0)) 
    
    ind <- str_pad(i, width = 2, side = "left", pad = "0")
    
    out <- glue("{tmp_dir}/plot_{ind}.png")
    
    ggsave(p, filename = out, 
           bg = "white", 
           width = 2000, 
           height = 2000, units = "px")
    
    pl <- image_read(out) |> 
      image_composite(rwb_img, gravity = "southwest", offset = "+75+150")
    
    image_write(pl, out)

  })
  
  files <- list.files(tmp_dir)
  files <- files[which(str_detect(files, "png"))]
  dir_files <- glue("{tmp_dir}/{files}")
  av::av_encode_video(dir_files, 
                      output = glue("plots/day_1/{species}.mp4"), 
                      framerate = fr)
  unlink(tmp_dir)
}

# I originally was using the data for every bird species,
# which is why it made sense to include species as a parameter.

make_bird_plot("Red-winged Blackbird")
