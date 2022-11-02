library(tidyverse)
library(osmdata)
library(sf)

d_files <- list.files("data/day_2")

d <- d_files[!str_detect(d_files, "service")]


not_service <- map_df(d, function(x) {
  cat(crayon::cyan(paste("Starting", x, "\n")))
  t <- readRDS(paste("data/day_2/", x, sep = ""))
  cat(crayon::red(paste("Finished", x, "\n")))
  
  if (!is.null(t$osm_lines)) {
    t$osm_lines %>%
      select(geometry)
  } 
})

t <- readRDS("data/day_2/service.rda")

s <- t$osm_lines %>%
  select(geometry)

roads <- list(not_service = not_service,
              service = s)
