library(tidyverse)
library(osmdata)

# The `osmdata` package facilitates OSM overpass queries.
# This is a vector of strings that will be passed as the
# value in a key value pair, with the key always being 
# "highway". Additionally, links are queried appending
# "_link" to the value. More info on these features can be found
# here: https://wiki.openstreetmap.org/wiki/Map_features.

queries <- c("motorway",
             "trunk",
             "primary",
             "secondary",
             "tertiary",
             "residential",
             "service",
             "unclassified")

q <- c(queries, paste(queries, "_link", sep = ""))

# This code chunk is where data is queried from OSM.
# I am saving the different queries because it kept failing
# when I tried to do them all at once.

walk(q, function(x) {
  cat(crayon::cyan(paste("Starting", x, "\n")))
  t <- opq("Colorado") %>%
    add_osm_feature(key = "highway", value = x) %>%
    osmdata_sf()
  
  saveRDS(t, file = paste("data/day_2/", x, ".rda", sep = ""))
  cat(crayon::red(paste("Finished", x, "\n")))
})