library(tidyverse)
library(tigris)
library(sf)
library(glue)
library(MetBrewer)
library(colorspace)

wi_places <- places("wi")
mke <- wi_places |> 
  filter(NAME == "Milwaukee")

county_roads <- roads(state = "wi", county = "milwaukee")

mke_roads <- st_intersection(county_roads, mke)

these_roads <- mke_roads |> 
  select(geometry) |> 
  st_as_sf() |> 
  mutate(type = st_geometry_type(geometry)) |> 
  filter(str_detect(type, "LINESTRING")) |> 
  mutate(ind = row_number())

west <- map_dbl(1:nrow(these_roads), function(i) {
  tmp <- these_roads[i, "geometry"]
  bb <- st_bbox(tmp)
  return(bb[["xmin"]])
})

these_roads$west <- west

done_roads <- these_roads |> 
  arrange(west) |> 
  ungroup() |> 
  st_cast("LINESTRING") |> 
  group_by(ind) |> 
  mutate(multiline_ind = row_number()) |> 
  ungroup() |> 
  st_cast("MULTIPOINT") |> 
  st_cast("POINT") |> 
  group_by(ind) |> 
  mutate(ind_n = row_number(),
         max = ifelse(ind_n == max(ind_n), TRUE, FALSE)) |> 
  ungroup() |> 
  mutate(row_n = row_number())

c_pal <- met.brewer("Hiroshige", n = 14)

do_it <- function(p) {
  these_pts <- done_roads[c(p:(p+1)),]
  
  cum_pts <- done_roads[c(1:(p+1)),]
  
  t <- done_roads[p,]
  
  
  this_line <- these_pts |> 
    summarise() |> 
    st_cast("LINESTRING")
  
  if (!t$max) {
    cum_line <- cum_pts |> 
      group_by(ind) |> 
      summarise(do_union = FALSE) |> 
      st_cast("MULTILINESTRING")
    
    this_p <- this_line |> 
      ggplot() +
      # base mke map
      geom_sf(data = mke, fill = NA, color = NA) +
      # border
      geom_sf(data = cum_line, color = c_pal[6],
              linewidth = .25) +
      # segment of border
      geom_sf(color = c_pal[1], linewidth = .25) +
      geom_sf(data = these_pts[1,], color = c_pal[1],
              size = .25) +
      theme_void()
    
    ind <- str_pad(as.character(p), width = 5, side = "left", pad = "0")
    tf <- glue("temp/roads1/{ind}.png") 
    ggsave(filename = tf, plot = this_p, bg = c_pal[12], 
           w = 1650, h = 2000, units = "px")
  } 
}

# Set up cores for parallel rendering of plots
registerDoParallel(10)

# foreach(i = 1:(nrow(done_roads) - 1)) %dopar% do_it(p = i)
foreach(i = 1:5000) %dopar% do_it(p = i)

# make the video

this_vec <- list.files("temp/roads1")
these <- paste("temp/roads1/", this_vec, sep = "")

av::av_encode_video(input = these[1:5000], framerate = 1000)

