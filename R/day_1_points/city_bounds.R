library(tidyverse)
library(tigris)
library(sf)
library(glue)
library(foreach)
library(doParallel)
library(MetBrewer)
library(colorspace)

# download data

wi_places <- places("WI")

mke <- wi_places |> 
  filter(NAME == "Milwaukee")

mke_poly <- mke |> 
  st_cast(to = "POLYGON") |> 
  mutate(ind = row_number()) |> 
  select(ind, geometry)

mke_point <- mke_poly |> 
  st_cast(to = "POINT") |> 
  group_by(ind) |> 
  mutate(n = row_number())

# Set up color palette

c_pal <- met.brewer("Hiroshige", n = 14)
cp <- c_pal[7:1]
swatchplot(c_pal)
swatchplot(cp)

# first frame

first_p <- mke |> 
  st_cast("MULTILINESTRING") |> 
  ggplot() +
  # base mke map
  geom_sf(fill = NA, color = NA) +
  theme_void()

ggsave(filename = "temp/city_bounds/0000.png", 
       plot = first_p, bg = c_pal[12], 
       w = 1650, h = 2000, units = "px")


do_it <- function(p) {
  these_pts <- mke_point[c(p:(p+1)),]

  cum_pts <- mke_point[c(1:(p+1)),]
    
  t <- mke_point[p,]
  

  
  this_line <- these_pts |> 
    summarise() |> 
    st_cast("LINESTRING")
  
  if (!(t$ind == 1 & t$n == 12)) {
    cum_line <- cum_pts |> 
      group_by(ind) |> 
      summarise(do_union = FALSE) |> 
      st_cast("LINESTRING")
    
    this_p <- this_line |> 
      ggplot() +
      # base mke map
      geom_sf(data = mke, fill = NA, color = NA) +
      geom_sf(data = cum_line, color = c_pal[6]) +
      # segment of border
      geom_sf(color = c_pal[1], linewidth = 1.5) +
      geom_sf(data = these_pts[1,], color = c_pal[1],
              size = .25) +
      theme_void()
    
    ind <- str_pad(as.character(p), width = 4, side = "left", pad = "0")
    tf <- glue("temp/city_bounds/{ind}.png") 
    ggsave(filename = tf, plot = this_p, bg = c_pal[12], 
           w = 1650, h = 2000, units = "px")
  } 
}

# Set up cores for parallel rendering of plots
registerDoParallel(10)

foreach(i = c(1:(nrow(mke_point) - 1))) %dopar% do_it(p = i)




