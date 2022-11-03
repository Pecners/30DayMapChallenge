library(sf)
library(tidyverse)
library(elevatr)
library(rayshader)
library(glue)
library(colorspace)
library(NatParksPalettes)
library(MetBrewer)
library(scico)
library(PrettyCols)
source("R/utils/make_ellipse.R")

###################################
# Set up polygon for clipping DEM #
###################################

# Set map name that will be used in file names, and 
# to get get boundaries from master NPS list

map <- "upheaval_dome"


# Example for using specific coordinates to create a geometry, here showing
# coords for the Eye of the Sahara

coords <- rev(c(4641301.038490833, -12237225.163963055))

# heart 

r <- 10000
t <- seq(0, 2 * pi, length.out = 100)
x <- r * sin(t) ^ 3 + coords[1]
y <- (13 * r/16) * cos(t) - (5 * r/16) * cos(2 * t) - (2 * r/16) * cos(3 * t) -(r/16) * cos(4 * t) + coords[2]

heart <- tibble(x = x, y = y) %>%
  st_as_sf(., coords = c("x", "y"), crs = 3857) %>%
  summarise(do_union = FALSE) %>%
  st_cast(., to = "POLYGON")

# circle

circle1 <- make_ellipse(c(38.436694, -109.928864), r = r * 1.3, npc = 360, crs = 3857) 
circle2 <- make_ellipse(c(38.436694, -109.928864), r = r * 1.075, npc = 360, crs = 3857) 

ring <- st_difference(circle1, circle2)

# rectangle

box1 <- st_bbox(ring) |> 
  st_as_sfc() |> 
  st_buffer(r * .3)

box2 <- st_bbox(ring) |> 
  st_as_sfc() |> 
  st_buffer(r * .075)

frame <- st_difference(box1, box2)


# data <- make_ellipse(coords, r = 40000, height_factor = 1.5, width_factor = .5, 
#                      npc = 360, tilt = -25, crs = 3338) 
# Plot to review
heart |>
  ggplot() +
  geom_sf() +
  geom_sf(data = ring) +
  geom_sf(data = frame)

all <- st_union(heart, ring) |> 
  st_union(frame)

all |> 
  ggplot() +
  geom_sf()

################
# Download DEM #
################

# Get DEM using `elevatr` package. Larger Z value (max is 14)
# results in greater resolution. Higher resolution takes more compute, though -- 
# I can't always max `z` up to 14 on my machine. 

z <- 14
zelev <- get_elev_raster(all, z = z, clip = "location")
mat <- raster_to_matrix(zelev)

# When initially building your object to render, you'll want to work with
# slimmed down data so you can iterate faster. I prefer to just start with
# a `z` value of 10 above, but an alternative is to create a smaller matrix
# with rayshader::resize_matrix().

# small <- resize_matrix(mat, .25)

# Set up color palette. The `pal` argument will be used in file names,
# so it's important. `colors` will also be passed along. 

pal <- "pink_greens"

c1 <- prettycols("PinkGreens")
colors <- c(c1[c(6:9, 1:5)])
swatchplot(colors)

texture <- grDevices::colorRampPalette(colors)(256)

swatchplot(texture)

###################
# Build 3D Object #
###################

# setting shadow to 500 feet below minimum value in DEM
shadow_depth <- min(mat, na.rm = TRUE) - 500

# setting resolution to about 5x for height
res <- mean(round(terra::res(zelev))) / 2.5

# Keep this line so as you're iterating you don't forget to close the
# previous window

try(rgl::rgl.close())

# Create the initial 3D object

mat %>%
  # This adds the coloring, we're passing in our `colors` object
  height_shade(texture = texture) %>%
  plot_3d(heightmap = mat, 
          # This is my preference, I don't love the `solid` in most cases
          solid = FALSE,
          # You might need to hone this in depending on the data resolution;
          # lower values exaggerate the height
          z = res,
          # Set the location of the shadow, i.e. where the floor is.
          # This is on the same scale as your data, so call `zelev` to see the
          # min/max, and set it however far below min as you like.
          shadowdepth = shadow_depth,
          # Set the window size relatively small with the dimensions of our data.
          # Don't make this too big because it will just take longer to build,
          # and we're going to resize with `render_highquality()` below.
          windowsize = c(800,800), 
          # This is the azimuth, like the angle of the sun.
          # 90 degrees is directly above, 0 degrees is a profile view.
          phi = 90, 
          zoom = 1, 
          # `theta` is the rotations of the map. Keeping it at 0 will preserve
          # the standard (i.e. north is up) orientation of a plot
          theta = 0, 
          background = "white") 

# Use this to adjust the view after building the window object
render_camera(phi = 90, zoom = 1.25, theta = 0)


###############################
# Create High Quality Graphic #
###############################

# Set up outfile where graphic will be saved.
# Note that I am not tracking the `images` directory, and this
# is because these files are big enough to make tracking them on
# GitHub difficult. 
outfile <- str_to_lower(glue("plots/day_3/{map}_{pal}_z{z}.png"))

# Now that everything is assigned, save these objects so we
# can use then in our markup script
saveRDS(list(
  map = map,
  pal = pal,
  z = z,
  colors = colors,
  outfile = outfile,
  coords = coords
), glue("R/day_3_polygons/header.rds"))

{
  # Test write a PNG to ensure the file path is good.
  # You don't want `render_highquality()` to fail after it's 
  # taken hours to render.
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), outfile)
  }
  # I like to track when I start the render
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  render_highquality(
    # We test-wrote to this file above, so we know it's good
    outfile, 
    # See rayrender::render_scene for more info, but best
    # sample method ('sobol') works best with values over 256
    samples = 400, 
    # Turn light off because we're using environment_light
    light = FALSE, 
    # All it takes is accidentally interacting with a render that takes
    # hours in total to decide you NEVER want it interactive
    interactive = FALSE,
    preview = FALSE,
    # HDR lighting used to light the scene
    environment_light = "assets/env/phalzer_forest_01_4k.hdr",
    # environment_light = "assets/env/small_rural_road_4k.hdr",
    # Adjust this value to brighten or darken lighting
    intensity_env = 2,
    # Rotate the light -- positive values move it counter-clockwise
    rotate_env = 90,
    # This effectively sets the resolution of the final graphic,
    # because you increase the number of pixels here.
    # width = round(6000 * wr), height = round(6000 * hr),
    width = 6000, height = 6000
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}\n"))
}




