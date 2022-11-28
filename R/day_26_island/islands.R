library(tidyverse)
library(sf)
library(tigris)
library(rayshader)
library(PrettyCols)
library(colorspace)
library(stars)
library(magick)
library(svgparser)
library(geomtextpath)
library(rnaturalearth)
library(glue)

data <- st_read("data/kontur_population_US_20220630.gpkg")

top <- together <- NULL
tmp <- data

for(i in 1:10) {
  cat(crayon::cyan(glue("Starting {i}")), "\n")
  top <- tmp |> 
    filter(population == max(population)) |> 
    bind_rows(top)
  
  together <- st_buffer(top, 1609.34 * 50) |> 
    st_union()
  
  tmp <- st_difference(tmp, together)
}


buff <- top |> 
  st_buffer(1609.34 * 50) 

buff10 <- st_intersection(data, buff)

states <- states()
skip <- c(
  "Puerto Rico",
  "Alaska",
  "Hawaii",
  "United States Virgin Islands",
  "Commonwealth of the Northern Mariana Islands",
  "American Samoa",
  "Guam"
)

skinny_states <- states |> 
  filter(!NAME %in% skip) |> 
  st_transform(crs = st_crs(buff10)) |> 
  st_union()

buff10 |> 
  ggplot() + 
  geom_sf() +
  coord_sf(crs = 5070)

buff_d <- st_transform(buff10, 5070)


bb <- st_bbox(buff_d)
yind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmin"]], bb[["ymax"]])))
xind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmax"]], bb[["ymin"]])))

xind / yind

size <- 2000
rast <- st_rasterize(buff_d |> 
                       select(population, geom),
                     nx = floor(size * (xind/yind)), ny = size)



mat <- matrix(rast$population, nrow = floor(size * (xind/yind)), ncol = size)

pal <- "day_night"

night <- "#4e82b4"
day <- "#fff5b6"
  

colors <- c(night, day, "white")
swatchplot(colors)

texture <- grDevices::colorRampPalette(colors, bias = 2)(256)

swatchplot(texture)

rgl::rgl.close()

mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat, 
          # This is my preference, I don't love the `solid` in most cases
          solid = FALSE,
          soliddepth = 0,
          # You might need to hone this in depending on the data resolution;
          # lower values exaggerate the height
          z = 25,
          # Set the location of the shadow, i.e. where the floor is.
          # This is on the same scale as your data, so call `zelev` to see the
          # min/max, and set it however far below min as you like.
          shadowdepth = 0,
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

render_camera(phi = 75, zoom = 1, theta = 0)

outfile <- "plots/day_26/islands.png"

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
    light = TRUE,
    lightdirection = c(315, 315, 45, 45),
    lightcolor = c(night, "white", day, "white"),
    lightintensity = c(600, 50, 1000, 50),
    lightaltitude = c(10, 80, 10, 80),
    # All it takes is accidentally interacting with a render that takes
    # hours in total to decide you NEVER want it interactive
    interactive = FALSE,
    preview = FALSE,
    # HDR lighting used to light the scene
    # environment_light = "assets/env/phalzer_forest_01_4k.hdr",
    # # environment_light = "assets/env/small_rural_road_4k.hdr",
    # # Adjust this value to brighten or darken lighting
    # intensity_env = 1.5,
    # # Rotate the light -- positive values move it counter-clockwise
    # rotate_env = 130,
    # This effectively sets the resolution of the final graphic,
    # because you increase the number of pixels here.
    # width = round(6000 * wr), height = round(6000 * hr),
    width = 6000, height = 6000
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"))
}

# compass
# Compass rose SVG from here: https://freesvg.org/compass-rose-vector-sketch
# I edited the SVG file to change fill from black to the palette blue

t_rose <- tempfile()
text_color <- darken(night, .5)


read_lines("R/day_21_kontur/CompassRose.svg") |> 
  gsub(x = _, pattern = "#326812", night) |> 
  gsub(x = _, pattern = "#99ce64", day) |> 
  write_lines(file = t_rose)

rose <- read_svg(t_rose)

tmp <- tempfile()
png(tmp, bg = "transparent", width = 1000, height = 1000)
grid::grid.newpage()
grid::grid.draw(rose)
dev.off()

svg_tmp <- image_read(tmp)
svg_tmp


f_image <- image_blank(width = image_info(svg_tmp)$width * 1.5, height = image_info(svg_tmp)$height * 1.5, color = "none") |> 
  image_composite(svg_tmp, operator = "plus", gravity = "center") |> 
  image_annotate(gravity = "north", text = "N", font = "El Messiri", 
                 size = 200, weight = 700,
                 color = night) |> 
  image_background(color = "none") |> 
  image_scale(geometry = "100%x85%")

f_image


img <- image_read(outfile)

annot <- glue("This map shows ten dense areas in the United States. ",
              "These areas were chosen sequentially by first selecting the ",
              "densest single 400 meter hexagonal area, creating a 50 mile ",
              "radius around the hexagon, and then finding the next densest ",
              "area outside that radius. This process was repeated until 10 ",
              "areas were defined.") |> 
  str_wrap(50)


img |> 
  image_crop(geometry = "6000x4500+0+1000", gravity = "center") |> 
  image_annotate(text = "Dense America", gravity = "north",
                 location = "+0+300", font = "El Messiri",
                 color = text_color,
                 size = 350, weight = 700) |> 
  # image_annotate(text = "Population Density", gravity = "north",
  #                location = "+0+800", font = "El Messiri",
  #                color = text_color,
  #                size = 250) |> 
  image_annotate(text = annot, gravity = "northwest",
                 location = "+400+2500", font = "El Messiri",
                 color = alpha(text_color, .75),
                 size = 125) |> 
  image_annotate(text = "Data: Kontur Population Data", gravity = "southeast",
                 location = "+200+200", font = "El Messiri",
                 color = alpha(text_color, .5),
                 size = 60, weight = 700) |> 
  image_annotate(text = "Graphic: Spencer Schien (@MrPecners)", gravity = "southeast",
                 location = "+200+100", font = "El Messiri",
                 color = alpha(text_color, .5),
                 size = 60, weight = 700) |> 
  image_composite(image_modulate(f_image, brightness = 75, saturation = 50) |> 
                    image_scale("50%x"), 
                  gravity = "south", 
                  offset = "+1500+1000") |> 
  image_write("plots/day_26/titled_top10.png")

