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


data <- st_read("data/kontur_population_US_20220630.gpkg")

s <- states() |> 
  st_transform(crs = st_crs(data))

co <- s |> 
  filter(NAME == "Colorado")


co_d <- st_intersection(data, co)


bb <- st_bbox(co_d)
yind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmin"]], bb[["ymax"]])))
xind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmax"]], bb[["ymin"]])))

xind / yind

size <- 2500
rast <- st_rasterize(co_d |> 
                       select(population, geom),
                     nx = floor(size * (xind/yind)), ny = size)



mat <- matrix(rast$population, nrow = floor(size * (xind/yind)), ncol = size)

pal <- "pink_greens"

c1 <- prettycols("PinkGreens")
colors <- c(c1[c(6:8, 2:4)])
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

render_camera(phi = 50, zoom = .85, theta = 45)

outfile <- "plots/day_21/co_pop.png"


render_highquality(
  # We test-wrote to this file above, so we know it's good
  outfile, 
  # See rayrender::render_scene for more info, but best
  # sample method ('sobol') works best with values over 256
  samples = 400, 
  # Turn light off because we're using environment_light
  light = TRUE,
  lightdirection = 290,
  lightcolor = c("white", "#fff5b6"),
  lightintensity = c(400, 200),
  lightaltitude = c(20, 80),
  # All it takes is accidentally interacting with a render that takes
  # hours in total to decide you NEVER want it interactive
  interactive = FALSE,
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

# compass
# Compass rose SVG from here: https://freesvg.org/compass-rose-vector-sketch
# I edited the SVG file to change fill from black to the palette blue
rose <- read_svg("R/day_21_kontur/CompassRose.svg")

tmp <- tempfile()
png(tmp, bg = "transparent", width = 1000, height = 1000)
grid::grid.newpage()
grid::grid.draw(rose)
dev.off()

svg_tmp <- image_read(tmp)
svg_tmp

f_image <- image_blank(width = image_info(svg_tmp)$width * 1.5, 
                       height = image_info(svg_tmp)$height * 1.5, color = "none") |> 
  image_composite(svg_tmp, operator = "plus", gravity = "center") |> 
  image_annotate(gravity = "north", text = "N", font = "El Messiri", 
                 size = 200, weight = 700,
                 color = colors[3]) |> 
  image_background(color = "none") |> 
  image_scale(geometry = "100%x70%") |> 
  image_rotate(-20)

f_image


img <- image_read(outfile)

text_color <- darken(colors[3], .5)

img |> 
  image_crop(geometry = "6000x5000+0+100", gravity = "center") |> 
  image_annotate(text = "Colorado Population Density", gravity = "northwest",
                 location = "+200+200", font = "El Messiri",
                 color = text_color,
                 size = 200, weight = 700) |> 
  image_annotate(text = "Data: Kontur Population Data", gravity = "southeast",
                 location = "+200+200", font = "El Messiri",
                 color = alpha(text_color, .5),
                 size = 60, weight = 700) |> 
  image_annotate(text = "Graphic: Spencer Schien (@MrPecners)", gravity = "southeast",
                 location = "+200+100", font = "El Messiri",
                 color = alpha(text_color, .5),
                 size = 60, weight = 700) |> 
  image_composite(image_modulate(f_image, saturation = 25) |> 
                    image_scale("50%x"), 
                  gravity = "southwest", 
                  offset = "+100+100") |> 
  image_write("plots/day_21/titled_co_pop.png")
