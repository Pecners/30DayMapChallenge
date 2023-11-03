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

gl <- ne_download(type = "rivers_lake_centerlines", 
                  category = "physical", scale = "large")  %>%
  st_as_sf() 

miss <- gl |> 
  filter(name == "Missouri")

longs <- map_df(c(-90, -100, -110), function(i) {
  tibble(x = i,
             y = c(38, 49)) |> 
    st_as_sf(coords = c("x", "y"), crs = 4326) |> 
    summarise() |> 
    st_cast(to = "LINESTRING") |> 
    st_transform(crs = 5070) |> 
    st_buffer(1609.34 * .5) |> 
    transmute(geom = geometry,
              population = 1)
})

lats <- map_df(c(40, 45), function(i) {
  tibble(y = i,
         x = seq(from = -112, to = -88, by = 1)) |> 
    st_as_sf(coords = c("x", "y"), crs = 4326) |> 
    summarise(do_union = FALSE) |> 
    st_cast(to = "LINESTRING") |> 
    st_transform(crs = 5070) |> 
    st_buffer(1609.34 * .5) |> 
    transmute(geom = geometry,
              population = 1)
})

grid <- bind_rows(lats, longs)


miss |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = grid) +
  coord_sf(crs = 5070)

data <- st_read("data/kontur_population_US_20220630.gpkg")



miss <- miss |> 
  st_buffer(1609.34 * 10) |> 
  st_transform(crs = st_crs(data))

miss_d <- st_intersection(data, miss)
miss_d <- st_transform(miss_d, 5070)

miss_dd <- bind_rows(miss_d, grid)


bb <- st_bbox(miss_dd)
yind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmin"]], bb[["ymax"]])))
xind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmax"]], bb[["ymin"]])))

xind / yind

size <- 6000
rast <- st_rasterize(miss_dd |> 
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
          z = 10,
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

render_camera(phi = 45, zoom = .8, theta = -20)

outfile <- "plots/day_28/miss_pop.png"

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
  samples = 450, 
  # Turn light off because we're using environment_light
  light = TRUE,
  lightdirection = rev(c(315, 315, 45, 45)),
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
  image_scale(geometry = "100%x50%") |> 
  image_rotate(-20)


f_image


img <- image_read(outfile)

annot <- glue("This map shows population density within ten miles ",
              "of the Missouri River. Population estimates are bucketed ",
              "into 400 meter hexagons.") |> 
  str_wrap(35)


img |> 
  image_crop(geometry = "6000x5250+0+700", gravity = "center") |> 
  image_annotate(text = "Missouri River", gravity = "northwest",
                 location = "+200+100", font = "El Messiri",
                 color = text_color,
                 size = 250, weight = 700) |> 
  image_annotate(text = "Population Density", gravity = "northwest",
                 location = "+200+400", font = "El Messiri",
                 color = text_color,
                 size = 250) |> 
  image_annotate(text = "110°W", gravity = "center",
                 location = "-1700+1225", font = "El Messiri",
                 color = text_color, degrees = -10,
                 size = 75) |> 
  image_annotate(text = "100°W", gravity = "center",
                 location = "+600+800", font = "El Messiri",
                 color = text_color, degrees = -12,
                 size = 75) |> 
  image_annotate(text = "90°W", gravity = "center",
                 location = "+2800+210", font = "El Messiri",
                 color = text_color, degrees = -15,
                 size = 75) |> 
  image_annotate(text = "40°N", gravity = "center",
                 location = "-2400+825", font = "El Messiri",
                 color = text_color, degrees = -7,
                 size = 75) |> 
  image_annotate(text = "45°N", gravity = "center",
                 location = "-2675-225", font = "El Messiri",
                 color = text_color, degrees = -7,
                 size = 75) |> 
  image_annotate(text = annot, gravity = "northeast",
                 location = "+200+3750", font = "El Messiri",
                 color = alpha(text_color, .75),
                 size = 125) |> 
  image_annotate(text = "Data: Kontur Population Data", gravity = "southwest",
                 location = "+200+200", font = "El Messiri",
                 color = alpha(text_color, .5),
                 size = 60, weight = 700) |> 
  image_annotate(text = "Graphic: Spencer Schien (@MrPecners)", gravity = "southwest",
                 location = "+200+100", font = "El Messiri",
                 color = alpha(text_color, .5),
                 size = 60, weight = 700) |> 
  image_write("plots/day_28/titled_miss_pop.png")

