library(tidyverse)
library(sf)
library(rayshader)
library(elevatr)
library(PrettyCols)

wi <- tigris::places(state = "Wisconsin")

mke <- wi |> 
  filter(NAME == "Milwaukee") |> 
  st_transform(8159)


gridded <- st_make_grid(mke, square = FALSE, cellsize = 500)

mke_grid <- gridded[mke] 

mke_grid |> 
  ggplot() +
  geom_sf()

ind <- sample(1:length(mke_grid), size = length(mke_grid), replace = FALSE)

colors <- c("#F6A71C", "#00A6E1", "#0D213F")


df <- tibble(mke_grid) |> 
  rename(geometry = mke_grid) |> 
  st_as_sf()

cs <- sample(colors, size = nrow(df), replace = TRUE)

df <- bind_cols(df, col = cs)

bb <- st_bbox(df)

walk2(1:length(ind), 1:nrow(df), function(x, y) {
  
  df[ind[x],] |> 
    ggplot(aes(fill = col)) +
    geom_sf(data = df[ind[1:x],], color = "white",
            alpha = .25, size = .1) +
    geom_sf(size = .1, color = "white") +
    scale_fill_identity() +
    coord_sf(xlim = c(bb[["xmax"]], bb[["xmin"]]),
             ylim = c(bb[["ymin"]], bb[["ymax"]])) +
    theme_void()
  
  ggsave(sprintf("plots/day_14/tmp/%04i.png", y), bg = "white",
         width = 6, height = 9)
})

full <- df |> 
  ggplot(aes(fill = col)) +
  geom_sf(size = .1, color = "white") +
  annotate(geom = "text", label = "Milwaukee, WI",
           x = 1.08 * bb[["xmin"]],
           y = 1.38 * bb[["ymin"]],
           family = "Aclonica",
           color = colors[3],
           size = 6) +
  annotate(geom = "text", label = "Graphic by Spencer Schien (@MrPecners)",
           x = 1.025 * bb[["xmin"]],
           y = 1.005 * bb[["ymin"]],
           family = "Aclonica",
           size = 3,
           color = colors[3]) +
  scale_fill_identity() +
  coord_sf(xlim = c(bb[["xmax"]], bb[["xmin"]]),
           ylim = c(bb[["ymin"]], bb[["ymax"]])) +
  theme_void()

full

ggsave(full, filename = sprintf("plots/day_14/tmp/%i.png", length(ind) + 2),
       bg = "white",
       width = 6, height = 9)

full_pale <- df |> 
  ggplot(aes(fill = col)) +
  geom_sf(size = .1, color = "white", alpha = .25) +
  annotate(geom = "text", label = "Milwaukee, WI",
           x = 1.08 * bb[["xmin"]],
           y = 1.38 * bb[["ymin"]],
           family = "Aclonica",
           color = alpha(colors[3], .25),
           size = 6) +
  annotate(geom = "text", label = "Graphic by Spencer Schien (@MrPecners)",
           x = 1.025 * bb[["xmin"]],
           y = 1.005 * bb[["ymin"]],
           family = "Aclonica",
           size = 3,
           color = alpha(colors[3], .25)) +
  scale_fill_identity() +
  coord_sf(xlim = c(bb[["xmax"]], bb[["xmin"]]),
           ylim = c(bb[["ymin"]], bb[["ymax"]]),
           clip = "off") +
  theme_void() +
  theme(text = element_text(family = "Aclonica",
                            color = alpha(colors[3], .25)),
        plot.title = element_text(hjust = 1,
                                  vjust = -8),
        plot.caption = element_text(size = 6, vjust = 5)) 

full_pale

ggsave(full_pale, filename = sprintf("plots/day_14/tmp/%i.png", length(ind) + 1),
       bg = "white",
       width = 6, height = 9)


files <- list.files("plots/day_14/tmp")
l <- length(files)
f_files <- c(files,
             rep(c(rep(files[l], 10),
                 rep(files[l-1], 10)), 5),
             rep(files[l], 10))
             
ff <- paste0("plots/day_14/tmp/", f_files)

av::av_encode_video(ff, output = "plots/day_14/output.mp4")
