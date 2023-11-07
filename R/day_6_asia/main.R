library(tidyverse)
library(tidycensus)
library(tigris)
library(rnaturalearth)
library(sf)

states <- states()

wi <- states |> 
  filter(NAME == "Wisconsin")

l <- ne_download(type = "lakes", category = "physical", scale = "large")  %>%
  st_as_sf(., crs = st_crs(states))

lakes <- c("Lake Erie",
           "Lake Michigan",
           "Lake Superior",
           "Lake Huron",
           "Lake Ontario")
gl <- l %>%
  filter(name %in% lakes) %>%
  st_transform(crs = st_crs(wi)) |> 
  st_union()

wi_skinny <- st_difference(wi, gl) |> 
  st_transform(crs = 3070)

groups <- get_pop_groups(year = "2020", sumfile = "ddhca")

dots <- get_decennial(geography = "tract",
                      variables = "T01001_001N",
                      state = "WI",
                      sumfile = "ddhca",
                      pop_group = "all",
                      pop_group_label = TRUE, 
                      geometry = TRUE)

hmong <- dots |> 
  filter(pop_group == "3823") |> 
  as_dot_density(value = "value",
                 values_per_dot = 22) |> 
  st_transform(crs = 3070)

wi_hex <- wi_skinny |> 
  st_transform(crs = 3070) |> 
  st_make_grid(square = FALSE, 4000) |> 
  st_as_sf()

# wi_hex |> 
#   ggplot() +
#   geom_sf()


wi_hex_skinny <- st_join(wi_hex, wi_skinny, left = FALSE) |> 
  mutate(hex_ind = row_number())

hex_dots <- st_join(hmong, wi_hex_skinny) |> 
  as_tibble() |> 
  mutate(count = 22) |> 
  group_by(hex_ind) |> 
  summarise(total = sum(count))

hex_summed <- left_join(wi_hex_skinny, hex_dots) |> 
  select(hex_ind, total, geometry)


hex_summed |> 
  ggplot(aes(fill = total)) +
  geom_sf(color = NA)

saveRDS(hex_summed, "R/day_6_asia/hex_summed.rda")
saveRDS(wi_skinny, "R/day_6_asia/wi_skinny.rda")
