# This script processes the raw data from eBird

library(tidyverse)
library(auk)

f_in <- "data/ebd_US-WI_relSep-2022/ebd_US-WI_relSep-2022.txt"
f_out <- "data/wi_birds.txt"

# this will read in all bird sightings
ebird_data <- f_in %>% 
  # 1. reference file
  auk_ebd() %>% 
  # 2. define filters
  # auk_species(species = "Snowy Owl") %>% 
  # auk_country(country = "US") %>% 
  # 3. run filtering
  auk_filter(file = f_out, overwrite = TRUE) %>% 
  # 4. read text file into r data frame
  read_ebd()

# this will read in red-winged blackbird sightings

redwinged_bb <- f_in %>% 
  # 1. reference file
  auk_ebd() %>% 
  # 2. define filters
  auk_species(species = "Red-winged Blackbird") %>% 
  # auk_country(country = "US") %>% 
  # 3. run filtering
  auk_filter(file = f_out, overwrite = TRUE) %>% 
  # 4. read text file into r data frame
  read_ebd()

saveRDS(redwinged_bb, "data/red-winged_blackbirds.rda")

counts <- ebird_data |> 
  group_by(common_name) |> 
  summarise(n = sum(as.numeric(observation_count), na.rm = TRUE)) |> 
  arrange(desc(n))

done <- ebird_data |> 
  mutate(time_observations_started = ifelse(is.na(time_observations_started),
                                            "12:00:00", time_observations_started),
         dt = ymd_hms(paste(observation_date, time_observations_started))) |> 
  select(common_name,
         dt, 
         latitude,
         longitude) |> 
  group_by(common_name) |> 
  mutate(n = n())

p <- done |> 
  ggplot(aes(reorder(common_name, n), dt)) +
  geom_point(alpha = .1, color = "white", size = .8) +
  scale_y_datetime(expand = c(0, 0)) +
  coord_flip() +
  theme(axis.text = element_text(color = "grey90", size = 4),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave(p, filename = "plots/bird_plots.png", bg = "black", height = 20 * 1.5, width = 15 * 1.5)

