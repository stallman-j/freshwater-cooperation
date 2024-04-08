# _______________________________#
# Environment
# Clean 02: ADHI African Database of Hydrometric Indices
# 
# Stallman
# Last edited: 2024-03-27
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","freshwater-cooperation")

source(file.path(home_folder,"code","00_startup_master.R"))

# packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf,
  tmap,
  countrycode,
  data.table
)


# bring in data ----

if (!file.exists(file.path(data_external_clean,"world.rds"))) {
  
  source(file.path(code_clean,"02_clean_01_world-tmap.R"))
}

world <- readRDS(file.path(data_external_clean,"world.rds"))

africa <- world %>% filter(continent== "Africa")

# note that this is NOT downloaded by 01_download_datasets_in_use.R because there's a
# checkbox you have to okay first and that'll take RSelenium

path <- file.path(data_external_raw,
                  "ADHI_african-database-hydrometric-indices",
                  "dataverse_files",
                  "ADHI",
                  "ADHI_stations.csv")

system.time(
  adhi_csv <- data.table::fread(path, header = T)
)

adhi_data <- adhi_csv %>%
              dplyr::rename(lat = Latitude,
                            lon = Longitude
                            ) %>%
              dplyr::mutate(country_iso3c = countrycode::countrycode(Country,
                                                                     origin = "country.name",
                                                                     destination = "iso3c")) %>%
              sf::st_as_sf(coords = c("lon",
                                      "lat"),
                           crs = 4326,
                           remove = FALSE)

plot(st_geometry(adhi_data))

# theme_map() is one of my functions in 
map <- ggplot() +
  geom_sf(data = africa,
          color = "gray70",
          fill = "gray99",
          alpha = 0.5,
          linewidth = .3) +
  geom_sf(data = adhi_data,
          color = "blue",
          shape = 3) +
  labs(title = paste0("Locations of African Database of Hydrology Stations"),
       caption = c("Source: ADHI (2018) and R package tmap")) +
  theme_map() 

map

save_map(output_folder = output_maps,
         plotname = map,
         filename = paste0("adhi_stations.png"),
         width = 8,
         height = 9,
         dpi  = 300)


out_path <- file.path(data_external_clean,"ADHI")
if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories

saveRDS(adhi_data,
        file = file.path(data_external_clean,"ADHI",paste0("adhi_stations_locations.rds")))

adhi_restricted <- adhi_data %>%
                  select(ID,lat,lon,country_iso3c)

saveRDS(adhi_restricted,
        file = file.path(data_external_clean,"ADHI",paste0("adhi_stations_for_river_points.rds")))
