# General R setup ----
# Options
options(stringsAsFactors = F)
# Load new packages
pacman::p_load(ggmap, leaflet)
# Load old packages
pacman::p_load(lubridate, rgdal, raster, broom, rgeos, GISTools)
pacman::p_load(dplyr, ggplot2, ggthemes, magrittr, viridis)
# My ggplot2 theme
theme_ed <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  # panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey95", size = 0.3),
  panel.grid.major = element_line(color = "grey95", size = 0.3),
  panel.grid.minor = element_line(color = "grey95", size = 0.3),
  legend.key = element_blank())
# My directories
dir_12 <- "/Users/edwardarubin/Dropbox/Teaching/ARE212/Section12/"
dir_12b <- "/Users/edwardarubin/Dropbox/Teaching/ARE212/Section12b/"

# Loading rasters ----

# Read the Chicago lights raster
chi_lights <- raster(paste0(dir_12b, "chicagoNightLights.tif"))
# Read the Chicago police beats shapefile
chi_beats <- readOGR(paste0(dir_12, "ChicagoPoliceBeats"), "chiBeats")

# Plot the Chicago night lights raster
plot(chi_lights, col = viridis(1e3))

# Plot the Chicago night lights raster
plot(chi_lights, col = viridis(1e3))
# Plot the police beats over the night lights raster
lines(chi_beats, col = "white", lwd = 0.8)

# Raster parts ----
# R's description of the raster
chi_lights
# R's summary of the raster
chi_lights %>% summary()
# The slot names
chi_lights %>% slotNames()
# The first 6 values
chi_lights %>% getValues() %>% head()
# The first six coordinates
chi_lights %>% coordinates() %>% head()

ggplot(data = data.frame(light = getValues(chi_lights)),
  aes(x = log(light))) +
  geom_histogram(bins = 20, fill = viridis(20)) +
  ylab("Count") +
  xlab("log(radiance)") +
  ggtitle("Distribution of light radiance near Chicago",
    subtitle = "January 2017, logged radiance") +
  theme_ed

# Summarize rasters with polygons ----
# Take averages of lights raster within each police beat
beat_extract <- raster::extract(
  x = chi_lights,
  y = chi_beats,
  fun = mean,
  na.rm = T,
  sp = T)

# Summarizing rasters with points ----
# Read crime points data file
crime_df <- readRDS(paste0(dir_12, "chicagoCrime.rds"))
# Convert to tbl_df
crime_df %<>% tbl_df()
# Change names
names(crime_df) <- c("id", "date", "primary_offense",
  "arrest", "domestic", "beat", "lat", "lon")
# Drop observations missing lat or lon
crime_df %<>% filter(!is.na(lat) & !is.na(lon))
# Take subset of crimes
crime_df %<>% subset(primary_offense %in% c("HOMICIDE", "NARCOTICS"))
# Convert dates
crime_df %<>% mutate(date = mdy_hms(date))
# Define longitude and latitude as our coordinates
coordinates(crime_df) <- ~ lon + lat
# Assing a CRS to the crime points data
proj4string(crime_df) <- crs(chi_beats)
# Take the union of the beats polygons
chicago_union <- gUnaryUnion(chi_beats)
# Check which crime points are inside of Chicago
test_points <- over(crime_df, chicago_union)
# From 1 and NA to T and F (F means not in Chicago)
test_points <- !is.na(test_points)
crime_df <- crime_df[which(test_points == T), ]

crime_extract <- raster::extract(
  x = chi_lights,
  y = crime_df,
  fun = mean,
  na.rm = T,
  sp = T)

# Rasters in ggplot2 ----
# Convert raster to matrix and then to data frame
lights_df <- chi_lights %>% rasterToPoints() %>% tbl_df()
# Plot with ggplot
ggplot(data = lights_df,
  aes(x = x, y = y, fill = chicagoNightLights)) +
  geom_raster() +
  ylab("") + xlab("") +
  ggtitle("Lights at night in Chicago",
    subtitle = "January 2017") +
  scale_fill_viridis(option = "D") +
  theme_ed +
  theme(legend.position = "none", axis.text = element_blank())

# Masks ----
# Mask night lights raster with Chicago's outline
lights_masked <- mask(x = chi_lights, mask = chicago_union)
# Convert raster to matrix and then to data frame
masked_df <- lights_masked %>% rasterToPoints() %>% tbl_df()
# Plot with ggplot
ggplot(data = masked_df,
  aes(x = x, y = y, fill = chicagoNightLights)) +
  geom_raster() +
  ylab("") + xlab("") +
  ggtitle("Lights at night in Chicago, masked",
    subtitle = "January 2017") +
  scale_fill_viridis(option = "D") +
  theme_ed +
  theme(legend.position = "none", axis.text = element_blank())
# Convert crime_extract to a data frame
crime_extract %<>% tbl_df()
# Density plots
ggplot() +
  geom_density(
    data = masked_df,
    aes(x = log(chicagoNightLights), color = "All of Chicago", fill = "All of Chicago"),
    alpha = 0.6) +
  geom_density(
    data = filter(crime_extract, primary_offense == "HOMICIDE"),
    aes(x = log(chicagoNightLights), color = "Homicide", fill = "Homicide"),
    alpha = 0.6) +
  geom_density(
    data = filter(crime_extract, primary_offense == "NARCOTICS"),
    aes(x = log(chicagoNightLights), color = "Narcotics", fill = "Narcotics"),
    alpha = 0.6) +
  ylab("Density") +
  xlab("Log(Light radiance)") +
  theme_ed +
  scale_color_viridis("", discrete = T) +
  scale_fill_viridis("", discrete = T)

# Compare means
masked_df %>%
  dplyr::select(chicagoNightLights) %>%
  summary()
crime_extract %>%
  filter(primary_offense == "HOMICIDE") %>%
  dplyr::select(chicagoNightLights) %>%
  summary()
crime_extract %>%
  filter(primary_offense == "NARCOTICS") %>%
  dplyr::select(chicagoNightLights) %>%
  summary()

# Creating a raster ----
# Creating an 11-by-11 grid
our_mat <- expand.grid(x = -5:5, y = -5:5)
# Check out the head
our_mat %>% head(10)

# Convert to data frame
our_df <- our_mat %>% tbl_df()
# Set seed
set.seed(12345)
# Add random numbers
our_df %<>% mutate(value = rnorm(nrow(our_df)))

# Plotting our raster data
ggplot(our_df, aes(x, y, fill = value)) +
  geom_raster() +
  ggtitle("Our (random) raster data") +
  scale_fill_viridis("Value") +
  coord_equal() +
  theme_ed +
  theme(
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(3/4, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# Finish creating our raster
our_raster <- rasterFromXYZ(our_df)
# See if it worked
our_raster

plot(our_raster, col = viridis(1e3),
  xlim = c(-5,5), ylim = c(-5,5))

# Geocoding ----
# Geocode with Google
geo_google <- geocode(
  "207 Giannini Hall #3310 University of California Berkeley, CA 94720-3310",
  output = "more",
  source = "google")
# Geocode with DST
geo_dsk <- geocode(
  "207 Giannini Hall #3310 University of California Berkeley, CA 94720-3310",
  output = "more",
  source = "dsk")

geo_google

geo_dsk

leaflet() %>%
  addMarkers(lng = geo_google$lon, lat = geo_google$lat, popup = "Google") %>%
  addMarkers(lng = geo_dsk$lon, lat = geo_dsk$lat, popup = "DSK") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  setView(lng = -99, lat = 38, zoom = 4)

leaflet() %>%
  addMarkers(lng = geo_google$lon, lat = geo_google$lat, popup = "Google") %>%
  addProviderTiles(providers$OpenStreetMap.HOT) %>%
  setView(lng = geo_google$lon, lat = geo_google$lat, zoom = 17)

leaflet() %>%
  addMarkers(lng = geo_dsk$lon, lat = geo_dsk$lat, popup = "DSK") %>%
  addProviderTiles(providers$OpenStreetMap.HOT) %>%
  setView(lng = geo_dsk$lon, lat = geo_dsk$lat, zoom = 13)

# Fun tools: Leaflet ----
m <- leaflet() %>%
  addMarkers(lng = -96.6852, lat = 40.8258, popup = "Lincoln, NE") %>%
  setView(lng = -96.6852, lat = 40.8258, zoom = 12)

m %>% addProviderTiles(providers$CartoDB)
m %>% addProviderTiles(providers$OpenMapSurfer.Roads)
m %>% addProviderTiles(providers$OpenStreetMap.HOT)
m %>% addProviderTiles(providers$Esri.WorldImagery)
m %>% addProviderTiles(providers$Esri.WorldTerrain) %>%
  setView(lng = -96.6852, lat = 40.8258, zoom = 4)
m %>% addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012) %>%
  setView(lng = -96.6852, lat = 40.8258, zoom = 4)
