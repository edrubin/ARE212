
# General R setup ----
# Options
options(stringsAsFactors = F)
# Load new packages
pacman::p_load(lubridate, rgdal, raster, broom, rgeos, GISTools)
# Load old packages
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
# My directory
dir_12 <- "/Users/edwardarubin/Dropbox/Teaching/ARE212/Section12/"

# Loading a shapefile ----
# Load the shapefile with the Chicago beats data
beats_shp <- readOGR(
  dsn = paste0(dir_12, "ChicagoPoliceBeats"),
  layer = "chiBeats")

# Class
class(beats_shp)
# Dimensions
dim(beats_shp)
# Plot
plot(beats_shp)

# Decomposing a shapefile ----
# Fied/variable names
names(beats_shp)
# Slot names
slotNames(beats_shp)

# Check the class of the object in the 'data' slot
beats_shp@data %>% class()
# Check the head of the object in the 'data' slot
beats_shp@data %>% head()
# Check the head of the object in the 'data' slot
beats_shp@data %>% dim()

# 'plotOrder' slot
beats_shp@plotOrder %>% head()
# 'bbox' slot
beats_shp@bbox
# 'proj4string' slot
beats_shp@proj4string

# Find the class
beats_shp@polygons %>% class()
# Find the length of the list
beats_shp@polygons %>% length()

# Class of the first element in the 'polygons' slot
beats_shp@polygons[[1]] %>% class()

# Slot names for a polygon
beats_shp@polygons[[1]] %>% slotNames()

# What is the class?
beats_shp@polygons[[1]]@Polygons %>% class()
# What is the length?
beats_shp@polygons[[1]]@Polygons %>% length()

# Deep in the rabbit hole
beats_shp@polygons[[1]]@Polygons[[1]] %>% slotNames()

# The head of the coordinates
beats_shp@polygons[[1]]@Polygons[[1]]@coords %>% head()
# The dimensions of the coordinates
beats_shp@polygons[[1]]@Polygons[[1]]@coords %>% dim()
# Plot the coordinates
beats_shp@polygons[[1]]@Polygons[[1]]@coords %>% plot()

subset(beats_shp, beat_num == "1713") %>% plot()

# Building a shapefile ----

# Create coordinates in data.frame
coord_df <- data.frame(
  x = c(0, 0, 1, 2, 1, 1),
  y = c(0, 1, 1, 0, 0, -1))
# Convert data.frame to matrix
coord_mat <- coord_df %>% as.matrix()
# Create a Polygon
the_polygon <- Polygon(coord_mat)
# Create the Polygons object
the_polygons <- Polygons(list(the_polygon), 1)
# Create the SpatialPolygons object
the_sp <- SpatialPolygons(list(the_polygons))
# Create a SpatialPolygonsDataFrame
the_spdf <- SpatialPolygonsDataFrame(the_sp,
  data = data.frame(a = 12, b = 6))

# Plot 'the_spdf'
plot(the_spdf)
# Check the data slot
the_spdf@data

# Plotting the shapefile with ggplot2 ----

# Tidy the shapefile
beats_df <- tidy(beats_shp)
# Look at the first three rows
beats_df %>% head(3)

# Plot the tidied shapefile
ggplot(data = beats_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey90") +
  geom_path(size = 0.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Chicago police beats") +
  theme_ed +
  coord_map()

# Points data ----

# Read crime points data file
crime_df <- readRDS(paste0(dir_12, "chicagoCrime.rds"))
# Convert to tbl_df
crime_df %<>% tbl_df()
# Check it out
crime_df

# Change names
names(crime_df) <- c("id", "date", "primary_offense",
  "arrest", "domestic", "beat", "lat", "lon")
# Drop observations missing lat or lon
crime_df %<>% filter(!is.na(lat) & !is.na(lon))
# Convert dates
crime_df %<>% mutate(date = mdy_hms(date))
# Check it out again
crime_df

# Define longitude and latitude as our coordinates
coordinates(crime_df) <- ~ lon + lat

# Check class
crime_df %>% class()
# Check head
crime_df %>% head()
# Check slot names
crime_df %>% slotNames()

# Check out the coords slot
crime_df@coords %>% head()

# Check the projection
crime_df@proj4string

# Assing a CRS to the crime points data
proj4string(crime_df) <- crs(beats_shp)
# See if it worked
crime_df@proj4string

# Points in polygons ----

# Take subset of crimes
crime_df %<>% subset(primary_offense %in% c("HOMICIDE", "NARCOTICS"))

# Take the union of the beats polygons
chicago_union <- gUnaryUnion(beats_shp)
# Plot the union
plot(chicago_union)

# Check which crime points are inside of Chicago
test_points <- over(crime_df, chicago_union)
# From 1 and NA to T and F (F means not in Chicago)
test_points <- !is.na(test_points)

# ALTERNATIVE
# crime_xy <- crime_df@coords
# chicago_xy <- chicago_union@polygons[[1]]@Polygons[[1]]@coords
# test_points2 <- point.in.polygon(
#   crime_xy[,1], crime_xy[,2],
#   chicago_xy[,1], chicago_xy[,2])

crime_df <- crime_df[which(test_points == T), ]

plot(chicago_union, col = "grey80")
plot(crime_df, pch = 20, add = T)

# Plotting points data with ggplot2 ----

# Copy the spatial points data frame, renaming
crime_spdf <- crime_df
# Convert the spatial points data frame to tbl_df
crime_df <- crime_df %>% tbl_df()
# Check classes
crime_spdf %>% class()
crime_df %>% class()

# Add a factor to crime_df
crime_df %<>% mutate(crime_fac =
  factor(primary_offense,
    levels = c("HOMICIDE", "NARCOTICS"),
    labels = c("Homicide", "Narcotics")
    ))
# Add year
crime_df %<>% mutate(year = year(date))
# The maps
ggplot(data = crime_df) +
  geom_path(data = beats_df, aes(long, lat, group = group),
    size = 0.3, color = "grey60") +
  geom_point(aes(lon, lat, color = crime_fac, alpha = crime_fac),
    size = 0.5) +
  xlab("") + ylab("") +
  ggtitle("Spatial distribution of homicides and narcotics offenses",
    subtitle = "Chicago, 2010 to present") +
  theme_ed +
  theme(legend.position = "none", axis.text = element_blank()) +
  scale_color_manual(values = viridis(2, end = 0.6)) +
  scale_alpha_manual(values = c(0.5, 0.05)) +
  coord_map() +
  facet_grid(. ~ crime_fac)

# The maps
ggplot(data = subset(crime_df, year %in% c(2010, 2013, 2016))) +
  geom_path(data = beats_df, aes(long, lat, group = group),
    size = 0.3, color = "grey60") +
  geom_point(aes(lon, lat, color = crime_fac, alpha = crime_fac),
    size = 0.5) +
  xlab("") + ylab("") +
  ggtitle("Spatial distribution of homicides and narcotics offenses",
    subtitle = "Chicago, 2010 to present") +
  theme_ed +
  theme(legend.position = "none", axis.text = element_blank()) +
  scale_color_manual(values = viridis(2, end = 0.6)) +
  scale_alpha_manual(values = c(0.5, 0.1)) +
  coord_map() +
  facet_grid(year ~ crime_fac)

# Aggregating points to polygons ----

# poly.counts(pts = crime_spdf, polys = beats_shp)

# Add 'year' to spatial points data frame
crime_spdf@data %<>% mutate(year = year(date))

# Count homicides by year
homicide_beats <- lapply(X = 2010:2016, FUN = function(y) {
  # Count points in polygons
  tmp <- poly.counts(
    pts = subset(crime_spdf, (year == y) & (primary_offense == "HOMICIDE")),
    polys = beats_shp)
  # Create data frame
  tmp_df <- data.frame(
    id = names(tmp),
    count = tmp)
  # Change names
  names(tmp_df)[2] <- paste0("hom_", y)
  # Return tmp_df
  return(tmp_df)
  })
# Count narcotics offenses by year
narcotics_beats <- lapply(X = 2010:2016, FUN = function(y) {
  # Count points in polygons
  tmp <- poly.counts(
    pts = subset(crime_spdf, (year == y) & (primary_offense == "NARCOTICS")),
    polys = beats_shp)
  # Create data frame
  tmp_df <- data.frame(
    id = names(tmp),
    count = tmp)
  # Change names
  names(tmp_df)[2] <- paste0("narc_", y)
  # Return tmp_df
  return(tmp_df)
  })

# Bind the lists together
narc_df <- bind_cols(narcotics_beats)
hom_df <- bind_cols(homicide_beats)
# Select unique columns
narc_df <- narc_df[, !duplicated(names(narc_df))] %>% tbl_df()
hom_df <- hom_df[, !duplicated(names(hom_df))] %>% tbl_df()

# Add row names
beats_shp@data$id <- rownames(beats_shp@data)
# Join the count data
beats_shp@data <- left_join(x = beats_shp@data, y = narc_df, by = "id")
beats_shp@data <- left_join(x = beats_shp@data, y = hom_df, by = "id")
# Re-create data frame
beats_df <- tidy(beats_shp, region = "id")
# Join the beats counts (again)
beats_df %<>% left_join(x = ., y = narc_df, by = "id")
beats_df %<>% left_join(x = ., y = hom_df, by = "id")

ggplot(beats_df, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = hom_2016), color = "black", size = 0.07) +
  xlab("") + ylab("") +
  ggtitle("Homicides by police beat, Chicago 2016") +
  theme_ed +
  theme(axis.text = element_blank()) +
  scale_fill_viridis("Homicides within police beat",
    option = "B") +
  coord_map()

ggplot(beats_df, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = hom_2016 - hom_2010), color = "black", size = 0.07) +
  xlab("") + ylab("") +
  ggtitle("Homicide change between 2010 and 2016, by police beat, Chicago") +
  theme_ed +
  theme(axis.text = element_blank()) +
  scale_fill_viridis("Change in number of homicides within police beat",
    option = "B") +
  coord_map()

ggplot(beats_df, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = narc_2016), color = "black", size = 0.07) +
  xlab("") + ylab("") +
  ggtitle("Narcotics by police beat, Chicago 2016") +
  theme_ed +
  theme(axis.text = element_blank()) +
  scale_fill_viridis("Narcotics offenses within police beat",
    option = "B") +
  coord_map()

ggplot(beats_df, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = narc_2016 - narc_2010), color = "black", size = 0.07) +
  xlab("") + ylab("") +
  ggtitle("Narcotics change between 2010 and 2016, by police beat, Chicago") +
  theme_ed +
  theme(axis.text = element_blank()) +
  scale_fill_viridis("Change in number of narcotics offenses within police beat",
    option = "B") +
  coord_map()
