library(RSQLite)
library(dplyr)
library(ggplot2)
library(broom)
library(lubridate)
library(readr)
library(ggspatial)
library(sf)
library(prettymapr)
library(raster)
library(gridExtra)
library(ggeffects)
library(forcats)
library(stringr)
library(tidytext)
library(grid)
library(tidyr)
# SQL Preprocessing

dcon <- dbConnect(SQLite(), dbname = "data/Collisions_DB.db")

dbExecute(dcon, "
CREATE VIEW IF NOT EXISTS crashes_enriched AS
WITH b AS (
  SELECT
    -- map raw columns to helper names
    substr(\"CRASH DATE\",7,4) || '-' || substr(\"CRASH DATE\",1,2) || '-' || substr(\"CRASH DATE\",4,2) AS crash_date_iso,
    \"CRASH TIME\"                                   AS crash_time_raw,
    \"BOROUGH\"                                      AS borough,
    \"ZIP CODE\"                                     AS zip_code,
    CAST(\"LATITUDE\"  AS REAL)                      AS latitude,
    CAST(\"LONGITUDE\" AS REAL)                      AS longitude,
    \"LOCATION\"                                     AS location,
    \"ON STREET NAME\"                               AS on_street_name,
    \"CROSS STREET NAME\"                            AS cross_street_name,
    \"OFF STREET NAME\"                              AS off_street_name,
    \"NUMBER OF PERSONS INJURED\"     AS number_of_persons_injured,
  \"NUMBER OF PERSONS KILLED\"      AS number_of_persons_killed,
  \"NUMBER OF PEDESTRIANS INJURED\" AS number_of_pedestrians_injured,
  \"NUMBER OF PEDESTRIANS KILLED\"  AS number_of_pedestrians_killed,
  \"NUMBER OF CYCLIST INJURED\"     AS number_of_cyclist_injured,
  \"NUMBER OF CYCLIST KILLED\"      AS number_of_cyclist_killed,
  \"NUMBER OF MOTORIST INJURED\"    AS number_of_motorist_injured,
  \"NUMBER OF MOTORIST KILLED\"     AS number_of_motorist_killed,
    \"CONTRIBUTING FACTOR VEHICLE 1\"                AS contributing_factor_vehicle_1,
    \"CONTRIBUTING FACTOR VEHICLE 2\"                AS contributing_factor_vehicle_2,
    \"CONTRIBUTING FACTOR VEHICLE 3\"                AS contributing_factor_vehicle_3,
    \"CONTRIBUTING FACTOR VEHICLE 4\"                AS contributing_factor_vehicle_4,
    \"CONTRIBUTING FACTOR VEHICLE 5\"                AS contributing_factor_vehicle_5,
    \"COLLISION_ID\"                                 AS collision_id,
    \"VEHICLE TYPE CODE 1\"                          AS vehicle_type_code_1,
    \"VEHICLE TYPE CODE 2\"                          AS vehicle_type_code_2,
    \"VEHICLE TYPE CODE 3\"                          AS vehicle_type_code_3,
    \"VEHICLE TYPE CODE 4\"                          AS vehicle_type_code_4,
    \"VEHICLE TYPE CODE 5\"                          AS vehicle_type_code_5
  FROM crashes
)
SELECT
  -- original columns with R-friendly names
  date(crash_date_iso)                               AS \"CRASH.DATE\",
  crash_time_raw                                     AS \"CRASH.TIME\",
  borough                                            AS \"BOROUGH\",
  zip_code                                           AS \"ZIP.CODE\",
  latitude                                           AS \"LATITUDE\",
  longitude                                          AS \"LONGITUDE\",
  location                                           AS \"LOCATION\",
  on_street_name                                     AS \"ON.STREET.NAME\",
  cross_street_name                                  AS \"CROSS.STREET.NAME\",
  off_street_name                                    AS \"OFF.STREET.NAME\",
  number_of_persons_injured                          AS \"NUMBER.OF.PERSONS.INJURED\",
  number_of_persons_killed                           AS \"NUMBER.OF.PERSONS.KILLED\",
  number_of_pedestrians_injured                      AS \"NUMBER.OF.PEDESTRIANS.INJURED\",
  number_of_pedestrians_killed                       AS \"NUMBER.OF.PEDESTRIANS.KILLED\",
  number_of_cyclist_injured                          AS \"NUMBER.OF.CYCLIST.INJURED\",
  number_of_cyclist_killed                           AS \"NUMBER.OF.CYCLIST.KILLED\",
  number_of_motorist_injured                         AS \"NUMBER.OF.MOTORIST.INJURED\",
  number_of_motorist_killed                          AS \"NUMBER.OF.MOTORIST.KILLED\",
  contributing_factor_vehicle_1                      AS \"CONTRIBUTING.FACTOR.VEHICLE.1\",
  contributing_factor_vehicle_2                      AS \"CONTRIBUTING.FACTOR.VEHICLE.2\",
  contributing_factor_vehicle_3                      AS \"CONTRIBUTING.FACTOR.VEHICLE.3\",
  contributing_factor_vehicle_4                      AS \"CONTRIBUTING.FACTOR.VEHICLE.4\",
  contributing_factor_vehicle_5                      AS \"CONTRIBUTING.FACTOR.VEHICLE.5\",
  collision_id                                       AS \"COLLISION_ID\",
  vehicle_type_code_1                                AS \"VEHICLE.TYPE.CODE.1\",
  vehicle_type_code_2                                AS \"VEHICLE.TYPE.CODE.2\",
  vehicle_type_code_3                                AS \"VEHICLE.TYPE.CODE.3\",
  vehicle_type_code_4                                AS \"VEHICLE.TYPE.CODE.4\",
  vehicle_type_code_5                                AS \"VEHICLE.TYPE.CODE.5\",
  -- derived fields
  strftime('%Y-%m', crash_date_iso)                  AS YEAR_MONTH,
  strftime('%Y',    crash_date_iso)                  AS YEAR,
  '(' || printf('%.4f', latitude) || ', ' ||
        printf('%.4f', longitude) || ')'             AS round_location,
  CASE
    WHEN instr(crash_time_raw, ':') > 0
      THEN CAST(substr(crash_time_raw, 1, instr(crash_time_raw, ':') - 1) AS INTEGER)
    ELSE NULL
  END                                                AS HOUR,
  CASE
    WHEN date(crash_date_iso) <  date('2020-03-01')                          THEN 'Pre-COVID'
    WHEN date(crash_date_iso) >= date('2020-03-01')
     AND date(crash_date_iso) <  date('2022-01-01')                          THEN 'During COVID'
    ELSE 'Post-COVID'
  END                                                AS covid_period,
  CASE
    WHEN date(crash_date_iso) >= date('2020-03-01')
     AND date(crash_date_iso) <  date('2022-01-01')                          THEN 1 ELSE 0
  END                                                AS is_covid,
  CASE
    WHEN date(crash_date_iso) >= date('2022-01-01')                          THEN 1 ELSE 0
  END                                                AS is_post_covid
FROM b;
")

dbExecute(dcon, "
CREATE VIEW IF NOT EXISTS vehicles_enriched AS
WITH v AS (
  SELECT
    -- original columns
    \"UNIQUE_ID\", \"COLLISION_ID\", \"CRASH_DATE\", \"CRASH_TIME\",
    \"VEHICLE_ID\", \"STATE_REGISTRATION\", \"VEHICLE_TYPE\", \"VEHICLE_MAKE\",
    \"VEHICLE_MODEL\", \"VEHICLE_YEAR\", \"TRAVEL_DIRECTION\", \"VEHICLE_OCCUPANTS\",
    \"DRIVER_SEX\", \"DRIVER_LICENSE_STATUS\", \"DRIVER_LICENSE_JURISDICTION\", \"PRE_CRASH\",
    \"POINT_OF_IMPACT\", \"VEHICLE_DAMAGE\", \"VEHICLE_DAMAGE_1\", \"VEHICLE_DAMAGE_2\",
    \"VEHICLE_DAMAGE_3\", \"PUBLIC_PROPERTY_DAMAGE\", \"PUBLIC_PROPERTY_DAMAGE_TYPE\",
    \"CONTRIBUTING_FACTOR_1\", \"CONTRIBUTING_FACTOR_2\",

    -- helpers
    substr(\"CRASH_DATE\",7,4) || '-' || substr(\"CRASH_DATE\",1,2) || '-' || substr(\"CRASH_DATE\",4,2) AS crash_date_iso,
    CAST(\"VEHICLE_YEAR\" AS INTEGER)      AS veh_year_int,
    CAST(\"VEHICLE_OCCUPANTS\" AS INTEGER) AS veh_occ_int
  FROM vehicles
)
SELECT
  v.*,
  CAST(strftime('%Y', crash_date_iso) AS INTEGER)                  AS year_int,
  CASE
    WHEN date(crash_date_iso) <  date('2020-03-01')                THEN 'Pre-COVID'
    WHEN date(crash_date_iso) >= date('2020-03-01')
     AND date(crash_date_iso) <  date('2022-01-01')                THEN 'During COVID'
    ELSE 'Post-COVID'
  END                                                              AS covid_period
FROM v;
")

dbExecute(dcon, "
CREATE VIEW IF NOT EXISTS vehicle_agg AS
SELECT
  COLLISION_ID,
  COUNT(*)                                                     AS VEHICLE_COUNT,
  AVG(CASE
        WHEN veh_occ_int BETWEEN 1 AND 60                      THEN veh_occ_int
      END)                                                     AS AV_PASSENGERS,
  AVG(CASE
        WHEN veh_year_int BETWEEN 1920 AND 2025
         AND veh_year_int <= year_int                          THEN veh_year_int
      END)                                                     AS AV_VEH_YEAR
FROM vehicles_enriched
GROUP BY COLLISION_ID;
")

nyc <- dbGetQuery(dcon, "
SELECT
  ce.*,                            
  va.VEHICLE_COUNT,
  va.AV_PASSENGERS,
  va.AV_VEH_YEAR,
  CAST(ce.YEAR AS INTEGER) - va.AV_VEH_YEAR AS AV_VEH_AGE
FROM crashes_enriched AS ce
LEFT JOIN vehicle_agg   AS va
  ON ce.COLLISION_ID = va.COLLISION_ID;
")

nyc$covid_period <- factor(nyc$covid_period, 
                           levels = c("Pre-COVID","During COVID","Post-COVID"))

nyc$BOROUGH <- factor(nyc$BOROUGH)

nyc <- nyc %>%
  mutate(across(
    c(NUMBER.OF.PERSONS.INJURED,
      NUMBER.OF.PERSONS.KILLED,
      NUMBER.OF.PEDESTRIANS.INJURED,
      NUMBER.OF.PEDESTRIANS.KILLED,
      NUMBER.OF.CYCLIST.INJURED,
      NUMBER.OF.CYCLIST.KILLED,
      NUMBER.OF.MOTORIST.INJURED,
      NUMBER.OF.MOTORIST.KILLED),
    as.integer
  ))



draw_color_key <- function(x = unit(0.9, "npc"),
                           y = unit(0.1, "npc"),
                           height = unit(0.8, "npc"),
                           width  = unit(0.03, "npc"),
                           n = 100,
                           breaks = NULL,
                           col_fun = pal) {
  
  # Generate colors
  cols <- col_fun(n)
  
  # One rect per color
  for (i in seq_len(n)) {
    grid.rect(
      x = x,
      y = y + height * (i - 1) / n,
      width = width,
      height = height / n,
      just = c("center", "bottom"),
      gp = grid::gpar(fill = cols[i], col = NA)
    )
  }
  
  # Optional: add labels (if breaks provided)
  if (!is.null(breaks)) {
    for (b in breaks) {
      pos <- y + height * b
      grid.text(label = sprintf("%.2f", b),
                x = x + width * 1.2,
                y = pos,
                just = "left",
                gp = grid::gpar(cex = 0.7))
    }
  }
}


# COMBINED W LOUIE CODE

library(sf)

# Data Processing
nyc_pre <- filter(nyc, covid_period == "Pre-COVID", round_location != "(0.0000, 0.0000)")
nyc_post <- filter(nyc, covid_period == "Post-COVID", round_location != "(0.0000, 0.0000)")
grid_size <- 50
xs <- seq(-74.3, -73.6, length.out=grid_size)
ys <- seq(40.4, 41.0, length.out=grid_size)
  
pre_count <- nyc_pre %>%
  mutate(
    x_bin = findInterval(LONGITUDE, xs),
    y_bin = findInterval(LATITUDE, ys)
  ) %>%
  filter(!is.na(x_bin), !is.na(y_bin),
         x_bin >= 1, x_bin < length(xs),
         y_bin >= 1, y_bin < length(ys)) %>%
  mutate(
    x_center = (xs[x_bin] + xs[x_bin + 1]) / 2,
    y_center = (ys[y_bin] + ys[y_bin + 1]) / 2
  ) %>%
  count(x_center, y_center, name = "pre_count")


post_count <- nyc_post %>%
  mutate(
    x_bin = findInterval(LONGITUDE, xs),
    y_bin = findInterval(LATITUDE, ys)
  ) %>%
  filter(!is.na(x_bin), !is.na(y_bin),
         x_bin >= 1, x_bin < length(xs),
         y_bin >= 1, y_bin < length(ys)) %>%
  mutate(
    x_center = (xs[x_bin] + xs[x_bin + 1]) / 2,
    y_center = (ys[y_bin] + ys[y_bin + 1]) / 2
  ) %>%
  count(x_center, y_center, name = "post_count")


a <- 0.01 #arrow length scale


full_count <- full_join(pre_count, post_count, by = c("x_center", "y_center")) %>%
  replace_na(list(pre_count <2, post_count <2)) %>%
  mutate(total_count = pre_count+post_count) %>%
  filter(total_count != 0) %>%
  mutate(angle = case_when(
    pre_count == 0 ~ pi/2,                      
    post_count == 0 ~ -pi/2,                     
    TRUE ~ atan(0.2*(log(post_count / pre_count)^5))),
    #TRUE ~ pi/2*tanh(log(post_count / pre_count))),
    angle_360 = angle*180/pi,
    x_end = x_center + a*cos(angle),
    y_end = y_center + a*sin(angle),
    total_count = total_count)
head(full_count)
start_x <- full_count$x_center
start_y <- full_count$y_center
end_x <- full_count$x_end
end_y <- full_count$y_end
total_count <- full_count$total_count

t <- (total_count - min(total_count)) / (max(total_count) - min(total_count))
colors <- rev(hcl.colors(n=100, palette= "YlOrRd"))[as.numeric(cut(t, breaks = 100))]
norm_count <- datawizard::normalize(total_count)

# Read shapefile
nyc_data <- st_read("data/nybb_25c/nybb.shp")

# Convert to lat/lon
nyc_geographic <- st_transform(nyc_data, crs = 4326)

# Extract polygons
extract_all_landmasses <- function(sf_object) {
  landmass_list <- list()
  
  for (i in 1:nrow(sf_object)) {
    borough_name <- sf_object$BoroName[i]
    borough_coords <- st_coordinates(sf_object$geometry[i])
    
    unique_polygons <- unique(borough_coords[, "L2"])
    
    for (poly_id in unique_polygons) {
      poly_mask <- borough_coords[, "L2"] == poly_id
      poly_data <- borough_coords[poly_mask, c("X", "Y")]  # lon, lat
      
      landmass_id <- paste(borough_name, "Landmass", poly_id)
      landmass_list[[landmass_id]] <- list(
        borough     = borough_name,
        poly_id     = poly_id,
        coordinates = poly_data
      )
    }
  }
  
  landmass_list
}

landmasses_geo <- extract_all_landmasses(nyc_geographic)

# Bounds for dataViewport
all_lon <- unlist(lapply(landmasses_geo, \(x) x$coordinates[,1]))
all_lat <- unlist(lapply(landmasses_geo, \(x) x$coordinates[,2]))

x_range <- range(all_lon)
y_range <- range(all_lat)

# Borough colors
borough_colors <- c(
  "Staten Island" = "red",
  "Bronx"         = "blue",
  "Brooklyn"      = "darkgreen",
  "Queens"        = "orange",
  "Manhattan"     = "purple"
)

# Draw with native coordinates
grid.newpage()
grid.rect(gp = grid::gpar(fill = "#6699FF"))
outer_vp <- plotViewport(margins = c(5.1, 4.1, 4.1, 2.1))
pushViewport(outer_vp)
inner_vp <- dataViewport(x_range, y_range)
pushViewport(inner_vp)

# Draw polygons directly in lon/lat
for (landmass_name in names(landmasses_geo)) {
  landmass <- landmasses_geo[[landmass_name]]
  coords   <- landmass$coordinates
  
  closed_coords <- rbind(coords, coords[1,]) # close polygon
  
  grid.polygon(
    x  = closed_coords[,1],   # longitude
    y  = closed_coords[,2],   # latitude
    default.units = "native",
    gp = grid::gpar(col  = borough_colors[landmass$borough],
              fill = "#669933",
              lwd  = 2
    )
  )
}

# plot color gradient based on total_count
n <- length(start_x)
for (i in seq_len(n)) {
  grid.segments(
    start_x[i], start_y[i],
    end_x[i],   end_y[i],
    default.units = "native",
    arrow = arrow(type = "open", length = unit(.01, "npc")),
    gp = grid::gpar(col = colors[i], lwd = 2)
  )
}
grid.text("Changes in the quantity of car crashes by area after COVID", y = 1.1,
          gp = grid::gpar(fontface = "bold", cex = 1.3))
grid.newpage()
#legend
grid.rect(
  x = unit(0.05, "npc"),   # left margin of box
  y = unit(0.95, "npc"),   # top of box
  width  = unit(0.35, "npc"),
  height = unit(0.2, "npc"),
  just = c("left", "top"),
  gp = grid::gpar(fill = "white", col = "black")
)

# Add key inside the box
grid.text(
  "Angle Definitions",
  x = unit(0.09, "npc"),
  y = unit(0.94, "npc"),
  just = c("left", "top"),
  gp=grid::gpar(fontsize=10)
)
grid.segments(
  .10, .90,
  .15, .90,
  default.units = "npc",
  arrow = arrow(type = "open", length = unit(.01, "npc")),
  gp = gpar(col = "black", lwd = 2)
)
grid.text(
  "0%",
  x = unit(0.16, "npc"),
  y = unit(0.91, "npc"),
  just = c("left", "top"),
  gp=gpar(fontsize=10)
)
grid.segments(
  .10, .90,
  .14, .87,
  default.units = "npc",
  arrow = arrow(type = "open", length = unit(.01, "npc")),
  gp = gpar(col = "black", lwd = 2)
)
grid.text(
  "50%",
  x = unit(0.15, "npc"),
  y = unit(0.87, "npc"),
  just = c("left", "top"),
  gp=gpar(fontsize=10)
)
grid.segments(
  .10, .90,
  .10, .85,
  default.units = "npc",
  arrow = arrow(type = "open", length = unit(.01, "npc")),
  gp = gpar(col = "black", lwd = 2)
)
grid.text(
  "100%",
  x = unit(0.09, "npc"),
  y = unit(0.84, "npc"),
  just = c("left", "top"),
  gp=gpar(fontsize=10)
)
grid.text(
  "Total Crashes in this area",
  x = unit(0.2, "npc"),
  y = unit(0.94, "npc"),
  just = c("left", "top"),
  gp=gpar(fontsize=10)
)
draw_color_key(x= unit(.24, "npc"),
               y= unit(.8, "npc"),
               height = unit(.1, "npc"),
               width = unit(.01, "npc"),
               col_fun=rev(hcl.colors(n=100, palette= "YlOrRd")))
grid.text(
  "23k",
  x = unit(0.25, "npc"),
  y = unit(0.9, "npc"),
  just = c("left", "top"),
  gp=grid::gpar(fontsize=10)
)
grid.text(
  "12k",
  x = unit(0.25, "npc"),
  y = unit(0.86, "npc"),
  just = c("left", "top"),
  gp=grid::gpar(fontsize=10)
)
grid.text(
  "0",
  x = unit(0.25, "npc"),
  y = unit(0.82, "npc"),
  just = c("left", "top"),
  gp=grid:gpar(fontsize=10)
)

popViewport(2)

