library(shiny)
library(dplyr)
library(sf)
library(grid)
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
library(shiny)

setwd("C:/Users/linds/OneDrive/Desktop/stat_605/R_studio_projects/Project")
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

### STATIC COMPUTATIONS

nyc_data <- sf::st_read("data/nybb_25c/nybb.shp", quiet = TRUE)
nyc_geographic <- sf::st_transform(nyc_data, crs = 4326)

# Extract polygons
extract_all_landmasses <- function(sf_object) {
  landmass_list <- list()
  for (i in 1:nrow(sf_object)) {
    borough_name <- sf_object$BoroName[i]
    borough_coords <- st_coordinates(sf_object$geometry[i])
    unique_polygons <- unique(borough_coords[, "L2"])
    
    for (poly_id in unique_polygons) {
      poly_mask <- borough_coords[, "L2"] == poly_id
      poly_data <- borough_coords[poly_mask, c("X", "Y")]  # lon/lat
      
      landmass_list[[paste(borough_name, poly_id)]] <- list(
        borough = borough_name,
        coords  = poly_data
      )
    }
  }
  landmass_list
}

landmasses_geo <- extract_all_landmasses(nyc_geographic)

# Precompute map ranges
all_lon <- unlist(lapply(landmasses_geo, \(x) x$coords[,1]))
all_lat <- unlist(lapply(landmasses_geo, \(x) x$coords[,2]))
x_range <- range(all_lon)
y_range <- range(all_lat)

borough_colors <- c(
  "Staten Island" = "red",
  "Bronx"         = "blue",
  "Brooklyn"      = "darkgreen",
  "Queens"        = "orange",
  "Manhattan"     = "purple"
)

# Grid definitions
grid_size <- 50
xs <- seq(-74.3, -73.6, length.out = grid_size)
ys <- seq(40.4, 41.0, length.out = grid_size)


### UI
ui <- fluidPage(
  titlePanel("Adjustable Map"),
  
  radioButtons(
    "mode", "Choose view:",
    choices = c("Accident Distribution" = "acdist",
                "Accident Trends"       = "trend")
  ),
  
  conditionalPanel(
    condition = "input.mode == 'acdist'",
    sliderInput("year", "Year:", min = 2012, max = 2025, value = 2018)
  ),
  
  conditionalPanel(
    condition = "input.mode == 'trend'",
    sliderInput("years", "Year Range:", min = 2012, max = 2025,
                value = c(2015, 2018), step = 1)
  ),
  
  plotOutput("mainplot", width = "650px", height = "650px")
)


### SERVER
server <- function(input, output, session) {
  
  ### ---------------------------------------------------
  ### REACTIVE: accident distribution for selected year
  ### ---------------------------------------------------
  acdist_data <- reactive({
    req(input$year)
    
    nyc_year <- nyc %>% filter(YEAR == input$year)
    
    nyc_year %>%
      mutate(
        x_bin = findInterval(LONGITUDE, xs),
        y_bin = findInterval(LATITUDE, ys)
      ) %>%
      filter(
        !is.na(x_bin), !is.na(y_bin),
        x_bin >= 1, x_bin < length(xs),
        y_bin >= 1, y_bin < length(ys)
      ) %>%
      mutate(
        x_center = (xs[x_bin] + xs[x_bin + 1]) / 2,
        y_center = (ys[y_bin] + ys[y_bin + 1]) / 2
      ) %>%
      count(x_center, y_center, name = "year_count")
  })

  trend_data <- reactive({
    yr <- input$years
    req(yr)
    
    nyc_range <- nyc %>%
      filter(YEAR >= yr[1], YEAR <= yr[2]) %>%
      mutate(
        x_bin = findInterval(LONGITUDE, xs),
        y_bin = findInterval(LATITUDE, ys)
      ) %>%
      filter(
        !is.na(x_bin), !is.na(y_bin),
        x_bin >= 1, x_bin < length(xs),
        y_bin >= 1, y_bin < length(ys)
      ) %>%
      mutate(
        x_center = (xs[x_bin] + xs[x_bin + 1]) / 2,
        y_center = (ys[y_bin] + ys[y_bin + 1]) / 2
      )
    
    cell_slopes <- nyc_range %>%
      mutate(
        YEAR = as.numeric(YEAR),     # <-- FIX: must be numeric BEFORE count()
        x_center = as.numeric(x_center),
        y_center = as.numeric(y_center)
      ) %>%
      count(x_center, y_center, YEAR, name = "count") %>%
      group_by(x_center, y_center) %>%
      summarize(
        slope = {
          if (n() < 2) {
            NA_real_
          } else {
            yrs <- YEAR
            cnt <- count
            coef_val <- coef(lm(cnt ~ I(yrs - mean(yrs))))
            if (length(coef_val) >= 2 && !is.na(coef_val[2])) unname(coef_val[2]) else 0
          }
        },
        n_obs = n(),
        .groups = "drop"
      )
    
    
    ### arrow scaling
    max_abs <- max(abs(cell_slopes$slope), na.rm = TRUE)
    if (max_abs == 0) max_abs <- 1
    gain <- 1.5
    
    mean_lat <- mean(cell_slopes$y_center)
    lon_scale <- cos(mean_lat * pi/180)
    arrow_len <- 0.008
    
    cell_slopes %>%
      mutate(
        slope_norm = slope / max_abs,
        vx_raw = 1,
        vy_raw = gain * slope_norm,
        vx_scaled = vx_raw * lon_scale,
        raw_len = sqrt(vx_scaled^2 + vy_raw^2),
        dx = (vx_scaled / raw_len) * arrow_len / lon_scale,
        dy = (vy_raw / raw_len) * arrow_len
      )
  })
  
  ### ---------------------------------------------------
  ### SINGLE renderPlot — switches based on input$mode
  ### ---------------------------------------------------
  output$mainplot <- renderPlot({
    grid.newpage()
    grid.rect(gp = gpar(fill = "#6699FF"))
    
    pushViewport(plotViewport(c(5.1, 4.1, 4.1, 2.1)))
    pushViewport(dataViewport(x_range, y_range))
    
    # Draw static map polygons
    for (name in names(landmasses_geo)) {
      item <- landmasses_geo[[name]]
      coords <- item$coords
      closed <- rbind(coords, coords[1,])
      
      grid.polygon(
        x = closed[,1], y = closed[,2],
        default.units = "native",
        gp = gpar(
          col = borough_colors[item$borough],
          fill = "#669933",
          lwd = 2
        )
      )
    }
    
    ### MODE SWITCH
    if (input$mode == "acdist") {
      df <- acdist_data()
      df$year_count <- datawizard::normalize(df$year_count)
      
      grid.points(df$x_center, df$y_center,
                  size = unit(df$year_count, "char"),
                  pch = 19)
      grid.text("Relative Number of Accidents in a Given Year by Area", y = 1.0,
                gp = gpar(fontface = "bold", cex = 1.3))
      
    } else {
      df <- trend_data()
      
      grid.segments(
        df$x_center, df$y_center,
        df$x_center + df$dx, df$y_center + df$dy,
        arrow = arrow(type = "open", length = unit(.007, "npc")),
        gp = gpar(col = "black", lwd=2),
        default.units = "native"
      )
      grid.text("Linear Regression Over Given Range of Years by Area", y = 1.0,
                gp = gpar(fontface = "bold", cex = 1.3))
    }
    
    popViewport(2)
  })
}

shinyApp(ui, server)
