# JoBSS: Import Detection Shapefiles to DB

# Define variables
wd <- "Y:/NMML_Polar_imagery_3/jobss_2021"

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")
install_pkg("sf")
install_pkg("tidyverse")

# Set working directory and connect to DB
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              password = Sys.getenv("admin_pw"))

RPostgreSQL::dbSendQuery(con, "DELETE FROM surv_jobss.geo_detections")

# Read shapefiles
dir <- list.dirs(wd, full.names = FALSE, recursive = FALSE)
dir <- data.frame(path = dir[grep("fl", dir)], stringsAsFactors = FALSE) %>%
  filter(stringr::str_starts(path, 'fl')) %>%
  mutate(path = paste(wd, "\\", path, "\\processed_results\\detection_shapefiles", sep = ""))

for (j in 1:nrow(dir)) {
  if(dir$path[j] == 'Y:/NMML_Polar_imagery_3/jobss_2021\\fl07\\processed_results\\detection_shapefiles') next 
  
  shps <- list.files(path = dir$path[j], pattern = "shp", full.names = TRUE)
  
  for (i in 1:length(shps)) {
    next_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM surv_jobss.geo_detections")
    next_id$max <- ifelse(length(which(!is.na(next_id$max))) == 0, 1, next_id$max + 1)
    
    shape <- sf::st_read(shps[i])
    
    shape <- shape %>%
      rename(
        geom = geometry, 
        image_name = img_filena
      ) %>%
      mutate(id = 1:n() + next_id$max,
             image_name = basename(image_name)) %>%
      mutate(flight = str_extract(image_name, "fl[0-9][0-9]"),
             camera_view = substring(str_extract(image_name, "_[A-Z]_"), 2, 2),
             dt = str_extract(image_name, "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9].[0-9][0-9][0-9][0-9][0-9][0-9]")) %>%
      select(id, flight, camera_view, dt, image_name, frame_id, track_id, img_left, img_right, img_top, img_bottom,
             confidence, length, conf_pairs, gsd_m, height_m, width_m, latitude, longitude, suppressed, geom)
    
    # Write data to DB
    sf::st_write(shape, con, c("surv_jobss", "geo_detections"), append = TRUE)
  }
}

RPostgreSQL::dbDisconnect(con)

rm(con, next_id, shape, i, wd, install_pkg)