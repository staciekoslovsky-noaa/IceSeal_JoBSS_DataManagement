# In Flight System: Process Footprints to DB
# S. Hardy, 14DEC2020

# Define variables
wd <- "L:/jobss_2021"

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
                              #rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

RPostgreSQL::dbSendQuery(con, "DELETE FROM surv_jobss.geo_images_footprint")

# Read shapefiles
dir <- list.dirs(wd, full.names = FALSE, recursive = FALSE)
dir <- data.frame(path = dir[grep("fl", dir)], stringsAsFactors = FALSE) %>%
  filter(stringr::str_starts(path, 'fl')) %>%
  mutate(path = paste(wd, "\\", path, "\\processed_results\\fov_shapefiles", sep = ""))

for (j in 1:nrow(dir)) {
  if(dir$path[j] == 'L:/jobss_2021\\fl07\\processed_results\\fov_shapefiles') next 
  
  shps <- list.files(path = dir$path[j], pattern = "shp", full.names = TRUE)
  
  for (i in 1:length(shps)) {
    next_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM surv_jobss.geo_images_footprint")
    next_id$max <- ifelse(length(which(!is.na(next_id$max))) == 0, 1, next_id$max + 1)
    
    shape <- sf::st_read(shps[i])
    
    shape <- shape %>%
      rename(
        geom = geometry, 
        image_name = image_file
      ) %>%
      mutate(id = 1:n() + next_id$max,
             image_name = as.character(image_name),
             effort = as.character(effort),
             trigger = as.character(trigger),
             reviewed = as.character(reviewed),
             fate = as.character(fate)) %>%
      mutate(flight = str_extract(image_name, "fl[0-9][0-9]"),
             camera_view = substring(str_extract(image_name, "_[A-Z]_"), 2, 2),
             dt = str_extract(image_name, "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9].[0-9][0-9][0-9][0-9][0-9][0-9]"),
             image_type = ifelse(grepl("rgb", image_name) == TRUE, "rgb_image", 
                                 ifelse(grepl("ir", image_name) == TRUE, "ir_image",
                                        ifelse(grepl("uv", image_name) == TRUE, "uv_image", "unknown")))
      ) %>%
      select(id, flight, camera_view, dt, image_type, image_name, time, latitude, longitude, altitude, heading, pitch, roll, effort, trigger, reviewed, fate, geom)
    
    # Write data to DB
    sf::st_write(shape, con, c("surv_jobss", "geo_images_footprint"), append = TRUE)
  }
}

RPostgreSQL::dbDisconnect(con)

rm(con, next_id, shape, i, wd, install_pkg)