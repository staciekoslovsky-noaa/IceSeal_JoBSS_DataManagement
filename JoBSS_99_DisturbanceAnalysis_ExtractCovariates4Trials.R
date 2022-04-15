# Extract environmental covariates for JoBSS disturbance trials

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

# Run code -------------------------------------------------------
# Connect to DB
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

# Read CSV
data <- read.csv("C:\\skh\\disturbance_date_location.csv")
data$dt <- as.POSIXct(paste(data$date, data$trial_start_time, sep = " "), format = "%Y%m%d %H:%M:%S", tz = "UTC")
data$acpcp <- as.numeric("")
data$air2m <- as.numeric("")
data$airsfc <- as.numeric("")
data$prmsl <- as.numeric("")
data$uwnd <- as.numeric("")
data$vwnd <- as.numeric("")

# Iterate through locations in CSV
for (i in 1:nrow(data)){
  data$acpcp[i] <- as.numeric(dbGetQuery(con, paste("SELECT * FROM (SELECT ST_Value(ST_Transform(rast, 3338), ST_Transform(ST_SetSRID(ST_MakePoint(", 
                                         data$ave_longitude_gis[i], 
                                         ", ", 
                                         data$ave_latitude_gis[i], 
                                         "), 4326), 3338)) as rast FROM environ.tbl_narr_acpcp WHERE fdatetime_range @> CAST(\'", 
                                         data$dt[i],
                                         "\' AS timestamp with time zone)) r WHERE rast IS NOT NULL", 
                                         sep = "")))
  data$air2m[i] <- as.numeric(dbGetQuery(con, paste("SELECT * FROM (SELECT ST_Value(ST_Transform(rast, 3338), ST_Transform(ST_SetSRID(ST_MakePoint(", 
                                         data$ave_longitude_gis[i], 
                                         ", ", 
                                         data$ave_latitude_gis[i], 
                                         "), 4326), 3338)) as rast FROM environ.tbl_narr_air2m WHERE fdatetime_range @> CAST(\'", 
                                         data$dt[i],
                                         "\' AS timestamp with time zone)) r WHERE rast IS NOT NULL", 
                                         sep = "")))
  data$airsfc[i] <- as.numeric(dbGetQuery(con, paste("SELECT * FROM (SELECT ST_Value(ST_Transform(rast, 3338), ST_Transform(ST_SetSRID(ST_MakePoint(", 
                                         data$ave_longitude_gis[i], 
                                         ", ", 
                                         data$ave_latitude_gis[i], 
                                         "), 4326), 3338)) as rast FROM environ.tbl_narr_airsfc WHERE fdatetime_range @> CAST(\'", 
                                         data$dt[i],
                                         "\' AS timestamp with time zone)) r WHERE rast IS NOT NULL", 
                                         sep = "")))
  data$prmsl[i] <- as.numeric(dbGetQuery(con, paste("SELECT * FROM (SELECT ST_Value(ST_Transform(rast, 3338), ST_Transform(ST_SetSRID(ST_MakePoint(", 
                                         data$ave_longitude_gis[i], 
                                         ", ", 
                                         data$ave_latitude_gis[i], 
                                         "), 4326), 3338)) as rast FROM environ.tbl_narr_prmsl WHERE fdatetime_range @> CAST(\'", 
                                         data$dt[i],
                                         "\' AS timestamp with time zone)) r WHERE rast IS NOT NULL", 
                                         sep = "")))
  data$uwnd[i] <- as.numeric(dbGetQuery(con, paste("SELECT * FROM (SELECT ST_Value(ST_Transform(rast, 3338), ST_Transform(ST_SetSRID(ST_MakePoint(", 
                                         data$ave_longitude_gis[i], 
                                         ", ", 
                                         data$ave_latitude_gis[i], 
                                         "), 4326), 3338)) as rast FROM environ.tbl_narr_uwnd WHERE fdatetime_range @> CAST(\'", 
                                         data$dt[i],
                                         "\' AS timestamp with time zone)) r WHERE rast IS NOT NULL", 
                                         sep = "")))
  data$vwnd[i] <- as.numeric(dbGetQuery(con, paste("SELECT * FROM (SELECT ST_Value(ST_Transform(rast, 3338), ST_Transform(ST_SetSRID(ST_MakePoint(", 
                                         data$ave_longitude_gis[i], 
                                         ", ", 
                                         data$ave_latitude_gis[i], 
                                         "), 4326), 3338)) as rast FROM environ.tbl_narr_vwnd WHERE fdatetime_range @> CAST(\'", 
                                         data$dt[i],
                                         "\' AS timestamp with time zone)) r WHERE rast IS NOT NULL", 
                                         sep = "")))
}

# Disconnect from DB
RPostgreSQL::dbDisconnect(con)
rm(con)

# Export results
write.table(data, "C:\\skh\\disturbance_date_location_withCovariates.csv", sep = ",", row.names = FALSE)
