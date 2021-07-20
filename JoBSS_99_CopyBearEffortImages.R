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

# Connect to DB
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

# Get list of images from DB
images <- RPostgreSQL::dbGetQuery(con, "SELECT image_dir || \'/\' || image_name as image
                                FROM surv_jobss.tbl_images 
                                LEFT JOIN surv_jobss.geo_images_meta
                                USING (flight, camera_view, dt)
                                WHERE image_type = \'rgb_image\'
                                AND effort_field = \'BEAR\'
                                ORDER BY image_name")

# Copy images
for (i in 1:nrow(images)){
  file.copy(images$image[i], "D:\\jobss_imagery_bearEffort")
  print(i)
}