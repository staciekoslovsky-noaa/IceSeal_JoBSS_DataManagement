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

get_nth_element <- function(vector, starting_position, n) { 
  vector[seq(starting_position, length(vector), n)] 
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")
install_pkg("rjson")
install_pkg("plyr")
install_pkg("stringr")
install_pkg("tidyverse")

# Connect to DB
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

# Get list of images from DB
images <- RPostgreSQL::dbGetQuery(con, "SELECT image_name, flight, camera_model, dt
                                FROM surv_jobss.tbl_images 
                                LEFT JOIN surv_jobss.geo_images_meta
                                USING (flight, camera_view, dt)
                                WHERE image_type = \'rgb_image\'
                                AND camera_view = \'C\'
                                AND ins_altitude >= 152.4
                                AND ins_altitude <= 609.6
                                ORDER BY image_name")

# Select images
images_selected <- data.frame(image_name = get_nth_element(images$image_name, 14, 15), stringsAsFactors = FALSE)
images_selected$dt <- str_extract(images_selected$image_name, "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9].[0-9][0-9][0-9][0-9][0-9][0-9]")

RPostgreSQL::dbSendQuery(con, "UPDATE surv_jobss.tbl_images SET rgb_manualreview = NULL")

for (i in 1:nrow(images_selected)) {
  RPostgreSQL::dbSendQuery(con, paste("UPDATE surv_jobss.tbl_images SET rgb_manualreview = \'Y\' WHERE camera_view = \'C\' AND dt = \'", images_selected$dt[i], "\'", sep = '' ))
}

write.table(images_selected, "C:\\Users\\stacie.hardy\\Work\\Work\\Projects\\AS_JoBSS\\Data\\ManualReview\\jobss_2021_manualReview_rgb_images_20210720.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

# Randomly order images
images_selected <- images_selected %>%
  filter(dt > '20210506_233620.664534') %>%
  select(image_name)
set.seed(29)
images_random <- sample(nrow(images_selected))
images_random <- data.frame(image_name = images_selected[images_random, ], stringsAsFactors = FALSE)
images_random <- images_random %>%
  mutate(random_id = row.names(images_random)) %>%
  mutate(random_id = str_pad(random_id, 5, pad = "0"))

images_ordered <- images_selected %>%
  mutate(original_id = row.names(images_selected))
images_ordered <- images_ordered %>%
  join(images_random, by = "image_name") %>%
  mutate(new_name = paste(random_id, image_name, sep = "_")) %>%
  join(images, by = "image_name") %>%
  mutate(image_path = paste("L:\\jobss_2021", flight, camera_model, "center_view", sep = "\\")) %>%
  select(image_name, original_id, random_id, new_name, image_path)
write.csv(images_ordered, "C:\\Users\\stacie.hardy\\Work\\Work\\Projects\\AS_JoBSS\\Data\\ManualReview\\jobss_2021_manualReview_rgb_images_batch2_reorderingDetails.csv", quote = FALSE, row.names = FALSE)

# Copy and rename images
for (i in 1:nrow(images_ordered)){
  file.copy(paste(images_ordered$image_path[i], images_ordered$image_name[i], sep = "/"), "D:\\jobss_imagery_manualReview_batch2")
  file.rename(paste("D:\\jobss_imagery_manualReview_batch2", images_ordered$image_name[i], sep = "/"),
              paste("D:\\jobss_imagery_manualReview_batch2", images_ordered$new_name[i], sep = "/"))
  print(i)
}
