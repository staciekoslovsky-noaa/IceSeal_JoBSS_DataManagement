# Process IR NuC'ed image list to DB
# S. Hardy, 10 August 2020

# Variables
image_list <- "C:/Users/stacie.hardy/Work/Work/Projects/AS__InFlight_KAMERA/Data/NUCedIR/NUCimages_jobss2021_20210726_YBprocessed.txt"
update_schema <- "surv_jobss"
update_table <- "tbl_images"
update_field <- "ir_nuc_yb"
 
# Install libraries
library(tidyverse)
library(RPostgreSQL)

# Set up working environment
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

# Import data and process
## IMAGE LIST
images <- read.table(image_list, stringsAsFactors = FALSE) 
colnames(images) <- "images"

images <- images %>%
  mutate(images = basename(images)) %>%
  mutate(flight = str_extract(images, "fl[0-9][0-9]"),
         camera_view = gsub("_", "", str_extract(images, "_[A-Z]_")),
         dt = str_extract(images, "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9].[0-9][0-9][0-9][0-9][0-9][0-9]"))

# Import data to DB
RPostgreSQL::dbWriteTable(con, c(update_schema, "temp_images"), images, append = TRUE, row.names = FALSE)
RPostgreSQL::dbSendQuery(con, paste("UPDATE ", update_schema, ".", update_table, " SET ", update_field, " = \'Y\' WHERE (flight, camera_view, dt) in (SELECT flight, camera_view, dt FROM ", update_schema, ".temp_images)", sep = ""))
RPostgreSQL::dbSendQuery(con, paste("UPDATE ", update_schema, ".", update_table, " SET ", update_field, " = \'N\' WHERE ", update_field, " IS NULL", sep = ""))
RPostgreSQL::dbSendQuery(con, paste("DROP TABLE ", update_schema, " .temp_images", sep = ""))
RPostgreSQL::dbDisconnect(con)
rm(con)
