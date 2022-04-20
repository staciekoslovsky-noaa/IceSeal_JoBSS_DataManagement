# Process IR NuC'ed image list to DB

# Variables
nonnuc_list <- "C:/Users/stacie.hardy/Work/Work/Projects/AS_JOBSS/Data/IR_NUC/JoBSS_NUCreview_images_ir_validated.txt"
image_list <- "C:/Users/stacie.hardy/Work/Work/Projects/AS_JOBSS/Data/IR_NUC/JoBSS_NUCreview_images_ir.txt"
update_schema <- "surv_jobss"
update_table <- "tbl_images"
update_field <- "ir_nuc_review"

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
nonnuc <- read.csv(nonnuc_list, skip = 2, header = FALSE, stringsAsFactors = FALSE, col.names = c("detection", "image_name", "frame_number", "bound_left", "bound_top", "bound_right", "bound_bottom", "score", "length", "detection_type", "type_score", "detection_comments"))
nonnuc <- nonnuc %>%
  mutate(image_name = sapply(strsplit(image_name, split= "\\/"), function(x) x[length(x)])) %>%
  select(image_name)

images <- read.csv(image_list, header = FALSE, col.names = "image_name")
images <- images %>%
  mutate(image_name = sapply(strsplit(image_name, split= "\\/"), function(x) x[length(x)]))

# Import data to DB
RPostgreSQL::dbSendQuery(con, paste("UPDATE ", update_schema, ".", update_table, " SET ", update_field, " = \'A\'"))
RPostgreSQL::dbWriteTable(con, c(update_schema, "temp_nonnuc"), nonnuc, append = TRUE, row.names = FALSE)
RPostgreSQL::dbWriteTable(con, c(update_schema, "temp_images"), images, append = TRUE, row.names = FALSE)
RPostgreSQL::dbSendQuery(con, paste("UPDATE ", update_schema, ".", update_table, " SET ", update_field, " = \'N\' WHERE (image_name) in (SELECT image_name FROM ", update_schema, ".temp_nonnuc)", sep = ""))
RPostgreSQL::dbSendQuery(con, paste("UPDATE ", update_schema, ".", update_table, " SET ", update_field, " = \'Y\' WHERE ", update_field, " <> \'N\' AND (image_name) in (SELECT image_name FROM ", update_schema, ".temp_images)", sep = ""))
RPostgreSQL::dbSendQuery(con, paste("DROP TABLE ", update_schema, ".temp_nonnuc", sep = ""))
RPostgreSQL::dbSendQuery(con, paste("DROP TABLE ", update_schema, ".temp_images", sep = ""))
RPostgreSQL::dbDisconnect(con)
rm(con)
