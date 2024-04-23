# Process JoBSS processed thermal detections to DB

# Install libraries
library(tidyverse)
library(RPostgreSQL)

# Set variables for processing
wd <- "O:\\Data\\Annotations\\jobss_2021_20210726_batchProcessing\\02_AnnotationFiles_ForProcessing"

# Set up working environment
"%notin%" <- Negate("%in%")
setwd(wd)
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              password = Sys.getenv("admin_pw"))

# Delete data from tables (if needed)
RPostgreSQL::dbSendQuery(con, "DELETE FROM surv_jobss.tbl_detections_processed_rgb")

# Import data and process
folders <- data.frame(folder_path = list.dirs(path = wd, full.names = TRUE, recursive = FALSE), stringsAsFactors = FALSE)
folders <- folders %>%
  mutate(flight = str_extract(folder_path, "fl[0-9][0-9]"),
         camera_view = gsub("_", "", str_extract(folder_path, "_[A-Z]$")))

for (i in 1:nrow(folders)) {
  if(folders$flight[i] == 'fl07' & folders$camera_view[i] == 'L') next 
  processed_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM surv_jobss.tbl_detections_processed_rgb")
  processed_id$max <- ifelse(is.na(processed_id$max), 0, processed_id$max)
  
  files <- list.files(folders$folder_path[i])
  rgb_validated <- files[grepl('_rgb_irDetectionsTransposed_processed', files)] 
  if(length(rgb_validated) == 0) next
  
  processed <- read.csv(paste(folders$folder_path[i], rgb_validated, sep = "\\"), skip = 2, header = FALSE, stringsAsFactors = FALSE, col.names = c("detection", "image_name", "frame_number", "bound_left", "bound_top", "bound_right", "bound_bottom", "score", "length", "detection_type", "type_score", 
                                                                                                                                                    "att1", "att2", "att3", "att4", "att5", "att6", "att7", "att8"))
  
  processed <- data.frame(lapply(processed, function(x) {gsub("\\(trk-atr\\) *", "", x)})) %>%
    mutate(image_name = basename(sapply(strsplit(image_name, split= "\\/"), function(x) x[length(x)]))) %>%
    mutate(id = 1:n() + processed_id$max) %>%
    mutate(detection_file = rgb_validated) %>%
    mutate(flight = folders$flight[i]) %>%
    mutate(camera_view = folders$camera_view[i]) %>%
    mutate(detection_id = paste("surv_jobss", flight, camera_view, detection, sep = "_")) %>%
    mutate(species_confidence = ifelse(grepl("^species_confidence", att1), gsub("species_confidence *", "", att1),
                                       ifelse(grepl("^species_confidence", att2), gsub("species_confidence *", "", att2),
                                              ifelse(grepl("^species_confidence", att3), gsub("species_confidence *", "", att3), 
                                                     ifelse(grepl("^species_confidence", att4), gsub("species_confidence *", "", att4),
                                                            ifelse(grepl("^species_confidence", att5), gsub("species_confidence *", "", att5), 
                                                                   ifelse(grepl("^species_confidence", att6), gsub("species_confidence *", "", att6),
                                                                          ifelse(grepl("^species_confidence", att7), gsub("species_confidence *", "", att7), 
                                                                                 ifelse(grepl("^species_confidence", att8), gsub("species_confidence *", "", att8), "NA"))))))))) %>%
    mutate(age_class = ifelse(grepl("^age_class[[:space:]]", att1), gsub("age_class *", "", att1),
                                       ifelse(grepl("^age_class[[:space:]]", att2), gsub("age_class *", "", att2),
                                              ifelse(grepl("^age_class[[:space:]]", att3), gsub("age_class *", "", att3), 
                                                     ifelse(grepl("^age_class[[:space:]]", att4), gsub("age_class *", "", att4),
                                                            ifelse(grepl("^age_class[[:space:]]", att5), gsub("age_class *", "", att5), 
                                                                   ifelse(grepl("^age_class[[:space:]]", att6), gsub("age_class *", "", att6),
                                                                          ifelse(grepl("^age_class[[:space:]]", att7), gsub("age_class *", "", att7), 
                                                                                 ifelse(grepl("^age_class[[:space:]]", att8), gsub("age_class *", "", att8), "NA"))))))))) %>%
    mutate(age_class_confidence = ifelse(grepl("^age_class_confidence", att1), gsub("age_class_confidence *", "", att1),
                              ifelse(grepl("^age_class_confidence", att2), gsub("age_class_confidence *", "", att2),
                                     ifelse(grepl("^age_class_confidence", att3), gsub("age_class_confidence *", "", att3), 
                                            ifelse(grepl("^age_class_confidence", att4), gsub("age_class_confidence *", "", att4),
                                                   ifelse(grepl("^age_class_confidence", att5), gsub("age_class_confidence *", "", att5), 
                                                          ifelse(grepl("^age_class_confidence", att6), gsub("age_class_confidence *", "", att6),
                                                                 ifelse(grepl("^age_class_confidence", att7), gsub("age_class_confidence *", "", att7), 
                                                                        ifelse(grepl("^age_class_confidence", att8), gsub("age_class_confidence *", "", att8), "NA"))))))))) %>%
    mutate(alt_species = ifelse(grepl("^alt_species[[:space:]]", att1), gsub("alt_species *", "", att1),
                              ifelse(grepl("^alt_species[[:space:]]", att2), gsub("alt_species *", "", att2),
                                     ifelse(grepl("^alt_species[[:space:]]", att3), gsub("alt_species *", "", att3), 
                                            ifelse(grepl("^alt_species[[:space:]]", att4), gsub("alt_species *", "", att4),
                                                   ifelse(grepl("^alt_species[[:space:]]", att5), gsub("alt_species *", "", att5), 
                                                          ifelse(grepl("^alt_species[[:space:]]", att6), gsub("alt_species *", "", att6),
                                                                 ifelse(grepl("^alt_species[[:space:]]", att7), gsub("alt_species *", "", att7), 
                                                                        ifelse(grepl("^alt_species[[:space:]]", att8), gsub("alt_species *", "", att8), "NA"))))))))) %>%
    mutate(alt_species_confidence = ifelse(grepl("^alt_species_confidence", att1), gsub("alt_species_confidence *", "", att1),
                                ifelse(grepl("^alt_species_confidence", att2), gsub("alt_species_confidence *", "", att2),
                                       ifelse(grepl("^alt_species_confidence", att3), gsub("alt_species_confidence *", "", att3), 
                                              ifelse(grepl("^alt_species_confidence", att4), gsub("alt_species_confidence *", "", att4),
                                                     ifelse(grepl("^alt_species_confidence", att5), gsub("alt_species_confidence *", "", att5), 
                                                            ifelse(grepl("^alt_species_confidence", att6), gsub("alt_species_confidence *", "", att6),
                                                                   ifelse(grepl("^alt_species_confidence", att7), gsub("alt_species_confidence *", "", att7), 
                                                                          ifelse(grepl("^alt_species_confidence", att8), gsub("alt_species_confidence *", "", att8), "NA"))))))))) %>%
    mutate(alt_age_class = ifelse(grepl("^alt_age_class[[:space:]]", att1), gsub("alt_age_class *", "", att1),
                                ifelse(grepl("^alt_age_class[[:space:]]", att2), gsub("alt_age_class *", "", att2),
                                       ifelse(grepl("^alt_age_class[[:space:]]", att3), gsub("alt_age_class *", "", att3), 
                                              ifelse(grepl("^alt_age_class[[:space:]]", att4), gsub("alt_age_class *", "", att4),
                                                     ifelse(grepl("^alt_age_class[[:space:]]", att5), gsub("alt_age_class *", "", att5), 
                                                            ifelse(grepl("^alt_age_class[[:space:]]", att6), gsub("alt_age_class *", "", att6),
                                                                   ifelse(grepl("^alt_age_class[[:space:]]", att7), gsub("alt_age_class *", "", att7), 
                                                                          ifelse(grepl("^alt_age_class[[:space:]]", att8), gsub("alt_age_class *", "", att8), "NA"))))))))) %>%
    mutate(alt_age_class_confidence = ifelse(grepl("^alt_age_class_confidence", att1), gsub("alt_age_class_confidence *", "", att1),
                                  ifelse(grepl("^alt_age_class_confidence", att2), gsub("alt_age_class_confidence *", "", att2),
                                         ifelse(grepl("^alt_age_class_confidence", att3), gsub("alt_age_class_confidence *", "", att3), 
                                                ifelse(grepl("^alt_age_class_confidence", att4), gsub("alt_age_class_confidence *", "", att4),
                                                       ifelse(grepl("^alt_age_class_confidence", att5), gsub("alt_age_class_confidence *", "", att5), 
                                                              ifelse(grepl("^alt_age_class_confidence", att6), gsub("alt_age_class_confidence *", "", att6),
                                                                     ifelse(grepl("^alt_age_class_confidence", att7), gsub("alt_age_class_confidence *", "", att7), 
                                                                            ifelse(grepl("^alt_age_class_confidence", att8), gsub("alt_age_class_confidence *", "", att8), "NA"))))))))) %>%
    mutate(bear_id = ifelse(grepl("^bear_id", att1), gsub("bear_id *", "", att1),
                                             ifelse(grepl("^bear_id", att2), gsub("bear_id *", "", att2),
                                                    ifelse(grepl("^bear_id", att3), gsub("bear_id *", "", att3), 
                                                           ifelse(grepl("^bear_id", att4), gsub("bear_id *", "", att4),
                                                                  ifelse(grepl("^bear_id", att5), gsub("bear_id *", "", att5), 
                                                                         ifelse(grepl("^bear_id", att6), gsub("bear_id *", "", att6),
                                                                                ifelse(grepl("^bear_id", att7), gsub("bear_id *", "", att7), 
                                                                                       ifelse(grepl("^bear_id", att8), gsub("bear_id *", "", att8), "NA"))))))))) %>%
    select("id", "detection", "image_name", "frame_number", "bound_left", "bound_top", "bound_right", "bound_bottom", "score", "length", "detection_type", "type_score", 
           "flight", "camera_view", "detection_id", "detection_file",
           "species_confidence", "age_class", "age_class_confidence", "alt_species", "alt_species_confidence", "alt_age_class", "alt_age_class_confidence", "bear_id")
  
  # Import data to DB
  RPostgreSQL::dbWriteTable(con, c("surv_jobss", "tbl_detections_processed_rgb"), processed, append = TRUE, row.names = FALSE)
}

# Disconnect from DB
RPostgreSQL::dbDisconnect(con)
rm(con)
