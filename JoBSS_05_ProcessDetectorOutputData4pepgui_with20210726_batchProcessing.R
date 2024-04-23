# Process detector output data (original detections) to DB
# S. Hardy, 10 August 2021

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

# Install libraries -----------------------------------------------
library(tidyverse)
library(RPostgreSQL)

# Set up working environment -----------------------------------------------
"%notin%" <- Negate("%in%")

con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              #port = Sys.getenv("pep_port"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

wd <- "O:/Data/Annotations/jobss_2021_20210726_batchProcessing"
setwd(wd)

# Prepare list of directories with original data -----------------------------------------------
dir <- data.frame(network_path = list.dirs(wd, full.names = TRUE, recursive = FALSE), stringsAsFactors = FALSE) %>%
  mutate(path = basename(network_path)) %>%
  subset(path != "_Database") %>%
  subset(path != "_ImportLogs") %>%
  subset(path != "_TEMPLATE_project_YYYYMMDD_datasetDescription") %>%
  subset(!grepl("X$", path)) %>%
  mutate(project_schema = ifelse(grep("jobss", path) == TRUE, "surv_jobss", "TO BE WRITTEN"))

for (i in 1:nrow(dir)){
  # Create detector_path variable
  if (file.exists(paste(dir$network_path[i], "01_DetectorOutputs", "outputs_success", sep = "/"))){
    detector_path <- paste(dir$network_path[i], "01_DetectorOutputs", "outputs_success", sep = "/")
  } else {
    detector_path <- paste(dir$network_path[i], "01_DetectorOutputs", sep = "/")
  }

  # Create pipeline dataset for import into DB
  if (file.exists(paste(dir$network_path[i], "01_DetectorOutputs", "pipelines", sep = "/"))){
    pipe_path <- paste(dir$network_path[i], "01_DetectorOutputs", "pipelines", sep = "/")
    
    pipes_by_file <- data.frame(detector_pipeline = list.files(pipe_path, full.names = FALSE), stringsAsFactors = FALSE) %>%
      mutate(flight = str_extract(detector_pipeline, "fl[0-9][0-9]")) %>%
      mutate(camera_view = gsub("_", "", str_extract(detector_pipeline, "_[A-Z]_")))

  }
  
  # Create input files dataset for import into DB
  inputs_by_file <- data.frame(file_name = list.files(paste(dir$network_path[i], "Archive_DetectorInputs", sep = "/"), full.names = FALSE), stringsAsFactors = FALSE) %>%
    filter(file_name != "dataset_manifest.csv") %>%
    mutate(flight = str_extract(file_name, "fl[0-9][0-9]")) %>%
    mutate(camera_view = gsub("_", "", str_extract(file_name, "_[A-Z]_"))) %>%
    mutate(image_list = ifelse(grepl("ir", file_name) == TRUE, "ir",
                               ifelse(grepl("rgb", file_name) == TRUE, "rgb", "uv"))) %>% 
    pivot_wider(id_cols = c(flight, camera_view), names_from = image_list, values_from = file_name, names_prefix = "input_image_list_")

  # Get list of detection files
  files <- data.frame(file_name = list.files(detector_path, full.names = FALSE), stringsAsFactors = FALSE) %>%
    mutate(file_type = ifelse(grepl("detections", file_name) == TRUE, "detection", "image_list")) %>%
    mutate(image_type = ifelse(grepl("rgb", file_name) == TRUE, "rgb", 
                               ifelse(grepl("ir", file_name) == TRUE, "ir", "uv"))) %>%
    mutate(flight = str_extract(file_name, "fl[0-9][0-9]")) %>%
    mutate(camera_view = gsub("_", "", str_extract(file_name, "_[A-Z]_"))) %>%
    mutate(processing_dt = str_extract(file_name, "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9][0-9]"))
  
  # Combine IR and RGB files to get list of files by processing run
  image_list_ir <- files %>%
    filter(image_type == "ir" & file_type == "image_list") %>%
    select(flight, camera_view, processing_dt, file_name) %>%
    rename(ir_image_list = file_name) 
  image_list_rgb <- files %>%
    filter(image_type == "rgb" & file_type == "image_list") %>%
    select(flight, camera_view, processing_dt, file_name) %>%
    rename(rgb_image_list = file_name)
  
  detections_ir <- files %>%
    filter(image_type == "ir" & file_type == "detection") %>%
    select(flight, camera_view, processing_dt, file_name) %>%
    rename(ir_detection_csv = file_name)
  detections_rgb <- files %>%
    filter(image_type == "rgb" & file_type == "detection") %>%
    select(flight, camera_view, processing_dt, file_name) %>%
    rename(rgb_detection_csv = file_name)
  
  # Create tbl_detector_meta dataset
  detector_meta <- image_list_ir %>%
    left_join(detections_ir, by = c("flight", "camera_view", "processing_dt")) %>%
    left_join(image_list_rgb, by = c("flight", "camera_view", "processing_dt")) %>%
    left_join(detections_rgb, by = c("flight", "camera_view", "processing_dt")) %>%
    mutate(process_status = ifelse(is.na(str_extract(rgb_detection_csv, "_X.csv")), "to_be_processed", "do_not_process")) %>%
    mutate(project_schema = dir$project_schema[i]) %>%
    mutate(algorithm_run_location = "office") %>%
    mutate(algorithm_run_machine = ifelse(exists('pipes_by_file') == TRUE, "GPU VM", "user_to_specify")) %>%
    mutate(algorithm_run_dt = as.POSIXlt(processing_dt, tz = "", "%Y%m%d-%H%M%S")) %>%
    mutate(input_image_list_uv = NA) %>%
    mutate(uv_image_list = NA) %>%
    mutate(uv_detection_csv = NA) %>%
    mutate(annotation_status_lku = ifelse(process_status == "to_be_processed", "R", "X"))
  
    # Join pipeline data
    if (exists("pipes_by_file")) {
      detector_meta <- detector_meta %>%
        left_join(pipes_by_file, by = c("flight", "camera_view"))
    } else {
      detector_meta$detector_pipeline <- "user_to_specify"
    }
  
    # Join input lists
    if (exists("inputs_by_file")) {
      detector_meta <- detector_meta %>%
        left_join(inputs_by_file, by = c("flight", "camera_view")) %>%
        mutate(input_image_list_ir = ifelse("input_image_list_ir" %in% names(.), input_image_list_ir, NA),
               input_image_list_rgb = ifelse("input_image_list_rgb" %in% names(.), input_image_list_rgb, NA),
               input_image_list_uv = ifelse("input_image_list_uv" %in% names(.), input_image_list_uv, NA))
    } else {
      detector_meta$input_image_list_ir <- "user_to_specify"
      detector_meta$input_image_list_rgb <- "user_to_specify"
      detector_meta$input_image_list_uv <- "user_to_specify"
    }
    
    # Add table record ID
    detector_meta_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM annotations.tbl_detector_meta")
    detector_meta_id$max <- ifelse(is.na(detector_meta_id$max), 1, detector_meta_id$max + 1)
  
    detector_meta <- detector_meta %>%
      mutate(id = 1:n() + detector_meta_id$max,
             detector_meta_comments = NA) %>%
      select(id, project_schema, flight, camera_view, detector_pipeline, 
             algorithm_run_location, algorithm_run_machine, algorithm_run_dt,
             input_image_list_ir, input_image_list_rgb, input_image_list_uv,
             ir_image_list, ir_detection_csv,
             rgb_image_list, rgb_detection_csv,
             uv_image_list, uv_detection_csv,
             annotation_status_lku, detector_meta_comments)
    
    RPostgreSQL::dbWriteTable(con, c("annotations", "tbl_detector_meta"), detector_meta, append = TRUE, row.names = FALSE)
  
    next_meta_id <- max(detector_meta$id) + 1
    RPostgreSQL::dbSendQuery(con, paste("ALTER SEQUENCE annotations.tbl_detector_meta_id_seq RESTART WITH ", next_meta_id, sep = ""))
  
    rm(image_list_ir, image_list_rgb, detections_ir, detections_rgb)
  
  # Process files where process_status == to_be_processed
  detector_process <- detector_meta %>%
    filter(annotation_status_lku == "R")
  
  # Add steps to tbl_detector_processing  
  if (dir$project_schema == "surv_jobss") {
    processing_steps <- RPostgreSQL::dbGetQuery(con, "SELECT processing_step_lku FROM annotations.lku_processing_step WHERE project_schema = \'surv_jobss\' AND processing_order <> 99")
    
    detector_process_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM annotations.tbl_detector_processing")
    detector_process_id$max <- ifelse(is.na(detector_process_id$max), 1, detector_process_id$max + 1)
    
    detector_processing <- detector_process %>%
      left_join(processing_steps, by = character()) %>%
      mutate(detector_meta_id = id,
             id = 1:n() + detector_process_id$max) %>%
      select(id, detector_meta_id, processing_step_lku)
    RPostgreSQL::dbWriteTable(con, c("annotations", "tbl_detector_processing"), detector_processing, append = TRUE, row.names = FALSE)
    
    next_processing_id <- max(detector_processing$id) + 1
    RPostgreSQL::dbSendQuery(con, paste("ALTER SEQUENCE annotations.tbl_detector_processing_id_seq RESTART WITH ", next_processing_id, sep = ""))
  }

  
  for (j in 1:nrow(detector_process)){
    image_list_ir <- paste(detector_path, detector_process$ir_image_list[j], sep = '/')
    image_list_rgb <- paste(detector_path, detector_process$rgb_image_list[j], sep = '/')
    detection_file_ir <- paste(detector_path, detector_process$ir_detection_csv[j], sep = '/')
    detection_file_rgb <- paste(detector_path, detector_process$rgb_detection_csv[j], sep = '/')
    
    # Import original data to DB                                                              
      
    # Import image lists to DB -- IR
    images_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM surv_jobss.tbl_images_imagelists")
    images_id$max <- ifelse(is.na(images_id$max) == TRUE, 0, images_id$max)

    images <- read.table(image_list_ir, header = FALSE, stringsAsFactors = FALSE, col.names = "image_name")

    images <- images %>%
      mutate(id = 1:n() + images_id$max) %>%
      mutate(image_list = detector_process$ir_image_list[j]) %>%
      select("id", "image_name", "image_list")

    rm(images_id)

    RPostgreSQL::dbWriteTable(con, c("surv_jobss", "tbl_images_imagelists"), images, append = TRUE, row.names = FALSE)

    # Import image lists to DB -- RGB
    images_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM surv_jobss.tbl_images_imagelists")
    images_id$max <- ifelse(is.na(images_id$max) == TRUE, 0, images_id$max)

    images <- read.table(image_list_rgb, header = FALSE, stringsAsFactors = FALSE, col.names = "image_name")

    images <- images %>%
      mutate(id = 1:n() + images_id$max) %>%
      mutate(image_list = detector_process$ir_image_list[j]) %>%
      select("id", "image_name", "image_list")

    rm(fields, original_id)

    RPostgreSQL::dbWriteTable(con, c("surv_jobss", "tbl_images_imagelists"), images, append = TRUE, row.names = FALSE)

    # Import original detections to DB -- IR
    original_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM surv_jobss.tbl_detections_original_ir")
    original_id$max <- ifelse(is.na(original_id$max) == TRUE, 0, original_id$max)

    fields <- max(count.fields(detection_file_ir, sep = ','))
    
    if(length(count.fields(detection_file_ir, skip = 4)) > 0) {
      original <- read.csv(detection_file_ir, header = FALSE, stringsAsFactors = FALSE, skip = 4, col.names = paste("V", seq_len(fields)))
      if(fields == 11) {
        colnames(original) <- c("detection", "image_name", "frame_number", "bound_left", "bound_top", "bound_right", "bound_bottom", "score", "length", "detection_type", "type_score")
      } else if (fields == 13) {
        colnames(original) <- c("detection", "image_name", "frame_number", "bound_left", "bound_top", "bound_right", "bound_bottom", "score", "length", "detection_type", "type_score", "detection_type_x1", "type_score_x1")
      }
      
      if("detection_type_x1" %notin% names(original)){
        original$detection_type_x1 <- ""
      }
      if("type_score_x1" %notin% names(original)){
        original$type_score_x1 <- 0.0000000000
      }
      
      original <- original %>%
        mutate(image_name = sapply(strsplit(image_name, split= "\\/"), function(x) x[length(x)])) %>%
        mutate(id = 1:n() + original_id$max) %>%
        mutate(detection_file = detection_file_ir) %>%
        mutate(flight = str_extract(image_name, "fl[0-9][0-9]")) %>%
        mutate(camera_view = gsub("_", "", str_extract(image_name, "_[A-Z]_"))) %>%
        mutate(detection_id = paste("surv_jobss", flight, camera_view, detection, sep = "_")) %>%
        select("id", "detection", "image_name", "frame_number", "bound_left", "bound_top", "bound_right", "bound_bottom", "score", "length", "detection_type", "type_score", "detection_type_x1", "type_score_x1", "flight", "camera_view", "detection_id", "detection_file")
      
      rm(fields, original_id)
      
      RPostgreSQL::dbWriteTable(con, c("surv_jobss", "tbl_detections_original_ir"), original, append = TRUE, row.names = FALSE)
    }

    # Import original detections to DB -- RGB
    original_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM surv_jobss.tbl_detections_original_rgb")
    original_id$max <- ifelse(is.na(original_id$max) == TRUE, 0, original_id$max)

    fields <- max(count.fields(detection_file_rgb, sep = ','))
    
    if(length(count.fields(detection_file_rgb, skip = 4)) > 0) {
      original <- read.csv(detection_file_rgb, header = FALSE, stringsAsFactors = FALSE, skip = 4, col.names = paste("V", seq_len(fields)))
      if(fields == 11) {
        colnames(original) <- c("detection", "image_name", "frame_number", "bound_left", "bound_top", "bound_right", "bound_bottom", "score", "length", "detection_type", "type_score")
      } else if (fields == 13) {
        colnames(original) <- c("detection", "image_name", "frame_number", "bound_left", "bound_top", "bound_right", "bound_bottom", "score", "length", "detection_type", "type_score", "detection_type_x1", "type_score_x1")
      }
      
      if("detection_type_x1" %notin% names(original)){
        original$detection_type_x1 <- ""
      }
      if("type_score_x1" %notin% names(original)){
        original$type_score_x1 <- 0.0000000000
      }
      
      original <- original %>%
        mutate(image_name = sapply(strsplit(image_name, split= "\\/"), function(x) x[length(x)])) %>%
        mutate(id = 1:n() + original_id$max) %>%
        mutate(detection_file = detection_file_rgb) %>%
        mutate(flight = str_extract(image_name, "fl[0-9][0-9]")) %>%
        mutate(camera_view = gsub("_", "", str_extract(image_name, "_[A-Z]_"))) %>%
        mutate(detection_id = paste("surv_jobss", flight, camera_view, detection, sep = "_")) %>%
        select("id", "detection", "image_name", "frame_number", "bound_left", "bound_top", "bound_right", "bound_bottom", "score", "length", "detection_type", "type_score", "detection_type_x1", "type_score_x1", "flight", "camera_view", "detection_id", "detection_file")
      
      rm(fields, original_id)
      
      RPostgreSQL::dbWriteTable(con, c("surv_jobss", "tbl_detections_original_rgb"), original, append = TRUE, row.names = FALSE)
    }
    
    # Create flight_cameraView folder in 02... folder and copy files there. Add _Processed to end of file name
    processed_path <- paste(dir$network_path[i], "02_AnnotationFiles_ForProcessing",
                            paste(dir$project_schema, detector_meta$flight[j], detector_meta$camera_view[j], sep = "_"),
                            sep= "/")
    
    dir.create(processed_path)
    
    if (!is.na(detector_meta$ir_image_list[j])) {
      file.copy(paste(detector_path, detector_meta$ir_image_list[j], sep = "/"), paste(processed_path, detector_meta$ir_image_list[j], sep = "/"))
      file.copy(paste(detector_path, detector_meta$ir_detection_csv[j], sep = "/"), paste(processed_path, detector_meta$ir_detection_csv[j], sep = "/"))
    }
    if (!is.na(detector_meta$rgb_image_list[j])) {
      file.copy(paste(detector_path, detector_meta$rgb_image_list[j], sep = "/"), paste(processed_path, detector_meta$rgb_image_list[j], sep = "/"))
      file.copy(paste(detector_path, detector_meta$rgb_detection_csv[j], sep = "/"), paste(processed_path, detector_meta$rgb_detection_csv[j], sep = "/"))
    }
    if (!is.na(detector_meta$uv_image_list[j])) {
      file.copy(paste(detector_path, detector_meta$uv_image_list[j], sep = "/"), paste(processed_path, detector_meta$uv_image_list[j], sep = "/"))
      file.copy(paste(detector_path, detector_meta$uv_detection_csv[j], sep = "/"), paste(processed_path, detector_meta$uv_detection_csv[j], sep = "/"))
    }
  }
  
  # Archive files
  archive_folder <- paste(dir$network_path[i], "01_DetectorOutputs", sep= "/")
  archive_path <- paste(dir$network_path[i], "Archive_DetectionFiles", sep= "/")
  
  file.copy(archive_folder, archive_path)
  
  # Write log file
  write.csv(detector_meta, paste("O:/Data/Annotations/_ImportLogs/importLog_20210902_", dir$path[i], ".csv", sep = ""), row.names = FALSE)
}

RPostgreSQL::dbDisconnect(con)
rm(con)

# DELETE FROM surv_jobss.tbl_detections_original_rgb;
# DELETE FROM surv_jobss.tbl_detections_original_ir;
# DELETE FROM surv_jobss.tbl_images_imagelists;
# DELETE FROM annotations.tbl_detector_processing;
# DELETE FROM annotations.tbl_detector_meta;