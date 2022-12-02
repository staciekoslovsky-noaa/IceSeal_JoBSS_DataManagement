# JoBSS: Process Data/Images to DB
# S. Hardy

# Set Working Variables
wd <- "L:/jobss_2021"
metaTemplate <- "L:/jobss_2021/Template4Import.json"
projectPrefix <- "jobss_2021"

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
install_pkg("rjson")
install_pkg("plyr")
install_pkg("stringr")

# Run code -------------------------------------------------------
setwd(wd)

# Create list of camera folders within which data need to be processed 
dir <- list.dirs(wd, full.names = FALSE, recursive = FALSE)
dir <- data.frame(path = dir[grep("fl", dir)], stringsAsFactors = FALSE)
camera_models <- list.dirs(paste(wd, dir$path[1], sep = "/"), full.names = TRUE, recursive = FALSE)
for (i in 2:nrow(dir)){
  temp <- list.dirs(paste(wd, dir$path[i], sep = "/"), full.names = TRUE, recursive = FALSE)
  camera_models <- append(camera_models, temp)
}

camera_models <- camera_models[!grepl("processed_results", camera_models)]
camera_models <- camera_models[!grepl("default", camera_models)]
camera_models <- camera_models[!grepl("detections", camera_models)]
camera_models <- unique(camera_models)
image_dir <- merge(camera_models, c("left_view", "center_view", "right_view"), ALL = true)
colnames(image_dir) <- c("path", "camera_loc")
image_dir$path <- as.character(image_dir$path)
image_dir$camera_dir <- paste(image_dir$path, image_dir$camera_loc, sep = "/")

rm(i, temp, wd)

# Process images and meta.json files
images2DB <- data.frame(image_name = as.character(""), dt = as.character(""), image_type = as.character(""), 
                        image_dir = as.character(""), stringsAsFactors = FALSE)
images2DB <- images2DB[which(images2DB == "test"), ]

meta2DB <- data.frame(rjson::fromJSON(paste(readLines(metaTemplate), collapse="")))
names(meta2DB)[names(meta2DB) == "effort"] <- "effort_field"
meta2DB$effort_reconciled <- ""
meta2DB$meta_file <- ""
meta2DB$dt <- ""
meta2DB$flight <- ""
meta2DB$camera_view <- ""
meta2DB$camera_model <- ""
meta2DB <- meta2DB[which(meta2DB == "test"), ]

for (i in 1:nrow(image_dir)){
  print(i)
  files <- list.files(image_dir$camera_dir[i], full.names = FALSE, recursive = FALSE)
  files <- data.frame(image_name = files[which(startsWith(files, projectPrefix) == TRUE)], stringsAsFactors = FALSE)
  files$dt <- str_extract(files$image_name, "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9].[0-9][0-9][0-9][0-9][0-9][0-9]")
  files$image_type <- ifelse(grepl("rgb", files$image_name) == TRUE, "rgb_image", 
                             ifelse(grepl("ir", files$image_name) == TRUE, "ir_image",
                                    ifelse(grepl("uv", files$image_name) == TRUE, "uv_image", 
                                           ifelse(grepl("meta", files$image_name) == TRUE, "meta.json", "Unknown"))))
  files$image_dir <- image_dir$camera_dir[i]
  
  images <- files[which(grepl("image", files$image_type)), ]
  images2DB <- rbind(images2DB, images)
  
  meta <- files[which(files$image_type == "meta.json"), ]
  if (nrow(meta) > 1) {
    for (j in 1:nrow(meta)){
      meta_file <- paste(image_dir$camera_dir[i], meta$image_name[j], sep = "/")
      metaJ <- data.frame(rjson::fromJSON(paste(readLines(meta_file), collapse="")), stringsAsFactors = FALSE)
      names(metaJ)[names(metaJ) == "effort"] <- "effort_field"
      metaJ$effort_reconciled <- NA
      metaJ$meta_file <- basename(meta_file)
      metaJ$dt <- str_extract(metaJ$meta_file, "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9].[0-9][0-9][0-9][0-9][0-9][0-9]")
      metaJ$flight <- str_extract(metaJ$meta_file, "fl[0-9][0-9]")
      metaJ$camera_view <- gsub("_", "", str_extract(metaJ$meta_file, "_[A-Z]_"))
      metaJ$camera_model <- basename(image_dir$path[i])
      meta2DB <- plyr::rbind.fill(meta2DB, metaJ)
    }
  }
}

colnames(meta2DB) <- gsub("\\.", "_", colnames(meta2DB))

images2DB$flight <- str_extract(images2DB$image_name, "fl[0-9][0-9]")
images2DB$camera_view <- gsub("_", "", str_extract(images2DB$image_name, "_[A-Z]_"))
images2DB$ir_nuc <- NA
images2DB$rgb_manualreview <- NA

# Process effort logs
# logs2DB <- data.frame(effort_log = as.character(""), gps_time = as.character(""), sys_time = as.character(""), note = as.character(""), stringsAsFactors = FALSE)
# logs2DB <- logs2DB[which(logs2DB == "test"), ]
# 
# for (i in 1:nrow(dir)){
#   logs <- list.files(dir$path[i], pattern = ".txt", full.names = FALSE, recursive = FALSE)
#   if (length(logs) != 0){
#     for (j in 1:length(logs)){
#       log <- paste(wd, dir$path[i], logs[j], sep = "/")
#       log_file <- scan(log,
#                        sep = "\n",
#                        multi.line = TRUE,
#                        what = "list")
#       log_file <- log_file[which(log_file != "" & log_file != "---")]
#       log_file <- gsub("'", "", log_file)
#       log_file <- paste(log_file, sep = "", collapse = "") 
#       log_file <- strsplit(log_file, "}")
#       log_file <- data.frame(parsed = unlist(log_file), stringsAsFactors = FALSE)
#       log_file$effort_log <- logs[j]
#       
#       # Extract GPS time
#       log_file$dt_gps <- str_match(log_file$parsed, "\\{gps_time: !!timestamp (.*?), note")
#       log_file$dt_gps <- log_file$dt_gps[, 2]
#       log_file$dt_gps <- gsub("-", "", log_file$dt_gps)
#       log_file$dt_gps <- gsub(":", "", log_file$dt_gps)
#       log_file$dt_gps <- gsub(" ", "_", log_file$dt_gps)
#       
#       # Extract SYS time
#       log_file$dt_sys <- substring(log_file$parsed, nchar(log_file$parsed)-22, nchar(log_file$parsed))
#       
#       # Extract note
#       log_file$note <- str_match(log_file$parsed, "note: (.*) sys_time")
#       log_file$note <- trimws(log_file$note[, 2])
#       log_file$note <- gsub(",*$", "", log_file$note, perl = T)
#       
#       # Final processing
#       log_file <- log_file[, c("effort_log", "dt_gps", "dt_sys", "note")]
#       logs2DB <- rbind(logs2DB, log_file)
#     }
#   }
# }
# logs2DB$flight <- substring(logs2DB$effort_log, 17, 20)
# logs2DB$effort <- ifelse(grepl("Selected effort:", logs2DB$note), logs2DB$note, NA)
# logs2DB$collection <- ifelse(grepl("Setting collection mode", logs2DB$note), logs2DB$note, NA)

rm(meta, image_dir, images, log_file, metaJ, i, j, log, logs, meta_file, wd, files)

# Export data to PostgreSQL -----------------------------------------------------------
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

# Create list of data to process
df <- list(images2DB, meta2DB)
dat <- c("tbl_images", "geo_images_meta")

# Identify and delete dependencies for each table
for (i in 1:length(dat)){
  sql <- paste("SELECT fxn_deps_save_and_drop_dependencies(\'surv_jobss\', \'", dat[i], "\')", sep = "")
  RPostgreSQL::dbSendQuery(con, sql)
  RPostgreSQL::dbClearResult(dbListResults(con)[[1]])
}
RPostgreSQL::dbSendQuery(con, "DELETE FROM deps_saved_ddl WHERE deps_ddl_to_run NOT LIKE \'%CREATE VIEW%\'")

# Push data to pepgeo database and process data to spatial datasets where appropriate
for (i in 1:length(dat)){
  RPostgreSQL::dbWriteTable(con, c("surv_jobss", dat[i]), data.frame(df[i]), overwrite = TRUE, row.names = FALSE)
  if (i == 2) {
    sql1 <- paste("ALTER TABLE surv_jobss.", dat[i], " ADD COLUMN geom geometry(POINT, 4326)", sep = "")
    sql2 <- paste("UPDATE surv_jobss.", dat[i], " SET geom = ST_SetSRID(ST_MakePoint(ins_longitude, ins_latitude), 4326)", sep = "")
    RPostgreSQL::dbSendQuery(con, sql1)
    RPostgreSQL::dbSendQuery(con, sql2)
  }
}

# Recreate table dependencies
for (i in length(dat):1) {
  sql <- paste("SELECT fxn_deps_restore_dependencies(\'surv_jobss\', \'", dat[i], "\')", sep = "")
  RPostgreSQL::dbSendQuery(con, sql)
  RPostgreSQL::dbClearResult(dbListResults(con)[[1]])
}

# Disconnect for database and delete unnecessary variables ----------------------------
RPostgreSQL::dbDisconnect(con)
rm(con, df, dat, i, sql, sql1, sql2)
