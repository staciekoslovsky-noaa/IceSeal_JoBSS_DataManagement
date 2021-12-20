# JoBSS: Read OCR from FLIR Images
# S. Hardy, 15OCT2019

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
install_pkg("tidyverse")
install_pkg("tesseract")

# Run code -------------------------------------------------------
# Set initial working directory -------------------------------------------------------
wd <- "C:\\skh\\noaa_screenshots"

# allowed <- tesseract(options = list(tessedit_char_whitelist = ".:0123456789"))

# Process images
data_ocr <- data.frame(ocr = tesseract::ocr("//nmfs/akc-nmml/Polar_Imagery/Surveys_HS/Coastal/Originals_FLIR/test_ocr.jpg", engine = tesseract("eng"), HOCR = FALSE), stringsAsFactors = FALSE)
data_ocr$image_path <- "test"
data_ocr <- data_ocr[0, c(1:2)]

images <- list.files(wd, pattern = "PNG$", full.names = TRUE)

for (j in 1:length(images)){
    temp_ocr <- data.frame(ocr = tesseract::ocr(images[j], #engine = allowed, 
                                                HOCR = FALSE), stringsAsFactors = FALSE)
    temp_ocr$image_path <- images[j]
    data_ocr <- rbind(data_ocr, temp_ocr)
}

rm(temp_ocr, j, images, wd)

data_ocr$image_name <- basename(data_ocr$image_path)

# Process OCR data...
# Date/time
data_ocr$dt <- trimws(str_sub(data_ocr$ocr,-23,-3))
data_ocr$dt_check <- ifelse(nchar(data_ocr$dt) != 20, "Yes", "No")

# Collection mode
data_ocr$mode <- ifelse(str_detect(data_ocr$ocr, "GeoPnt"), "GeoPnt", 
                        ifelse(str_detect(data_ocr$ocr, "Hd Hold"), "Hd Hold",
                               ifelse(str_detect(data_ocr$ocr, "Inrp"), "Inrp", "")))
data_ocr$mode_check <- ifelse(data_ocr$mode == "", "Yes", "No")

# Latitude
data_ocr$latitude <- gsub('.*Lat:|Lon:.*', '', data_ocr$ocr)
data_ocr$latitude <- trimws(gsub("[^[:alnum:]\\.\\s]", " ", data_ocr$latitude))
data_ocr$latitude <- ifelse(nchar(data_ocr$latitude) > 12 | nchar(data_ocr$latitude) < 11, "Missing", substring(data_ocr$latitude, 1, 12))
# data_ocr$latitude_check <- ifelse(data_ocr$latitude == "Missing", "Yes", "No")
data_ocr$latitude_dir <- ifelse(data_ocr$latitude == "Missing", "", substring(data_ocr$latitude, 1, 1))
data_ocr$latitude_deg <- ifelse(data_ocr$latitude == "Missing", "", substring(data_ocr$latitude, 3, 4))
data_ocr$latitude_min <- ifelse(data_ocr$latitude == "Missing", "", substring(data_ocr$latitude, 5, nchar(data_ocr$latitude)))
data_ocr$latitude_min <- gsub("[0-9] [0-9]", ".", trimws(data_ocr$latitude_min))
data_ocr$latitude_min <- gsub(" ", "", data_ocr$latitude_min)
# data_ocr$latitude_check <- ifelse(nchar(data_ocr$latitude_min) < 5 | nchar(data_ocr$latitude_min) > 6, "Yes", data_ocr$latitude_check)
# data_ocr$latitude_check <- ifelse(data_ocr$latitude_deg < 50 | data_ocr$latitude_deg > 60, "Yes", data_ocr$latitude_check)

# Longitude
data_ocr$longitude <- gsub('.*Lon:|Az.*', '', data_ocr$ocr)
data_ocr$longitude <- trimws(gsub("[^[:alnum:]\\.\\s]", " ", data_ocr$longitude))
data_ocr$longitude <- ifelse(nchar(data_ocr$longitude) > 13 | nchar(data_ocr$longitude) < 12, "Missing", substring(data_ocr$longitude, 1, 13))
# data_ocr$longitude_check <- ifelse(data_ocr$longitude == "Missing", "Yes", "No")
data_ocr$longitude_dir <- ifelse(data_ocr$longitude == "Missing", "", substring(data_ocr$longitude, 1, 1))
data_ocr$longitude_deg <- ifelse(data_ocr$longitude == "Missing", "", substring(data_ocr$longitude, 3, 5))
data_ocr$longitude_min <- ifelse(data_ocr$longitude == "Missing", "", substring(data_ocr$longitude, 7, nchar(data_ocr$longitude)))
data_ocr$longitude_min <- gsub("[0-9] [0-9]", ".", trimws(data_ocr$longitude_min))
data_ocr$longitude_min <- gsub(" ", "", data_ocr$longitude_min)
# data_ocr$longitude_check <- ifelse(nchar(data_ocr$longitude_min) < 5 | nchar(data_ocr$longitude_min) > 6, "Yes", data_ocr$longitude_check)
# data_ocr$longitude_check <- ifelse(data_ocr$longitude_deg < 150, "Yes", data_ocr$longitude_check)

# Target Latitude
data_ocr$t_latitude <- ifelse(data_ocr$mode == "GeoPnt", gsub('.*TLat|TLon.*', '', data_ocr$ocr), ifelse(data_ocr$mode == "", "DoubleCheck", "NotApplicable"))
data_ocr$t_latitude <- trimws(gsub("[^[:alnum:]\\.\\s]", " ", data_ocr$t_latitude))
data_ocr$t_latitude <- ifelse(data_ocr$t_latitude == "NotApplicable", "NotApplicable", ifelse(nchar(data_ocr$t_latitude) > 12 | nchar(data_ocr$t_latitude) < 11, "Missing", substring(data_ocr$t_latitude, 1, 12)))
# data_ocr$t_latitude_check <- ifelse(data_ocr$t_latitude == "Missing" | data_ocr$t_latitude == "DoubleCheck", "Yes", "No")
data_ocr$t_latitude_dir <- ifelse(data_ocr$t_latitude == "NotApplicable", "NotApplicable", ifelse(data_ocr$t_latitude == "Missing" | data_ocr$t_latitude == "DoubleCheck", "", substring(data_ocr$t_latitude, 1, 1)))
data_ocr$t_latitude_deg <- ifelse(data_ocr$t_latitude == "NotApplicable", "NotApplicable", ifelse(data_ocr$t_latitude == "Missing" | data_ocr$t_latitude == "DoubleCheck", "", substring(data_ocr$t_latitude, 3, 4)))
data_ocr$t_latitude_min <- ifelse(data_ocr$t_latitude == "NotApplicable", "NotApplicable", ifelse(data_ocr$t_latitude == "Missing" | data_ocr$t_latitude == "DoubleCheck", "", substring(data_ocr$t_latitude, 5, nchar(data_ocr$t_latitude))))
data_ocr$t_latitude_min <- gsub("[0-9] [0-9]", ".", trimws(data_ocr$t_latitude_min))
data_ocr$t_latitude_min <- gsub(" ", "", data_ocr$t_latitude_min)
# data_ocr$t_latitude_check <- ifelse(nchar(data_ocr$t_latitude_min) < 5 | nchar(data_ocr$t_latitude_min) > 6, "Yes", data_ocr$t_latitude_check)
# data_ocr$t_latitude_check <- ifelse(data_ocr$t_latitude_deg < 50 | data_ocr$t_latitude_deg > 60, "Yes", data_ocr$t_latitude_check)
# data_ocr$t_latitude_check <- ifelse(data_ocr$t_latitude == "NotApplicable", "No", data_ocr$t_latitude_check)

# Target Longitude
data_ocr$t_longitude <- ifelse(data_ocr$mode == "GeoPnt", gsub('.*TLon|Alt.*', '', data_ocr$ocr), ifelse(data_ocr$mode == "", "DoubleCheck", "NotApplicable"))
data_ocr$t_longitude <- trimws(gsub("[^[:alnum:]\\.\\s]", " ", data_ocr$t_longitude))
data_ocr$t_longitude <- ifelse(data_ocr$t_longitude == "NotApplicable", "NotApplicable", ifelse(nchar(data_ocr$t_longitude) > 13 | nchar(data_ocr$t_longitude) < 12, "Missing", substring(data_ocr$t_longitude, 1, 13)))
# data_ocr$t_longitude_check <- ifelse(data_ocr$t_longitude == "Missing" | data_ocr$t_longitude == "DoubleCheck", "Yes", "No")
data_ocr$t_longitude_dir <- ifelse(data_ocr$t_longitude == "NotApplicable", "NotApplicable", ifelse(data_ocr$t_longitude == "Missing" | data_ocr$t_longitude == "DoubleCheck", "", substring(data_ocr$t_longitude, 1, 1)))
data_ocr$t_longitude_deg <- ifelse(data_ocr$t_longitude == "NotApplicable", "NotApplicable", ifelse(data_ocr$t_longitude == "Missing" | data_ocr$t_longitude == "DoubleCheck", "", substring(data_ocr$t_longitude, 3, 5)))
data_ocr$t_longitude_min <- ifelse(data_ocr$t_longitude == "NotApplicable", "NotApplicable", ifelse(data_ocr$t_longitude == "Missing" | data_ocr$t_longitude == "DoubleCheck", "", substring(data_ocr$t_longitude, 7, nchar(data_ocr$t_longitude))))
data_ocr$t_longitude_min <- gsub("[0-9] [0-9]", ".", trimws(data_ocr$t_longitude_min))
data_ocr$t_longitude_min <- gsub(" ", "", data_ocr$t_longitude_min)
# data_ocr$t_longitude_check <- ifelse(nchar(data_ocr$t_longitude_min) < 5 | nchar(data_ocr$t_longitude_min) > 6, "Yes", data_ocr$t_longitude_check)
# data_ocr$t_longitude_check <- ifelse(data_ocr$t_longitude_deg < 150, "Yes", data_ocr$t_longitude_check)
# data_ocr$t_longitude_check <- ifelse(data_ocr$t_longitude == "NotApplicable", "No", data_ocr$t_longitude_check)

data_ocr <- data_ocr[, c("image_name", "mode", "latitude_dir", "latitude_deg", "latitude_min", "longitude_dir", "longitude_deg", "longitude_min", 
                         "t_latitude_dir", "t_latitude_deg", "t_latitude_min", "t_longitude_dir", "t_longitude_deg", "t_longitude_min", "dt")]

# Export data for review
write.csv(data_ocr, "C:/skh/JoBSS_FLIR_OCR4Review_20211220_SKH.csv", row.names = FALSE)
