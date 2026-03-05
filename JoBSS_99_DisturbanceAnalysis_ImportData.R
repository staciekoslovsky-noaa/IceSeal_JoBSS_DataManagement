# Import JoBSS: disturbance trial data into DB
# S. Koslovsky, February 2026

library(RPostgreSQL)
library(tidyverse)


# Read CSV file
data_by_seal <- read.csv("C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_JOBSS\\Data\\Disturbance\\disturbance_data_by_seal_20240911.csv", header = TRUE) %>%
  rename(distance_from_plane_m = distance.from.plane..m.,
         distance_from_trackline_m = distance.from.trackline..m.,
         near_distance_arc_trackline_m = near_distance_ARC_trackline_m,
         in_kamera_footprint_gis_visual = in_KAMERA_footprint_gis_visual) 

trial_by_group <- read.csv("C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_JOBSS\\Data\\Disturbance\\disturbance_trial_by_group_20240911.csv", header = TRUE) %>%
  rename(trial_date = date,
         near_distance_arc_trackline_m = near_distance_ARC_trackline_m,
         in_kamera_footprint_gis_visual = in_KAMERA_footprint_gis_visual,
         trial_duration_time = trial_duration) #%>%
  #mutate(geom = "0101000020E610000000000000000000000000000000000000")
  
  
# Connect to PostgreSQL (for getting information for processing data)
con <- RPostgreSQL::dbConnect(PostgreSQL(),
                              dbname = Sys.getenv("pep_db"),
                              host = Sys.getenv("pep_ip"),
                              user = Sys.getenv("pep_user"),
                              password = Sys.getenv("user_pw"))

RPostgreSQL::dbWriteTable(con, c("surv_jobss", "tbl_disturbance_by_seal"), data_by_seal, overwrite = TRUE, row.names = FALSE)
RPostgreSQL::dbWriteTable(con, c("surv_jobss", "tbl_disturbance_by_group"), trial_by_group, overwrite = TRUE, row.names = FALSE)
#RPostgreSQL::dbSendQuery(con, "UPDATE surv_jobss.tbl_disturbance_by_group SET geom = ST_SetSRID(ST_MakePoint(ave_longitude_gis, ave_latitude_gis), 4326)")

RPostgreSQL::dbDisconnect(con)