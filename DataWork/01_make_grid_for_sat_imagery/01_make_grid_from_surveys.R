# Make Grid

# Load Data --------------------------------------------------------------------
opm <- readRDS(file.path(secure_file_path, "Data", "OPM", "FinalData - PII", "GPS_uid_crosswalk.Rds"))
dhs <- readRDS(file.path(secure_file_path, "Data", "DHS", "FinalData - PII", "GPS_uid_crosswalk.Rds"))

## Append
dhs <- dhs %>%
  mutate(survey = "dhs")
opm <- opm %>%
  mutate(uid = uid %>% as.character(),
         survey = "opm")

survey_df <- bind_rows(opm, dhs)

## Cluster IDs
MAX_SIZE <- 100/111.12
survey_dist <- survey_df[,c("latitude", "longitude")] %>% dist()
survey_df$cluster_id <- hclust(survey_dist, method = "ward.D2") %>%
  cutree(h = MAX_SIZE)

## Clusters
cluster_id_i <- 1
cluster_i <- survey_df[survey_df$cluster_id %in% cluster_id_i,]
coordinates(cluster_i) <- ~longitude+latitude
cl_ext <- extent(cluster_i) 
cl_centroid <- data.frame()

(cl_ext@xmin + cl_ext@xmax)/2
(cl_ext@ymin + cl_ext@ymax)/2

leaflet() %>%
  addTiles() %>%
  addCircles(data=cluster_i)




