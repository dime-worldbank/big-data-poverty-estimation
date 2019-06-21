# Enter UC Names into Google's Geocoding API

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"

library(stringr)
library(ggmap)

register_google(key="PUT-API-KEY-HERE")

# Load Data --------------------------------------------------------------------
survey_uc_df <- read.csv(file.path(project_file_path, "Data", "RawData", "NSER", "Union Council Level Dataset","nser_data_uc.csv"))

# Prep variable to input into geocoding API
survey_uc_df$full_address <- paste(survey_uc_df$unioncouncil_name,
                                                survey_uc_df$tehsil_name,
                                                survey_uc_df$district_name,
                                                "punjab",
                                                "pakistan",
                                                sep=", ") %>% tolower %>% str_squish()

i <- 1
geocode_using_google <- function(full_address){
  df_out <- geocode(full_address, output="more", source="google")
  #df_out <- lapply(df_out$results, as.data.frame) %>% bind_rows
  if(nrow(df_out) == 0){
    df_out <- data.frame(address_input = full_address)
  } else{
    df_out$address_input <- full_address
  }
  
  i <<- i + 1
  print(i)
  return(df_out)
}

survey_uc_df_geocoded <- lapply(survey_uc_df$full_address, geocode_using_google) %>% bind_rows

# Export -----------------------------------------------------------------------
write.csv(survey_uc_df_geocoded, file.path(project_file_path, "Data", "FinalData", "UC Google Geocoder Output", "uc_google_geocoder_output.csv"), row.names=F)
saveRDS(survey_uc_df_geocoded, file.path(project_file_path, "Data", "FinalData", "UC Google Geocoder Output", "uc_google_geocoder_output.Rds"))



