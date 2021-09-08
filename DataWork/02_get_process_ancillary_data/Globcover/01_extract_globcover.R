# Extract Globcover Variables

replace_if_extracted <- F
SURVEY_NAME <- "DHS"

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

# Function to Extract Globcover ------------------------------------------------
country_code_i <- "IA"
buffer_m <- 5000
extract_globcover <- function(country_code_i, buffer_m){
  
  ## Subset to country and grab year
  df_country <- df[df$country_code %in% country_code_i,]
  year_i <- df_country$year[1]
  
  ## Project, buffer, then back to WGS
  # Go back to WGS so don't have to project larger raster
  UTM_PROJ <- define_country_proj(country_code_i)
  
  coordinates(df_country) <- ~longitude+latitude
  crs(df_country) <- CRS("+init=epsg:4326")
  df_country <- df_country %>%
    spTransform(CRS(UTM_PROJ)) %>%
    gBuffer(width = buffer_m, byid = T) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  ## Load globcover
  if(year_i > 2018) year_i <- 2018
  
  if(year_i <= 2015){
    gc <- raster(file.path(globcover_dir, "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), (year_i - 1991))
  } else{
    gc <- raster(file.path(globcover_dir, "RawData", "2016_2018_data", paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-",year_i,"-v2.1.1.tif")))
  }
  
  ## Crop globcover
  gc_crop <- crop(gc, bbox(df_country))
  
  for(gc_id_i in c(10, 11, 12,
                   20, 
                   30, 
                   40, 
                   50,
                   60, 61, 62,
                   70, 71, 72,
                   80, 81, 82,
                   90,
                   100,
                   110,
                   120, 121, 122,
                   130,
                   140,
                   150, 151, 152, 153,
                   160,
                   170,
                   180,
                   190,
                   200,
                   201,
                   202,
                   210,
                   220)){
    print(gc_id_i)
    
    gc_crop_i <- gc_crop
    gc_crop_i[] <- as.numeric(gc_crop_i[] == gc_id_i)
    
    gc_crop_i_vx <- velox(gc_crop_i)
    df_country[[paste0("gc_", gc_id_i)]] <- gc_crop_i_vx$extract(sp = df_country, fun = mean)[,1] %>% as.numeric()
  }
  
  df_out <- df_country@data %>%
    dplyr::select(uid, contains("gc_"))
  
  return(df_out)
}

# Implement Function and Export ------------------------------------------------
for(buffer_i in c(5000)){
  for(country_i in unique(df$country_code)){
    print(paste0(country_i, " - ", buffer_i))
    
    OUT_PATH <- file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", 
                          "globcover", 
                          paste0("gc_", country_i, "_", buffer_i, "m.Rds"))
    
    
    
    if(replace_if_extracted | !file.exists(OUT_PATH)){
      df_glob_i <- extract_globcover(country_i, 5000)
      saveRDS(df_glob_i, OUT_PATH)
    }
  }
}








