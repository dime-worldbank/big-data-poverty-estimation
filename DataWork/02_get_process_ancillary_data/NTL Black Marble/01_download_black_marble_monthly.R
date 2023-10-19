# Download Monthly Black Marble

BEARER <- "BEARER-HERE"

# Load data --------------------------------------------------------------------

#### Load and prep survey data
survey_sp <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
coordinates(survey_sp) <- ~longitude+latitude
crs(survey_sp) <- CRS("+init=epsg:4326")

survey_buff_sp <- gBuffer_chunks(survey_sp, 10/111.12, 1000)
#survey_buff_df_IA <- geo.buffer_chunks(survey_df_IA, r = buffer_m, chunk_size = 100)

## Dissolve into one polygon
survey_buff_sp$id <- 1
survey_buff_sp <- raster::aggregate(survey_buff_sp, by = "id")

## to sf
survey_buff_sf <- survey_buff_sp %>% st_as_sf()

# Download data ----------------------------------------------------------------
months <- seq(from = ymd("2012-01-01"), to = ymd("2021-12-01"), by=1) %>%
  substring(1, 7) %>%
  unique()

for(year_month_i in months){
  
  OUT_FILE <- file.path(ntl_bm_dir, "FinalData", "monthly_rasters", 
                        paste0("bm_vnp46A3_",
                               year_month_i %>% str_replace_all("-", "_"),
                               ".tif"))
  
  if(!file.exists(OUT_FILE)){
    print(year_month_i)
    
    r <- bm_mk_raster(loc_sf = survey_buff_sf,
                      product_id = "VNP46A3",
                      time = year_month_i,
                      bearer = BEARER,
                      mask = F)
    
    writeRaster(r, OUT_FILE)
  }
}
