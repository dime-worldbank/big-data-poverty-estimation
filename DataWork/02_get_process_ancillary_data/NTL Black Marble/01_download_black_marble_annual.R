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
years <- 2012:2021

for(year_i in years){
  
  OUT_FILE <- file.path(ntl_bm_dir, "FinalData", "annual_rasters", 
                        paste0("bm_vnp46A4_",
                               year_i,
                               ".tif"))
  
  if(!file.exists(OUT_FILE)){
    print(year_i)
    
    r <- bm_mk_raster(loc_sf = survey_buff_sf,
                      product_id = "VNP46A4",
                      time = year_i,
                      bearer = BEARER,
                      mask = F)
    
    writeRaster(r, OUT_FILE)
  }
}
