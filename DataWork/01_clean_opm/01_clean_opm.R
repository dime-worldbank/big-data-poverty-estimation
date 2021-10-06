# Clean OPM Data

# Clean Oxford Policy Management (OPM) survey data. Create Household Level 
# dataframe with relevant socioeconomic variables.

set.seed(42)

# Load Data --------------------------------------------------------------------
bisp_plist <- read_dta(file.path(opm_dir, "RawData - Deidentified", 'bisp_combined_plist.dta'))
bisp_povscore <- read_dta(file.path(opm_dir, "RawData - Deidentified", 'UID_pscores.dta'))

bisp_df <- bisp_plist %>%
  left_join(bisp_povscore, by = c("period", "uid"))

# Clean Main Data --------------------------------------------------------------
names(bisp_df) <- names(bisp_df) %>% tolower
bisp_df <- bisp_df %>%
  dplyr::rename(days_worked_last_month = cq09,
                income_last_month = cq10,
                months_worked_last_year = cq11,
                income_last_year = cq12) %>%
  mutate(adult = aq02 >= 18) %>%
  dplyr::select(uid, province, psu, locality, period, treatment, panel, 
                present11, present13, present13, present16, bispln,
                days_worked_last_month, income_last_month,
                months_worked_last_year, income_last_year,
                pscores, adult) %>%
  group_by(uid, province, psu, locality, period, treatment, panel, 
           present11, present13, present16) %>%
  dplyr::summarise(hh_size = n(),
                   income_last_month_N_NAs = sum(is.na(income_last_month)), 
                   income_last_month = sum(income_last_month, na.rm=T),
                   income_last_year_N_NAs = sum(is.na(income_last_year)), 
                   income_last_year = sum(income_last_year, na.rm=T),
                   pscores = mean(pscores, na.rm=T),
                   N_adults = sum(adult %in% 1),
                   N_children = sum(adult %in% 0)) %>%
  ungroup() %>%
  mutate(year = period %>% haven::as_factor() %>% as.character %>% as.numeric,
         hh_size = N_adults + N_children) %>%
  as.data.frame()

# Clean and Add Asset Data -----------------------------------------------------
#### Load Data
asset_df <- read_dta(file.path(opm_dir, "RawData - Deidentified", 'male', '06_ModuleL.dta'))

#### Prep Data
asset_df <- asset_df %>%
  dplyr::mutate(asset = LLN %>% 
                  haven::as_factor() %>% 
                  as.character() %>%
                  tolower() %>%
                  str_replace_all("[[:punct:]]", "") %>%
                  str_replace_all(" ", "_") %>%
                  str_replace_all("__", "_"),
                own = LNONE %>% 
                  haven::as_factor() %>% 
                  as.character(),
                year = period %>% 
                  haven::as_factor() %>% 
                  as.character %>% 
                  as.numeric) %>%
  dplyr::select(uid, year, asset, own)

## If not Yes/No, replace with NA (some "9"s)
asset_df$own[!(asset_df$own %in% c("Yes", "No"))] <- NA

## Convert to 0/1 [1 = Yes]
asset_df$own <- ifelse(asset_df$own == "Yes", 1, 0)

## Not distinct
# For example, uid 22203737 in year 2016 had three rows for "TV", where only
# in one of rows said "own." Arrange by own, so in cases like this we use
# "1" (own), not "0".
asset_df <- asset_df %>%
  arrange(desc(own)) %>%
  distinct(uid, year, asset, .keep_all = T) 

## Reshape
asset_df <- asset_df %>%
  pivot_wider(id_cols = c(uid, year),
              names_from = asset,
              values_from = own,
              values_fill = 0)

## Replace NA with 0
for(var in names(asset_df)[!names(asset_df) %in% c("uid", "year")]){
  asset_df[[var]][is.na(asset_df[[var]])] <- 0
}

#### Rename Asset Variables
asset_df <- asset_df %>% rename_at(vars(-uid, -year), ~ paste0("asset_", .))

#### PCA Index
pca <- asset_df %>%
  dplyr::select(-c(uid, year)) %>%
  prcomp()

asset_df$asset_index_pca1 <- pca$x[,1]

#### Addiditive Index
asset_df$asset_index_additive <- asset_df %>%
  dplyr::select(-c(uid, year, 
                   asset_index_pca1)) %>%
  apply(1, sum)

bisp_df <- merge(bisp_df, asset_df, by = c("uid", "year"),
                 all.x=T, all.y=T)

# Clean and Add Consumption Data -----------------------------------------------
#### Load Data
consum_fm_a <- read_dta(file.path(opm_dir, "RawData - Deidentified", "female",  "ModuleGPartA.dta")) %>%
  dplyr::select(uid, period, v11, v21, v31, v41) %>%
  mutate(days = 14)

consum_fm_b <- read_dta(file.path(opm_dir, "RawData - Deidentified", "female",  "ModuleGPartB.dta")) %>%
  dplyr::select(uid, period, v11, v21, v31, v41) %>%
  mutate(days = 30)

consum_ma_b <- read_dta(file.path(opm_dir, "RawData - Deidentified", "male",  "ModuleGPartB.dta")) %>%
  dplyr::select(uid, period, v11, v21, v31, v41) %>%
  mutate(days = 30)

consum_ma_c <- read_dta(file.path(opm_dir, "RawData - Deidentified", "male",  "ModuleGPartC.dta")) %>%
  dplyr::select(uid, period, v11, v21, v31, v41) %>%
  mutate(days = 365)

consum_ma_d <- read_dta(file.path(opm_dir, "RawData - Deidentified", "male",  "ModuleGPartD.dta")) %>%
  dplyr::select(uid, period, v11, v21, v31, v41) %>%
  mutate(days = 365)

#### Clean
consum_all <- bind_rows(consum_fm_a,
                        consum_fm_b,
                        consum_ma_b,
                        consum_ma_c,
                        consum_ma_d)

## Total Consumption
consum_sum_all <- consum_all %>%
  dplyr::select(uid, period, v11, v21, v31, v41, days) %>%
  dplyr::mutate_if(is.numeric, ~ tidyr::replace_na(., 0)) %>%
  dplyr::mutate(consumption_total = v11 + v21 + v31 + v41) %>%
  
  ## Adjust comsumption based on days reported
  dplyr::mutate(consumption_total = consumption_total*(30/days)) %>%
  
  ## Aggregate
  dplyr::group_by(uid, period) %>%
  dplyr::summarise(consumption_total = sum(consumption_total)) %>%
  ungroup()

bisp_df <- merge(bisp_df, consum_sum_all, by = c("uid", "period"),
                 all.x=T, all.y=T)

# Add Poverty Line -------------------------------------------------------------
pov_line_2013 <- 3030.32 
pov_line_2011 <- (116.189 / 136.913)*3030.32
pov_line_2014 <- (142.798 / 136.913)*3030.32
pov_line_2016 <- (152.797 / 136.913)*3030.32

bisp_df$pov_line <- NA
bisp_df$pov_line[bisp_df$year %in% 2011] <- pov_line_2011
bisp_df$pov_line[bisp_df$year %in% 2013] <- pov_line_2013
bisp_df$pov_line[bisp_df$year %in% 2014] <- pov_line_2014
bisp_df$pov_line[bisp_df$year %in% 2016] <- pov_line_2016

# Create/Adjust Variables ------------------------------------------------------
# Number adult equivalent
bisp_df$N_adult_equiv <- bisp_df$N_adults + bisp_df$N_children * 0.8

# Consumption per adult equivalent
bisp_df$consumption_adult_equiv <- (bisp_df$consumption_total / bisp_df$N_adult_equiv)

# Create Survey Round Variables
bisp_df$survey_round <- NA
bisp_df$survey_round[bisp_df$year %in% 2011] <- 1
bisp_df$survey_round[bisp_df$year %in% 2013] <- 2
bisp_df$survey_round[bisp_df$year %in% 2014] <- 3
bisp_df$survey_round[bisp_df$year %in% 2016] <- 4

# UID to numeric
bisp_df$uid <- bisp_df$uid %>% as.numeric()

# Constant sample identifies
bisp_df <- bisp_df %>%
  group_by(uid) %>%
  dplyr::mutate(years_surveyed = year %>% paste(collapse=";")) %>%
  ungroup() %>%
  mutate(constant_11_13 = 
           str_detect(years_surveyed, "2011") & 
           str_detect(years_surveyed, "2013"),
         constant_11_13_14 = 
           str_detect(years_surveyed, "2011") & 
           str_detect(years_surveyed, "2013") &
           str_detect(years_surveyed, "2014"),
         constant_11_13_14_16 = 
           str_detect(years_surveyed, "2011") & 
           str_detect(years_surveyed, "2013") &
           str_detect(years_surveyed, "2014") &
           str_detect(years_surveyed, "2016"))

# Merge in Coordinates ---------------------------------------------------------
## Load Data
opm_coords <- read_dta(file.path(secure_dir, "Data", "OPM", "RawData - PII", "GPS_uid_crosswalk.dta"))
pak_adm0 <- readRDS(file.path(gadm_dir, "RawData", "gadm36_PAK_0_sp.rds"))

## To crs:4326
opm_coords <- opm_coords %>%
  filter(!is.na(GPSN)) %>%
  
  mutate(latitude = get_lat_lon(GPSN),
         longitude = get_lat_lon(GPSE),
         uid = uid %>% as.numeric()) %>%
  
  dplyr::select(uid, latitude, longitude) %>%
  
  filter(latitude <= 100,
         longitude <= 100)

## Restrict to Coordinates in Pakistan & Tile ID
opm_coords_sdf <- opm_coords
coordinates(opm_coords_sdf) <- ~longitude+latitude
crs(opm_coords_sdf) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

dist_to_pak <- gDistance(opm_coords_sdf, pak_adm0, byid=T) %>% as.vector()
opm_coords <- opm_coords[dist_to_pak == 0,]

## Merge
bisp_df <- bisp_df %>%
  left_join(opm_coords, by = "uid")

# Merge in GADM ----------------------------------------------------------------
pak_adm3 <- readRDS(file.path(gadm_dir, "RawData", 'gadm36_PAK_3_sp.rds'))

## Add within country fold at ADM2 level
pak_adm2_df <- pak_adm3@data %>%
  distinct(GID_2)

within_country_fold <- rep_len(1:5, length.out = nrow(pak_adm2_df)) %>% sample()
pak_adm2_df$within_country_fold <- paste0("PK_", within_country_fold)

pak_adm3 <- merge(pak_adm3, pak_adm2_df, by = "GID_2")

bisp_df_geo <- bisp_df %>%
  dplyr::select(uid, latitude, longitude) %>%
  dplyr::filter(!is.na(latitude)) %>%
  distinct()

coordinates(bisp_df_geo) <- ~longitude+latitude
crs(bisp_df_geo) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

bisp_df_geo_OVER_pak_adm3 <- over(bisp_df_geo, pak_adm3)
bisp_df_geo_OVER_pak_adm3$uid <- bisp_df_geo$uid 

bisp_df_geo_OVER_pak_adm3 <- bisp_df_geo_OVER_pak_adm3 %>%
  dplyr::select(uid,
                within_country_fold,
                GID_1, NAME_1,
                GID_2, NAME_2,
                GID_3, NAME_3) 

bisp_df <- left_join(bisp_df,
                     bisp_df_geo_OVER_pak_adm3,
                     by = "uid")

# Aggregate to PSU -------------------------------------------------------------
bisp_df$uid <- paste0(bisp_df$psu, "_", bisp_df$GID_3)

bisp_mean_df <- bisp_df %>%
  dplyr::group_by(uid, locality, year, survey_round,
                  GID_3, GID_2, GID_1, 
                  NAME_3, NAME_2, NAME_1,
                  within_country_fold) %>%
  dplyr::summarise_at(vars(pscores, 
                           income_last_month, 
                           consumption_total,
                           consumption_adult_equiv,
                           contains("asset_")), mean, na.rm = T)

bisp_latlon_df <- bisp_df %>%
  dplyr::group_by(uid) %>%
  dplyr::summarise_at(vars(latitude, longitude), median, na.rm = T)

bisp_sum_df <- bisp_df %>%
  dplyr::group_by(uid, locality, year, survey_round) %>%
  mutate(N = 1) %>%
  dplyr::summarise_at(vars(N), sum, na.rm = T)

bisp_agg_df <- bisp_mean_df %>%
  left_join(bisp_latlon_df, by = c("uid")) %>%
  left_join(bisp_sum_df, by = c("uid", "locality", "year", "survey_round")) %>%
  dplyr::rename(urban_rural = locality) %>%
  dplyr::mutate(urban_rural = urban_rural %>% 
                  haven::as_factor() %>% 
                  as.character() %>% 
                  substring(1,1),
                country_code = "PK") 

## Remove if no coordinates
bisp_agg_df <- bisp_agg_df %>%
  dplyr::filter(!is.na(latitude),
                !is.na(longitude))

bisp_agg_df <- bisp_agg_df %>%
  ungroup()

## Add country name
bisp_agg_df$country_name <- "Pakistan"

# CHECK DIST BETWEEN PSUs
if(F){
  df <- map_df(unique(bisp_df$psu_gadm3), function(i){
    bisp_df_i <- bisp_df[bisp_df$psu_gadm3 %in% i,]
    
    lat_max <- max(bisp_df_i$latitude)
    lon_max <- max(bisp_df_i$longitude)
    
    lat_min <- min(bisp_df_i$latitude)
    lon_min <- min(bisp_df_i$longitude)
    
    d <- sqrt((lat_max - lat_min)^2 + (lon_max - lon_min)^2)*111.12
    
    return(data.frame(psu = i,
                      dist = d))
  })
  
  mean(df$dist < 10)
  
  leaflet() %>%
    addTiles() %>%
    #addCircles(data = bisp_df[bisp_df$psu != 1,], color = "red") %>%
    addCircles(data = bisp_df[bisp_df$psu_gadm3 %in% "57PAK.7.8.4_1",])  
}


# Export -----------------------------------------------------------------------
saveRDS(bisp_agg_df, file.path(opm_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
write.csv(bisp_agg_df, file.path(opm_dir, "FinalData", "Individual Datasets", "survey_socioeconomic.csv"), row.names = F)

saveRDS(bisp_agg_df, file.path(gdrive_dir, "Data", "OPM", "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))
write.csv(bisp_agg_df, file.path(gdrive_dir, "Data", "OPM", "FinalData", "Individual Datasets", "survey_socioeconomic.csv"), row.names = F)



# ## Without Lat/Lon
# saveRDS(bisp_df %>% 
#           dplyr::select(-c(latitude, longitude)), 
#         file.path(opm_dir, "FinalData", "Individual Datasets", "opm_socioeconomic.Rds"))
# 
# write.csv(bisp_df %>% 
#             dplyr::select(-c(latitude, longitude)), 
#           file.path(opm_dir, "FinalData", "Individual Datasets", "opm_socioeconomic.csv"), row.names = F)
# 
# ## With Lat/Lon
# saveRDS(bisp_df, file.path(secure_file_path, "Data", "OPM", "FinalData - PII", "opm_socioeconomic_geo.Rds"))
# 
# write.csv(bisp_df, file.path(secure_file_path, "Data", "OPM", "FinalData - PII", "opm_socioeconomic_geo.csv"), row.names = F)






