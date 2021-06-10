# Clean OPM Data

# Clean Oxford Policy Management (OPM) survey data. Create Household Level 
# dataframe with relevant socioeconomic variables.

# Load Data --------------------------------------------------------------------
bisp_plist <- read_dta(file.path(opm_dir, "RawData - Deidentified", 'bisp_combined_plist.dta'))
bisp_povscore <- read_dta(file.path(opm_dir, "RawData - Deidentified", 'UID_pscores.dta'))

bisp_df <- merge(bisp_plist, bisp_povscore, by=c("period", "uid"))

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
  mutate(year = period %>% as_factor %>% as.character %>% as.numeric,
         hh_size = N_adults + N_children) %>%
  as.data.frame()

# Clean and Add Asset Data -----------------------------------------------------
#### Load Data
asset_df <- read_dta(file.path(opm_dir, "RawData - Deidentified", 'male', '06_ModuleL.dta'))

#### Prep Data
asset_df <- asset_df %>%
  dplyr::mutate(asset = LLN %>% 
                  as_factor() %>% 
                  as.character() %>%
                  tolower() %>%
                  str_replace_all("[[:punct:]]", "") %>%
                  str_replace_all(" ", "_") %>%
                  str_replace_all("__", "_"),
                own = LNONE %>% 
                  as_factor() %>% 
                  as.character(),
                year = period %>% 
                  as_factor %>% 
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
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate(consumption_total = v11 + v21 + v31 + v41) %>%
  
  ## Adjust comsumption based on days reported
  mutate(consumption_total = consumption_total*(30/days)) %>%
  
  ## Aggregate
  group_by(uid, period) %>%
  dplyr::summarise(consumption_total = sum(consumption_total)) 

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

# Merge in Coordinates & GADM Data ---------------------------------------------
#### Coords
opm_coords <- read.csv(file.path(secure_file_path, "Data", "OPM", "FinalData - PII", "GPS_uid_crosswalk.csv"))

bisp_df <- bisp_df %>%
  left_join(opm_coords, by = "uid")

#### GADM
pak_adm3 <- readRDS(file.path(gadm_dir, "RawData", 'gadm36_PAK_3_sp.rds'))

bisp_df_geo <- bisp_df %>%
  dplyr::select(uid, latitude, longitude) %>%
  dplyr::filter(!is.na(latitude)) %>%
  distinct()

coordinates(bisp_df_geo) <- ~longitude+latitude
crs(bisp_df_geo) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

bisp_df_geo_OVER_pak_adm3 <- over(bisp_df_geo, pak_adm3)
bisp_df_geo_OVER_pak_adm3$uid <- bisp_df_geo$uid 
#bisp_df_geo_OVER_pak_adm3$tile_id <- bisp_df_geo$tile_id 

bisp_df_geo_OVER_pak_adm3 <- bisp_df_geo_OVER_pak_adm3 %>%
  dplyr::select(uid, 
                GID_1, NAME_1,
                GID_2, NAME_2,
                GID_3, NAME_3) %>%
  dplyr::rename(gadm_id_1 = GID_1,
                gadm_id_2 = GID_2,
                gadm_id_3 = GID_3,
                gadm_name_1 = NAME_1,
                gadm_name_2 = NAME_2,
                gadm_name_3 = NAME_3)

bisp_df <- left_join(bisp_df,
                     bisp_df_geo_OVER_pak_adm3,
                     by = "uid")

# Export -----------------------------------------------------------------------
## Without Lat/Lon
saveRDS(bisp_df %>% 
          dplyr::select(-c(latitude, longitude)), 
        file.path(opm_dir, "FinalData", "Individual Datasets", "opm_socioeconomic.Rds"))

write.csv(bisp_df %>% 
            dplyr::select(-c(latitude, longitude)), 
          file.path(opm_dir, "FinalData", "Individual Datasets", "opm_socioeconomic.csv"), row.names = F)

## With Lat/Lon
saveRDS(bisp_df, file.path(secure_file_path, "Data", "OPM", "FinalData - PII", "opm_socioeconomic_geo.Rds"))

write.csv(bisp_df, file.path(secure_file_path, "Data", "OPM", "FinalData - PII", "opm_socioeconomic_geo.csv"), row.names = F)






