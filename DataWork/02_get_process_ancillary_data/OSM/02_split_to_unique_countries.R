# Split into unique countries

# In some cases, two countries are combined in the same datset. Here, we save a 
# datset for each country.

# Split into new directories: 2 countries --------------------------------------
split_files_to_new_dirs <- function(dirs_vec,
                                    root_name_orig,
                                    new_name_1,
                                    new_name_2,
                                    iso_1,
                                    iso_2,
                                    extract_1 = TRUE,
                                    extract_2 = TRUE){
  # DESCRIPTION: Splits files by country into new country-specific directories
  # ARGS:
  # dirs_vec: Vector of names of all OSM directories with country pairs
  # root_name_orig: Original root name of osm dir (eg, "haiti-and-domrep")
  # new_name_1: What to rename directory for country 2 (eg, "domrep")
  # new_name_2: What to rename directory for country 2 (eg, "haiti")
  # iso_1: iso3 of country 1
  # iso_2: iso3 of country 2
  # extract_1: If want to create new directory+files for country 1
  # extract_2: If want to create new directory+files for country 2
  
  ## Loop through directories 
  for(dir_i in dirs_vec){
    print(dir_i)
    
    files <- list.files(file.path(osm_dir, "FinalData", dir_i))
    
    dir_1_i <- dir_i %>% str_replace_all(root_name_orig, new_name_1)
    dir_2_i <- dir_i %>% str_replace_all(root_name_orig, new_name_2)
    
    if(extract_1 %in% TRUE) dir.create(file.path(osm_dir, "FinalData", dir_1_i))
    if(extract_2 %in% TRUE) dir.create(file.path(osm_dir, "FinalData", dir_2_i))
    
    ## Loop through files within directory i
    for(file_i in files){
      print(file_i)
      df_i <- readRDS(file.path(osm_dir, "FinalData", dir_i, file_i))
      
      ## Load country files
      if(extract_1 %in% TRUE){
        ## Load GADM
        gadm_1 <- readRDS(file.path(gadm_dir, "RawData", paste0("gadm36_",iso_1,"_0_sp.rds")))
        
        ## Extent files 
        gadm_1_extent <- as(extent(gadm_1), "SpatialPolygons")
        crs(gadm_1_extent) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        gadm_1_extent$id <- 1
        
        ## Intersects
        df_1_i <- df_i[gIntersects_chunks(df_i, gadm_1_extent, 100),]
        df_1_i <- df_1_i[gIntersects_chunks(df_1_i, gadm_1, 50),]
        
        ## Export
        saveRDS(df_1_i, file.path(osm_dir, "FinalData", dir_1_i, file_i))
      } 
      
      if(extract_2 %in% TRUE){ 
        ## Load GADM
        gadm_2 <- readRDS(file.path(gadm_dir, "RawData", paste0("gadm36_",iso_2,"_0_sp.rds")))
        
        ## Extent files 
        gadm_2_extent <- as(extent(gadm_2), "SpatialPolygons")
        crs(gadm_2_extent) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        gadm_2_extent$id <- 1
        
        ## Intersects
        df_2_i <- df_i[gIntersects_chunks(df_i, gadm_2_extent, 100),]
        df_2_i <- df_2_i[gIntersects_chunks(df_2_i, gadm_2, 50),]
        
        ## Export
        saveRDS(df_2_i, file.path(osm_dir, "FinalData", dir_2_i, file_i))
      }
    }
  }
  
  return("Done!") 
}

## Haiti and Dominican Republic
dom_hti_dirs <- file.path(osm_dir, "FinalData") %>%
  list.files() %>%
  str_subset("haiti-and-domrep")

split_files_to_new_dirs(dirs_vec = dom_hti_dirs,
                        root_name_orig = "haiti-and-domrep",
                        new_name_1 = "domrep",
                        new_name_2 = "haiti",
                        iso_1 = "DOM",
                        iso_2 = "HTI")

## Haiti and Dominican Republic
sen_gam_dirs <- file.path(osm_dir, "FinalData") %>%
  list.files() %>%
  str_subset("senegal-and-gambia")

split_files_to_new_dirs(dirs_vec = sen_gam_dirs,
                        root_name_orig = "senegal-and-gambia",
                        new_name_1 = "senegal",
                        new_name_2 = "gambia",
                        iso_1 = "SEN",
                        iso_2 = "GMB")

## Timor-Leste (Extracted from Indonesia)
sen_gam_dirs <- file.path(osm_dir, "FinalData") %>%
  list.files() %>%
  str_subset("senegal-and-gambia")

split_files_to_new_dirs(dirs_vec = sen_gam_dirs,
                        root_name_orig = "indonesia",
                        new_name_1 = "timor-leste",
                        new_name_2 = "indonesia_TEMP",
                        iso_1 = "TLS",
                        iso_2 = "IDN",
                        extract_1 = TRUE,
                        extract_2 = FALSE)





