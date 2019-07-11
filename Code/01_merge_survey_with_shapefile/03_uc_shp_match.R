# Match Based on Shapefile

# Load Data --------------------------------------------------------------------
survey_uc_df <- readRDS(file.path(final_data_file_path, "UC with NSER Data", "individual_files", "01_districttehsil.Rds"))

survey_uc_df$unioncouncil_name_howfoundmatch <- "matched names"

# Levenstein Distance Match Function -------------------------------------------
uc_survey_nomatch <- survey_uc_df$unioncouncil_name[!survey_uc_df$unioncouncil_name %in% uc_sdf$UC]

determine_best_match <- function(uc_survey_nomatch_i){
  
  if(sum(survey_uc_df$unioncouncil_name == uc_survey_nomatch_i) == 1){
    
    survey_tehsil_i <- survey_uc_df$tehsil_name[survey_uc_df$unioncouncil_name == uc_survey_nomatch_i]
    survey_district_i <- survey_uc_df$district_name[survey_uc_df$unioncouncil_name == uc_survey_nomatch_i]
    
    #### Restirct list of GIS UCs
    # UCs must be in same district/tehsil
    gis_ucs_in_districttehsil <- uc_sdf$UC[uc_sdf$DISTRICT == survey_district_i & uc_sdf$TEHSIL == survey_tehsil_i]
    
    # First letters must be the same
    gis_ucs_in_districttehsil <- gis_ucs_in_districttehsil[substr(gis_ucs_in_districttehsil,1,1) %in% substr(uc_survey_nomatch_i,1,1)]
    
    #### Determine Matches
    # Determine Best Match
    gis_bestmatch <- gis_ucs_in_districttehsil[amatch(uc_survey_nomatch_i, gis_ucs_in_districttehsil, maxDist=20)]
    levenstein_distance <- adist(uc_survey_nomatch_i, gis_bestmatch) %>% as.numeric
    
    # Determine Second Best Match
    gis_ucs_in_districttehsil_minusbest <- gis_ucs_in_districttehsil[gis_ucs_in_districttehsil != gis_bestmatch]
    
    gis_secondmatch <- gis_ucs_in_districttehsil_minusbest[amatch(uc_survey_nomatch_i, gis_ucs_in_districttehsil_minusbest, maxDist=20)]
    levenstein_distance_2nd <- adist(uc_survey_nomatch_i, gis_secondmatch) %>% as.numeric
    
  } else{
    gis_bestmatch <- NA
    levenstein_distance <- NA
    
    gis_secondmatch <- NA
    levenstein_distance_2nd <- NA
  }
  
  df_out <- data.frame(unioncouncil_name= uc_survey_nomatch_i,
                       gis_bestmatch=gis_bestmatch,
                       levenstein_distance=levenstein_distance,
                       gis_secondmatch,
                       levenstein_distance_2nd)
  
  return(df_out)
}

uc_leven_matches <- lapply(uc_survey_nomatch, determine_best_match) %>% bind_rows
uc_leven_matches <- uc_leven_matches[!is.na(uc_leven_matches$levenstein_distance),]

### Determine which matches to use
uc_leven_matches$use_match <- FALSE

### LEVEN DIST 1
# If best match has levenstein distance of 1 and no second best match
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 1 & is.na(uc_leven_matches$levenstein_distance_2nd)] <- TRUE

# If best match has levenstein distance of 1 and second match has dist >= 3
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 1 & uc_leven_matches$levenstein_distance_2nd %in% 3:100] <- TRUE

### LEVEN DIST 2
# If best match has levenstein distance of 2 and no second best match
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 2 & is.na(uc_leven_matches$levenstein_distance_2nd)] <- TRUE

# If best match has levenstein distance of 2 and second best match >= 5 AND "^chak " not in UC name
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 2 & uc_leven_matches$levenstein_distance_2nd %in% 5:100 & !grepl("^chak ", uc_leven_matches$unioncouncil_name)] <- TRUE

### LEVEN DIST 3
# If best match has levenstein distance of 3 and no second best match AND "^chak " not in UC name
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 3 & is.na(uc_leven_matches$levenstein_distance_2nd) & !grepl("^chak ", uc_leven_matches$unioncouncil_name)] <- TRUE

# If best match has levenstin distance of 3 and second best match has dist >= 9 AND "^chack " not in UC name
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 3 & uc_leven_matches$levenstein_distance_2nd %in% 9:100 & !grepl("^chak ", uc_leven_matches$unioncouncil_name)] <- TRUE

### LEVEN DIST 4
# If best match has levenstein distance of 4 and no second best match and "^chack " not in UC name and UC name has at least 6 characters
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 4 & is.na(uc_leven_matches$levenstein_distance_2nd) & !grepl("^chak ", uc_leven_matches$unioncouncil_name) & nchar(uc_leven_matches$unioncouncil_name) >= 6] <- TRUE

# If best match has levenstein distance of 4 and second best match has distance >= 10 and "^chack " not in UC name and UC name has at least 6 characters
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 4 & uc_leven_matches$levenstein_distance_2nd %in% 10:100 & !grepl("^chak ", uc_leven_matches$unioncouncil_name) & nchar(uc_leven_matches$unioncouncil_name) >= 6] <- TRUE

### LEVEN DIST 5
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 5 & is.na(uc_leven_matches$levenstein_distance_2nd) & !grepl("^chak ", uc_leven_matches$unioncouncil_name) & !(uc_leven_matches$unioncouncil_name %in% c("bindyal","abbas pura","jamil abad","son miani","shah wali"))] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 5 & uc_leven_matches$levenstein_distance_2nd %in% 7:100 & !grepl("^chak ", uc_leven_matches$unioncouncil_name) & !(uc_leven_matches$unioncouncil_name %in% c("ladhike","kathyala virkan","parel","saboor"))] <- TRUE

### LEVEN DIST 6
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 6 & is.na(uc_leven_matches$levenstein_distance_2nd) & !grepl("^chak ", uc_leven_matches$unioncouncil_name) & !(uc_leven_matches$unioncouncil_name %in% c("bhattian","harsa sheikh","farooqabad","dalowal","amarpura"))] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 6 & uc_leven_matches$levenstein_distance_2nd %in% 10:100 & !grepl("^chak ", uc_leven_matches$unioncouncil_name)] <- TRUE

### LEVEN DIST 7
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 7 & uc_leven_matches$levenstein_distance_2nd %in% 8:9 & !grepl("^chak ", uc_leven_matches$unioncouncil_name) & !(uc_leven_matches$unioncouncil_name %in% c("bajra gharhi","bhon fazbla","dianat pura","gullu wali","hardo saharan","maroof","nawan shehr","nawan shehr","phoklian","wanjo wali","hafiz wala","kotha kalan","mahmood abad","roshan bhila","sanjwal cantt"))] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 7 & uc_leven_matches$levenstein_distance_2nd %in% 10:100 & !grepl("^chak ", uc_leven_matches$unioncouncil_name) & !(uc_leven_matches$unioncouncil_name %in% c("khanke mor"))] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 7 & is.na(uc_leven_matches$levenstein_distance_2nd) & !grepl("^chak ", uc_leven_matches$unioncouncil_name) & !(uc_leven_matches$unioncouncil_name %in% c("urban 6", "urban 5","urban 7","urban 1","hali","new multan","abid abad","rehmatabad","parial","ranial"))] <- TRUE

# Sample code to check matches
#candidate_match_df <- uc_leven_matches[uc_leven_matches$levenstein_distance == 4 & uc_leven_matches$levenstein_distance_2nd %in% 10:100 & !grepl("^chak ", uc_leven_matches$unioncouncil_name),]
#uc_leven_matches$use_match[uc_leven_matches$levenstein_distance == 4 & uc_leven_matches$levenstein_distance_2nd %in% 10:100 & !grepl("^chak ", uc_leven_matches$unioncouncil_name)] <- TRUE

### Manaully Accept Certain Ones
uc_leven_matches$use_match[grepl("^okara [[:digit:]]", uc_leven_matches$unioncouncil_name)] <- TRUE
uc_leven_matches$use_match[grepl("^attock [[:digit:]]", uc_leven_matches$unioncouncil_name)] <- TRUE
uc_leven_matches$use_match[grepl("^gujrat [[:digit:]]", uc_leven_matches$unioncouncil_name)] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "mad pir wala"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "ghar maharaja"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "shah pur uc"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "korianwali"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "barasajwar khan"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "kamra cantt"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "kamra kalan"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "donga akoka"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "musa bhota"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "rojhanwali"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "chak no. 12bc"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "chak no. 24b.c"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "chak no. 37/b.c"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "chak no. 4b.c"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "jamal chand"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "chak no. 60/61 ml"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "ali pur syedan"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "bhalwal -iv"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "bhera -i"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "157 10 r"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "abas pur"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "aman gharh"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "androon bhaati gate uc"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "androon bhatti gate uc"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "androon dehli gate uc"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "arif wala eb/147"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "badorta"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "bali wala"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "baloksar"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "bankey cheema"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "bariat (chak bawa)"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "barj atari"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "basir pur 1"] <- TRUE
uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% "basir pur 2"] <- TRUE
#uc_leven_matches$use_match[uc_leven_matches$unioncouncil_name %in% ""] <- TRUE

### Check which ones don't match
# uc_leven_matches_CHECK <- uc_leven_matches[uc_leven_matches$use_match == FALSE,]

#### Merge matches back into dataset
uc_leven_matches <- uc_leven_matches[uc_leven_matches$use_match == TRUE,]
uc_leven_matches <- subset(uc_leven_matches, select=c(unioncouncil_name, gis_bestmatch))

survey_uc_df <- merge(survey_uc_df, uc_leven_matches, by="unioncouncil_name", all.x=T)
survey_uc_df$unioncouncil_name[!is.na(survey_uc_df$gis_bestmatch)] <- survey_uc_df$gis_bestmatch[!is.na(survey_uc_df$gis_bestmatch)]
survey_uc_df <- subset(survey_uc_df, select=-c(gis_bestmatch))

# Export =======================================================================
saveRDS(survey_uc_df, file.path(final_data_file_path,"UC with NSER Data", "individual_files", "02_districttehsil_shpmatch.Rds"))


