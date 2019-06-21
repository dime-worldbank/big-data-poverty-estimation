# Create a Union Council Level Dataset from the Survey Data
# Average variables within Union Councils and determine number of observations 
# within UCs

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Pakistan Poverty Estimation from Satellites"

if(Sys.info()[["user"]] == "WB521633") nser_pitb_file_path <- "C:/Users/wb521633/Dropbox/NSER from PITB"
if(Sys.info()[["user"]] == "robmarty") nser_pitb_file_path <- "~/Dropbox/NSER from PITB"

library(rgdal)
library(readstata13)
library(dplyr)
library(data.table)
library(mltools)

# Load Data --------------------------------------------------------------------
var_names <- c("document_id",
               "hhr_id",            
               "citizen_no",        
               "gender",           
               "age",               
               "head_relation",     
               "district_name",     
               "tehsil_name",      
               "unioncouncil_name", 
               "village_name",      
               "postoffice_name",   
               "head_address",    
               "disability",        
               "marital_status",    
               "emp_category",      
               "education_status", 
               "education_level",   
               "no_rooms",          
               "toilet_kind",       
               "tv_p",             
               "aircooler_p",       
               "freezer_p",         
               "washingmachine_p",  
               "airconditioner_p", 
               "heater_p",          
               "microwaveoven_p",   
               "cookingrange_p",    
               "sheep_p",          
               "bull_p",            
               "cow_p",             
               "buffalo_p",         
               "goat_p",           
               "land_area",         
               "land_unit",        
               "pmt_score",         
               "v36")  

dta_files <- list.files(file.path(nser_pitb_file_path, "temp"),
                        pattern = "^_general*",
                        full.names=T)

load_and_clean_asset_other_vars <- function(file){
  
  survey_df_i <- read.dta13(file)
  names(survey_df_i) <- var_names
  survey_df_i <- subset(survey_df_i, select=c(#document_id,hhr_id,
                                              head_relation,
                                              district_name,tehsil_name,unioncouncil_name,
                                              no_rooms,tv_p,aircooler_p,freezer_p,
                                              washingmachine_p,airconditioner_p,
                                              heater_p,microwaveoven_p,cookingrange_p,
                                              sheep_p,bull_p,cow_p,buffalo_p,goat_p,pmt_score,age,
                                              village_name,
                                              postoffice_name))

  for(var in c("tv_p","aircooler_p","freezer_p","washingmachine_p","airconditioner_p",
               "heater_p","microwaveoven_p","cookingrange_p","sheep_p","bull_p","cow_p",
               "buffalo_p","goat_p")){
    survey_df_i[[var]] <- as.numeric(survey_df_i[[var]] %in% "y")
  }
  
  #survey_df_i$document_id <- as.character(survey_df_i$document_id)
  #survey_df_i$hhr_id <- as.character(survey_df_i$hhr_id)
  survey_df_i$village_name <- as.character(survey_df_i$village_name)
  survey_df_i$postoffice_name <- as.character(survey_df_i$postoffice_name)
  survey_df_i$district_name <- as.character(survey_df_i$district_name)
  survey_df_i$tehsil_name <- as.character(survey_df_i$tehsil_name)
  survey_df_i$unioncouncil_name <- as.character(survey_df_i$unioncouncil_name)
  
  survey_df_i$no_rooms <- as.numeric(survey_df_i$no_rooms)
  survey_df_i$pmt_score <- as.numeric(survey_df_i$pmt_score)
  survey_df_i$age <- as.numeric(survey_df_i$age)
  
  survey_df_i$head_relation_head <- as.numeric(survey_df_i$head_relation %in% "head")
  survey_df_i <- subset(survey_df_i, select=-c(head_relation))
  
  # Asset Indices --------------------------------------------------------------
  survey_df_i$asset_item_index <- survey_df_i$tv_p +
    survey_df_i$aircooler_p +
    survey_df_i$freezer_p +
    survey_df_i$washingmachine_p + 
    survey_df_i$airconditioner_p +
    survey_df_i$heater_p +
    survey_df_i$microwaveoven_p + 
    survey_df_i$cookingrange_p
  
  survey_df_i$asset_animal_index <- survey_df_i$sheep_p + 
    survey_df_i$bull_p + 
    survey_df_i$cow_p + 
    survey_df_i$buffalo_p + 
    survey_df_i$goat_p
  
  if(F){
  # Maritial Status
  survey_df_i$marital_status_divorced <- as.numeric(survey_df_i$marital_status %in% "divorced")
  survey_df_i$marital_status_married <- as.numeric(survey_df_i$marital_status %in% "married")
  survey_df_i$marital_status_separated <- as.numeric(survey_df_i$marital_status %in% "separated")
  survey_df_i$marital_status_unmarried <- as.numeric(survey_df_i$marital_status %in% "unmarried")
  survey_df_i$marital_status_widowed <- as.numeric(survey_df_i$marital_status %in% "widowed")
  
  # Gender
  survey_df_i$gender_female <- as.numeric(survey_df_i$gender %in% "female")
  survey_df_i$gender_male <- as.numeric(survey_df_i$gender %in% "male")
  
  # Head Relation
  survey_df_i$head_relation_child <- as.numeric(survey_df_i$head_relation %in% "child/adopted child")
  survey_df_i$head_relation_fathermother <- as.numeric(survey_df_i$head_relation %in% "father/mother")
  survey_df_i$head_relation_fatherMotherInLaw <- as.numeric(survey_df_i$head_relation %in% "father/mother in law")
  survey_df_i$head_relation_grandchild <- as.numeric(survey_df_i$head_relation %in% "grandchild")
  survey_df_i$head_relation_grandfatherMother <- as.numeric(survey_df_i$head_relation %in% "grandfather/grandmother")
  survey_df_i$head_relation_head <- as.numeric(survey_df_i$head_relation %in% "head")
  survey_df_i$head_relation_husband <- as.numeric(survey_df_i$head_relation %in% "husband")
  survey_df_i$head_relation_nephew <- as.numeric(survey_df_i$head_relation %in% "nephew")
  survey_df_i$head_relation_other <- as.numeric(survey_df_i$head_relation %in% "other")
  survey_df_i$head_relation_sisterBrotherInLaw <- as.numeric(survey_df_i$head_relation %in% "sister/bother in law")
  survey_df_i$head_relation_sisterBrother <- as.numeric(survey_df_i$head_relation %in% "sister/brother")
  survey_df_i$head_relation_sonDaughter <- as.numeric(survey_df_i$head_relation %in% "son/daughter")
  survey_df_i$head_relation_sonDaughterInLaw <- as.numeric(survey_df_i$head_relation %in% "son/daughter in law")
  survey_df_i$head_relation_uncleAnt <- as.numeric(survey_df_i$head_relation %in% c("uncle/ant","uncle/aunt"))
  survey_df_i$head_relation_wife <- as.numeric(survey_df_i$head_relation %in% "wife")
  
  # Employment Category
  survey_df_i$emp_category_government <- as.numeric(survey_df_i$emp_category %in% "government")
  survey_df_i$emp_category_notEmployed <- as.numeric(survey_df_i$emp_category %in% "not currently employed")
  survey_df_i$emp_category_pensioner <- as.numeric(survey_df_i$emp_category %in% "pensioner")
  survey_df_i$emp_category_private <- as.numeric(survey_df_i$emp_category %in% "private")
  survey_df_i$emp_category_selfEmployed <- as.numeric(survey_df_i$emp_category %in% "self employed")
  survey_df_i$emp_category_semigovernment <- as.numeric(survey_df_i$emp_category %in% "semi-government")
  
  # Education Status
  survey_df_i$education_status_completedEducation <- as.numeric(survey_df_i$education_status %in% "completed education/stopped attending school")
  survey_df_i$education_status_currentlyAttendingSchool <- as.numeric(survey_df_i$education_status %in% "currently attending school")
  survey_df_i$education_status_neverAttendedSchool <- as.numeric(survey_df_i$education_status %in% "never attended school")

  # Education Level  
  survey_df_i$education_level_class1 <- as.numeric(survey_df_i$education_level %in% "class 1")
  survey_df_i$education_level_class2 <- as.numeric(survey_df_i$education_level %in% "class 2")
  survey_df_i$education_level_class3 <- as.numeric(survey_df_i$education_level %in% "class 3")
  survey_df_i$education_level_class4 <- as.numeric(survey_df_i$education_level %in% "class 4")
  survey_df_i$education_level_class5 <- as.numeric(survey_df_i$education_level %in% "class 5")
  survey_df_i$education_level_class6 <- as.numeric(survey_df_i$education_level %in% "class 6")
  survey_df_i$education_level_class7 <- as.numeric(survey_df_i$education_level %in% "class 7")
  survey_df_i$education_level_class8 <- as.numeric(survey_df_i$education_level %in% "class 8")
  survey_df_i$education_level_class9 <- as.numeric(survey_df_i$education_level %in% "class 9")
  survey_df_i$education_level_class10 <- as.numeric(survey_df_i$education_level %in% "class 10")
  survey_df_i$education_level_class11 <- as.numeric(survey_df_i$education_level %in% "class 11")
  survey_df_i$education_level_class12 <- as.numeric(survey_df_i$education_level %in% "class 12")
  survey_df_i$education_level_class13beyond <- as.numeric(survey_df_i$education_level %in% "class 13 or beyond")
  survey_df_i$education_level_kachiNursery <- as.numeric(survey_df_i$education_level %in% "kachi/nursery")
  
  # Toilet Kind
  survey_df_i$toilet_kind_flushConnectedToSewageSystem <- as.numeric(survey_df_i$toilet_kind %in% "flush connected to sewerage system")
  survey_df_i$toilet_kind_noToilet <- as.numeric(survey_df_i$toilet_kind %in% "no toilet")
  survey_df_i$toilet_kind_pitLatrine <- as.numeric(survey_df_i$toilet_kind %in% "pit latrine")
  
  # Disability
  survey_df_i$disability_hearing <- as.numeric(survey_df_i$disability %in% "hearing disability")
  survey_df_i$disability_lowerLimb <- as.numeric(survey_df_i$disability %in% "lower limb disability")
  survey_df_i$disability_mental <- as.numeric(survey_df_i$disability %in% "mental disability")
  survey_df_i$disability_none <- as.numeric(survey_df_i$disability %in% "none")
  survey_df_i$disability_speech <- as.numeric(survey_df_i$disability %in% "speech disability")
  survey_df_i$disability_upperLimb <- as.numeric(survey_df_i$disability %in% "upper limb disability")
  survey_df_i$disability_visual <- as.numeric(survey_df_i$disability %in% "visual disability")
  }
  
  survey_df_i$N <- 1
  
  survey_df_i <- data.table(survey_df_i)
  
  print(file)
  return(survey_df_i)
}

# Bind Datasets Together -------------------------------------------------------
survey_append <- lapply(dta_files, load_and_clean_asset_other_vars) %>% bind_rows

# Summarize Data ---------------------------------------------------------------
# Average by HH Head
survey_append_hh_heads <- survey_append[survey_append$head_relation_head == 1,]

survey_append_hh_heads_uc <- survey_append_hh_heads[, j=list(no_rooms_mean = mean(no_rooms, na.rm = TRUE), 
                                                             no_rooms_median = median(no_rooms, na.rm = TRUE), 
                                                             
                                           tv_p = mean(tv_p, na.rm = TRUE),
                                           aircooler_p = mean(aircooler_p, na.rm = TRUE),
                                           freezer_p = mean(freezer_p, na.rm = TRUE),
                                           washingmachine_p = mean(washingmachine_p, na.rm = TRUE),
                                           airconditioner_p = mean(airconditioner_p, na.rm = TRUE),
                                           heater_p = mean(heater_p, na.rm = TRUE),
                                           microwaveoven_p = mean(microwaveoven_p, na.rm = TRUE),
                                           cookingrange_p = mean(cookingrange_p, na.rm = TRUE),
                                           sheep_p = mean(sheep_p, na.rm = TRUE),
                                           bull_p = mean(bull_p, na.rm = TRUE),
                                           cow_p = mean(cow_p, na.rm = TRUE),
                                           buffalo_p = mean(buffalo_p, na.rm = TRUE),
                                           goat_p = mean(goat_p, na.rm = TRUE),
                                           pmt_score_mean = mean(pmt_score, na.rm = TRUE),
                                           pmt_score_median = median(pmt_score, na.rm = TRUE),
                                           asset_item_index_mean = mean(asset_item_index, na.rm = TRUE),
                                           asset_item_index_median = median(asset_item_index, na.rm = TRUE),
                                           asset_animal_index_mean = mean(asset_animal_index, na.rm = TRUE),
                                           asset_animal_index_median = median(asset_animal_index, na.rm = TRUE),
                                           
                                           #toilet_kind_flushConnectedToSewageSystem_N_households = sum(toilet_kind_flushConnectedToSewageSystem, na.rm = TRUE),
                                           #toilet_kind_noToilet_N_households = sum(toilet_kind_noToilet, na.rm = TRUE),
                                           #toilet_kind_pitLatrine_N_households = sum(toilet_kind_pitLatrine, na.rm = TRUE),
                                           
                                           households_N = sum(N, na.rm = TRUE)), 
                                  by = list(district_name,tehsil_name,unioncouncil_name)]

# Average by Individual
survey_append_uc <- survey_append[, j=list(individuals_N = sum(N, na.rm = TRUE),
                                           
                                           #marital_status_divorced_N = sum(marital_status_divorced, na.rm = TRUE),
                                           #marital_status_married_N = sum(marital_status_married, na.rm = TRUE),
                                           #marital_status_separated_N = sum(marital_status_separated, na.rm = TRUE),
                                           #marital_status_unmarried_N = sum(marital_status_unmarried, na.rm = TRUE),
                                           #marital_status_widowed_N = sum(marital_status_widowed, na.rm = TRUE),
                                           
                                           #gender_female_N = sum(gender_female, na.rm = TRUE),
                                           #gender_male_N = sum(gender_male, na.rm = TRUE),
                                           
                                           #head_relation_child_N = sum(head_relation_child, na.rm = TRUE),
                                           #head_relation_fathermother_N = sum(head_relation_fathermother, na.rm = TRUE),
                                           #head_relation_fatherMotherInLaw_N = sum(head_relation_fatherMotherInLaw, na.rm = TRUE),
                                           #head_relation_grandchild_N = sum(head_relation_grandchild, na.rm = TRUE),
                                           #head_relation_grandfatherMother_N = sum(head_relation_grandfatherMother, na.rm = TRUE),
                                           #head_relation_head_N = sum(head_relation_head, na.rm = TRUE),
                                           #head_relation_husband_N = sum(head_relation_husband, na.rm = TRUE),
                                           #head_relation_nephew_N = sum(head_relation_nephew, na.rm = TRUE),
                                           #head_relation_other_N = sum(head_relation_other, na.rm = TRUE),
                                           #head_relation_sisterBrotherInLaw_N = sum(head_relation_sisterBrotherInLaw, na.rm = TRUE),
                                           #head_relation_sisterBrother_N = sum(head_relation_sisterBrother, na.rm = TRUE),
                                           #head_relation_sonDaughter_N = sum(head_relation_sonDaughter, na.rm = TRUE),
                                           #head_relation_sonDaughterInLaw_N = sum(head_relation_sonDaughterInLaw, na.rm = TRUE),
                                           #head_relation_uncleAnt_N = sum(head_relation_uncleAnt, na.rm = TRUE),
                                           #head_relation_wife_N = sum(head_relation_wife, na.rm = TRUE),
                                           
                                           #emp_category_government_N = sum(emp_category_government, na.rm = TRUE),
                                           #emp_category_notEmployed_N = sum(emp_category_notEmployed, na.rm = TRUE),
                                           #emp_category_pensioner_N = sum(emp_category_pensioner, na.rm = TRUE),
                                           #emp_category_private_N = sum(emp_category_private, na.rm = TRUE),
                                           #emp_category_selfEmployed_N = sum(emp_category_selfEmployed, na.rm = TRUE),
                                           #emp_category_semigovernment_N = sum(emp_category_semigovernment, na.rm = TRUE),
                                           
                                           #education_status_completedEducation_N = sum(education_status_completedEducation, na.rm = TRUE),
                                           #education_status_currentlyAttendingSchool_N = sum(education_status_currentlyAttendingSchool, na.rm = TRUE),
                                           #education_status_neverAttendedSchool_N = sum(education_status_neverAttendedSchool, na.rm = TRUE),
                                           
                                           #education_level_class1_N = sum(education_level_class1, na.rm = TRUE),
                                           #education_level_class2_N = sum(education_level_class2, na.rm = TRUE),
                                           #education_level_class3_N = sum(education_level_class3, na.rm = TRUE),
                                           #education_level_class4_N = sum(education_level_class4, na.rm = TRUE),
                                           #education_level_class5_N = sum(education_level_class5, na.rm = TRUE),
                                           #education_level_class6_N = sum(education_level_class6, na.rm = TRUE),
                                           #education_level_class7_N = sum(education_level_class7, na.rm = TRUE),
                                           #education_level_class8_N = sum(education_level_class8, na.rm = TRUE),
                                           #education_level_class9_N = sum(education_level_class9, na.rm = TRUE),
                                           #education_level_class10_N = sum(education_level_class10, na.rm = TRUE),
                                           #education_level_class11_N = sum(education_level_class11, na.rm = TRUE),
                                           #education_level_class12_N = sum(education_level_class12, na.rm = TRUE),
                                           #education_level_class13beyond_N = sum(education_level_class13beyond, na.rm = TRUE),
                                           #education_level_kachiNursery_N = sum(education_level_kachiNursery, na.rm = TRUE),
                                           
                                           #disability_hearing_N = sum(disability_hearing, na.rm = TRUE),
                                           #disability_lowerLimb_N = sum(disability_lowerLimb, na.rm = TRUE),
                                           #disability_mental_N = sum(disability_mental, na.rm = TRUE),
                                           #disability_none_N = sum(disability_none, na.rm = TRUE),
                                           #disability_speech_N = sum(disability_speech, na.rm = TRUE),
                                           #disability_upperLimb_N = sum(disability_upperLimb, na.rm = TRUE),
                                           #disability_visual_N = sum(disability_visual, na.rm = TRUE),
                                           
                                           age_mean = mean(age, na.rm = TRUE),
                                           age_median = median(age, na.rm = TRUE)), 
                                          by = list(district_name,tehsil_name,unioncouncil_name)]

# Merge Datasets Together ------------------------------------------------------
survey_append_all_uc <- merge(survey_append_hh_heads_uc, survey_append_uc, by=c("district_name","tehsil_name","unioncouncil_name"), all=T)

# Export -----------------------------------------------------------------------
write.csv(survey_append_all_uc, file.path(project_file_path,"Data","RawData","NSER","Union Council Level Dataset","nser_uc.csv"),row.names=F)

