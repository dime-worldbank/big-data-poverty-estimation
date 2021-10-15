# Feature Correlation

df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))
df <- df %>%
  dplyr::select(uid, country_code, wealth_index_score, pca_allvars, contains("fb_estimate_mau_")) %>%
  dplyr::rename(DHSID = uid)

if_1000_make_0 <- function(x){
  x[x %in% 1000] <- 0
  return(x)
}

df <- df %>%
  dplyr::mutate_at(vars(contains("mau"), -fb_estimate_mau_1), if_1000_make_0) %>%
  dplyr::mutate_at(vars(contains("mau"), -fb_estimate_mau_1), ~(. /  fb_estimate_mau_1)) %>%
  dplyr::select(-c(fb_estimate_mau_1))


df_wide_prop <- df_wide %>%
  dplyr::mutate_at(vars(contains("dau"), -estimate_dau_1), ~(. /  estimate_dau_1)) %>%
  dplyr::mutate_at(vars(contains("mau"), -estimate_mau_1), ~(. /  estimate_mau_1)) %>%
  dplyr::select(-c(estimate_dau_1, estimate_mau_1)) %>%
  dplyr::mutate_if(is.numeric, tidyr::replace_na, 0)




df <- df[df$fb_estimate_mau_1 > 1000,]
df <- df[df$fb_estimate_mau_7 > 1000,]

table(df$fb_estimate_mau_13 <= df$fb_estimate_mau_14)

# df <- df %>%
#   #dplyr::mutate_at(vars(contains("dau"), -fb_estimate_dau_1), ~(. /  fb_estimate_dau_1)) %>%
#   dplyr::mutate_at(vars(contains("mau"), -fb_estimate_mau_1), ~(. /  fb_estimate_mau_1)) %>%
#   dplyr::select(-c(fb_estimate_mau_1)) %>%
#   dplyr::mutate_if(is.numeric, tidyr::replace_na, 0) %>%
#   dplyr::mutate_if(is.numeric, truncate_1)

df$fb_estimate_mau_7 <- df$fb_estimate_mau_7 / df$fb_estimate_mau_1 
df$fb_estimate_mau_7[df$fb_estimate_mau_7 > 1] <- 0

p_df <- read_csv("~/Downloads/Philippines_dataset.csv")
i_df <- read_csv("~/Downloads/India_dataset.csv")

dfa <- df %>%
  dplyr::filter(country_code == "IA") %>%
  left_join(i_df, by = "DHSID")

dfaa <- dfa[!is.na(dfa$Wifi_18p_All_frac),]

table(dfaa$fb_estimate_mau_7 > dfaa$Wifi_18p_All_frac)

dfaa_l <- dfaa %>%
  pivot_longer(cols = -c(DHSID, country_code, wealth_index_score, pca_allvars))

a <- dfaa_l %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(cor = cor(value, wealth_index_score))

cor.test(dfaa$wealth_index_score, dfaa$Wifi_18p_All_frac)
cor.test(dfaa$wealth_index_score, dfaa$fb_estimate_mau_7)
cor.test(dfaa$Wifi_18p_All_frac, dfaa$fb_estimate_mau_7)
cor.test(rank(dfaa$Wifi_18p_All_frac), rank(dfaa$fb_estimate_mau_7))







table(df$DHSID %in% i_df$DHSID)




truncate_1 <- function(x){
  x[x > 1] <- 1
  return(x)
}

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Merged Datasets", "survey_alldata_clean.Rds"))

table(df$fb_estimate_mau_11 <= df$fb_estimate_mau_12)

df <- df %>%
  dplyr::filter(fb_estimate_mau_1 > 10000)

dfa <- df %>%
  dplyr::select(contains("fb_estimate_"))

df_wide_prop <- dfa %>%
  #dplyr::mutate_at(vars(contains("dau"), -fb_estimate_dau_1), ~(. /  fb_estimate_dau_1)) %>%
  dplyr::mutate_at(vars(contains("mau"), -fb_estimate_mau_1), ~(. /  fb_estimate_mau_1)) %>%
  dplyr::select(-c(fb_estimate_mau_1)) %>%
  dplyr::mutate_if(is.numeric, tidyr::replace_na, 0) %>%
  dplyr::mutate_if(is.numeric, truncate_1)

df_wide_prop$fb_estimate_mau_1 <- df$fb_estimate_mau_1
df_wide_prop$country_code <- df$country_code
df_wide_prop$pca_allvars <- df$pca_allvars

table(df_wide_prop$fb_estimate_mau_11 <= df_wide_prop$fb_estimate_mau_15)


df_wide_prop %>%
  ggplot() +
  geom_point(aes(x = fb_estimate_mau_15,
                 y =  pca_allvars),
             size = 0.2) +
  facet_wrap(~country_code,
             scales = "free")

df_cor <- df_wide_prop %>%
  dplyr::select(country_code,
                pca_allvars,
                contains("fb_")) %>%
  pivot_longer(cols = -c(country_code, pca_allvars)) %>%
  dplyr::filter(!is.na(pca_allvars),
                !is.na(value)) %>%
  group_by(country_code, name) %>%
  dplyr::summarise(cor = cor(pca_allvars, value))

df_cor %>%
  ggplot(aes(x = cor,
             y = name)) +
  geom_boxplot()


df_cor$cor %>% summary()


df <- df %>%
  dplyr::select(country_code,
                country_name,
                pca_allvars,
                contains("fb_"),
                contains("gc_")) %>%
  pivot_longer(cols = -c(country_code, country_name, pca_allvars)) %>%
  dplyr::filter(!is.na(pca_allvars),
                !is.na(value)) %>%
  group_by(country_code, country_name, name) %>%
  dplyr::summarise(cor = cor(pca_allvars, value))


# Cleanup Names ----------------------------------------------------------------
fb_names_df <- readRDS(file.path(data_dir, SURVEY_NAME, "FinalData", "Individual Datasets", "facebook_marketing_parameters_clean.Rds"))
fb_names_df <- fb_names_df %>%
  dplyr::mutate(fb_id = paste0("fb_", param_id)) %>%
  dplyr::select(fb_id, param_category, param_name_simple, param_name_clean)

df$fb_id <- df$name %>% str_replace_all("fb_estimate_mau_|fb_prop_estimate_mau_|fb_wp_estimate_mau_", "fb_")
df <- df %>%
  left_join(fb_names_df, by = "fb_id")

# fb_estimate
# fb_prop_estimate
# fb_wp_estimate

df <- df %>%
  dplyr::filter(name %>% str_detect("fb_wp_estimate"))

df %>%
  ggplot(aes(x = cor,
             y = param_name_clean)) +
  geom_boxplot()

a <- df[df$fb_prop_estimate_mau_2 > 1,]
nrow(a)
