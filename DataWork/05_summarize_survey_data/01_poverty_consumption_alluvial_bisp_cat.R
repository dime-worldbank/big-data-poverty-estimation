# Alluvial Diagram of Poverty Over Time

# Load Data --------------------------------------------------------------------
bisp_df <- readRDS(file.path(project_file_path, "Data", "BISP", "FinalData",
                             "Individual Datasets", "bisp_socioeconomic.Rds"))

# Determine Poverty Line -------------------------------------------------------
# Number adult equivalent
bisp_df$N_adult_equiv <- bisp_df$N_adults + bisp_df$N_children * 0.8

# Consumption per adult equivalent
bisp_df$consumption_adult_equiv <- bisp_df$consumption_total / bisp_df$N_adult_equiv

# 20% consumption at baseline
# quantile(bisp_df$consumption_total[bisp_df$year %in% 2011], .2)

## restrict to households with a poverty score in all four years
bisp_df <- bisp_df %>%
  filter(!is.na(consumption_total)) %>%
  
  dplyr::group_by(uid) %>%
  dplyr::mutate(uid_count = n()) %>%
  filter(uid_count %in% 4) %>%
  ungroup()

## define poverty threshold
# 3,030 per month per adult equivalent. out data has every 14 days, so 38
# (31/14) for monthly multiplier
bisp_df$poor <- ifelse(bisp_df$consumption_adult_equiv <= 3030*(31/14), "Below", "Above") %>% 
  factor(levels=c( "Below","Above"))

mean(bisp_df$poor[bisp_df$year %in% 2011] %in% "Above")
mean(bisp_df$poor[bisp_df$year %in% 2013] %in% "Above")

bisp_df$poor_bisp <- ifelse(bisp_df$pscores <= 16.17, "Below", "Above") %>% 
   factor(levels=c("Below","Above"))

bisp_df <- bisp_df %>%
  group_by(uid) %>%
  mutate(poor_bisp_2011 = ifelse(pscores[year %in% 2011] <= 16.17, "Below", "Above")) %>%
  filter(!is.na(poor_bisp_2011))

## Number of households
bisp_df$uid %>% unique() %>% length()

bisp_wide_df <- bisp_df %>% 
  dplyr::select(uid, year, poor, poor_bisp_2011) %>%
  pivot_wider(names_from=year, values_from=poor) 

bisp_wide_freq <- bisp_wide_df %>% 
  group_by(`2011`, `2013`, `2014`, `2016`, poor_bisp_2011) %>%
  summarise(Freq = n())

png(file.path(figures_file_path, "bisp_poverty_consumption_alluvial_bispstatus.png"), width=3000, height=1800, res=500)
alluvial(bisp_wide_freq[,1:4], freq=bisp_wide_freq$Freq,
         col = ifelse(bisp_wide_freq$poor_bisp_2011 == "Above", "forestgreen", "gold3"),
         border="white",
         cex = 0.95,
         alpha=.6,
         cw=.15,
         blocks=T,
         axis_labels=c("2011", "2013", "2014", "2016")
)
dev.off()


