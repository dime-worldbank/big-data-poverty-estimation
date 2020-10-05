# Alluvial Diagram of Poverty Over Time

# Load Data --------------------------------------------------------------------
bisp_df <- readRDS(file.path(project_file_path, "Data", "BISP", "FinalData",
                             "Individual Datasets", "bisp_socioeconomic.Rds"))

## restrict to households with a poverty score in all four years
bisp_df <- bisp_df %>%
  filter(!is.na(consumption_total)) %>%
  
  dplyr::group_by(uid) %>%
  dplyr::mutate(uid_count = n()) %>%
  filter(uid_count %in% 4) %>%
  ungroup()

## define poverty threshold
bisp_df$poor <- ifelse(bisp_df$consumption_total <= 62842.12, "Below", "Above") %>%
  factor(levels=c( "Below","Above"))

## Number of households
bisp_df$uid %>% unique() %>% length()

bisp_wide_df <- bisp_df %>% 
  dplyr::select(uid, year, poor) %>%
  pivot_wider(names_from=year, values_from=poor) 

bisp_wide_freq <- bisp_wide_df %>% 
  group_by(`2011`, `2013`, `2014`, `2016`) %>%
  summarise(Freq = n())

png(file.path(figures_file_path, "bisp_poverty_consumption_alluvial.png"), width=3000, height=1800, res=500)
alluvial(bisp_wide_freq[,1:4], freq=bisp_wide_freq$Freq,
         col = ifelse(bisp_wide_freq$`2011` == "Above", "forestgreen", "gold3"),
         border="white",
         cex = 0.95,
         alpha=.6,
         cw=.15,
         blocks=T,
         axis_labels=c("2011", "2013", "2014", "2016")
)
dev.off()


