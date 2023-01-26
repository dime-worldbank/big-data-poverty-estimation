# Alluvial Diagram of Poverty Over Time

# Load Data --------------------------------------------------------------------
bisp_df <- readRDS(file.path(final_data_file_path, "BISP", "Individual Datasets", "bisp_socioeconomic.Rds"))

bisp_df$pscores


bisp_wide_df <- bisp_df %>% 
  dplyr::select(uid, year, pscores) %>%
  pivot_wider(names_from=year, values_from=pscores) %>%
  filter(!is.na(`2011`) & !is.na(`2013`) & !is.na(`2014`) & !is.na(`2016`))

for(var in c("2011", "2013", "2014", "2016")){
  bisp_wide_df[[var]] <- ifelse(bisp_wide_df[[var]] <= 16.17, "Below", "Above") %>%
    factor(levels=c( "Below","Above"))
} 

bisp_wide_freq <- bisp_wide_df %>% 
  group_by(`2011`, `2013`, `2014`, `2016`) %>%
  summarise(Freq = n())

png(file.path(figures_file_path, "bisp_poverty_alluvial.png"), width=3000, height=1800, res=500)
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


