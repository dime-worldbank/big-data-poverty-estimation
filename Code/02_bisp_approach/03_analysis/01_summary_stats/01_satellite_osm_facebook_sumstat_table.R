# Summary Stats

#### Load Data
bisp_df <- readRDS(file.path(final_data_file_path, "BISP", "Merged Datasets", "bisp_socioeconomic_satellite_panel_full_satPovNAsRemoved.Rds"))
bisp_df <- bisp_df[bisp_df$year %in% 2014,]


ROUND_NUM <- 2

bisp_df_sum <- lapply(c("viirs_spatialmean_monthlymean_buff_2km",
         "b25_buff_2km_min",
         "estimate_dau",
         "dist_osm_fclass_residential_kms"), function(var){
  data.frame(
    var = var,
    mean_poor = bisp_df[[var]][bisp_df$pscores_poor %in% 1] %>% mean(na.rm=T) %>% round(ROUND_NUM) %>% format(big.mark=",",scientific=FALSE),
    mean_nonpoor = bisp_df[[var]][bisp_df$pscores_poor %in% 0] %>% mean(na.rm=T) %>% round(ROUND_NUM) %>% format(big.mark=",",scientific=FALSE),
    median_poor = bisp_df[[var]][bisp_df$pscores_poor %in% 1] %>% median(na.rm=T) %>% round(ROUND_NUM) %>% format(big.mark=",",scientific=FALSE),
    median_nonpoor = bisp_df[[var]][bisp_df$pscores_poor %in% 0] %>% median(na.rm=T) %>% round(ROUND_NUM) %>% format(big.mark=",",scientific=FALSE)
  )
}) %>% bind_rows()
bisp_df_sum

bisp_df_sum$var[bisp_df_sum$var %in% "estimate_dau"] <- "Facebook: Daily Active Users"
bisp_df_sum$var[bisp_df_sum$var %in% "viirs_spatialmean_monthlymean_buff_2km"] <- "Nighttime Lights: Average"
bisp_df_sum$var[bisp_df_sum$var %in% "b25_buff_2km_min"] <- "Daytime Imagery: Indice with Bands 2 and 5"
bisp_df_sum$var[bisp_df_sum$var %in% "dist_osm_fclass_residential_kms"] <- "OSM: Kilometers to Residential Road"

bisp_df_sum$latex <- apply(bisp_df_sum, 1, paste, collapse=" & ") %>% paste(" \\\\ \n")

sink(file.path(tables_file_path, "meanmedian_vars_by_poor.tex"))
cat("\\begin{tabular}{l|cc|cc} \n")
cat("\\hline \n")
cat("& \\multicolumn{2}{c|}{Mean} & \\multicolumn{2}{c}{Median} \\\\ \n")
cat("Variable & Poor & Non-Poor & Poor & Non-Poor \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(bisp_df_sum)) cat(bisp_df_sum$latex[i])

cat("\\hline \n")
cat("\\end{tabular} ")
sink()











ggplot(data=bisp_df) +
  geom_violin(aes(x=pscores_poor, y=log(b25_buff_2km_min+1), group=pscores_poor))


b45_buff_0.1km_mean

var <- "b14_buff_1km_mean"
dff_df <- lapply(names(bisp_df), function(var){
  print(var)
  
  bisp_df[[var]] <- bisp_df[[var]] %>% as.numeric()
  
  nonpoor <- bisp_df[[var]][bisp_df$pscores_poor %in% 0] %>% median()
  poor <- bisp_df[[var]][bisp_df$pscores_poor %in% 1] %>% median()
  per_diff <- (nonpoor - poor)/poor
  diff <- nonpoor - poor
  
  data.frame(var,
             nonpoor,
             poor,
             per_diff,
             diff)
}) %>% bind_rows
dff_df <- dff_df[grepl("fclass", dff_df$var),]





bisp_df$pscores[bisp_df$estimate_dau > 450] %>% summary()

lm(pscores ~ estimate_mau, data=bisp_df) %>% summary()

bisp_df$year %>% table()


