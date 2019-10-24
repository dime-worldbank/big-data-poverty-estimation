
# Load and Prep HH Data --------------------------------------------------------------------
bisp_df <- read.csv(file.path(final_data_file_path, "BISP", "bisp_satellite_data_combined_buffer_1km.Rds"))

bisp_df <- bisp_df[!is.na(bisp_df$viirs_2012),]
bisp_df <- bisp_df[bisp_df$period %in% c(2011, 2013),]

bisp_df <- summaryBy(. ~ uid + period + LOCALITY, data=bisp_df, FUN=sum, na.rm=T, keep.names = T)
bisp_df$hh_inc[bisp_df$hh_inc == 0] <- NA
bisp_df$hh_inc_lastmonth[bisp_df$hh_inc_lastmonth == 0] <- NA

# Transform Variables ----------------------------------------------------------
bisp_df$viirs_ln <- log(bisp_df$viirs+1)

bisp_df$hh_inc[bisp_df$hh_inc > 9000000] <- NA
bisp_df$hh_inc_ln <- log(bisp_df$hh_inc + 1)

bisp_df_2011 <- bisp_df[bisp_df$period == 2011,] %>%
  dplyr::select(uid, hh_inc) %>%
  dplyr::rename(hh_inc_2011 = hh_inc)
bisp_df_2013 <- bisp_df[bisp_df$period == 2013,] %>%
  dplyr::select(uid, hh_inc) %>%
  dplyr::rename(hh_inc_2013 = hh_inc)
bisp_df <- merge(bisp_df, bisp_df_2011, by="uid")
bisp_df <- merge(bisp_df, bisp_df_2013, by="uid")

# Plot -------------------------------------------------------------------------
bisp_df <- bisp_df[bisp_df$period %in% 2013,]
bisp_df <- bisp_df[!is.na(bisp_df$hh_inc),]

bisp_df$dmspols_group <- 0
bisp_df$dmspols_group[bisp_df$dmspols > 40] <- 4
bisp_df$dmspols_group[bisp_df$dmspols <= 40] <- 3
bisp_df$dmspols_group[bisp_df$dmspols <= 10] <- 2
bisp_df$dmspols_group[bisp_df$dmspols <= 2] <- 1

bisp_df$viirs_group <- 0
bisp_df$viirs_group[bisp_df$viirs > 20] <- 3
bisp_df$viirs_group[bisp_df$viirs <= 20] <- 2
bisp_df$viirs_group[bisp_df$viirs <= 1] <- 1

bisp_df$viirs_group <- bisp_df$viirs_group %>% factor(levels=1:3,
                               labels = c("Low", "Medium", "High"))

bisp_df$hh_inc <- bisp_df$hh_inc / 1000
ggplot() +
  geom_boxplot(data=bisp_df[bisp_df$hh_inc > 15000/1000 & bisp_df$hh_inc < 300000/1000,], 
               aes(x=viirs_group, y=hh_inc, group=viirs_group),
               fill="burlywood1") +
  ylab("Household Income (1000s)") + 
  xlab("Luminosity") +
  ggtitle("Distribution of Annual Household Income\nAcross Levels of Nighttime Lights") +
  labs(caption="Boxes show 25th, 50th and 75th percentile of household income.") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=11),
        #axis.title.y = element_text(angle=0, vjust=.5, face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_text(face="bold"))
ggsave("~/Desktop/pakistan_ntl_hhinc.png", width=3.87, height=6)

