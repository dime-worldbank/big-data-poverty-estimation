# Download Country-Level WDI Data

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(data_dir, "DHS", "FinalData", "Individual Datasets", "survey_socioeconomic.Rds"))

# Extract WDI ------------------------------------------------------------------
wdi_df <- WDI(country=unique(survey_df$iso2), 
              indicator=c("SP.POP.TOTL",
                          "NY.GDP.PCAP.CD",
                          "NV.AGR.TOTL.ZS"), 
              start=2020,
              end=2020,
              extra=TRUE, 
              cache=NULL)

wdi_df <- wdi_df %>%
  dplyr::rename(wdi_population = SP.POP.TOTL,
                wdi_gdp_pc = NY.GDP.PCAP.CD,
                wdi_agric_per_gdp = NV.AGR.TOTL.ZS,
                iso2 = iso2c)

# Export -----------------------------------------------------------------------
saveRDS(wdi_df, file.path(data_dir, "WDI", "FinalData", "wdi.Rds"))
