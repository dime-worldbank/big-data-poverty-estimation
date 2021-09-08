# Download GADM

setwd(file.path(data_dir, "GADM", "RawData"))

for(country_i in c("ALB", "ARM", "AGO", "BFA", "BEN", "BOL", "BDI", "COD", "CAF",
                   "CIV", "CMR", "COL", "DOM", "EGY", "ETH", "GAB", "GHA", "GMB",
                   "GIN", "GTM", "GUY", "HND", "HTI", "JOR", "KEN", "COM", "LBR",
                   "LSO", "MAR", "MDA", "MDG", "MLI", "MWI", "MOZ", "NGA", "NER",
                   "NAM", "PER", "RWA", "SLE", "SEN", "TCD", "TGO", "TZA", "ZAF",
                   "ZMB", "ZWE",
                   "BGD", "IND", "KHM", "KGZ", "MMR", "NGA", "NPL", "PHL", "PAK", "TJK", "TLS", "UGA")){
  for(i in 0:2){
    getData('GADM', country=country_i, level=i)
  } 
}

