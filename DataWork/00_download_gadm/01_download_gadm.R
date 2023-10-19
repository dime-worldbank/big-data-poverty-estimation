# Download GADM

only_adm1_avail <- c("ARM", "COM", "LSO", "MDA")

for(country_i in c("ALB", "ARM", "AGO", "BFA", "BEN", "BOL", "BDI", "COD", "CAF",
                   "CIV", "CMR", "COL", "DOM", "EGY", "ETH", "GAB", "GHA", "GMB",
                   "GIN", "GTM", "GUY", "HND", "HTI", "JOR", "KEN", "COM", "LBR",
                   "LSO", "MAR", "MDA", "MDG", "MLI", "MWI", "MOZ", "NGA", "NER",
                   "NAM", "PER", "RWA", "SLE", "SEN", "TCD", "TGO", "TZA", "ZAF",
                   "ZMB", "ZWE",
                   "SWZ", "LBN",
                   "BGD", "IND", "KHM", "KGZ", "MMR", "NGA", "NPL", "PHL", "PAK", "TJK", "TLS", "UGA")){
  
  if(country_i %in% only_adm1_avail){
    for(i in 0:1) getData('GADM', country=country_i, level=i, path = file.path(data_dir, "GADM", "RawData"))
  } else{
    for(i in 0:2) getData('GADM', country=country_i, level=i, path = file.path(data_dir, "GADM", "RawData"))
  }
}

# Get higher level ADMs for select countries
getData('GADM', country="PAK", level=3)


