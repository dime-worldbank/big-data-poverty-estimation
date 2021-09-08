# Download GADM

setwd(file.path(data_dir, "GADM", "RawData"))

for(country_i in c("BGD", "IND", "KHM", "KGZ", "MMR", "NGA", "NPL", "PHL", "PAK", "SEN", "TJK", "TLS", "UGA")){
  for(i in 0:2){
    getData('GADM', country=country_i, level=i)
  } 
}

