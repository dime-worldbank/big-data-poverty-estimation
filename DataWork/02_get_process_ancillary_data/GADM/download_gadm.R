# Download GADM

setwd(file.path(project_file_path, "Data", "GADM", "RawData"))

getData('GADM', country='PAK', level=0)
getData('GADM', country='PAK', level=1)
getData('GADM', country='PAK', level=2)
getData('GADM', country='PAK', level=3)


