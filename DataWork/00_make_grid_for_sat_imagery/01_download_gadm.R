# Download GADM

setwd(file.path(project_file_path, "Data", "GADM", "RawData"))

# Pakistan
getData('GADM', country='PAK', level=0)
getData('GADM', country='PAK', level=1)
getData('GADM', country='PAK', level=2)
getData('GADM', country='PAK', level=3)

# India
getData('GADM', country='IND', level=0)
getData('GADM', country='IND', level=1)
getData('GADM', country='IND', level=2)
getData('GADM', country='IND', level=3)

# Afghanista
getData('GADM', country='AFG', level=0)
getData('GADM', country='AFG', level=1)
getData('GADM', country='AFG', level=2)

# Bangladesh
getData('GADM', country='BGD', level=0)
getData('GADM', country='BGD', level=1)
getData('GADM', country='BGD', level=2)
