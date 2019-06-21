# -*- coding: utf-8 -*-

import pandas
import geopandas
from shapely.geometry import Point, Polygon

PAK_PROVINCE = "punjab"
DELIM = "%"
NF = "Not Found"


def getCoordinates(filename):
    """
    Method goes through the  dataframe and pulls the coordinates from the dataframe. 
    It stores the coordinates in a list and returns that list. 
    """
    dataFrame = pandas.read_csv(filename)
    dataFrame = dataFrame.drop(columns=["Unnamed: 0"])
    toReturn = dict()
    unmapped = []
    for index, row in dataFrame.iterrows():
        lat = 0
        lon = 0
        if (len(row[1]) != 2):
            lat = (row[1].split()[-2].replace(",", ""))
            lon = (row[1].split()[-1]).replace("]","")
        toReturn[row["0"]] = (float(lon), float(lat)) 
    
    return toReturn
  
    
def main():
    """
    Now that we have the survey data with its Google API returned value, we now cross match this with 
    the shape file. Taking the lat, long from the API's returned value, we see if the point intersects 
    any polygon in the shape files. If it does, we pull the address associated with that polygon (uc, tehsil, district).  
    """
    coordinates = getCoordinates("currCoordinates.csv")     
    eachData = geopandas.read_file("pak_uc.shp")
    eachData = eachData[eachData.PROVINCE == PAK_PROVINCE]

    mapped = dict()
    for key, val in coordinates.items():
        point = Point(val)
        returned = (eachData.contains(Point(val)))
        for index, found in returned.iteritems():
            if (found): 
                row = eachData.loc[index]
                assert (key not in mapped)
                mapped[key] = (row.UC if (row.UC != None) else "None") + DELIM + (row.TEHSIL if (row.TEHSIL != None) else "None") + DELIM + (row.DISTRICT if (row.DISTRICT != None) else "None")
                break
        
    
    csv_file = pandas.read_csv("currCoordinates.csv")
    UC = []
    TEHSIL = []
    DISTRICT = []


    """
    Code section below pulls uc, tehsil, and district associated with a particular pair of coordinates 
    and appends them to the dataframe, which is printed as a csv. 
    """
    for key, latLong in coordinates.items():
        if (key not in mapped): 
            UC.append(NF)
            TEHSIL.append(NF)
            DISTRICT.append(NF)
            print(key)
            continue
        values = mapped[key].split(DELIM)
        UC.append(values[0])
        TEHSIL.append(values[1])
        DISTRICT.append(values[2])
        
    csv_file["UC_Shape"] = UC
    csv_file["TEHSIL_Shape"] =  TEHSIL
    csv_file["DISTRICT_Shape"] = DISTRICT

    csv_file.to_csv("updatedShape.csv")


if ( __name__ == "__main__" ): main()
