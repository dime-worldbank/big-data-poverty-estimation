#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 25 10:38:03 2019

Script retrieves unmapped addresses and gets Google API's best guest of what the address is referring to,
returning both its best guess of the address itself and the associated lat, long coordinates. This is then 
printed as a csv file. 

@author: jackylin
"""

import pandas
import geopandas
from geopy.geocoders import GoogleV3
import csv


def returnUnmapped():
    """
    Thanks to Dr. R, we know that the Tehsil and District of each data entry maps from survey to shapefile. The only thing that 
    may not necessarily match would be the Union Council (UC), in which the data entry is demarcated by
    "NA". Hence, for each data entry that does not perfectly match from survey data to shape file (i.e. whose 
    UC is "NA" since Tehsils and Districts match perfectly), we create a new dataframe by removing matched entries
    in the dataframe from "nser_data_uc.csv"
    """
    dataFrame = pandas.read_csv("nser_data_uc.csv")
    values =  dataFrame["unioncouncil_name_gislayer"]
    index = 0
    toDelete = []
    for entry in values:
        if (type(entry) !=  type(0.0)): toDelete.append(index) #NA is viewed as nan
        index += 1
    dataFrame = dataFrame.drop(toDelete)
    return dataFrame
   

def getAddress(unmapped):
    """
    This simply creates the list of addresses that will be passed off to the Google API:
    [ [uc, tehsil, district, punjab, pakistan] . . . ]
    """

    curr = zip(unmapped["unioncouncil_name"], unmapped["tehsil_name"], unmapped["district_name"], ["punjab" for x in range(unmapped.shape[0])], ["pakistan" for x in range(unmapped.shape[0])])
    return curr
  
def cleanAddress(address):
    """
    This takes an entry in the list of addresses, an address, and parses it into the correct format for
    the Google API: uc, tehsil, district, punjab, pakistan
    """
    toReturn = ""
    for elem in address:
        if (type(elem) != type(0.0)): 
            if (toReturn != ""): toReturn += ", "
            toReturn = toReturn + elem
    return toReturn
                

def getLongLat(addresses):
    """
    Returns the Google API returned value of the address from survey data. 
    """
    geolocator = GoogleV3(api_key="enter api key here")
    locations = dict()
    for address in addresses:
        try:
            address = cleanAddress(address) #nan is very frustrating!!!!! 
            #lat, long
            location = geolocator.geocode(address, timeout=60) 
            items  = []
            if (location != None): items =[location.address, location.altitude, location.latitude, location.longitude]
            locations[address] = items
        except:
            print("$$Failed: ", address)
    return locations
    

def main():
    """
    Removes matched survey data (i.e. direct matches from survey data to shape file) and 
    appends Google API's geocoder returned value, 
    including address and lat, long
    """
    unmapped = returnUnmapped()
    addresses = getAddress(unmapped)
    locations = getLongLat(addresses) 
    dataFrame = pandas.DataFrame.from_dict(locations.items())
    dataFrame.to_csv("curr.csv")

if ( __name__ == "__main__" ): main()

