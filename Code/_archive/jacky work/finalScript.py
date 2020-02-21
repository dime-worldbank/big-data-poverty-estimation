#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 20 00:53:21 2019
Script commpares to see if the UC of the survey data is in the Google API returned value. Next, it checks to
see how the corresponding tehsil and district from the  shape files compare to that of the survey data. Finally, 
we compare the tehil and district of the corresponding shapefiles to that of the manual matches performed by Dr. R. 

The above test strips the spaces and case of the strings and matches them via direct equality ("=="). In
addition, a fuzzy match is performed on everything. 


@author: jackylin
"""

import pandas as pd
import re
from fuzzywuzzy import fuzz

GoogleAPIValue = "Google API Returned Value"
TehsilColumn = "TEHSIL_Shape"
DistrictColumn = "DISTRICT_Shape"

class entryClass:
    
    def __init__(self, uc, tehsil, district):
        if (type(uc) == type(0.0)): self.uc = "NA"
        else: self.uc = uc.strip().lower()
        if (type(tehsil) == type(0.0)): self.tehsil = "NA"
        else: self.tehsil = tehsil.strip().lower()
        self.district = district.strip().lower()
        
    
    def __eq__(self, other):
        
        return (self.uc == other.uc and self.tehsil == other.tehsil and self.district == other.district)
    
def getSurvey(df):
    """
    The UCs, TEHSILs, and DISTRICTs of the survey data are returned. 
    """
    toReturn = []
    for index, row in df.iterrows():
        toReturn.append(entryClass(row[0], row[1], row[2])) #hardcoded from csv
    return toReturn
        
def regexAdd(df, survey):
    """
    We see if the uc in the survey data is in the returned value of the Google API
    """
    toReturn = []
    for index, row in df.iterrows():
        uc = re.compile(".*" + survey[index].uc + ".*", re.IGNORECASE)
        if (uc.search(row[4]) != None): toReturn.append(True)
        else: toReturn.append(False)
    return toReturn


def getShapefile(df):
    """
    We get the shape files' uc, tehsil, and district
    """
    toReturn = []
    for index, row in df.iterrows():
        toReturn.append(entryClass(row[5], row[6], row[7])) #hardcoded from csv
    return toReturn



def doesTehsilMatch(survey, shapeData):
    toReturn = []
    for index in range(len(survey)):
        if(survey[index].tehsil == shapeData[index].tehsil): toReturn.append(True)
        else: toReturn.append(False)
    return toReturn

    
def doesDistrictMatch(survey, shapeData):
    toReturn = []
    for index in range(len(survey)):
        if(survey[index].district == shapeData[index].district): toReturn.append(True)
        else: toReturn.append(False)
    return toReturn

def checkMatches(df):
    survey = getSurvey(df)
    googleAPI = regexAdd(df, survey)
    shapeData = getShapefile(df) 
    matchTehsil = doesTehsilMatch(survey, shapeData)
    matchDistrict = doesDistrictMatch(survey, shapeData)
    
    df["UC Match with Returned Address in Google API"] = googleAPI
    df["Tehsil Match"]   = matchTehsil
    df["District Match"] = matchDistrict 
    
    
def getFuzzyUC(row, googleReturnedValue):
    """
    https://stackoverflow.com/questions/31806695/when-to-use-which-fuzz-function-to-compare-2-strings 
    for more info on how the fuzzywuzzy works. 

    Method compares row (which is a UC from the survey data) and the returned address from the Google 
    API. 
    """
    ucSurvey = row[0]
    pattern = re.compile("\['.*?,")
    found = pattern.match(googleReturnedValue)
    if (found and type(ucSurvey) != type(0.0)):
        assert (len(found.group()) != 1)
        ucGoogle = found.group()[2:len(found.group()) -1]
        return fuzz.partial_ratio(ucSurvey, ucGoogle)     
    else: 
        return -1
    

def getFuzzyTehsil(row, shapefileTehsil):
    if (type(row[1]) == type(0.0)): return -1
    return fuzz.partial_ratio(row[1], shapefileTehsil)
    
    
def getFuzzyDistrict(row, shapefileDistrict):
    return fuzz.partial_ratio(row[2], shapefileDistrict)


def getFuzzyMatch(df):
    """
    We get the fuzzy match of all comparisons between survey data to the Google API returned value and shape file data
    """
    apiValues = df[GoogleAPIValue]
    fuzzyUC = []
    fuzzyTehsil = []
    fuzzyDistrict = []

    for index, row in df.iterrows():
        fuzzyUC.append(getFuzzyUC(row, apiValues[index]))
        fuzzyTehsil.append(getFuzzyTehsil(row, df[TehsilColumn][index]))
        fuzzyDistrict.append(getFuzzyDistrict(row, df[DistrictColumn][index]))
   
    df["fuzzyUC"] = fuzzyUC
    df["fuzzyTehsil"] = fuzzyTehsil
    df["fuzzyDistrict"] = fuzzyDistrict
    
    
def getManualMatch(df, surveyFile):
    """
    We check to see if the shape file's district and tehsil match what Dr. R manually verified the 
    district and tehsil to be. 
    """
    matches = dict()
    for index, row in surveyFile.iterrows():
        
        tehsil = row[1]
        district = row[2]
        
        if ( type(tehsil) == type(0.0) or type(district) == type(0.0) ): continue
    
        key = (tehsil.strip().lower(), district.strip().lower())
        value = [row[31].strip().lower(), row[32].strip().lower()]
        if (key in matches):
            assert(matches[key] == value)
        else:
            matches[key] = value
    
    manualTehsil = []
    manualDistrict = []        
    for index, row in df.iterrows():
        
        tehsil = row[1]
        district = row[2]
        
        if ( type(tehsil) == type(0.0) or type(district) == type(0.0) ): 
        
            manualTehsil.append("NA")
            manualDistrict.append("NA")
            continue
        tehsil = tehsil.strip().lower()
        district = district.strip().lower()
        
        value = matches[(tehsil, district)]
        manualTehsil.append(value[0])
        manualDistrict.append(value[1])
        
    df["Manaual Tehsil"] = manualTehsil
    df["Manaual District"] = manualDistrict


    crossMatchTehsil = []
    crossMatchDistrict = []

    teshilShape = df["TEHSIL_Shape"]
    districtShape = df["DISTRICT_Shape"]

    for x in range(len(manualTehsil)):
        if (manualTehsil[x] ==  teshilShape[x]):
            crossMatchTehsil.append(True)
        else: crossMatchTehsil.append(False)

        if (manualDistrict[x] == districtShape[x]):
            crossMatchDistrict.append(True)
        else: crossMatchDistrict.append(False)

    df["crossMatchTehsil"]  =  crossMatchTehsil   
    df["crossMatchDistrict"]   = crossMatchDistrict 
        
        
    

def main():
    """
    Compares matches and prints entry as CSV. 
    """
    filename = "cleanedMap.csv"
    df = pd.read_csv(filename)
    checkMatches(df)
    getFuzzyMatch(df)
    surveyFile = pd.read_csv("nser_data_uc.csv")
    getManualMatch(df, surveyFile)
    
    
    
    df.to_csv("finalResult.csv")   
    
    
if ( __name__ == "__main__" ): main()
    
    
    
    
