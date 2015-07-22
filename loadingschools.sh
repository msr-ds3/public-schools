#!/bin/bash
#
# description:
#   fetches schools files from the NYC Department of Education

#Create directory
mkdir schools

#Save files to housing
cd schools

#Get all .xls files from website
wget  http://schools.nyc.gov/NR/rdonlyres/14B7086D-9EE8-42FB-9D10-2160BE72C1EA/0/2013_2014_All_Schools_SQR_Results_2015_01_20.xlsx

wget http://www.nyc.gov/html/doed/downloads/datasets/DOE_LocationMasterData_001.xls

#Change name

mv 2013_2014_All_Schools_SQR_Results_2015_01_20.xlsx schoolratings.xlsx

mv DOE_LocationMasterData_001.xls schooldirectory.xls
