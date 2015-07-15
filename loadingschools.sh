#!/bin/bash
#
# description:
#   fetches schools files from the NYC Department of Education
#
# usage: ./download_housing_sales.sh
#
# requirements: curl or wget
#
# author: Thomas Patino with group, supervised by jake hofman

#Create directory
mkdir schools

#Save files to housing
cd schools

#Get all .xls files from website
wget  http://schools.nyc.gov/NR/rdonlyres/14B7086D-9EE8-42FB-9D10-2160BE72C1EA/0/2013_2014_All_Schools_SQR_Results_2015_01_20.xlsx

wget http://www.nyc.gov/html/doed/downloads/datasets/DOE_LocationMasterData_001.xls

#Change name

mv ~/public-schools/schools/2013_2014_All_Schools_SQR_Results_2015_01_20.xlsx ~/public-schools/schools/schoolratings.xlsx

mv ~/public-schools/schools/DOE_LocationMasterData_001.xls ~/public-schools/schools/schooldirectory.xls
