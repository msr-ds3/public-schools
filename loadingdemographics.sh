#!/bin/bash
#
# description:
# fetches school files from the NYC Department of Finance
# 
# author: Thomas Patino supervised by jake Hofman

#Save files to housing
cd schools

#Get all .xls files from website
wget http://schools.nyc.gov/NR/rdonlyres/77954FB0-FD24-476B-AB81-3E9BBE8655D9/183200/DemographicSnapshot201011to201415Public_FINAL.xlsx

mv DemographicSnapshot201011to201415Public_FINAL.xlsx demographics.xlsx
