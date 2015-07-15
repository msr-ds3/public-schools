#!/bin/bash
#
# description:
#   fetches house files from the NYC Department of Finance
#
# usage: ./download_housing_sales.sh
#
# requirements: curl or wget
#
# author: Thomas Patino with Group project supervised by jake hofman
#

# set a relative path for the housing data
# (use current directory by default)
#Not sure what this is for but Jake had it in his code DATA_DIR=.

#Create directory
mkdir housing

#Save files to housing
cd housing

#Get all .xls files from website
wget -r -A .xls http://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page

#Deletes all the empty folders
#I dont think I need to do this twice 
#find -depth -type d -empty -exec rmdir {} \;

#Moves files to the housing folder
mv housing/www1.nyc.gov/assets/finance/downloads/pdf/rolling_sales/rollingsales_*.xls housing/

#Delete remainder empty folders
find -depth -type d -empty -exec rmdir {} \; 

#Rename files
mv rollingsales_bronx.xls bronx.xls
mv rollingsales_brooklyn.xls brooklyn.xls
mv rollingsales_manhattan.xls manhattan.xls
mv rollingsales_queens.xls queens.xls
mv rollingsales_statenisland.xls statenisland.xls

# change to the data directory
#Still dont know what this is cd $DATA_DIR

