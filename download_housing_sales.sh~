#!/bin/bash
#
# description:
#   fetches house files from the NYC Department of Finance
#
# usage: ./download_housing_sales.sh
#
# requirements: curl or wget
#
# author: Group project supervised by jake hofman
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
mv housing/rollingsales_bronx.xls housing/bronx.xls
mv housing/rollingsales_brooklyn.xls housing/brooklyn.xls
mv housing/rollingsales_manhattan.xls housing/manhattan.xls
mv housing/rollingsales_queens.xls housing/queens.xls
mv housing/rollingsales_statenisland.xls housing/statenisland.xls

# change to the data directory
#Still dont know what this is cd $DATA_DIR

