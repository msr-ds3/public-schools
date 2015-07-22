#!/bin/bash
#
# description:
#   fetches house files from the NYC Department of Finance

#Create directory
mkdir housing

#Save files to housing
cd housing

#Get all .xls files from website
wget -r -A .xls http://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page

mv ~/public-schools/housing/www1.nyc.gov/assets/finance/downloads/pdf/rolling_sales/* ~/public-schools/housing

#Delete remainder empty folders
find -depth -type d -empty -exec rmdir {} \; 

#Rename files
mv rollingsales_bronx.xls bronx.xls
mv rollingsales_brooklyn.xls brooklyn.xls
mv rollingsales_manhattan.xls manhattan.xls
mv rollingsales_queens.xls queens.xls
mv rollingsales_statenisland.xls statenisland.xls
