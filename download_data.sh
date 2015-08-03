###Load Housing Data

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

cd ..

###Load School Data

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

#Get all .xls files from website
wget  http://schools.nyc.gov/NR/rdonlyres/73680CA1-C768-4708-856A-99658B1DFECB/0/SchoolELAResults20132014.xlsx

wget http://schools.nyc.gov/NR/rdonlyres/320B6BB2-63D2-4E08-9246-385E1BBB4990/0/SchoolMathResults20132014.xlsx

#Change name

mv SchoolELAResults20132014.xlsx englishscores.xlsx

mv SchoolMathResults20132014.xlsx mathscores.xlsx


#Get all .xls files from website
wget http://schools.nyc.gov/NR/rdonlyres/77954FB0-FD24-476B-AB81-3E9BBE8655D9/183200/DemographicSnapshot201011to201415Public_FINAL.xlsx

mv DemographicSnapshot201011to201415Public_FINAL.xlsx demographics.xlsx

#Get files about about class sizes

wget http://schools.nyc.gov/NR/rdonlyres/5EC03778-74AD-45FF-9951-44441AE570F2/0/SchoolLevelDetailSummary_Updated_2015.xlsx

mv SchoolLevelDetailSummary_Updated_2015.xlsx classsizes.xlsx

cd ..

### Shapefile data
mkdir geocoding

cd geocoding

wget -O nyc_school_shapefiles_2014.zip https://data.cityofnewyork.us/api/geospatial/pp5b-95kq?method=export\&format=Shapefile

unzip nyc_school_shapefiles_2014.zip

