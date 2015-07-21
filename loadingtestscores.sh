#!/bin/bash
#
# description:
#   fetches schools exam math and english scores files from the NYC Department of Education
#
# requirements: curl or wget
#
# author: Thomas Patino supervised by jake hofman

#Save files to schools
cd schools

#Get all .xls files from website
wget  http://schools.nyc.gov/NR/rdonlyres/73680CA1-C768-4708-856A-99658B1DFECB/0/SchoolELAResults20132014.xlsx

wget http://schools.nyc.gov/NR/rdonlyres/320B6BB2-63D2-4E08-9246-385E1BBB4990/0/SchoolMathResults20132014.xlsx

#Change name

mv SchoolELAResults20132014.xlsx englishscores.xlsx

mv SchoolMathResults20132014.xlsx mathscores.xlsx
