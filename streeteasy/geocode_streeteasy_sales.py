from nyc_geoclient import Geoclient
import csv
import json
import os
import glob
import sys
from time import sleep

# For each row in the TSV:
	# Geocode address
	# Write full json contents to a file
	# write the original data + school, lat and long to a second file

if len(sys.argv) != 5:
	print "Error: %s Need App ID, App Key, borough, and sold/listed status to run" % sys.argv[0]
	sys.exit(1)

g = Geoclient(sys.argv[1], sys.argv[2])
borough = sys.argv[3]
status = sys.argv[4].lower()

if status != 'sold' and status != 'listed':
	print "Error: %s Stats must be written as sold or listed" % sys.argv[4]
	sys.exit(1)


# Name the files that will be output, based on their status of sold or listed
path = borough + '_' + status + '.csv'
path2 = borough + '_' + status + '.json'

# For every file the directory
for file in glob.glob(os.getcwd() + "/" + borough + "/" + "*_" + status +".tsv"):
	# Use to skip unnecessary lines in file
	count = 0
	# extract ps<num>-<borough> from school name
	dirname, filename = os.path.split(file)
	s, ext = os.path.splitext(filename)
	s = s.split('_')[0]
	
	# Open the file and the two files that will store the JSON and the original TSV + school info
	with open(file,'r') as tsvin, open (path2, 'a') as jsonout, open (path, 'a') as csvout:
    		tsvin = csv.reader(tsvin, delimiter='\t')
    		csvout = csv.writer(csvout, delimiter = ',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    		
    		# For every row in the TSV
    		for row in tsvin:
    			# Get only a few rows
    			count+=1
    			if count == 1 or count == 2:
    				continue
    			
    			### For testing code purposes to make sure we can get data w/o pulling it all at once ###
    			#if count == 5:
    			#		break;
    			
    			# Get the address row and split the number from the street for API purposes
        		address = row[7]
        		address = address.split(' ', 1)
        		
        		# Call the API and store in jsonData
        		jsonData = g.address(address[0], address[1], row[5])
        		
        		# Write to the TSV storing file the original data, only with address column replaced with separated address,
        		# plus the longitude, latitude
        		csvout.writerow(row[0:6] + [s, address[0], address[1]] + row[8:11] + [jsonData['latitude'], jsonData['longitude']] + row[12:])
        		jsonout.write(json.dumps(jsonData) + '\n')

                        # respect the API
                        sleep(0.5)
