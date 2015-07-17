import requests
import pprint
import csv
import urllib2
from time import sleep
import os.path

import sys
from time import sleep

if len(sys.argv) !=6:
	print "usage: %s api_key bed_list school_list sqfts type_list" % sys.argv[0]
	sys.exit(1)
api_key = sys.argv[1]
bed_list = sys.argv[2].split(',')
school_list= sys.argv[3].split(',')
sqfts = sys.argv[4].split(',')
type_list =  sys.argv[5].split(',')

base = 'http://streeteasy.com/nyc/api/sales/data?criteria='
key_and_format = '&key=%s&format=json'% api_key

for schooly in school_list: #go through defined schools
    school = 'school:%s' % schooly
    filename = '%s_school.csv' % (schooly)

    # check for existing file
    # if exists, don't write header and create a set of previously covered criteria
    write_header = 1
    previous_criteria = set()
    if os.path.exists(filename):
        write_header = 0
        with open(filename, 'r') as f:
                reader = csv.DictReader(f, quotechar='"', delimiter=',')
                previous_criteria = set([row["criteria"] + "|housing1:" + row["housing"] for row in reader])

    with open(filename, 'a') as f:
        school = 'school:%s' % schooly
        for housing in type_list: #go through defined housing
	    housing1 = 'housing1:%s' % housing
            for num in bed_list: # go through 1,2,3 bedrooms
                beds = 'beds:%s' % num
                for sq in sqfts: # go through defined sqft
	    	    sgft = 'sqft>%s' % sq
                    criteria = "|".join([sgft, school, beds, housing1])
                    if criteria not in previous_criteria:
                        api_request = base + criteria + key_and_format
                        #api_request = base+housing1+sgft+beds+school+key_and_format #create api request based on the parameters
                        print api_request
                        response = requests.get(api_request)
                        sleep(5)
                        data = response.json()
                        print data
                        dict2 = {'school':schooly,'housing':housing, 'sqft':sq, 'beds':num} #creating dict to separate the adjusted criteria column to dif columns
                        dict2.update(data)
                        w = csv.DictWriter(f, dict2.keys(), quotechar='"', delimiter=',')
                        if write_header == 1:#to write header only one time check if flag is 0
                                w.writeheader()
                                write_header = 1
                        w.writerow(dict2)
                        f.flush()# see line by line in a file
