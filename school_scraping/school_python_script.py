import requests
import pprint
import csv
import urllib2
from time import sleep
import os.path

import sys
from time import sleep
'''
if len(sys.argv) !=6:
	print "usage: %s api_key bed_list school_list sqfts type_list" % sys.argv[0]
	sys.exit(1)
api_list = sys.argv[1]
bed_list = sys.argv[2].split(',')
school_list= sys.argv[3].split(',')
sqfts = sys.argv[4].split(',')
type_list =  sys.argv[5].split(',')
'''


school_list = ['ps282-brooklyn', 'ps321-brooklyn','ps107-brooklyn', 'ps39-brooklyn','ps124-brooklyn', 'ps10-brooklyn']

type_list = ['condos','coops','miltifamily','houses']
bed_list = [1,2,3]
sqfts = [1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000]

api_key_list = [
    '3ae8e9e5bc2862aac38f9a3adae1592bda78c9ec',
    '89fad834cfa823e4e7a715c734ca36467c9d11ba',
    'a3cad565bf2da0e7140dad4835ba532135c5f7ac',
    'aca19ac6c6a1063493c674dcb7dbca96e1ca3776',
    '0b3a57da6ddebcf4a3a5047418556c613a61296d',
    '9ed7a74500caba5031604613b0d78f012613832e',
    'a7b3aea6b49d1b871965cf44513b460f7b7d9242',
    'c3bb7803a74b4965526e29f01f53399f916f1109'
]

api_key_number = 0 #start using api key number 0 (first position)
api_key_used_counter = 0 #times current api key has been used

base = 'http://streeteasy.com/nyc/api/sales/data?criteria='



for schooly in school_list: #go through defined schools
    school = 'school:%s' % schooly
    filename = '%s_school.csv' % (schooly)

    # check for existing file
    # if exists, don't write header and create a set of previously covered criteria
    previous_criteria = set()
    write_header = 1
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
                        key_and_format = '&key=%s&format=json'% api_key_list[api_key_number]
                        api_request = base + criteria + key_and_format
                        api_key_used_counter = api_key_used_counter + 1
                        if(api_key_used_counter >= 100):
                            api_key_used_counter = 0
                            api_key_number = api_key_number + 1
                        #api_request = base+housing1+sgft+beds+school+key_and_format #create api request based on the parameters
                        print api_request
                        print "apikey:", api_key_number, "numTimesUsed:", api_key_used_counter

                        response = requests.get(api_request)
                        sleep(2)
                        data = response.json()
                        print data
                        dict2 = {'school':schooly,'housing':housing, 'sqft':sq, 'beds':num} #creating dict to separate the adjusted criteria column to dif columns
                        dict2.update(data)
                        w = csv.DictWriter(f, dict2.keys(), quotechar='"', delimiter=',')
                        if write_header == 1:#to write header only one time check if flag is 0
                                w.writeheader()
                                write_header = 0
                        w.writerow(dict2)
                        f.flush()# see line by line in a file
