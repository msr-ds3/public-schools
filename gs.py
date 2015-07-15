import requests
import pprint
import csv
import urllib2
from time import sleep

base = 'http://streeteasy.com/nyc/api/sales/data?criteria='
key_and_format = '&key=204b3888c9d242aff1b3dbc10be732a38eaef24f&format=json'


bed_list = (1,2,3)
school_list = ('ps39-brooklyn','ps124-brooklyn','ps10-brooklyn')
sqfts = (800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)
type_list = ('coops','condos','houses','multi-family')
flag = 0

with open('mycsvfile.csv', 'a') as f: 
    for schooly in school_list: #go through defined schools
        school = 'school:%s' % schooly	
        for housing in type_list: #go through defined housing 
	    housing1 = 'housing1:%s' % housing
            for num in bed_list: # go through 1,2,3 bedrooms
                beds = '|beds:%s' % num	 	
                for sq in sqfts: # go through defined sqft 
	    	    sgft = '|sqft:%s' % sq	
	            api_request = base+housing1+sgft+beds+school+key_and_format #create api request based on the parameters 
	            print api_request

	            response = requests.get(api_request)
                    sleep(20)
	            data = response.json()
	            #print data

		    dict2 = {'sqft':sq, 'beds':num, 'school':schooly, 'housing':housing} #creating dict to separate the adjusted criteria column to dif columns
		    dict2.update(data)
                    w = csv.DictWriter(f, dict2.keys(), quotechar='"', delimiter=',') 
		    if flag == 0:#to write header only one time check if flag is 0
                        w.writeheader() 
                        flag += 1
                    w.writerow(dict2)
                    f.flush()# see line by line in a file     
