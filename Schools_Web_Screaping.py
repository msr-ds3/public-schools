import requests
import pprint
import csv
import urllib2
from time import sleep

base = 'http://streeteasy.com/nyc/api/sales/data?criteria='
key_and_format = '&key=4ffecbd2fc90ab5764f321686df6b1fe1fee7962&format=json'

school_list = ('ps282-brooklyn', 'ps321-brooklyn', 'ps107-brooklyn', 'ps10-brooklyn', 'ps8-brooklyn', 'ps24-brooklyn')
bed_list = (1, 2, 3)
sqft_list = (800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000)
type_list = ('coop', 'condos', 'houses', 'multi-family')

flag = 0
with open('mycsvfile.csv', 'a') as f:
    for school in school_list:
        myschool = '|schools:%s' % school
        #print 'number of schools:', school

        for housing in type_list:
            housing1 = '|housing1: %s' % housing

            for nums in bed_list:
                beds = '|beds:%s' % nums
                #print 'number of beds:', beds

                for sqft in sqft_list:
                    squaref = '|squaref:%s' % sqft
                    #print 'square feet:',

                    api_request = base + myschool + housing1 + beds + squaref + key_and_format
                    print api_request

                    response = requests.get(api_request)
                    sleep(60)
                    data = response.json()
                    print data

                    dict2 = {'school': school, 'housing': housing, 'sqft': sqft, 'bed': nums}
                    dict2.update(data)
                    w = csv.DictWriter(f, dict2.keys(), quotechar='"', delimiter=',')
                    if flag == 0:
                        w.writeheader()
                        flag += 1
                    w.writerow(dict2)
                    f.flush()

