import requests
import pprint
import csv
import urllib2
from time import sleep

base = 'http://streeteasy.com/nyc/api/sales/data?criteria='
key_and_format = '&key=830ee76fb86655786d068341d480a61ca642b375&format=json'
school_list = ('ps39-brooklyn','ps124-brooklyn','ps10-brooklyn')
bed_list = (1, 2, 3)
sqft_list = (800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000)
type_list = ('coop', 'condos', 'houses', 'multi-family')
flag = 0

with open('mycsvfile3.csv', 'a') as f:
   for school in school_list:
       schools = '|schools:%s' % school
       #to print defined schools       
       for housing in type_list:
           housing1 = '|housing1: %s' % housing
           #to print defined housing            
           for nums in bed_list:
               beds = '|beds:%s' % nums
               #to print 1,2,3 bedrooms                
               for sqft in sqft_list:
                   squaref = '|squaref:%s' % sqft
                   #create api request based on the parameters                    
                   api_request = base + schools + housing1 + beds + squaref + key_and_format
                   #create api request based on the parameters 
                   print api_request                    
                   response = requests.get(api_request)
                   sleep(5)
                   data = response.json()
                   print data                    
                   dict2 = {'school':school, 'housing': housing, 'sqft':sqft, 'beds':nums}
		   #creating dict to separate the adjusted criteria column to dif columns
                   dict2.update(data)
                   w = csv.DictWriter(f, dict2.keys(), quotechar='"', delimiter=',')
                   if flag == 0:
                   #to write header only one time check if flag is 0
                       w.writeheader()
                       flag += 1
                   w.writerow(dict2)
                   f.flush() 
