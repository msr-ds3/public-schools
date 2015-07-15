import requests
import pprint
import csv

base = 'http://streeteasy.com/nyc/api/sales/data?criteria='
key_and_format = '&key=81eba8961e750d4cc87f3280278cb70cc9ccf09f&format=json'
fixed = 'monthly>=1500|taxes>=500|building:condos|sqft>=800|sqft<=2000'


bed_list = (0, 1, 2, 3)
bat_list = (0, 1, 2, 3)
borough_list = ('manhattan', 'brooklyn', 'queens', 'staten island', 'bronx')
school_list = ('ps8-brooklyn', 'ps24-brooklyn', 'ps10-brooklyn')

for school in school_list:
    myschool = '|schools:%s' % school
    # print 'number of schools:', schools
    for nums in bed_list:
        beds = '|beds:%s' % nums
        #print 'number of beds:', beds
        for num in bat_list:
            baths = '|baths:%s' % num
            #print 'number of baths:', beds

            for borough in borough_list:
                area = '|area:%s' % borough
                #   print 'area:', area
                api_request = base + fixed + area + beds + key_and_format
                print api_request

                response = requests.get(api_request)
                data = response.json()
                print data

                filename = '%s_%sbeds.csv' % (borough, num)
                with open(filename, 'wb') as f:  #converts dictionary into csv file
                    w = csv.DictWriter(f, data.keys())
                    w.writeheader()
                    w.writerow(data)