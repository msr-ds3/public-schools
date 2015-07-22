__author__ = 'gascencio'

import csv
import json
import sys


from nyc_geoclient import Geoclient

g = Geoclient('d0695d00', '8525a047221e0446b376bc98bf8d68ae')

#print g.address('1500', 'Madison Ave', 'Manhattan')


boroughs =['manhattan', 'brooklyn', 'bronx', 'statenisland', 'queens']


if len(sys.argv) !=3:
    print "usage: %s g borough" % sys.argv[0]
    sys.exit(1)
    g = sys.argv[1]
    boroughts = sys.argv[2].split(',')

count = 0

for borough in boroughs:
    with open('SoldListingsInNYC.xls', 'r') as stvn, open('borough_sold.json', 'wb') as csvout, open('borough_sold.tsu', 'wb') as csvout:

        tsvin = csv.reader(tsvin, delimiter='\t')
        csvout = csv.writer(csvout)

        tsvin2 = csv.reader(tsvin2, delimiter='\t')
        csvout2 = csv.write(csvout2)

        for row in tsvin:
            count += 1

             if count == 1 or count == 2:
                csvout.writerows([row[1:6] +[address[0]+ address[1]]+ row[8:26])

            dict2 = {'borough': boroughs, 'beds': num} #creating dict to separate the adjusted
                        dict2.update(data)
                        w = csv.DictWriter(f, dict2.keys(), quotechar='"', delimiter=',')
                        if write_header == 1:#to write header only one time check if flag is 0
                                w.writeheader()
                                write_header = 0
                        w.writerow(dict2)
                        f.flush()# see line by line in a file
