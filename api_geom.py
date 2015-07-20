__author__ = 'gascencio'

import csv
import json


from nyc_geoclient import Geoclient

g = Geoclient('d0695d00', '8525a047221e0446b376bc98bf8d68ae')

print g.address('1500', 'Madison Ave', 'Manhattan')




'''
count = 0
with open('SoldListingsInNYC.xls', 'r') as stvn, open('new.csv', 'wb') as csvout:
    tsvin = csv.reader(tsvin, delimiter='\t')
    csvout = csv.writer(csvout)

    for row in tsvin:
        count += 1

        if count > 0:
            csvout.writerows([row[2:4] for _ in xrange(count)])
'''