#!/usr/bin/env python

import sys
from mechanize import Browser
from getpass import getpass
from time import sleep
from random import randrange
import os

if __name__=='__main__':
    if len(sys.argv) < 4:
        sys.stderr.write('usage: %s <borough> <status> <username>\n' % sys.argv[0])
        sys.exit(1)

    # parse args
    borough, status, user = sys.argv[1], sys.argv[2], sys.argv[3]
    passwd = getpass('password: ')

    # set up browser
    br = Browser()
    br.addheaders = [('User-agent', 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008071615 Fedora/3.0.1-1.fc9 Firefox/3.0.1')]
    br.set_handle_robots(False)

    # sign in
    signin_url = 'http://streeteasy.com/nyc/user/sign_in_dialog'
    br.open(signin_url)
    br.select_form(nr=1)
    br['login'] = user
    br['password'] = passwd
    response = br.submit()

    if not os.path.exists(borough):
        os.mkdir(borough)

    for n, line in enumerate(open('../schools/elementary_schools_%s.csv' % borough, 'r')):
        if n == 0:
            continue

        dbn, school = line.rstrip().split(',')

        fname = '%s/%s_%s.tsv' % (borough, school, status)

        if not os.path.exists(fname):
            print fname

            try:
                url = 'http://streeteasy.com/nyc/process/sales/xls/school:%s|status:%s' % (school, status)
                f = br.retrieve(url, fname)
            except:
                sys.stderr.write('error retrieving %s' % url)

            sleep(randrange(60))
