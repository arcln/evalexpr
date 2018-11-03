#!/usr/bin/env python

import argparse
import json
import subprocess

OFF     = '\33[0m'
RED     = '\33[31m'
GREEN   = '\33[32m'
BLUE    = '\33[34m'

parser = argparse.ArgumentParser()
parser.add_argument('--file', type=str, required=True)
args = parser.parse_args()

with open(args.file) as file:
    i=0
    success=0
    for line in file:
        data = json.loads(line)
        p1 = subprocess.Popen(['echo', data['expr']], stdout=subprocess.PIPE)
        p2 = subprocess.Popen(['./calc', data['base'], data['operators'], str(data['length'])], stdin=p1.stdout, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        p1.stdout.close()
        out, err = p2.communicate()
        if data['result'][0] != '-':
            if (out or err) != data['result']:
                print RED
            else:
                print GREEN
                success += 1
            print "Test :\t" + str(i)
            print "expr :\t" + data['expr']
            print '<\t' + (out or err)
            print '>\t' + data['result']
            print OFF
            i += 1
            print BLUE + "Results :\t" + str(success) + "/" + str(i) + " (" + str(success * 100 / i) + "%)" + OFF
            if i % 1500 == 0:
                file.seek(0)
    file.close()
