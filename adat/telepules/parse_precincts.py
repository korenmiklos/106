#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os, csv, re
import lxml.html
import glob

'''
Jelolt-id: 2014 / OEVK # / jelolt sorszam
'''

TELEPULES_RE = re.compile(r'<h2>(.*?) szavazókörei</h2>', re.UNICODE and re.DOTALL)

def find_city(html, regex):
    matches = regex.search(html)
    return matches.group(1)

def parse_file(filename, datastore):
    html = open(filename,'r', encoding='latin2').read()
    telepules_id = re.search('M\d{2}/T\d{3}', filename).group(0)

    telepules_nev = find_city(html, TELEPULES_RE)
    datastore.append(dict(telepules_id=telepules_id, telepules_nev=telepules_nev))

def write_csv(list_name, datastore):
    writer = csv.DictWriter(open('{}.csv'.format(list_name), 'w', encoding='utf-8'), fieldnames=datastore[0].keys())
    writer.writeheader()
    for row in datastore:
        writer.writerow(row)

if __name__ == '__main__':

    datastore = []
    file_list = glob.glob('html/M??/T???/v21.html')

    for filename in file_list:
        parse_file(filename, datastore)

    write_csv('telepules_kodok', datastore)