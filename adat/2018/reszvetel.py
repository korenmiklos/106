#!/usr/bin/env python
# -*- coding: utf-8 -*-
import csv, re
import lxml.html, lxml.etree
import glob

NONPRINTABLE_RE = re.compile(r'[\W_]+', re.UNICODE)

def clean_cell(text):
    if text:
        return text.strip() or 'id'
    else:
        return 'id'

def numeric_value(text):
    return int(NONPRINTABLE_RE.sub('', text))

def html_table(element):
    output = []
    # first <th> element is used as header
    headers = [clean_cell(col.text) for col in element.xpath('tr[th]')[0]]
    for row in element.xpath('tr[td]'):
        values = [clean_cell(col.text) for col in row]
        output.append(dict(zip(headers, values)))
    return output

def last_table(html):
    return lxml.html.fromstring(html).xpath('//table')[-1]

def oevk_mapping(filename):
    reader = csv.DictReader(open(filename, 'r'))
    return {row['szavazokor']: row['oevk'] for row in reader}

def store_reszvetel(row, reszvetel_lista):
    if 'EE' in row:
        # kulkepviseletes
        row['valasztopolgarok'] = numeric_value(row['EE'])
    else:
        row['valasztopolgarok'] = numeric_value(row['AE'])
    row['ervenyes'] = numeric_value(row['NE'])
    row['ervenytelen'] = numeric_value(row['ME'])
    reszvetel_lista.append(dict(szavazokor=row['szavazokor'], 
        valasztopolgarok=row['valasztopolgarok'],
        ervenyes=row['ervenyes'],
        ervenytelen=row['ervenytelen']))

def parse_file(filename, datastore):
    html = open(filename,'r', encoding='utf-8').read()
    telepules = re.search(r'M\d{2}/T\d{3}', filename).group(0)
    idopont = re.search(r'naptelszk(\d)\.html', filename).group(1)

    output = html_table(last_table(html))
    row = {}
    [row.update(table) for table in output]
    row['szavazokor'] = '{}/{}'.format(telepules, row['Sorszám'])
    row['idopont'] = idopont
    datastore['reszvetel'].append(row)

def write_csv(list_name, datastore):
    data = datastore[list_name]
    writer = csv.DictWriter(open('{}.csv'.format(list_name), 'w', encoding='utf-8'), fieldnames=data[0].keys())
    writer.writeheader()
    for row in data:
        writer.writerow(row)

if __name__ == '__main__':

    # FIXME: ugyanazok a szavazokorok?
    oevk_map = oevk_mapping('../telepules/szavazokor_oevk_2014.csv')
    datastore = dict(oevk_map=oevk_map, reszvetel=[])
    file_list = glob.glob('html/M??/T???/naptelszk?.html')

    for filename in file_list:
        parse_file(filename, datastore)

    write_csv('reszvetel', datastore)