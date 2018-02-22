#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os, csv, re
import lxml.html
import glob

'''
Jelolt-id: 2014 / OEVK # / jelolt sorszam
'''

EGYENI_RE = re.compile(r'<p style="text-align:left;">A szavazatok száma jelöltenként</p>(.*?</table>)', re.UNICODE and re.DOTALL)
LISTAS_RE = re.compile(r'<p style="text-align:left;">A szavazatok száma pártlistánként</p>(.*?</table>)', re.UNICODE and re.DOTALL)
NONPRINTABLE_RE = re.compile(r'[\W_]+', re.UNICODE)


def clean_cell(text):
    if text:
        return text.strip() or 'id'
    else:
        return 'id'

def numeric_value(text):
    return int(NONPRINTABLE_RE.sub('', text))

def html_table(etre_element):
    output = []
    rows = iter(etre_element)
    headers = [clean_cell(col.text) for col in next(rows)]
    for row in rows:
        values = [clean_cell(col.text) for col in row]
        output.append(dict(zip(headers, values)))
    return output

def find_table(html, regex):
    matches = regex.search(html)
    return lxml.html.fromstring(matches.group(1)).xpath('//table')[0]

def oevk_mapping(filename):
    reader = csv.DictReader(open(filename, 'r'))
    return {row['szavazokor']: row['oevk'] for row in reader}

def jelolt_id(szavazokor, id, oevk_map):
    oevk = oevk_map[szavazokor]
    return '{}/{}'.format(oevk, id)

def store_relation(row, jelolt_lista, eredmeny_lista, oevk_map):
    jid = jelolt_id(row['szavazokor'], row['id'], oevk_map)
    if jid not in [_['id'] for _ in jelolt_lista]:
        jelolt_lista.append(dict(id=jid, nev=row['A jelölt neve'], szervezet=row['Jelölő szervezet(ek)']))
    else:
        assert dict(id=jid, nev=row['A jelölt neve'], szervezet=row['Jelölő szervezet(ek)']) in jelolt_lista
    eredmeny_lista.append(dict(szavazokor=row['szavazokor'], jelolt=jid, szavazat=numeric_value(row['Kapott érvényes szavazat'])))

def store_listas(row, szervezet_lista, eredmeny_lista):
    jid = row['id']
    if jid not in [_['id'] for _ in szervezet_lista]:
        szervezet_lista.append(dict(id=jid, szervezet=row['A pártlista neve']))
    else:
        try:
            assert dict(id=jid, szervezet=row['A pártlista neve']) in szervezet_lista
        except: 
            pass 
    eredmeny_lista.append(dict(szavazokor=row['szavazokor'], part=jid, szavazat=numeric_value(row['Szavazat'])))

def parse_file(filename, datastore):
    html = open(filename,'r', encoding='latin2').read()
    m1 = re.search('M\d{2}/T\d{3}/', filename)
    m2 = re.search('\d{3}(?=\.)', filename)
    szavazokor_id = m1.group(0)+m2.group(0)

    for fajl in fuggvenyek.keys():
        output = html_table(find_table(html, fuggvenyek[fajl]))
        for row in output:
            row['szavazokor'] = szavazokor_id
            if fajl=='egyeni':
                store_relation(row, datastore['jelolt'], datastore['egyeni'], datastore['oevk_map'])
            else:
                store_listas(row, datastore['szervezet'], datastore['listas'])

def write_csv(list_name, datastore):
    data = datastore[list_name]
    writer = csv.DictWriter(open('{}.csv'.format(list_name), 'w', encoding='utf-8'), fieldnames=data[0].keys())
    writer.writeheader()
    for row in data:
        writer.writerow(row)

fuggvenyek = {'egyeni': EGYENI_RE, 'listas': LISTAS_RE}

if __name__ == '__main__':

    oevk_map = oevk_mapping('../telepules/szavazokor_oevk_2014.csv')
    datastore = dict(oevk_map=oevk_map, jelolt=[], egyeni=[], szervezet=[], listas=[])
    file_list = glob.glob('html/M??/T???/*.html')

    for filename in file_list:
        parse_file(filename, datastore)

    for key in ['jelolt', 'egyeni', 'szervezet', 'listas']:
        write_csv(key, datastore)