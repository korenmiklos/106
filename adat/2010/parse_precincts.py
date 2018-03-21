#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os, csv, re
import lxml.html, lxml.etree
import glob

'''
Jelolt-id: 2014 / OEVK # / jelolt sorszam
'''

EGYENI_RE = re.compile(r'<p>d\) Érvényes szavazatok száma jelöltenként</p>(.*?</table>)', re.UNICODE and re.DOTALL)
LISTAS_RE = re.compile(r'<p>d\) Érvényes szavazatok száma listánként</p>(.*?</table>)', re.UNICODE and re.DOTALL)
RESZVETEL_RE = re.compile(r'<p>a\) A választók nyilvántartása</p>(.*?</div>\n<p>d\) Érvényes szavazatok száma jelöltenként</p>)', re.M and re.UNICODE and re.DOTALL)
NONPRINTABLE_RE = re.compile(r'[\W_]+', re.UNICODE)


def clean_cell(text):
    if text:
        return text.strip() or 'id'
    else:
        return 'id'

def numeric_value(text):
    return int(NONPRINTABLE_RE.sub('', text))

def html_table(etree_elements, header=1):
    output = []
    for element in etree_elements:
        if header==1:
            # first <td> element is used as header
            headers = [clean_cell(col.text_content()) for col in element.xpath('tr[td]')[0]]
            for row in element.xpath('tr[td]')[1:]:
                values = [clean_cell(col.text_content()) for col in row]
                output.append(dict(zip(headers, values)))
        else:
            # second <td> element is used as header
            headers = [clean_cell(col.text_content()) for col in element.xpath('tr[td]')[1]]
            for row in element.xpath('tr[td]')[2:]:
                values = [clean_cell(col.text_content()) for col in row]
                output.append(dict(zip(headers, values)))
    return output

def find_tables(html, regex):
    table_text = regex.search(html).group(1)
    if table_text[0:4]=='<td>':
        table_text = '<table><tr>'+table_text
    return lxml.html.fromstring(table_text).xpath('//table')

def oevk_mapping(filename):
    reader = csv.DictReader(open(filename, 'r'))
    return {row['szavazokor']: row['oevk'] for row in reader}

def jelolt_id(szavazokor, id, oevk_map):
    oevk = oevk_map[szavazokor]
    return '{}/{}'.format(oevk, id)

def szervezet_id(szavazokor, id):
    return '{}/{}'.format(szavazokor[:3], id)

def store_relation(row, jelolt_lista, eredmeny_lista, oevk_map):
    jid = jelolt_id(row['szavazokor'], row['id'], oevk_map)
    if jid not in [_['id'] for _ in jelolt_lista]:
        jelolt_lista.append(dict(id=jid, nev=row['Jelölt neve'], szervezet=row['Jelölő szervezet(ek)']))
    eredmeny_lista.append(dict(szavazokor=row['szavazokor'], jelolt=jid, szavazat=numeric_value(row['Kapottérvényesszavazat'])))

def store_listas(row, szervezet_lista, eredmeny_lista):
    jid = row['id']
    if jid not in [_['id'] for _ in szervezet_lista]:
        szervezet_lista.append(dict(id=jid, szervezet=row['Lista neve']))
    else:
        try:
            assert dict(id=jid, szervezet=row['Lista neve']) in szervezet_lista
        except: 
            pass 
    eredmeny_lista.append(dict(szavazokor=row['szavazokor'], part=jid, szavazat=numeric_value(row['Kapottérvényesszavazat'])))

def store_reszvetel(row, reszvetel_lista):
    if 'E' in row:
        # kulkepviselet
        row['valasztopolgarok'] = numeric_value(row['E'])
    else:
        row['valasztopolgarok'] = numeric_value(row['C'])
    row['ervenyes'] = numeric_value(row['M'])
    row['ervenytelen'] = numeric_value(row['L'])
    reszvetel_lista.append(dict(szavazokor=row['szavazokor'], 
        valasztopolgarok=row['valasztopolgarok'],
        ervenyes=row['ervenyes'],
        ervenytelen=row['ervenytelen']))

def parse_file(filename, datastore):
    html = open(filename,'r', encoding='latin2').read()
    m1 = re.search('\d{2}/\d{3}', filename)
    m2 = re.search('jkv(\d+)\.', filename)
    szavazokor_id = 'M{}/T{}/{:03}'.format(*m1.group(0).split('/'),int(m2.group(1)))

    print(szavazokor_id)

    for fajl in fuggvenyek.keys():
        if fajl=='reszvetel':
            output = html_table(find_tables(html, fuggvenyek[fajl]), header=2)
            row = {}
            [row.update(table) for table in output]
            row['szavazokor'] = szavazokor_id
            store_reszvetel(row, datastore['reszvetel'])
        else:
            output = html_table(find_tables(html, fuggvenyek[fajl]), header=1)
            for row in output:
                row['szavazokor'] = szavazokor_id
                if fajl=='egyeni':
                    store_relation(row, datastore['jelolt'], datastore['egyeni'], datastore['oevk_map'])
                elif fajl=='listas':
                    row['id'] = szervezet_id(szavazokor_id, row['id'])
                    store_listas(row, datastore['szervezet'], datastore['listas'])

def write_csv(list_name, datastore):
    data = datastore[list_name]
    writer = csv.DictWriter(open('{}.csv'.format(list_name), 'w', encoding='utf-8'), fieldnames=data[0].keys())
    writer.writeheader()
    for row in data:
        writer.writerow(row)

fuggvenyek = {'egyeni': EGYENI_RE, 'listas': LISTAS_RE, 'reszvetel': RESZVETEL_RE}

if __name__ == '__main__':

    oevk_map = oevk_mapping('szavazokor.csv')
    datastore = dict(oevk_map=oevk_map, jelolt=[], egyeni=[], szervezet=[], listas=[], reszvetel=[])
    file_list = glob.glob('html/??/???/*.htm')

    for filename in file_list:
        parse_file(filename, datastore)

    for key in ['jelolt', 'egyeni', 'szervezet', 'listas', 'reszvetel']:
        write_csv(key, datastore)