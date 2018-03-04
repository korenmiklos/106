#!/usr/bin/env python
# -*- coding: utf-8 -*-
import requests
import re
import csv

OEVK_URL = 'http://www.valasztas.hu/dyn/pv10vt/vertaj-static/pv10vt/v11.html'
TELEPULES_URL = 'http://www.valasztas.hu/dyn/pv10vt/vertaj-static/pv10vt/maz/{}/oevk/{}/v12.html'
SZAVAZOKOR_URL = 'http://www.valasztas.hu/dyn/pv10vt/vertaj-static/pv10vt/maz/{}/taz/{}/v21.html'

TELEPULESNEV_RE = re.compile(r'<i>&nbsp;(.*?) szavazókörei&nbsp;</i>')
OEVK_RE = re.compile(r'/maz/(?P<megye>\d{2})/oevk/(?P<telepules>\d{2})')
TELEPULES_RE = re.compile(r'/maz/(?P<megye>\d{2})/taz/(?P<telepules>\d{3})')
SZAVAZOKOR_RE = re.compile(r'/maz/(\d{2})/taz/(\d{3})/v23-so(\d{3}).html')

def parse_table(html, regex):
	return regex.findall(html)

def store_telepules(telepules, telepules_lista):
	if telepules not in telepules_lista:
		telepules_lista.append(telepules)

def write_csv(list_name, datastore):
    writer = csv.DictWriter(open('{}.csv'.format(list_name), 'w', encoding='utf-8'), fieldnames=datastore[0].keys())
    writer.writeheader()
    for row in datastore:
        writer.writerow(row)

if __name__ == '__main__':
	datastore = dict(
		szavazokor=[],
		telepules=[])
	oevk_lista = parse_table(requests.get(OEVK_URL).text, OEVK_RE)
	for oevk in oevk_lista:
		oevk_id = 'M{}/E{}'.format(*oevk)
		print(oevk_id)
		telepules_lista = parse_table(requests.get(TELEPULES_URL.format(*oevk)).text, TELEPULES_RE)
		for telepules in telepules_lista:
			telepules_id = 'M{}/T{}'.format(*telepules)
			html = requests.get(SZAVAZOKOR_URL.format(*telepules)).text
			telepules_nev = TELEPULESNEV_RE.search(html).group(1)
			store_telepules(dict(id2010=telepules_id, nev=telepules_nev), datastore['telepules'])
			szavazokor_lista = parse_table(html, SZAVAZOKOR_RE)
			for kor in szavazokor_lista:
				szavazokor_id = 'M{}/T{}/{}'.format(*kor)
				datastore['szavazokor'].append(dict(
					szavazokor=szavazokor_id,
					telepules=telepules_id,
					oevk=oevk_id
					))
	for key in datastore.keys():
		write_csv(key, datastore[key])
