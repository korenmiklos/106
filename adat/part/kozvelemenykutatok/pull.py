import requests
import csv
import re
from datetime import datetime

BASE_URL = 'https://docs.google.com/spreadsheets/d/{}/gviz/tq?tqx=out:csv&gid={}'
SHEETS = dict(
	teljes_nepesseg=dict(
		id='1eitltXmNIU4tHgrYiaQPWPHaG4tN0xVSZO4w_k0mVyA',
		sheet_ids=dict(
			fidesz='0',
			mszp='1391330603',
			jobbik='269899118',
			lmp='1232469361',
			egyutt='1887552498',
			parbeszed='1647908331',
			dk='549675480',
			liberalis='1030782975',
			ketfarku='1917250250',
			momentum='1152159692',
			bizonytalan='1000036518')),
	biztos_valasztok=dict(
		id='1YcsDnVDipTWlcGk78EEeQw5K7O3Y7fJ_8W14LPaQytw',
		sheet_ids=dict(
			fidesz='0',
			mszp='78915130',
			jobbik='1224790716',
			lmp='1308644628',
			egyutt='576367741',
			parbeszed='446987244',
			dk='1701849270',
			liberalis='810680759',
			ketfarku='2082367707',
			momentum='1610294492')))
PARTOK = 'fidesz mszp jobbik lmp egyutt parbeszed dk liberalis ketfarku momentum'.split()
KUTATOK = 'Ipsos Medián Nézőpont Századvég Tárki Publicus Iránytű Republikon IDEA'.split()
DATUM_RE = re.compile(r'(\d{4})\.(\d{2})\.(\d{2})')

def build_url(sheet, part):
	return BASE_URL.format(SHEETS[sheet]['id'], SHEETS[sheet]['sheet_ids'][part])

def get_csv(sheet, part):
	url = build_url(sheet, part)
	response = requests.get(url)
	decoded_content = response.content.decode('utf-8').splitlines()
	decoded_content[0] = 'datum'+decoded_content[0][2:]

	return list(csv.DictReader(decoded_content))

def get_kutato(key):
	return re.split('\W+', key.strip(), re.UNICODE)[0]

def iso_datum(text):
	return datetime.strptime(text, '%Y.%m.%d.').isoformat()[:10]

def parse_row(row):
	for key in row.keys():
		if key=='datum':
			row[key] = iso_datum(row[key])
		kutato = get_kutato(key)
		if kutato in KUTATOK:
			row[kutato] = row.pop(key)
	return row

def store_row(row, part, fajl, datastore):
	for key, value in row.items():
		if key in KUTATOK:
			datastore.append(dict(
				datum=iso_datum(row['datum']),
				part=part,
				minta=fajl,
				kutato=key,
				szazalek=value))

if __name__ == '__main__':
	datastore = []
	for sheet in SHEETS.keys():
		for part in PARTOK:
			for row in get_csv(sheet, part):
				store_row(row, part, sheet, datastore)
	writer = csv.DictWriter(open('kozvelemenykutatas.csv', 'w'), fieldnames=datastore[0].keys())
	writer.writeheader()
	for row in datastore:
		writer.writerow(row)
