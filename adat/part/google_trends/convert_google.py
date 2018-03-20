import csv
import re
from datetime import datetime
import glob

FIELD_RE = re.compile(r'(.+?):\W\((.+?)\)', re.UNICODE and re.DOTALL)
NONPRINTABLE_RE = re.compile(r'[\W_]+', re.UNICODE)

def numeric_value(text):
    return int(NONPRINTABLE_RE.sub('', text))

def parse_field(field):
    match = FIELD_RE.search(field)
    if match:
        return dict(search_term=match.group(1), geo=match.group(2))

def value_mapping(filename, key, value):
    reader = csv.DictReader(open(filename, 'r', encoding='utf-8'))
    return {row[key]: row[value] for row in reader}

def jelolt_id(szavazokor, id, oevk_map):
    oevk = oevk_map[szavazokor]
    return '{}/{}'.format(oevk, id)

def convert_dictionary(row):
    fields_to_reshape = [key for key in list(row.keys()) if parse_field(key) is not None]
    output = [parse_field(key) for key in fields_to_reshape]
    for key, value in row.items():
        if key in fields_to_reshape:
            i = fields_to_reshape.index(key)
            # insert in proper row
            output[i].update(dict(search_volume=numeric_value(value)))
        else:
            # common variables, insert in all rows
            [line.update({key.lower(): value}) for line in output]
    return output


def read_csv(filename):
    with open(filename, 'r') as f:
        [f.readline() for _ in range(2)]
        return list(csv.DictReader(f))

def write_csv(list_name, datastore, mappings):
    writer = csv.DictWriter(open('{}.csv'.format(list_name), 'w', encoding='utf-8'), fieldnames=datastore[0].keys())
    writer.writeheader()
    for row in datastore:
        for key in row.keys():
            if key in mappings:
                row[key] = mappings[key][row[key]]
        writer.writerow(row)

if __name__ == '__main__':

    tablak = ['reszvetel', 'nagypartok', 'kispartok']
    mappings = dict(
        geo=value_mapping('key.csv', 'google_nev', 'megye'),
        search_term=value_mapping('term.csv', 'search_term', 'partnev'),
        )
    print(mappings)
    for tabla in tablak:
        datastore = []
        file_list = glob.glob('{}/*.csv'.format(tabla))

        for filename in file_list:
            for line in read_csv(filename):
                datastore.extend(convert_dictionary(line))

        write_csv(tabla, datastore, mappings)

