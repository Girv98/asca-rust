#!/usr/bin/env python
import warnings
warnings.filterwarnings("ignore", "\nPyarrow", DeprecationWarning)
import os
import pandas as pd
import json


def set_features(d):
    grapheme = ''
    
    root = 0
    manner = 0
    laryngeal = 0
    
    labial = 0
    coronal = 0
    dorsal = 0
    pharyngeal = 0

    place_head = 0


    grapheme = d.get("$")
    del d["$"]

    # Set Root Features
    if d['cons']:
        root += 0b100
    if d['son']:
        root += 0b010
    if d['syll']:
        root += 0b001

    # Set Manner Features
    if d['cont']:
        manner += 0b10000000
    if d['app']:
        manner += 0b01000000
    if d['lat']:
        manner += 0b00100000
    if d['nas']:
        manner += 0b00010000
    if d['dr']:
        manner += 0b00001000
    if d['stri']:
        manner += 0b00000100
    if d['rho']:
        manner += 0b00000010
    if d['clk']:
        manner += 0b00000001

    # Set Laryngeal Features
    if d['voi']:
        laryngeal += 0b100
    if d['sg']:
        laryngeal += 0b010
    if d['cg']:
        laryngeal += 0b001

    # Set Labial Features
    if d['LAB'] == True:
        place_head |= 0x8000
        if d['ldl']:
            labial += 0b10_00_000000_00
        if d['rnd']:
            labial += 0b01_00_000000_00

    # Set Coronal Features
    if d['COR'] == True:
        place_head |= 0x4000
        if d['ant']:
            coronal+= 0b10_000000_00
        if d['dist']:
            coronal+= 0b01_000000_00

    # Set Dorsal Features
    if d['DOR'] == True:
        place_head |= 0x2000
        if d['fr']:
            dorsal += 0b100000_00
        if d['bk']:
            dorsal += 0b010000_00
        if d['hi']:
            dorsal += 0b001000_00
        if d['lo']:
            dorsal += 0b000100_00
        if d['tens']:
            dorsal += 0b000010_00
        if d['red']:
            dorsal += 0b000001_00

    # Set Pharyngeal Features
    if d['PHR'] == True:
        place_head |= 0x1000
        if d['atr']:
            pharyngeal += 0b10
        if d['rtr']:
            pharyngeal += 0b01

    place = None

    if place_head != 0:
        place = 0
        place = (place_head | labial | coronal | dorsal | pharyngeal)

    return {
        'grapheme': grapheme,
        'root': root,
        'manner': manner,
        'laryngeal': laryngeal,
        'place': place
    }

    

def replace(d):
    for k, v in d.items():
        if v == '+':
            d[k] = True
        if v == '-':
            d[k] = False
        
    return d


def main():
    filepath = os.path.join(os.path.dirname(__file__), "features.tsv")
    data = pd.read_csv(filepath, sep="\t")
    x = data.to_dict(orient="records")
    cardinals = []
    cardinals = {}

    for i in range(len(x)):
        asdf = set_features((replace(x[i])))
        g = asdf.get("grapheme")
        cardinals[g] = asdf

        del cardinals[g]["grapheme"]

    with open('cardinals.json', 'w', encoding='utf8') as file:
        json.dump(cardinals, file, ensure_ascii=False)

    print("Done")
    
if __name__ == "__main__":
    main()
