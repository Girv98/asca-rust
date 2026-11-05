#!/usr/bin/env python
import warnings
warnings.filterwarnings("ignore", "\nPyarrow", DeprecationWarning)
import os
import pandas as pd
import json



def render(d):    
    grapheme = d.get("$")
    del d["$"]

    rut = f"{d['cons']}{d['son']}{d['syll']}"
    man = f"{d['cont']}{d['app']}{d['lat']}{d['nas']} {d['dr']}{d['stri']}{d['rho']}{d['clk']} -"
    lar = f"{d['voi']}{d['sg']}{d['cg']}"

    lab = f"{d['ldl']}{d['rnd']}"
    cor = f"{d['ant']}{d['dist']}"
    dor = f"{d['fr']}{d['bk']}{d['hi']}{d['lo']}{d['tens']}{d['red']}"
    phr = f"{d['atr']}{d['rtr']}"

    res = f"{rut}|{man}|{lar}|{lab}|{cor}|{dor}|{phr} = {grapheme}"

    # print(res)

    return res




def main():
    filepath = os.path.join(os.path.dirname(__file__), "features.tsv")
    data = pd.read_csv(filepath, sep="\t")
    x = data.to_dict(orient="records")
    cardinals = []

    for i in range(len(x)):
        cardinals.append(render(x[i]))


    res = '\n'.join(cardinals)
    print(res)

    # for i in range(len(x)):
    #     asdf = set_features((replace(x[i])))
    #     g = asdf.get("grapheme")
    #     cardinals[g] = asdf

    #     del cardinals[g]["grapheme"]

    # with open('cardinals.json', 'w', encoding='utf8') as file:
    #     json.dump(cardinals, file, ensure_ascii=False)

    # print("Done")
    
if __name__ == "__main__":
    main()