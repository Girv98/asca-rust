#!/usr/bin/env python
import json
import os
import sys

from dataclasses import dataclass
from typing import Tuple


def get_section(line: str, line_num: int, line_len: int, i: int) -> Tuple[str, int]:
    section = ""

    while True:
        if line_len <= i: 
            print(f"Error(ln {line_num}): Unexpected end of line")

        match line[i]:
            case '-': section += '-'
            case '+': section += '+'
            case '0': section += '0'
            case ' ': pass
            case '\t': pass
            case _: break

        i += 1
    
    return section, i

def expect_char(line: str, line_num: int, i: int, char: str) -> int: 
    if line[i] == char: 
        i += 1 
    else: 
        print(f"Error(ln {line_num}): expected 'char' got '{line[i]}'")
        sys.exit(1)
    return i
    
def skip_space(line: str, line_len: int, i: int) -> int:
    while i < line_len and line[i] == ' ': 
        i += 1
    return i


def get_section_advance(line: str, line_num: int, line_len: int, i: int, sep: str = '|') -> Tuple[str, int]: 
    rut, i = get_section(line, line_num, line_len, i)
    i = skip_space(line, line_len, i)
    i = expect_char(line, line_num, i, sep)

    return rut, i



def get_binary_str(s: str) -> str | None:
    if '0' in s:
        if len(set(s)) == 1:
            return None
        else:
            print("Error: if section contains '0', all must be '0'")
            sys.exit(1)

    return s.replace('+', '1').replace('-', '0')

def get_binary_str_not_null(s: str, node: str) -> str:
    val = get_binary_str(s)
    if val is None: 
        print(f"Error: {node} node can't be null")
        sys.exit(1)

    return val

@dataclass
class SegmentRepr: 
    glyphs: list[str]
    rut: str; man: str; lar: str
    lab: str; cor: str; dor: str; phr: str

    def get_class(self) -> int:
        rut = get_binary_str_not_null(self.rut, "root")
        lar = get_binary_str_not_null(self.lar, "laryngeal")
        man = get_binary_str_not_null(self.man, "manner")

        # NOTE: last manner digit is currently unused and unrepresented 
        # and so must be manually appended
        return int(f"{rut}{lar}{man}0", base=2) 

    def get_place(self) -> int:
        head = ""

        lab = get_binary_str(self.lab)
        cor = get_binary_str(self.cor)
        dor = get_binary_str(self.dor)
        phr = get_binary_str(self.phr)

        if lab is not None:   head += '1'
        else:   lab = '00';   head += '0'

        if cor is not None:   head += '1'
        else:   cor = '00';   head += '0'

        if dor is not None:   head += '1'
        else: dor = '000000'; head += '0'

        if phr is not None:   head += '1'
        else:   phr = '00';   head += '0'

        return int(f"{head}{lab}{cor}{dor}{phr}", base=2)

    def get_vals(self) -> Tuple[int, int]:
        return self.get_class(), self.get_place()
        
    def __eq__(self, other):
        return (self.rut == other.rut
        and self.man == other.man
        and self.lar == other.lar
        and self.lar == other.lar
        and self.cor == other.cor
        and self.dor == other.dor
        and self.phr == other.phr)

    def __hash__(self):
        return hash((
            self.rut, self.man, self.lar,
            self.lab, self.cor, self.dor, self.phr
        ))
    
    def __str__(self):
        return f"{self.rut}|{self.man}|{self.lar}|{self.lab}|{self.cor}|{self.dor}|{self.phr} = {self.glyphs}"

    def __repr__(self):
        return self.__str__()

        
def read_file(file_path: str) -> list[SegmentRepr]:
    seg_reps: list[SegmentRepr] = []
    with open(file_path, "r") as file:
        for line_n, line in enumerate(file):
            line = line.strip(); line_n += 1

            if not line or line[0] == '#': continue

            line_len = len(line)
            i = 0
            
            rut, i = get_section_advance(line, line_n, line_len, i)
            man, i = get_section_advance(line, line_n, line_len, i)
            lar, i = get_section_advance(line, line_n, line_len, i)
            
            lab, i = get_section_advance(line, line_n, line_len, i)
            cor, i = get_section_advance(line, line_n, line_len, i)
            dor, i = get_section_advance(line, line_n, line_len, i)
            phr, i =         get_section(line, line_n, line_len, i)

            i = skip_space(line, line_len, i)
            i = expect_char(line, line_n, i, '=')
            i = skip_space(line, line_len, i)

            glyphs = line[i:].strip().split()

            if not glyphs: 
                print(f"Error(ln {line_n}): Expected glyph(s), got end of line")
                sys.exit(1)
    
            # print(f"{rut}|{man}|{lar}|{lab}|{cor}|{dor}|{phr} = {glyphs}")

            if len(rut) != 3: print(f"Error(ln {line_n}): root node must be length 3");       sys.exit(1)
            if len(man) != 9: print(f"Error(ln {line_n}): manner node must be length 9");     sys.exit(1)
            if len(lar) != 3: print(f"Error(ln {line_n}): laryngeal node must be length 3");  sys.exit(1)

            if len(lab) != 2: print(f"Error(ln {line_n}): labial node must be length 2");     sys.exit(1)
            if len(cor) != 2: print(f"Error(ln {line_n}): coronal node must be length 2");    sys.exit(1)
            if len(dor) != 6: print(f"Error(ln {line_n}): dorsal node must be length 6");     sys.exit(1)
            if len(phr) != 2: print(f"Error(ln {line_n}): pharyngeal node must be length 2"); sys.exit(1)

            sr = SegmentRepr(
                glyphs=glyphs, 
                rut=rut, man=man, lar=lar, 
                lab=lab, cor=cor, dor=dor, phr=phr
            )

            seg_reps.append(sr)

    return seg_reps


def check_dups(reprs: list[SegmentRepr]):
    seen = set(); duplicates = []

    for i, r in enumerate(reprs):
        if r in seen:
            duplicates.append(f"{i+1}: {r}")
        else:
            seen.add(r)

    if len(duplicates):
        print("Error: Duplicates found\n"); [print(dup) for dup in duplicates]
        sys.exit(1)
    

def main():
    if len(sys.argv) <= 1:
        print("No file provided.")
        sys.exit(1)
    
    file_name = sys.argv[1]
    file_path = os.path.join(os.path.dirname(__file__), file_name)
    
    reprs = read_file(file_path)

    check_dups(reprs)
    
    lst = []

    for r in reprs:
        clss, place = r.get_vals()
        if place == 0: place = "null"
        for gl in r.glyphs:
            lst.append(f'"{gl}": {{"class": {clss}, "place": {place}}}')

    
    res = ", ".join(lst)
    res = f"{{{res}}}"

    with open('cardinals.json', 'w', encoding='utf8') as file:
        file.write(res)
    
    print("Done")

if __name__ == "__main__":
    main()