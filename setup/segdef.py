#!/usr/bin/env python
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

@dataclass
class SegmentRep: 
    glyphs: list[str]
    rut: str; man: str; lar: str
    lab: str; cor: str; dor: str; phr: str

def read_file(file_path: str):
    seg_reps: list[SegmentRep] = []
    with open(file_path, "r") as file:
        for line_num, line in enumerate(file):
            line = line.strip(); line_num += 1

            if not line or line[0] == '#': continue

            line_len = len(line)
            i = 0
            
            rut, i = get_section_advance(line, line_num, line_len, i)
            man, i = get_section_advance(line, line_num, line_len, i)
            lar, i = get_section_advance(line, line_num, line_len, i)
            
            lab, i = get_section_advance(line, line_num, line_len, i)
            cor, i = get_section_advance(line, line_num, line_len, i)
            dor, i = get_section_advance(line, line_num, line_len, i)
            phr, i =         get_section(line, line_num, line_len, i)

            i = skip_space(line, line_len, i)
            i = expect_char(line, line_num, i, '=')
            i = skip_space(line, line_len, i)

            glyphs = line[i:].strip().split()

            if not glyphs: 
                print(f"Error(ln {line_num}): Expected glyph(s), got end of line")
                sys.exit(1)
    
            # print(f"{rut}|{man}|{lar}|{lab}|{cor}|{dor}|{phr} = {glyphs}")

            sr = SegmentRep(
                glyphs=glyphs, 
                rut=rut, man=man, lar=lar, 
                lab=lab, cor=cor, dor=dor, phr=phr
            )

            seg_reps.append(sr)

    return seg_reps

def main():
    if len(sys.argv) <= 1:
        print("No file provided.")
        sys.exit(1)
    
    file_name = sys.argv[1]
    file_path = os.path.join(os.path.dirname(__file__), file_name)
    
    reprs = read_file(file_path)

    print("TODO")
    # print(reprs)




if __name__ == "__main__":
    main()