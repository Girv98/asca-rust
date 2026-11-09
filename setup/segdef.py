#!/usr/bin/env python
import argparse
import itertools
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
        error(f"Error(ln {line_num}): expected '{char}' got '{line[i]}'")
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
            error("Error: if section contains '0', all must be '0'")

    return s.replace('+', '1').replace('-', '0')

def get_binary_str_not_null(s: str, node: str) -> str:
    val = get_binary_str(s)
    if val is None: 
        error(f"Error: {node} node can't be null")

    return val

@dataclass
class SegmentRepr: 
    glyphs: list[str]; line_num: int;
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

            glyph_comment = line[i:].split("#")
            glyphs = glyph_comment[0].split()

            if not glyphs: 
                print(f"Error(ln {line_n}): Expected glyph(s), got end of line")
                sys.exit(1)
    
            # print(f"{rut}|{man}|{lar}|{lab}|{cor}|{dor}|{phr} = {glyphs}")

            if len(rut) != 3: error(f"Error(ln {line_n}): root node must be length 3")
            if len(man) != 9: error(f"Error(ln {line_n}): manner node must be length 9")
            if len(lar) != 3: error(f"Error(ln {line_n}): laryngeal node must be length 3")

            if len(lab) != 2: error(f"Error(ln {line_n}): labial node must be length 2")
            if len(cor) != 2: error(f"Error(ln {line_n}): coronal node must be length 2")
            if len(dor) != 6: error(f"Error(ln {line_n}): dorsal node must be length 6")
            if len(phr) != 2: error(f"Error(ln {line_n}): pharyngeal node must be length 2")

            sr = SegmentRepr(
                glyphs=glyphs, line_num=line_n,
                rut=rut, man=man, lar=lar, 
                lab=lab, cor=cor, dor=dor, phr=phr
            )

            seg_reps.append(sr)

    return seg_reps


def check_dups(reprs: list[SegmentRepr]):
    seen = set(); duplicates = []

    for r in reprs:
        if r in seen:
            duplicates.append(f"{r.line_num}: {r}")
        else:
            seen.add(r)

    if len(duplicates):
        print("Error: Duplicates found\n"); [print(dup) for dup in duplicates]
        sys.exit(1)

def render(gl: str, line_num: int) -> str:
    res = []
    i = 0

    while i < len(gl):
        start = gl.find('{', i)
        if start == -1:
            res.append(gl[i:]); break
        
        res.append(gl[i:start])

        end = gl.find('}', start)
        if end == -1:
            error(f"Error(ln {line_num}): '{{' has no matching '}}'")

        hex_str = gl[start + 1 : end]

        if len(hex_str) == 2:
            hex_str = "3" + hex_str
        try:
            res.append(chr(int(hex_str, 16)))
        except ValueError:
            error(f"Error(ln {line_num}): '{hex_str}' is not a valid hexidecimal number")

        i = end + 1

    return "".join(res)

def evaluate(expr):
    if isinstance(expr, str): return [expr]
    
    if isinstance(expr, list):
        if not expr: return [""]
        
        match expr[0]:
            case 'SET':
                result = []
                for sub_expr in expr[1:]:
                    result.extend(evaluate(sub_expr))
                return result
            
            case 'SEQ':
                sub_evals = [evaluate(sub) for sub in expr[1:]]
                combos = list(itertools.product(*sub_evals))

                result = []
                for i in range(len(sub_evals)):
                    for combo in combos:
                        perm = list(itertools.permutations(combo))[i]
                        result.append("".join(perm))
                return result

            case _:
                sub_evals = [evaluate(sub) for sub in expr]
                prod = itertools.product(*sub_evals)
                return ["".join(combo) for combo in prod]


def parse_cluster(cluster:str, line_num: int) -> list[str | ...]:
    stack = [[]]

    for ch in cluster:
        match ch:
            case '[': 
                stack[-1].append(["SET"])
                stack.append(stack[-1][-1])
            case ']': 
                if stack[-1][0] != "SET":
                    error(f"Error(ln {line_num}): ASDASDs")
                stack.pop()
                if not stack:
                    error(f"Error(ln {line_num}): ASDASDs")
            case '<': 
                stack[-1].append(["SEQ"])
                stack.append(stack[-1][-1])
            case '>': 
                if stack[-1][0] != "SEQ":
                    error(f"Error(ln {line_num}): ASDASDs")
                stack.pop()
                if not stack:
                    error(f"Error(ln {line_num}): ASDASDs")
            case _:
                stack[-1].append(ch)
    if len(stack) > 1:
        error(f"Error(ln {line_num}): ASDADASDASSDs")

    return stack.pop()

def get_graphemes(cluster: str, line_num: int) -> list[str]:
    cluster = render(cluster, line_num)
    ast = parse_cluster(cluster, line_num)
    output = evaluate(ast)

    for i, r in enumerate(output):
        output[i] = r.replace("*", "")
    return output
    
def error(s: str):
    print(s, file=sys.stderr)
    sys.exit(1)

def main():
    parser = argparse.ArgumentParser(
        prog='segdef',
    )
    parser.add_argument('file')
    parser.add_argument(
        '-f', '--format', 
        action='store_true',
        help='formatted output instead of default compact output'
    ) 

    args = parser.parse_args()
    
    file_name = args.file
    file_path = os.path.join(os.path.dirname(__file__), file_name)
    
    reprs = read_file(file_path)

    check_dups(reprs)

    seg_lst = []
    grapheme_set  = set()

    for r in reprs:
        clss, place = r.get_vals()
        if place == 0: place = "null"
        for gl in r.glyphs:
            for grapheme in get_graphemes(gl, r.line_num):
                if grapheme in grapheme_set:
                    error(f"Error(ln {r.line_num}): grapheme {grapheme} is already defined") # TODO: where is it previously defined?
                grapheme_set.add(grapheme)
                if args.format:
                    seg_lst.append(f'"{grapheme}": {{"class": {clss}, "place": {place}}}')
                else:
                    seg_lst.append(f'"{grapheme}":{{"class":{clss},"place":{place}}}')

    print(f"{len(reprs)} unique segments parsed, {len(seg_lst)} total variants created", file=sys.stderr)

    sep =  ", \n    " if args.format else ","
    res = sep.join(seg_lst)

    if args.format:
        res = f"{{\n    {res}\n}}"
    else:
        res = f"{{{res}}}"

    with open('cardinals.json', 'w', encoding='utf8') as file:
        file.write(res)
    
    print("Done")

if __name__ == "__main__":
    main()