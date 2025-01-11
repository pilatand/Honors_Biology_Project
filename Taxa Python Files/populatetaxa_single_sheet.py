##Written by Daniel Barich

import openpyxl
import re

def _populate_taxa(text:str, sheet, col):
    row = 2
    while 1:
        taxa = sheet.cell(row, 1).value
        #print(taxa)
        if taxa is None:
            if row > 50: break
            row += 1
            continue
        regex = f"^(\d+\.\d+).*? *{taxa}$"
        results = re.findall(regex, text, re.MULTILINE)
        if len(results) == 1:
            sheet.cell(row, col).value = results[0]
        row += 1

def populate_taxa(infilename:str, sheet, col):
    with open(infilename) as file:
        text = file.read()
    _populate_taxa(text, sheet, col)