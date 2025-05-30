##Written by Daniel Barich
##Adopted for use by Andrew Pilat

import openpyxl
#import re
from glob import glob
from populatetaxa_single_sheet import populate_taxa

#regex = re.compile(r"_\w+\.")

def titleFromFilename(file:str):
    #global regex
    return file.split(sep="_")[-1].split(sep=".")[0]
    #return re.match(r"\W+", file)

tmplName = r"/Users/andrewpilat/Documents/Template_Standard_General.xlsx" ##Template File
outfile = r"/Users/andrewpilat/Documents/Honors/Kraken Spreadsheets/PFP_Antarctica_Single_Sheet.xlsx" ##Output spreadsheet

wb:openpyxl.Workbook = openpyxl.load_workbook(tmplName)
sheet = wb.active

outCol = 2
for file in sorted(glob(r"/Users/andrewpilat/Documents/Kraken_Braken/*.txt")):
    print(titleFromFilename(file))
    sheet.cell(1, outCol).value = titleFromFilename(file)
    populate_taxa(file,sheet, outCol)
    outCol += 1

wb.save(outfile)