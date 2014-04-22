import csv
import time
import datetime
import xlrd


##Get all the 710 susceptible files from screening

def Get_710_files(FilesScreen , ReportName):
    FilesScreened = csv.DictReader(open(FilesScreen))
    paths = []
    for row in FilesScreened :
        if row['Status'] not in ['Nothing Ok' , 'Date and Name not Ok'] and row['ReportType'] == ReportName :
            paths.append(row['Path'])
    return (paths)

#print Get_710_files('C:\Users\grlurton\Documents\KenyaHMIS\ReportScreen.csv' , '710 - Immunization Summary')


##Test which have sheets with name compatible with actually being 710

list710 = Get_710_files('C:\Users\grlurton\Documents\KenyaHMIS\ReportScreen.csv' , '710 - Immunization Summary')

def getchecks(filename) :
    booknames = {}
    for filename in list710 :
        try :
            book = xlrd.open_workbook(filename)
            for sheet in book.sheet_names():
                booknames.update({'ExcelFile':filename , 'ExcelSheet':sheet})
            if 'MOH 710 Section A' in book.sheet_names():
                booknames.update({'RowsSectionA':nrows(book.sheet_by_name('MOH 710 Section A'))})
            if 'MOH 710 Section B' in book.sheet_names():
                booknames.update({'RowsSectionB':nrows(book.sheet_by_name('MOH 710 Section B'))})
        except :
            booknames.update({'ExcelFile':"not working"})




    ## Test sheet names and number of sheets

    ## For each sheet test number of rows and structure of the sheet

    ## For each sheet get test specific values in specific cells



    
##Import
    ## Get the list of merged values
    ## Get the list of error cells
    ##first column, reproducing merged values in merged cells
    ##Second column
        ## reproduce these columns 12 times
    ##columns 3 to 14 and stack them next to previous 

def lookup(section , i , j , merge):
    merge_cell = [(c[0] , c[2]) for c in merge
                  if i >= c[0] and i < c[1] and j >= c[2] and j < c[3] ]
    assert len(merge_cell) <= 1
    if not merge_cell :
        return section.cell_value(i , j)
    return section.cell_value(*merge_cell[0])




#sectionA = book.sheet_by_name('MOH 710 Section A')
#sectionB = book.sheet_by_name('MOH 710 Section B')

#print sectionA.cell_type(40,12)
#       for colx in xrange(clo, chi):


#cell = sectionA.cell(0,0)
#print cell.dump
#print sectionA.row(0)
#print sectionA.col(0)
#print sectionA.nrows
