import csv
import xlrd

def Get_710_files(FilesScreen , ReportName):
    FilesScreened = csv.DictReader(open(FilesScreen))
    paths = []
    for row in FilesScreened :
        if row['Status'] not in ['Nothing Ok' , 'Date and Name not Ok'] and row['ReportType'] == ReportName :
            paths.append(row['Path'])
    return paths


def lookup(section , i , j , merge):
    merge_cell = [(c[0] , c[2]) for c in merge
                  if i >= c[0] and i < c[1] and j >= c[2] and j < c[3] ]
    assert len(merge_cell) <= 1
    if not merge_cell :
        return section.cell_value(i , j)
    return section.cell_value(*merge_cell[0])

def import710(FilePath, writer):
    book = xlrd.open_workbook(FilePath , formatting_info  = True )
    assert book.sheet_names() == ['MOH 710 Section A', 'MOH 710 Section B']
    sectionA = book.sheet_by_name('MOH 710 Section A')
    A_START_ROW = 7
    assert sectionA.cell_value(A_START_ROW,0) == 'SECTION A'
    for row in xrange(A_START_ROW+1, sectionA.nrows):
        for col in xrange(2, 2+12):
            indic1 = lookup(sectionA, row, 0, sectionA.merged_cells)
            indic2 = lookup(sectionA, row, 1, sectionA.merged_cells)
            month = lookup(sectionA, A_START_ROW, col, sectionA.merged_cells)
            value = lookup(sectionA, row, col, sectionA.merged_cells)
            if (sectionA.cell_type(row, col) == xlrd.XL_CELL_NUMBER or
                    sectionA.cell_type(row, col) == xlrd.XL_CELL_BLANK):
                t = [FilePath, 'A' , indic1, indic2 ,  month, value ]
            elif sectionA.cell_type(row,col) == xlrd.XL_CELL_TEXT and len(value.strip()) == 0:
                t = [FilePath, 'A' , indic1, indic2 ,  month, value.strip() ]
            else :
                continue
            writer.writerow(t)
 
    sectionB = book.sheet_by_name('MOH 710 Section B')
    assert sectionB.cell_value(1,0) == 'BCG'
    B_START_ROW = 0
    for row in xrange(B_START_ROW+1, sectionB.nrows):
        for col in xrange(2, 2+12):
            indic1 = lookup(sectionB, row, 0, sectionB.merged_cells)
            indic2 = lookup(sectionB, row, 1, sectionB.merged_cells)
            month = lookup(sectionB, B_START_ROW, col, sectionB.merged_cells)
            value = lookup(sectionB, row, col, sectionB.merged_cells)
            if (sectionB.cell_type(row, col) == xlrd.XL_CELL_NUMBER or
                    sectionB.cell_type(row, col) == xlrd.XL_CELL_BLANK):
                t = [FilePath, 'B' , indic1 , indic2, month, value ]
            elif sectionB.cell_type(row,col) == xlrd.XL_CELL_TEXT and len(value.strip()) == 0:
                t = [FilePath, 'B' , indic1 , indic2, month, value.strip() ]
            else :
                continue
            writer.writerow(t)

with open('J:\\Project\\abce\\ken\\HMIS\\data\\710Data.csv', 'wb') as output:
    writer = csv.writer(output , delimiter = '\t')
    writer.writerow(['Path' , 'Sheet' , 'Indicator1' ,  'Indicator2' , 'Month' , 'Value'])
    cntok = 0
    cntnonok = 0
    for filename in sorted(Get_710_files('J:\\Project\\abce\\ken\\HMIS\\data\\ReportScreen.csv' , '710 - Immunization Summary')):
        try:
            import710(filename, writer)
            cntok += 1
        except Exception as e:
            print "skipping {} because {}".format(filename, e)
            cntnonok += 1
        print '710 ok' , cntok , '710 non ok' , cntnonok
