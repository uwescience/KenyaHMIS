import csv
import xlrd

def Get_705A_files(FilesScreen , ReportName):
    FilesScreened = csv.DictReader(open(FilesScreen))
    paths = []
    for row in FilesScreened :
        if row['Status'] not in ['Nothing Ok' , 'Date and Name not Ok'] and row['ReportType'] == ReportName and row['ReportTested'] != '705B - Outpatient Summary >5':
            paths.append(row['Path'])
    return paths

def lookup(section , i , j , merge):
    merge_cell = [(c[0] , c[2]) for c in merge
                  if i >= c[0] and i < c[1] and j >= c[2] and j < c[3] ]
    assert len(merge_cell) <= 1
    if not merge_cell :
        return section.cell_value(i , j)
    return section.cell_value(*merge_cell[0])

def import705A(FilePath, writer):
    book = xlrd.open_workbook(FilePath , formatting_info  = True )
    assert book.sheet_names() == ['MOH 705A']
    sheet = book.sheet_by_name('MOH 705A')
    START_ROW = 6
    assert sheet.cell_value(START_ROW,0) == 'DISEASES'
    for row in xrange(START_ROW+1, sheet.nrows):
        for col in xrange(2, 2+12):
            indic = lookup(sheet, row, 1, sheet.merged_cells)
            month = lookup(sheet, START_ROW, col, sheet.merged_cells)
            value = lookup(sheet, row, col, sheet.merged_cells)
            if (sheet.cell_type(row, col) == xlrd.XL_CELL_NUMBER or
                    sheet.cell_type(row, col) == xlrd.XL_CELL_BLANK):
                t = [FilePath, indic, month, value ]
            elif sheet.cell_type(row,col) == xlrd.XL_CELL_TEXT and len(value.strip()) == 0:
                t = [FilePath, indic, month, value.strip() ]
            else :
                continue            
            writer.writerow(t)

with open('J:\\Project\\abce\\ken\\HMIS\\data\\705AData.csv', 'wb') as output:
    writer = csv.writer(output , delimiter = "\t")
    writer.writerow(['Path' , 'Indicator' , 'Month' , 'Value'])
    cntok = 0
    cntnonok = 0
    for filename in sorted(Get_705A_files('J:\\Project\\abce\\ken\\HMIS\\data\\ReportScreen.csv' , '705A - Outpatient Summary <5')):
        try:
            import705A(filename, writer)
            cntok += 1
        except Exception as e:
            print "skipping {} because {}".format(filename, e)
            cntnonok += 1
        print '705a ok' , cntok , '705a non ok' , cntnonok
