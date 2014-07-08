import csv
import xlrd

def Get_105_files(FilesScreen , ReportName):
    FilesScreened = csv.DictReader(open(FilesScreen))
    paths = []
    for row in FilesScreened :
        if row['Status'] not in ['Nothing Ok' , 'Date and Name not Ok'] and row['ReportType'] == ReportName and row['ReportTested'] != '105 - Service Delivery Summary':
            paths.append(row['Path'])
    return paths

def lookup(section , i , j , merge):
    merge_cell = [(c[0] , c[2]) for c in merge
                  if i >= c[0] and i < c[1] and j >= c[2] and j < c[3] ]
    assert len(merge_cell) <= 1
    if not merge_cell :
        return section.cell_value(i , j)
    return section.cell_value(*merge_cell[0])

def import105(FilePath, writer):
    book = xlrd.open_workbook(FilePath , formatting_info  = True )
    assert book.sheet_names() == ['Indicators Manual' , 'EligiPOPN_Baseline_Target' , 'July' , 'Aug' , 'Sep' , 'Oct' , 'Nov' , 'Dec' , 'Jan' , 'Feb' , 'March' , 'April' , 'May' , 'June' , 'Achievement']
    for month in book.sheet_names()[2:14]:
        sheet = book.sheet_by_name(month)
        print month
        A_START_ROW = 6
        assert sheet.cell_value(A_START_ROW,1) == 'Pregnancy, Delivery and the Newborn (upto 2 weeks)'
        for row in xrange(A_START_ROW+1, sheet.nrows):
            for col in xrange(2, 2+5):
                indic = lookup(sheet, row, 1, sheet.merged_cells)
                level = lookup(sheet, A_START_ROW-1, col, sheet.merged_cells)
                value = lookup(sheet, row, col, sheet.merged_cells)
                if (sheet.cell_type(row, col) == xlrd.XL_CELL_NUMBER or
                        sheet.cell_type(row, col) == xlrd.XL_CELL_BLANK):
                    t = [FilePath, month , indic, level, value ]
                elif sheet.cell_type(row,col) == xlrd.XL_CELL_TEXT and len(value.strip()) == 0:
                    t = [FilePath, month , indic, level, value.strip() ]
                else :
                    continue
                writer.writerow(t)

with open('J:\\Project\\abce\\ken\\HMIS\\data\\105AData.csv', 'wb') as output:
    writer = csv.writer(output , delimiter = "\t")
    cntok = 0
    cntnonok = 0
    for filename in sorted(Get_105_files('J:\\Project\\abce\\ken\\HMIS\\data\\ReportScreen.csv' , '105 - Service Delivery Summary')):
        try:
            import105(filename, writer)
            cntok += 1
        except Exception as e:
            print "skipping {} because {}".format(filename, e)
            cntnonok += 1
        print '105 ok' , cntok , '105 non ok' , cntnonok
