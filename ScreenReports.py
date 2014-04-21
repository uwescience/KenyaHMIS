import csv
import time
import datetime

# Get the Metadata Profile for the given report  
def Get_Report_Meta_Trace(TraceFile , ReportName):
    MetaTrace = csv.DictReader(open(TraceFile))
    for row in MetaTrace :
        if row["ReportType"] == ReportName:
            DateCrea = time.strptime(row['DateCreation'], "%m/%d/%Y %H:%M")
            Trace = {'Author':row['Author'] , 'DateCreated':datetime.datetime(*DateCrea[:5])}
    return Trace

# Classify paths according to name and metadata
# In current version, date and author are not matched in the Metadata Trace
# Ie for reports with >1 author or date considered, any combinaison of possible
# author or date is considered valid
def Get_Valid_Paths(ReportDescriptionFile , MetaTrace):
    try :
        DateCrea = time.strptime(ReportDescriptionFile['DateCreated'] , "%Y-%m-%d %H:%M:%S")
        DateCrea = datetime.datetime(*DateCrea[:5])
        if ReportDescriptionFile['Author'] in MetaTrace['Author'] :
            if  DateCrea == MetaTrace['DateCreated'] :
                if ReportDescriptionFile['ReportType'] == '710 - Immunization Summary':
                    PathOk = 'All OK'
                else :
                    PathOk = 'Name not Ok'
            else :
                if ReportDescriptionFile['ReportType'] == '710 - Immunization Summary':
                    PathOk = 'Date not Ok'
                else :
                    PathOk = 'Date and Name not Ok'
        else :
            if  DateCrea == MetaTrace['DateCreated'] :
                if ReportDescriptionFile['ReportType'] == '710 - Immunization Summary':
                    PathOk = 'Author not Ok'
                else :
                    PathOk = 'Author and Name not Ok'
            else :
                if ReportDescriptionFile['ReportType'] == '710 - Immunization Summary':
                    PathOk = 'Author and Date not Ok'
                else :
                    PathOk = 'Nothing Ok'
    except :
        PathOk  = 'Bad Something'
    ReportScreen = ((ReportDescriptionFile['Author']  , ReportDescriptionFile['DateCreated'], ReportDescriptionFile['Path'] , PathOk))
    return ReportScreen

def main(TraceFile , ReportDescriptionFile , ReportType):
    MetaTrace = Get_Report_Meta_Trace(TraceFile , ReportType)
    WindowsMeta = csv.DictReader(open(ReportDescriptionFile))
    for row in WindowsMeta :
        result = Get_Valid_Paths(row,MetaTrace)
        yield result
        
with open('ReportScreen.csv', 'wb') as output :
    writer = csv.writer(output , quoting = csv.QUOTE_MINIMAL)
    writer.writerow(['Author' , 'DateCreated', 'Path' , 'Status'])
    for results in main('C:\Users\grlurton\Documents\KenyaHMIS\MetadataTrace.csv','C:\Users\grlurton\Documents\KenyaHMIS\WindowsMetadata.csv','710 - Immunization Summary') :
        writer.writerow(results)
