import csv
import time
import datetime

# Get the Metadata Profile for the given report  
def Get_Report_Meta_Trace(TraceFile , ReportName):
    MetaTrace = csv.DictReader(open(TraceFile))
    for row in MetaTrace :
        if row['DateCreation'] != '':
            DateCrea = time.strptime(row['DateCreation'], "%Y-%m-%d %H:%M:%S")
            if row["ReportType"] == ReportName:
                Trace = {'Author':row['Author'] , 'DateCreated':datetime.datetime(*DateCrea[:5]) , 'ReportTest':ReportName}
        else :
            if row["ReportType"] == ReportName:
                Trace = {'Author':row['Author'] , 'DateCreated':'No date' , 'ReportTest':ReportName}
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
                if ReportDescriptionFile['ReportType'] == MetaTrace['ReportTest']:
                    PathOk = 'All OK'
                else :
                    PathOk = 'Name not Ok'
            else :
                if ReportDescriptionFile['ReportType'] == MetaTrace['ReportTest']:
                    PathOk = 'Date not Ok'
                else :
                    PathOk = 'Date and Name not Ok'
        else :
            if  DateCrea == MetaTrace['DateCreated'] :
                if ReportDescriptionFile['ReportType'] == MetaTrace['ReportTest']:
                    PathOk = 'Author not Ok'
                else :
                    PathOk = 'Author and Name not Ok'
            else :
                if ReportDescriptionFile['ReportType'] == MetaTrace['ReportTest']:
                    PathOk = 'Author and Date not Ok'
                else :
                    PathOk = 'Nothing Ok'
    except :
        PathOk  = 'Bad Something'
    ReportScreen = ((ReportDescriptionFile['Author']  , ReportDescriptionFile['DateCreated'],
                     ReportDescriptionFile['Path'] , PathOk , ReportDescriptionFile['ReportType'] , MetaTrace['ReportTest']))
    print PathOk
    return ReportScreen

def main(TraceFile , ReportDescriptionFile , ReportType):
    MetaTrace = Get_Report_Meta_Trace(TraceFile , ReportType)
    WindowsMeta = csv.DictReader(open(ReportDescriptionFile))
    for row in WindowsMeta :
        result = Get_Valid_Paths(row,MetaTrace)
        yield result

ReportTypes = ['105 - Service Delivery Summary' , '705A - Outpatient Summary <5' , '705B - Outpatient Summary >5' , '710 - Immunization Summary' ,
               '711B - RH, TB, Malaria, HIV & Chanis Summary' , '717 - Service Statistics' , '718 - Inpatient Mortality and Morbidity']

with open('J:\\Project\\abce\\ken\\HMIS\\data\\ReportScreen.csv', 'wb') as output :
    writer = csv.writer(output , quoting = csv.QUOTE_MINIMAL)
    writer.writerow(['Author' , 'DateCreated', 'Path' , 'Status','ReportType','ReportTested'])
    for report in ReportTypes:
        for results in main('J:\\Project\\abce\\ken\\HMIS\\data\\WindowsMetadataTrace.csv','J:\\Project\\abce\\ken\\HMIS\\data\\WindowsMetadata.csv',report) :
            writer.writerow(results)
