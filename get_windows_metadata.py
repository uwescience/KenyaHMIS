import itertools
import os
import sys
import struct
import csv
import OleFileIO_PL
import re

def combine_paths(directory, files):
    return (os.path.join(directory, filename) for filename in files)

def Get_Year_of_report(FilePath):
    if FilePath.find('2008') != -1 or FilePath.find(' 08 ') != -1:
        Year = '2008'
    elif FilePath.find('2009')  != -1 or FilePath.find(' 09 ')  != -1:
        Year = '2009'
    elif FilePath.find('2010')  != -1 or FilePath.find(' 10 ')  != -1:
        Year = '2010'
    elif FilePath.find('2011')  != -1 or FilePath.find(' 11 ')  != -1:
        Year = '2011'
    else :
        Year = 'No year'
    return (Year ,)

def get_excel_for_district(district_path):
    files = os.walk(district_path)
    files_per_directory = [combine_paths(walk[0],walk[2]) for walk in files]
    all_files = list(itertools.chain(*files_per_directory))
    return (f for f in all_files if f.endswith('xls') or f.endswith('xlsx'))

def get_districts(root_path):
    """Start from the directory containing all the districts. A district is assumed to be any
    directory in root_path."""
    return (os.path.join(root_path,directory) for directory in os.listdir(root_path) if os.path.isdir(os.path.join(root_path,directory)))

def get_district_name(filename):
    DistrictName = re.split('/', filename.replace("\\", "/"))[7]
    return (DistrictName ,)

def get_districts_with_files(root_path):
    return ((district, get_excel_for_district(district)) for district in get_districts(root_path))

def get_OLE_metadata(filename):
    try :
        ole = OleFileIO_PL.OleFileIO(filename)
        meta = ole.get_metadata()
        metadata = ((filename.replace("\\", "/") ,  meta.author , meta.last_saved_by , meta.create_time , meta.last_saved_time))
    except :
        metadata = ((filename.replace("\\", "/"), "Not working" , "Not working"   , "Not working"   , "Not working"))
    return metadata

def identify_report_type(filename):
    ReportType = []
    if filename.find('105') != -1:
        ReportType = ('105 - Service Delivery Summary' , )
    elif filename.find('705A') != -1:
        ReportType = ('705A - Outpatient Summary <5' , )
    elif filename.find('705B')  != -1:
        ReportType = ('705B - Outpatient Summary >5' , )
    elif filename.find('710') != -1:
        ReportType = ('710 - Immunization Summary' , )
    elif filename.find('711B') != -1:
        ReportType = ('711B - RH, TB, Malaria, HIV & Chanis Summary' , )
    elif filename.find('717') != -1:
        ReportType = ('717 - Service Statistics' , )
    elif filename.find('718') != -1:
        ReportType = ('718 - Inpatient Mortality and Morbidity' , )
    else :
        ReportType = ('No Standard Report Type' , )
    return ReportType

def full_function(root_path) :
    for district, files in get_districts_with_files(root_path) :
        for filename in files :
            result = get_OLE_metadata(filename) + identify_report_type(filename) + Get_Year_of_report(filename) + get_district_name(filename)
            print result
            yield result

with open('WindowsMetadata.csv', 'wb') as output :
    writer = csv.writer(output , quoting = csv.QUOTE_MINIMAL)
    writer.writerow(['Path' , 'Author' , 'Modifier' , 'DateCreated' , 'DateModifier' , 'ReportType'])
    for results in full_function('J:\LIMITED_USE\PROJECT_FOLDERS\KEN\ART_ABCE\HMIS\Districts') :
        writer.writerow(results)
