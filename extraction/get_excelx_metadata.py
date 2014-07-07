import xlrd
import itertools
import os
import sys
import struct
import csv

def combine_paths(directory, files):
    return (os.path.join(directory, filename) for filename in files)

def get_excel_for_district(district_path):
    files = os.walk(district_path)
    files_per_directory = [combine_paths(walk[0],walk[2]) for walk in files]
    all_files = list(itertools.chain(*files_per_directory))
    return (f for f in all_files if f.endswith('xlsx'))

def get_districts(root_path):
    """Start from the directory containing all the districts. A district is assumed to be any
    directory in root_path."""
    return (os.path.join(root_path,directory) for directory in os.listdir(root_path) if os.path.isdir(os.path.join(root_path,directory)))

def get_districts_with_files(root_path):
    return ((district, get_excel_for_district(district)) for district in get_districts(root_path))

def get_excel_metadata(filename):
    try :
        book = xlrd.open_workbook(filename , on_demand = True )
    except :
        return ((filename.replace("\\", "/")) , "error opening file" )
    try :
        metadata = ((filename.replace("\\", "/")),)+(book.props["creator"]  , book.props["last_modified_by"] , book.props["created"] ,  book.props["modified"])
    except :
        metadata = ((filename.replace("\\", "/")) , "file has no props")
    return metadata

def full_function(root_path) :
    for district, files in get_districts_with_files(root_path) :
        for filename in files :
            yield get_excel_metadata(filename)

with open('J:\\Project\\abce\\ken\\HMIS\\data\\ExcelxMetadata.csv', 'wb') as output :
    writer = csv.writer(output , quoting = csv.QUOTE_MINIMAL)
    writer.writerow(['Path' , 'Author' , 'Saver' , 'DateCreated' , 'DateSaved'])
    for results in full_function('J:\LIMITED_USE\PROJECT_FOLDERS\KEN\ART_ABCE\HMIS\Districts') :
        writer.writerow(results)
