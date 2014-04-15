import xlrd
import itertools
import os
import sys
import struct

def combine_paths(directory, files):
    return (os.path.join(directory, filename) for filename in files)

def get_excel_for_district(district_path):
    files = os.walk(district_path)
    files_per_directory = [combine_paths(walk[0],walk[2]) for walk in files]
    all_files = list(itertools.chain(*files_per_directory))
    return (f for f in all_files if f.endswith('xls') or f.endswith('xlsx'))

def get_districts(root_path):
    """Start from the directory containing all the districts. A district is assumed to be any
    directory in root_path."""
    return (os.path.join(root_path,directory) for directory in os.listdir(root_path) if os.path.isdir(os.path.join(root_path,directory)))

def get_districts_with_files(root_path):
    return ((district, get_excel_for_district(district)) for district in get_districts(root_path))

def get_excel_metadata(filename):
    # print "opening %s" % (filename,)
    try :
        book = xlrd.open_workbook(filename , on_demand = True )
    except :
        return (filename , "error opening file" )
    try :
        metadata = (filename,)+(book.props["created"] , book.props["creator"] , book.props["modified"] , book.props["last_modified_by"])
    except :
        metadata = (filename , "file has no props")
    return metadata

    

district_files = get_districts_with_files('J:\LIMITED_USE\PROJECT_FOLDERS\KEN\ART_ABCE\HMIS\Districts')
for district, files in district_files:
    for filename in files :
        print get_excel_metadata(filename)
