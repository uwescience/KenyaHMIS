import xlrd
import itertools
import os
import sys

def combine_paths(directory, files):
    return [os.path.join(directory, filename) for filename in files]

def get_excel_for_district(district_path):
    files = os.walk(district_path)
    files_per_directory = [combine_paths(walk[0],walk[2]) for walk in files]
    all_files = list(itertools.chain(*files_per_directory))
    return [f for f in all_files if f.endswith('xls') or f.endswith('xlsx')]

def get_districts(root_path):
    """Start from the directory containing all the districts. A district is assumed to be any
    directory in root_path."""
    return [directory for directory in os.listdir(root_path) if os.path.isdir(directory)]

def get_districts_with_files(root_path):
    ret = {}
    for district in get_districts(root_path):
        ret[district] = get_excel_for_district(district)
    return ret

def get_excel_metadata(filename):
    print "opening %s" % (filename,)
    book = xlrd.open_workbook(filename)
    metadata = (district,)+(book.props["created"] , book.props["creator"] , book.props["modified"] , book.props["last_modified_by"])
    return metadata

district_files = get_districts_with_files(r'C://Users//grlurton//Documents//KenyaHIS//python')
for district, files in district_files.items():
    if len(files) == 0:
        continue   
    print get_excel_metadata(files[0])

    
district_files = get_districts_with_files(r'J:\LIMITED_USE\PROJECT_FOLDERS\KEN\ART_ABCE\HMIS\Districts')
for district, files in district_files.items():
    if len(files) == 0:
        continue   
    print get_excel_metadata(files[0])

"""C:\Users\grlurton\Documents\Kenya HIS Data 2008-2011\Districts\Buret"""
"""J:\LIMITED_USE\PROJECT_FOLDERS\KEN\ART_ABCE\HMIS\Districts"""



