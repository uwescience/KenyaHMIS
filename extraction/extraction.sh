#!/bin/sh

python c://users/grlurton/documents/kenyahmis/extraction
python get_excel_metadata.py
python get_excelx_metadata.py
python get_windows_metadata.py

Rscript windows_metadata_trace.R

python Parse105.py
python Parse705A.py
python Parse705B.py
python Parse710.py

Rscript map_indic_105.r
Rscript map_indic_705.r
Rscript map_indic_710.r

Rscript metadata_merge.r
       