DistHosp <- grep(pattern = ' DISTRICT HOSPITAL' , facilities$F_NAME)
SubDistHosp <- grep(pattern = 'SUB-DISTRICT HOSP' , facilities$F_NAME)
MedCentre <- grep(pattern = 'MEDICAL SERVICES|MEDICARE|MEDICAL CENTRE|HEALTH SERVICE|MED CENTRE', 
                  facilities$F_NAME)
NursingHome <- grep(pattern = 'NURSING |M N H|MNH|M&N| MAT' , facilities$F_NAME)
HealthCentre <- grep(pattern = 'HEALTH CENTRE| H C|HC|HEALTH CARE CENTRE', facilities$F_NAME)
Dispensary <- grep(pattern = 'DISP' , facilities$F_NAME)
MissionHC <- grep(pattern = 'MISS H|MISSION HC' , facilities$F_NAME)
MissionHosp <- grep(pattern = 'MISSION HOSPITAL|MISS. HOSPITAL|CATHOLIC HOSPITAL' , facilities$F_NAME)
Hosp <- grep(pattern = 'HOSP' , facilities$F_NAME)
Clinic <- grep(pattern = 'CLINIC' , facilities$F_NAME)



facilities$Level <- ''
facilities$Level[Hosp] <- 'Hospital'
facilities$Level[HealthCentre] <- 'Health Center'
facilities$Level[MedCentre] <- 'Medical Center'
facilities$Level[Clinic] <- 'Clinic'
facilities$Level[Dispensary] <- 'Dispensary'
facilities$Level[NursingHome] <- 'Nursing Home'
facilities$Level[MissionHC] <- 'Mission Health Center'
facilities$Level[DistHosp] <- 'District Hospital'
facilities$Level[MissionHosp] <- 'Mission Hospital'
facilities$Level[SubDistHosp] <- 'Sub District Hospital'



table(facilities$Level)