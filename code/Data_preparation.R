# Data preparation
rm(list=ls())

library(openxlsx)
library(plyr)
# library(dplyr) DO OT DPLYR AND PLYR --> DOESN'T WORK


# set the path to the files
dir = "C:/Users/hezo/Dropbox/PhD/Stroke/PowerAnalyse_GewichteteMittelwerte_MixedModelSD_und_PredictionModel/"
# dir = "/Users/janne.hamann/Dropbox/StatistikOtherSideProjekt/"

setwd(dir)


##########################
#### Get patient data ####
##########################

# read in the patient data
dat0 = read.xlsx("data/Clean_Olea_Analyzed_Data_20190304.xlsx", sheet=1, na.strings="NA")

# rename the columns
colnames(dat0)
dat0 = rename(dat0, c("PID"="p_id",
                    "Alter.(Jahre)"="age",
                    "Sex.0=Mann.1=Frau" = "sex", 
                    "Selbständig.vor.Stroke:.mRS.0-2:.0=nein.1=ja" = "independent_pre_stroke",
                    "NIHSS.bei.Eintritt" = "nihss_bl",
                    "NIHSS.nach.24.h" = "nihss_24h",
                    "Delta.NIHSS.nach.24h.-.bei.Eintritt" = "nihss_diff_bl_24h",
                    "NIHSS.nach.3.Monaten" = "nihss_3months",
                    "Delta.NIHSS.nach.3.Monaten.-.bei.Eintritt" = "nihss_diff_bl_3months",
                    "mRS.nach.3.Monaten" = "mrs_3months",
                    "Tod.nach.3.Monaten.0=nein.1=ja" = "death_after_3months",
                    "Symptomat.Intacranielle.Blutung.(SITS-MOST).0=nein.1=ja" = "intracranial_bleeding",
                    "TAH.(ASS.oder.Clopidogrel.oder.Ticagrelor).pre-stroke.0=nein.1=ja" = "tah_pre_stroke",
                    "Orale.Antikoagulation.pre-stroke.0=nein.1=ja" = "antikoagulation_pre_stroke",
                    # Wirkstoff in Medikamenten zum Blutfettwerte zu senken
                    "Statin.pre-stroke.0=nein.1=ja" = "statin_pre_stroke",
                    # Medikamente gegen Bluthochdruck
                    "Antihypertensiva.pre-stroke.0=nein.1=ja" = "antihypertensiva_pre_stroke",
                    "systolischer.Blutdruck.bei.Eintritt.[mmHg]" = "sys_bloodpressure_bl",
                    "diastolischer.Blutdruck.bei.Eintritt.[mmHg]" = "dias_bloodpressure_bl",                                                                                                                         
                    "Glucose.bei.Eintritt.[mmol/l]" = "glucose_bl",                                                                                                                                         
                    "HbA1c.bei.Eintritt.[%]" = "hba1c",                                                                                                                                         
                    "LDL-Cholesterin.bei.Eintritt.[mmol/l]" = "ldl",                                                                                                                               
                    "HDL-Cholesterin.bei.Eintritt.[mmol/l]" = "hdl",                                                                                                                                 
                    "Triglyceride.bei.Eintritt.[mmol/l]" = "triglyceride",                                                                                                                                   
                    "CRP.bei.Eintritt.[mg/l]" = "crp",                                                                                                                                               
                    "INR.bei.Eintritt" = "inr",                                                                                                                                             
                    "Anästhesie.0=Sedation/Keine.1=Vollnarkose" = "anaesthesia",                                                                                                                             
                    "Hyperdenses.Media-Zeichen.0=nein.1=ja" = "dense_media",                                                                                                                              
                    "Infarktfrühzeichen.0=nein.1=ja" = "early_infarct_sign",
                    "TOAST.1=Makroangiopathie.2=kardioembolisch.3=Mikroangiopathie.4=Andere.5=ungeklärt.(Abklärung.komplett).6=ungeklärt.(Abklärung.inkomplett).7=Mehrere.mögliche.Ursachen" = "toast",
                    "bekannte.RF:.Vorhofflimmern.0=nein.1=ja" = "atrial_fibrillation",
                    "bekannte.RF:.Diabetes.mellitus.0=nein.1=ja" = "rf_diabetes",
                    # Bluthochdruck
                    "bekannte.RF:.Hypertonie.0=nein.1=ja" = "rf_hypertonia",
                    # zu hohe Blutfettwerte
                    "bekannte.RF:.Hypercholesterinämie.(Gesamtcholesterin.>5.0.mmol/l.oder.behandelt.mit.Statin).0=nein.1=ja" = "rf_hypercholesterinaemie",
                    "bekannte.RF:.Rauchen.(aktuell).0=nein.1=ja" = "rf_smoker",
                    "bekannte.RF:.Koronare.Herzkrankheit.0=nein.1=ja" = "rf_chd",
                    "bekannte.RF:.pAVK.0=nein.1=ja" = "rf_pavk",
                    "bekannte.RF:.früherer.Stroke.OR.TIA.0=nein.1=ja" = "rf_tia_stroke",
                    "M1.Verschluss.Seite.1=links.2=rechts" = "infarct_side",
                    # weitere Verschlüsse
                    "Weitere.Verschlüsse.(Ort.2).0=nein.1=ja" = "additional_occlusion",
                    # Ort2.Seite.1=links.2=rechts.3=bds.4=unklar
                    "Ort2.Seite.1=links.2=rechts.3=bds.4=unklar" = "additional_occlusion_side",
                    "Ort.2.ICA.extracranial.0=nein.1=ja" = "additional_occlusion_ica_excranial",
                    "Ort.2.ICA.intracranial.0=nein.1=ja" = "additional_occlusion_ica_intracranial",
                    "Ort.2.ICA.T.0=nein.1=ja" = "additional_occlusion_ica_t",
                    "Ort.2.MCA.M2.0=nein.1=ja" = "additional_occlusion_mca_m2",
                    "Ort.2.MCA.M3.or.M4.0=nein.1=ja" = "additional_occlusion_mca_m2_m3",
                    "Kollateralisierung.0=schlecht/keine.1=mittel/mässig.2=gut/sehr.gut" = "collateralization",
                    # Lyse = Medikament das gegeben wird
                    "Lyse.0=nein.1=ja"  = "lyse",
                    "Konventionelle.Angiographie.0=nein.1=ja" = "angio",
                    # Thormbectomy = Behandlung
                    "Thrombektomie.(mechanisch).0=nein.1=ja" = "thrombectomy",
                    # Onset to imaging (CT/MRI) time (min.)
                    # time between onset and imaging
                    "Onset.to.imaging.(CT/MRI).time.(min.)" = "time_to_imaging",
                    "Onset.to.needle.time.(min.)" = "time_to_needle",
                    "Onset.to.groin.puncture.(min.)" = "time_to_groin_puncture",
                    # Gefäss vor Therapie wieder offen
                    "Gefäss.bei.Beginn.Angio.wieder.offen?.0=nein.1=ja" = "vessel_open_before_therapy",
                    # Perfusion nach Therapie --> wie erfolgreich war die Therapie?
                    "Recanalization.TICI.AFTER.IAT.0=.Grade.0,.1=.Grade.1.,.2=.Grade.2a,.3=.Grade.2b,.4=.Grade.3" = "tici",
                    "Recanalization.TICI.AFTER.IAT.dichotomisiert.0=schlecht.(mTICI0-2a).1=gut.(mTICI.2b-3)" = "tici_binary"))
names(dat0)
head(dat0)
tail(dat0)

dat0 = dat0[1:222,]

# too many missings
dat0 = dat0[,!colnames(dat0) %in% c("dense_media","early_infarct_sign")]

write.table(dat0, file = "data/Clean_Olea_Analyzed_Data_20190304_LH.csv", sep = ",")



#######################
#### Read MRI data ####
#######################

#### Test how i can best read in the data

# patient
pat = read.csv("data/linksseitiger Schlaganfall/0000653853_20161216_4193466.csv", row.names = NULL, header=F)
pat = pat[,1:7]

# find the measures of interest
# start point 4 regions + 8 regions (5-12) a 10 slices = 4+8*10 = 84 
# 4:(4+84-1) includes row 4:87 = 84 elements since 4 and 87 are included

# save the patient id and the columnnames for the measures
(p_id = pat$V1[2])

k = which(pat$V1=="rBF")[1]
rbf = pat[(k+2):(k+2+83),]
k_adc = which(pat$V1=="ADC")[2]
print(paste0("This row contains ADC Volume 1: ", pat$V1[k_adc+2] == "Volume 1"))
k_tmax = which(pat$V1=="TMAX")[2]
print(paste0("This row contains TMAX Volume 1: ", pat$V1[k_tmax+2] == "Volume 2"))
k_rbf = which(pat$V1=="rBF")[2]
print(paste0("This row contains rBF-ADC Volume 1: ", pat$V1[k_rbf+2] == "Volume 1"))
print(paste0("This row contains rBF-ADC Volume 2: ", pat$V1[k_rbf+3] == "Volume 2"))
k_rbv = which(pat$V1=="rBV")[2]
print(paste0("This row contains rBV-ADC Volume 1: ", pat$V1[k_rbv+2] == "Volume 1"))
print(paste0("This row contains rBV-ADC Volume 2: ", pat$V1[k_rbv+3] == "Volume 2"))
rbf$volume_adc = pat[k_adc+2,"V7"]
rbf$volume_tmax = pat[k_tmax+2,"V7"]
rbf$volume_rbf_adc = pat[k_rbf+2,"V2"]
rbf$volume_rbf_tmax = pat[k_rbf+3,"V2"]
rbf$volume_rbv_adc = pat[k_rbv+2,"V2"]
rbf$volume_rbv_tmax = pat[k_rbv+3,"V2"]
rbf$measure = "rBF"

k = which(pat$V1=="rBV")[1]
rbv = pat[(k+2):(k+2+83),]
k_adc = which(pat$V1=="ADC")[2]
print(paste0("This row contains ADC Volume 1: ", pat$V1[k_adc+2] == "Volume 1"))
k_tmax = which(pat$V1=="TMAX")[2]
print(paste0("This row contains TMAX Volume 2: ", pat$V1[k_tmax+2] == "Volume 2"))
rbv$volume_adc = pat[k_adc+2,"V7"]
rbv$volume_tmax = pat[k_tmax+2,"V7"]
k_rbf = which(pat$V1=="rBF")[2]
print(paste0("This row contains rBF-ADC Volume 1: ", pat$V1[k_rbf+2] == "Volume 1"))
print(paste0("This row contains rBF-ADC Volume 2: ", pat$V1[k_rbf+3] == "Volume 2"))
k_rbv = which(pat$V1=="rBV")[2]
print(paste0("This row contains rBV-ADC Volume 1: ", pat$V1[k_rbv+2] == "Volume 1"))
print(paste0("This row contains rBV-ADC Volume 2: ", pat$V1[k_rbv+3] == "Volume 2"))
rbv$volume_rbf_adc = pat[k_rbf+2,"V2"]
rbv$volume_rbf_tmax = pat[k_rbf+3,"V2"]
rbv$volume_rbv_adc = pat[k_rbv+2,"V2"]
rbv$volume_rbv_tmax = pat[k_rbv+3,"V2"]
rbv$measure = "rBV"

k = which(pat$V1=="TTP")[1]
ttp = pat[(k+2):(k+2+83),]
k_adc = which(pat$V1=="ADC")[2]
print(paste0("This row contains ADC Volume 1: ", pat$V1[k_adc+2] == "Volume 1"))
k_tmax = which(pat$V1=="TMAX")[2]
print(paste0("This row contains TMAX Volume 2: ", pat$V1[k_tmax+2] == "Volume 2"))
ttp$volume_adc = pat[k_adc+2,"V7"]
ttp$volume_tmax = pat[k_tmax+2,"V7"]
k_rbf = which(pat$V1=="rBF")[2]
print(paste0("This row contains rBF-ADC Volume 1: ", pat$V1[k_rbf+2] == "Volume 1"))
print(paste0("This row contains rBF-ADC Volume 2: ", pat$V1[k_rbf+3] == "Volume 2"))
k_rbv = which(pat$V1=="rBV")[2]
print(paste0("This row contains rBV-ADC Volume 1: ", pat$V1[k_rbv+2] == "Volume 1"))
print(paste0("This row contains rBV-ADC Volume 2: ", pat$V1[k_rbv+3] == "Volume 2"))
ttp$volume_rbf_adc = pat[k_rbf+2,"V2"]
ttp$volume_rbf_tmax = pat[k_rbf+3,"V2"]
ttp$volume_rbv_adc = pat[k_rbv+2,"V2"]
ttp$volume_rbv_tmax = pat[k_rbv+3,"V2"]
ttp$measure = "TTP"

k = which(pat$V1=="TMAX")[1]
tmax = pat[(k+2):(k+2+83),]
k_adc = which(pat$V1=="ADC")[2]
print(paste0("This row contains ADC Volume 1: ", pat$V1[k_adc+2] == "Volume 1"))
k_tmax = which(pat$V1=="TMAX")[2]
print(paste0("This row contains TMAX Volume 1: ", pat$V1[k_tmax+2] == "Volume 2"))
k_rbf = which(pat$V1=="rBF")[2]
print(paste0("This row contains rBF-ADC Volume 1: ", pat$V1[k_rbf+2] == "Volume 1"))
print(paste0("This row contains rBF-ADC Volume 2: ", pat$V1[k_rbf+3] == "Volume 2"))
k_rbv = which(pat$V1=="rBV")[2]
print(paste0("This row contains rBV-ADC Volume 1: ", pat$V1[k_rbv+2] == "Volume 1"))
print(paste0("This row contains rBV-ADC Volume 2: ", pat$V1[k_rbv+3] == "Volume 2"))
tmax$volume_adc = pat[k_adc+2,"V7"]
tmax$volume_tmax = pat[k_tmax+2,"V7"]
tmax$volume_rbf_adc = pat[k_rbf+2,"V2"]
tmax$volume_rbf_tmax = pat[k_rbf+3,"V2"]
tmax$volume_rbv_adc = pat[k_rbv+2,"V2"]
tmax$volume_rbv_tmax = pat[k_rbv+3,"V2"]
tmax$measure = "TMAX"

k = which(pat$V1=="MTT")[1]
mtt = pat[(k+2):(k+2+83),]
k_adc = which(pat$V1=="ADC")[2]
print(paste0("This row contains ADC Volume 1: ", pat$V1[k_adc+2] == "Volume 1"))
k_tmax = which(pat$V1=="TMAX")[2]
print(paste0("This row contains TMAX Volume 1: ", pat$V1[k_tmax+2] == "Volume 2"))
k_rbf = which(pat$V1=="rBF")[2]
print(paste0("This row contains rBF-ADC Volume 1: ", pat$V1[k_rbf+2] == "Volume 1"))
print(paste0("This row contains rBF-ADC Volume 2: ", pat$V1[k_rbf+3] == "Volume 2"))
k_rbv = which(pat$V1=="rBV")[2]
print(paste0("This row contains rBV-ADC Volume 1: ", pat$V1[k_rbv+2] == "Volume 1"))
print(paste0("This row contains rBV-ADC Volume 2: ", pat$V1[k_rbv+3] == "Volume 2"))
mtt$volume_adc = pat[k_adc+2,"V7"]
mtt$volume_tmax = pat[k_tmax+2,"V7"]
mtt$volume_rbf_adc = pat[k_rbf+2,"V2"]
mtt$volume_rbf_tmax = pat[k_rbf+3,"V2"]
mtt$volume_rbv_adc = pat[k_rbv+2,"V2"]
mtt$volume_rbv_tmax = pat[k_rbv+3,"V2"]
mtt$measure = "MTT"

k = which(pat$V1=="tMIP")[1]
tmip = pat[(k+2):(k+2+83),]
k_adc = which(pat$V1=="ADC")[2]
print(paste0("This row contains ADC Volume 1: ", pat$V1[k_adc+2] == "Volume 1"))
k_tmax = which(pat$V1=="TMAX")[2]
print(paste0("This row contains TMAX Volume 1: ", pat$V1[k_tmax+2] == "Volume 2"))
k_rbf = which(pat$V1=="rBF")[2]
print(paste0("This row contains rBF-ADC Volume 1: ", pat$V1[k_rbf+2] == "Volume 1"))
print(paste0("This row contains rBF-ADC Volume 2: ", pat$V1[k_rbf+3] == "Volume 2"))
k_rbv = which(pat$V1=="rBV")[2]
print(paste0("This row contains rBV-ADC Volume 1: ", pat$V1[k_rbv+2] == "Volume 1"))
print(paste0("This row contains rBV-ADC Volume 2: ", pat$V1[k_rbv+3] == "Volume 2"))
tmip$volume_adc = pat[k_adc+2,"V7"]
tmip$volume_tmax = pat[k_tmax+2,"V7"]
tmip$volume_rbf_adc = pat[k_rbf+2,"V2"]
tmip$volume_rbf_tmax = pat[k_rbf+3,"V2"]
tmip$volume_rbv_adc = pat[k_rbv+2,"V2"]
tmip$volume_rbv_tmax = pat[k_rbv+3,"V2"]
tmip$measure = "tMIP"


pat_new = rbind(rbf, rbv, ttp, tmax, mtt, tmip)
head(pat_new)
# ROI-Bez. Mittel SD Min. Max. Pixelzahl OberflÃ¤che (mm2) Verh. NA
pat_new = rename(pat_new, c("V1"="roi", 
                            "V2"="mean",
                            "V3" = "sd",
                            "V4" = "min",
                            "V5" = "max",
                            "V6" = "n_pixels",
                            "V7" = "surface_mm2"))
pat_new$p_id = p_id







#### Define functions to read in the data

# create function to read the tables
read_patient = function(path_to_csv_file){
  # patient
  
  pat = read.csv(path_to_csv_file, row.names = NULL, header=F, stringsAsFactors=F)
  pat = pat[,1:7]
  
  # find the measures of interest
  # start point 4 regions + 8 regions (5-12) a 10 slices = 4+8*10 = 84 
  # 4:(4+84-1) includes row 4:87 = 84 elements since 4 and 87 are included
  
  # save the patient id and the columnnames for the measures
  (p_id = pat$V1[2])
  
  # find the row with the name rBF
  k = which(pat$V1=="rBF")[1]
  rbf = pat[(k+2):(k+2+83),]
  # find volume 1 of ADC
  k_adc = which(pat$V1=="ADC")
  if(length(k_adc)>1){ # there are some which have only one row with ADC not two --> had problems to read in
    k_adc = k_adc[2]
  }
  print(paste0("This row contains ADC Volume 1: ", pat$V1[k_adc+2] == "Volume 1"))
  rbf$volume_adc = pat[k_adc+2,"V7"]
  # find volume 2 of TMAX
  k_tmax = which(pat$V1=="TMAX")
  if(length(k_tmax)>1){ # there are some which have only one row with ADC not two --> had problems to read in
    k_tmax = k_tmax[2]
  }
  print(paste0("This row contains TMAX Volume 2: ", pat$V1[k_tmax+2] == "Volume 2"))
  rbf$volume_tmax = pat[k_tmax+2,"V7"]
  
  k_rbf = which(pat$V1=="rBF")[2]
  print(paste0("This row contains rBF-ADC Volume 1: ", pat$V1[k_rbf+2] == "Volume 1"))
  print(paste0("This row contains rBF-ADC Volume 2: ", pat$V1[k_rbf+3] == "Volume 2"))
  rbf$volume_rbf_adc = pat[k_rbf+2,"V2"]
  rbf$volume_rbf_tmax = pat[k_rbf+3,"V2"]
  
  k_rbv = which(pat$V1=="rBV")[2]
  print(paste0("This row contains rBV-ADC Volume 1: ", pat$V1[k_rbv+2] == "Volume 1"))
  print(paste0("This row contains rBV-ADC Volume 2: ", pat$V1[k_rbv+3] == "Volume 2"))
  rbf$volume_rbv_adc = pat[k_rbv+2,"V2"]
  rbf$volume_rbv_tmax = pat[k_rbv+3,"V2"]
  
  # get the measure variable
  rbf$measure = "rBF"
  # check if the first element is 1 and the last 12_9 of the roi
  rbf_cor = rbf$V1[1]=="1" & rbf$V1[dim(rbf)[1]]=="12_9"
  
  
  
  #### rBV
  
  k = which(pat$V1=="rBV")[1]
  rbv = pat[(k+2):(k+2+83),]
  rbv$volume_adc = rbf$volume_adc[1]
  rbv$volume_tmax = rbf$volume_tmax[1]
  rbv$volume_rbf_adc = rbf$volume_rbf_adc
  rbv$volume_rbf_tmax = rbf$volume_rbf_tmax
  rbv$volume_rbv_adc = rbf$volume_rbv_adc
  rbv$volume_rbv_tmax = rbf$volume_rbv_tmax
  rbv$measure = "rBV"
  # check if the first element is 1 and the last 12_9 of the roi
  rbv_cor = rbv$V1[1]=="1" & rbv$V1[dim(rbv)[1]]=="12_9"
  
  
  #### TTP
  
  k = which(pat$V1=="TTP")[1]
  ttp = pat[(k+2):(k+2+83),]
  ttp$volume_adc = rbf$volume_adc[1]
  ttp$volume_tmax = rbf$volume_tmax[1]
  ttp$volume_rbf_adc = rbf$volume_rbf_adc
  ttp$volume_rbf_tmax = rbf$volume_rbf_tmax
  ttp$volume_rbv_adc = rbf$volume_rbv_adc
  ttp$volume_rbv_tmax = rbf$volume_rbv_tmax
  ttp$measure = "TTP"
  # check if the first element is 1 and the last 12_9 of the roi
  ttp_cor = ttp$V1[1]=="1" & ttp$V1[dim(ttp)[1]]=="12_9"
  
  
  #### TMAX
  
  k = which(pat$V1=="TMAX")[1]
  tmax = pat[(k+2):(k+2+83),]
  tmax$volume_adc = rbf$volume_adc[1]
  tmax$volume_tmax = rbf$volume_tmax[1]
  tmax$volume_rbf_adc = rbf$volume_rbf_adc
  tmax$volume_rbf_tmax = rbf$volume_rbf_tmax
  tmax$volume_rbv_adc = rbf$volume_rbv_adc
  tmax$volume_rbv_tmax = rbf$volume_rbv_tmax
  tmax$measure = "TMAX"
  # check if the first element is 1 and the last 12_9 of the roi
  tmax_cor = tmax$V1[1]=="1" & tmax$V1[dim(tmax)[1]]=="12_9"
  
  
  #### MTT
  
  k = which(pat$V1=="MTT")[1]
  mtt = pat[(k+2):(k+2+83),]
  mtt$volume_adc = rbf$volume_adc[1]
  mtt$volume_tmax = rbf$volume_tmax[1]
  mtt$volume_rbf_adc = rbf$volume_rbf_adc
  mtt$volume_rbf_tmax = rbf$volume_rbf_tmax
  mtt$volume_rbv_adc = rbf$volume_rbv_adc
  mtt$volume_rbv_tmax = rbf$volume_rbv_tmax
  mtt$measure = "MTT"
  # check if the first element is 1 and the last 12_9 of the roi
  mtt_cor = mtt$V1[1]=="1" & mtt$V1[dim(mtt)[1]]=="12_9"
  
  
  #### tMIP
  
  k = which(pat$V1=="tMIP")[1]
  tmip = pat[(k+2):(k+2+83),]
  tmip$volume_adc = rbf$volume_adc[1]
  tmip$volume_tmax = rbf$volume_tmax[1]
  tmip$volume_rbf_adc = rbf$volume_rbf_adc
  tmip$volume_rbf_tmax = rbf$volume_rbf_tmax
  tmip$volume_rbv_adc = rbf$volume_rbv_adc
  tmip$volume_rbv_tmax = rbf$volume_rbv_tmax
  tmip$measure = "tMIP"
  # check if the first element is 1 and the last 12_9 of the roi
  tmip_cor = tmip$V1[1]=="1" & tmip$V1[dim(tmip)[1]]=="12_9"
  
  
  

  print(paste0("All tables start with 1 and end with 12_9: ", 
               all(rbf_cor, rbv_cor, ttp_cor, tmax_cor, mtt_cor, tmip_cor)))
  pat_new = rbind(rbf, rbv, ttp, tmax, mtt, tmip)
  head(pat_new)
  # ROI-Bez. Mittel SD Min. Max. Pixelzahl OberflÃ¤che (mm2) Verh. NA
  pat_new = rename(pat_new, c("V1"="roi", 
                              "V2"="mean",
                              "V3" = "sd",
                              "V4" = "min",
                              "V5" = "max",
                              "V6" = "n_pixels",
                              "V7" = "surface_mm2"))
  pat_new$p_id = p_id
  return(pat_new)
}

read_patient0002593122 = function(path_to_csv_file){
  # patient
  
  pat = read.csv(path_to_csv_file, row.names = NULL, header=F, stringsAsFactors=F)
  pat = pat[,1:7]
  
  # find the measures of interest
  # start point 4 regions + 8 regions (5-12) a 10 slices = 4+8*10 = 84 
  # 4:(4+84-1) includes row 4:87 = 84 elements since 4 and 87 are included
  
  # save the patient id and the columnnames for the measures
  (p_id = pat$V1[2])
  
  # find the row with the name rBF
  k = which(pat$V1=="rBF")[1]
  rbf = pat[(k+2):(k+2+83),]
  # find volume 1 of ADC
  k_adc = which(pat$V1=="ADC")
  if(length(k_adc)>1){ # there are some which have only one row with ADC not two --> had problems to read in
    k_adc = k_adc[2]
  }
  print(paste0("This row contains ADC Volume 1: ", pat$V1[k_adc+2] == "Volume 1"))
  rbf$volume_adc = pat[k_adc+2,"V7"]
  # find volume 2 of TMAX
  k_tmax = which(pat$V1=="TMAX")
  if(length(k_tmax)>1){ # there are some which have only one row with ADC not two --> had problems to read in
    k_tmax = k_tmax[2]
  }
  print(paste0("This row contains TMAX Volume 2: ", pat$V1[k_tmax+2] == "Volume 2"))
  rbf$volume_tmax = pat[k_tmax+2,"V7"]
  
  k_rbf = which(pat$V1=="rBF")[2]
  print(paste0("This row contains rBF-ADC Volume 2: ", pat$V1[k_rbf+2] == "Volume 2"))
  print(paste0("This row contains rBF-ADC null: ", pat$V1[k_rbf+3] == "null"))
  rbf$volume_rbf_adc = pat[k_rbf+3,"V2"]
  rbf$volume_rbf_tmax = pat[k_rbf+2,"V2"]
  
  k_rbv = which(pat$V1=="rBV")[2]
  print(paste0("This row contains rBV-ADC Volume 2: ", pat$V1[k_rbv+2] == "Volume 2"))
  print(paste0("This row contains rBV-ADC null: ", pat$V1[k_rbv+3] == "null"))
  rbf$volume_rbv_adc = pat[k_rbv+3,"V2"]
  rbf$volume_rbv_tmax = pat[k_rbv+2,"V2"]
  
  # get the measure variable
  rbf$measure = "rBF"
  # check if the first element is 1 and the last 12_9 of the roi
  rbf_cor = rbf$V1[1]=="1" & rbf$V1[dim(rbf)[1]]=="12_9"
  
  
  
  #### rBV
  
  k = which(pat$V1=="rBV")[1]
  rbv = pat[(k+2):(k+2+83),]
  rbv$volume_adc = rbf$volume_adc[1]
  rbv$volume_tmax = rbf$volume_tmax[1]
  rbv$volume_rbf_adc = rbf$volume_rbf_adc
  rbv$volume_rbf_tmax = rbf$volume_rbf_tmax
  rbv$volume_rbv_adc = rbf$volume_rbv_adc
  rbv$volume_rbv_tmax = rbf$volume_rbv_tmax
  rbv$measure = "rBV"
  # check if the first element is 1 and the last 12_9 of the roi
  rbv_cor = rbv$V1[1]=="1" & rbv$V1[dim(rbv)[1]]=="12_9"
  
  
  #### TTP
  
  k = which(pat$V1=="TTP")[1]
  ttp = pat[(k+2):(k+2+83),]
  ttp$volume_adc = rbf$volume_adc[1]
  ttp$volume_tmax = rbf$volume_tmax[1]
  ttp$volume_rbf_adc = rbf$volume_rbf_adc
  ttp$volume_rbf_tmax = rbf$volume_rbf_tmax
  ttp$volume_rbv_adc = rbf$volume_rbv_adc
  ttp$volume_rbv_tmax = rbf$volume_rbv_tmax
  ttp$measure = "TTP"
  # check if the first element is 1 and the last 12_9 of the roi
  ttp_cor = ttp$V1[1]=="1" & ttp$V1[dim(ttp)[1]]=="12_9"
  
  
  #### TMAX
  
  k = which(pat$V1=="TMAX")[1]
  tmax = pat[(k+2):(k+2+83),]
  tmax$volume_adc = rbf$volume_adc[1]
  tmax$volume_tmax = rbf$volume_tmax[1]
  tmax$volume_rbf_adc = rbf$volume_rbf_adc
  tmax$volume_rbf_tmax = rbf$volume_rbf_tmax
  tmax$volume_rbv_adc = rbf$volume_rbv_adc
  tmax$volume_rbv_tmax = rbf$volume_rbv_tmax
  tmax$measure = "TMAX"
  # check if the first element is 1 and the last 12_9 of the roi
  tmax_cor = tmax$V1[1]=="1" & tmax$V1[dim(tmax)[1]]=="12_9"
  
  
  #### MTT
  
  k = which(pat$V1=="MTT")[1]
  mtt = pat[(k+2):(k+2+83),]
  mtt$volume_adc = rbf$volume_adc[1]
  mtt$volume_tmax = rbf$volume_tmax[1]
  mtt$volume_rbf_adc = rbf$volume_rbf_adc
  mtt$volume_rbf_tmax = rbf$volume_rbf_tmax
  mtt$volume_rbv_adc = rbf$volume_rbv_adc
  mtt$volume_rbv_tmax = rbf$volume_rbv_tmax
  mtt$measure = "MTT"
  # check if the first element is 1 and the last 12_9 of the roi
  mtt_cor = mtt$V1[1]=="1" & mtt$V1[dim(mtt)[1]]=="12_9"
  
  
  #### tMIP
  
  k = which(pat$V1=="tMIP")[1]
  tmip = pat[(k+2):(k+2+83),]
  tmip$volume_adc = rbf$volume_adc[1]
  tmip$volume_tmax = rbf$volume_tmax[1]
  tmip$volume_rbf_adc = rbf$volume_rbf_adc
  tmip$volume_rbf_tmax = rbf$volume_rbf_tmax
  tmip$volume_rbv_adc = rbf$volume_rbv_adc
  tmip$volume_rbv_tmax = rbf$volume_rbv_tmax
  tmip$measure = "tMIP"
  # check if the first element is 1 and the last 12_9 of the roi
  tmip_cor = tmip$V1[1]=="1" & tmip$V1[dim(tmip)[1]]=="12_9"
  
  
  
  
  print(paste0("All tables start with 1 and end with 12_9: ", 
               all(rbf_cor, rbv_cor, ttp_cor, tmax_cor, mtt_cor, tmip_cor)))
  pat_new = rbind(rbf, rbv, ttp, tmax, mtt, tmip)
  head(pat_new)
  # ROI-Bez. Mittel SD Min. Max. Pixelzahl OberflÃ¤che (mm2) Verh. NA
  pat_new = rename(pat_new, c("V1"="roi", 
                              "V2"="mean",
                              "V3" = "sd",
                              "V4" = "min",
                              "V5" = "max",
                              "V6" = "n_pixels",
                              "V7" = "surface_mm2"))
  pat_new$p_id = p_id
  return(pat_new)
}



# for patient XXX we have 11 instead of 12 regions (same for YYY)
read_patientXXXYYY = function(path_to_csv_file){
  # patient
  pat = read.csv(path_to_csv_file, row.names = NULL, header=F, stringsAsFactors=F)
  pat = pat[,1:7]
  
  # find the measures of interest
  # start point 4 regions + 7 regions (5-11) a 10 slices = 4+7*10 = 84 
  # 4:(4+74-1) includes row 4:77 = 74 elements since 4 and 77 are included
  
  # save the patient id and the columnnames for the measures
  (p_id = pat$V1[2])
  
  # find the row with the name rBF
  k = which(pat$V1=="rBF")[1]
  rbf = pat[(k+2):(k+2+73),]
  # find volume 1 of ADC
  k_adc = which(pat$V1=="ADC")
  if(length(k_adc)>1){ # there are some which have only one row with ADC not two --> had problems to read in
    k_adc = k_adc[2]
  }
  print(paste0("This row contains ADC Volume 1: ", pat$V1[k_adc+2] == "Volume 1"))
  rbf$volume_adc = pat[k_adc+2,"V7"]
  # find volume 2 of TMAX
  k_tmax = which(pat$V1=="TMAX")
  if(length(k_tmax)>1){ # there are some which have only one row with ADC not two --> had problems to read in
    k_tmax = k_tmax[2]
  }
  print(paste0("This row contains TMAX Volume 2: ", pat$V1[k_tmax+2] == "Volume 2"))
  rbf$volume_tmax = pat[k_tmax+2,"V7"]
  
  k_rbf = which(pat$V1=="rBF")[2]
  print(paste0("This row contains rBF-ADC Volume 1: ", pat$V1[k_rbf+2] == "Volume 1"))
  print(paste0("This row contains rBF-ADC Volume 1: ", pat$V1[k_rbf+3] == "Volume 2"))
  rbf$volume_rbf_adc = pat[k_rbf+2,"V2"]
  rbf$volume_rbf_tmax = pat[k_rbf+3,"V2"]
  
  k_rbv = which(pat$V1=="rBV")[2]
  print(paste0("This row contains rBV-ADC Volume 1: ", pat$V1[k_rbv+2] == "Volume 1"))
  print(paste0("This row contains rBV-ADC Volume 1: ", pat$V1[k_rbv+3] == "Volume 2"))
  rbf$volume_rbv_adc = pat[k_rbv+2,"V2"]
  rbf$volume_rbv_tmax = pat[k_rbv+3,"V2"]
  
  # get the measure variable
  rbf$measure = "rBF"
  # check if the first element is 1 and the last 11_9 of the roi
  rbf_cor = rbf$V1[1]=="1" & rbf$V1[dim(rbf)[1]]=="11_9"
  
  
  
  #### rBV
  
  k = which(pat$V1=="rBV")[1]
  rbv = pat[(k+2):(k+2+73),]
  rbv$volume_adc = rbf$volume_adc[1]
  rbv$volume_tmax = rbf$volume_tmax[1]
  rbv$volume_rbf_adc = rbf$volume_rbf_adc
  rbv$volume_rbf_tmax = rbf$volume_rbf_tmax
  rbv$volume_rbv_adc = rbf$volume_rbv_adc
  rbv$volume_rbv_tmax = rbf$volume_rbv_tmax
  rbv$measure = "rBV"
  # check if the first element is 1 and the last 12_9 of the roi
  rbv_cor = rbv$V1[1]=="1" & rbv$V1[dim(rbv)[1]]=="11_9"
  
  
  #### TTP
  
  k = which(pat$V1=="TTP")[1]
  ttp = pat[(k+2):(k+2+73),]
  ttp$volume_adc = rbf$volume_adc[1]
  ttp$volume_tmax = rbf$volume_tmax[1]
  ttp$volume_rbf_adc = rbf$volume_rbf_adc
  ttp$volume_rbf_tmax = rbf$volume_rbf_tmax
  ttp$volume_rbv_adc = rbf$volume_rbv_adc
  ttp$volume_rbv_tmax = rbf$volume_rbv_tmax
  ttp$measure = "TTP"
  # check if the first element is 1 and the last 12_9 of the roi
  ttp_cor = ttp$V1[1]=="1" & ttp$V1[dim(ttp)[1]]=="11_9"
  
  
  #### TMAX
  
  k = which(pat$V1=="TMAX")[1]
  tmax = pat[(k+2):(k+2+73),]
  tmax$volume_adc = rbf$volume_adc[1]
  tmax$volume_tmax = rbf$volume_tmax[1]
  tmax$volume_rbf_adc = rbf$volume_rbf_adc
  tmax$volume_rbf_tmax = rbf$volume_rbf_tmax
  tmax$volume_rbv_adc = rbf$volume_rbv_adc
  tmax$volume_rbv_tmax = rbf$volume_rbv_tmax
  tmax$measure = "TMAX"
  # check if the first element is 1 and the last 12_9 of the roi
  tmax_cor = tmax$V1[1]=="1" & tmax$V1[dim(tmax)[1]]=="11_9"
  
  
  #### MTT
  
  k = which(pat$V1=="MTT")[1]
  mtt = pat[(k+2):(k+2+73),]
  mtt$volume_adc = rbf$volume_adc[1]
  mtt$volume_tmax = rbf$volume_tmax[1]
  mtt$volume_rbf_adc = rbf$volume_rbf_adc
  mtt$volume_rbf_tmax = rbf$volume_rbf_tmax
  mtt$volume_rbv_adc = rbf$volume_rbv_adc
  mtt$volume_rbv_tmax = rbf$volume_rbv_tmax
  mtt$measure = "MTT"
  # check if the first element is 1 and the last 12_9 of the roi
  mtt_cor = mtt$V1[1]=="1" & mtt$V1[dim(mtt)[1]]=="11_9"
  
  
  #### tMIP
  
  k = which(pat$V1=="tMIP")[1]
  tmip = pat[(k+2):(k+2+73),]
  tmip$volume_adc = rbf$volume_adc[1]
  tmip$volume_tmax = rbf$volume_tmax[1]
  tmip$volume_rbf_adc = rbf$volume_rbf_adc
  tmip$volume_rbf_tmax = rbf$volume_rbf_tmax
  tmip$volume_rbv_adc = rbf$volume_rbv_adc
  tmip$volume_rbv_tmax = rbf$volume_rbv_tmax
  tmip$measure = "tMIP"
  # check if the first element is 1 and the last 12_9 of the roi
  tmip_cor = tmip$V1[1]=="1" & tmip$V1[dim(tmip)[1]]=="11_9"
  
  
  
  
  print(paste0("All tables start with 1 and end with 11_9: ", 
               all(rbf_cor, rbv_cor, ttp_cor, tmax_cor, mtt_cor, tmip_cor)))
  pat_new = rbind(rbf, rbv, ttp, tmax, mtt, tmip)
  head(pat_new)
  # ROI-Bez. Mittel SD Min. Max. Pixelzahl OberflÃ¤che (mm2) Verh. NA
  pat_new = rename(pat_new, c("V1"="roi", 
                              "V2"="mean",
                              "V3" = "sd",
                              "V4" = "min",
                              "V5" = "max",
                              "V6" = "n_pixels",
                              "V7" = "surface_mm2"))
  pat_new$p_id = p_id
  return(pat_new)
}


# get all patient files
left = list.files(paste0(dir,"data/linksseitiger Schlaganfall")) # list all files
left = paste0("data/linksseitiger Schlaganfall/",left) # create the path
right = list.files(paste0(dir,"data/rechtsseitiger Schlaganfall"))
right = paste0("data/rechtsseitiger Schlaganfall/",right)
path_pat_files = c(right,left)
length(path_pat_files)

# get the XXX and YYY files
xxx_files = grepl("XXX",path_pat_files)
yyy_files = grepl("YYY",path_pat_files)

pat = data.frame()
j=0
k=0
for(i in 1:length(path_pat_files)){
  print(i)
  if(path_pat_files[i] == "data/linksseitiger Schlaganfall/0002593122_20160922_4112749.csv"){
    pat_tmp = read_patient0002593122(path_pat_files[i])
  }
  if(xxx_files[i] | yyy_files[i]){
    j = j+1
    pat_tmp = read_patientXXXYYY(path_pat_files[i])
  } else{
    k = k+1
    pat_tmp = read_patient(path_pat_files[i])
  }
  pat = rbind(pat,pat_tmp)
}
k
j

# for some patients the volume seems to be missing
# which(is.na(pat$volume_adc))
# which(is.na(pat$volume_tmax))

length(unique(pat$p_id))
length(unique(dat0$p_id))
# are all csv files contained within the table
for(i in unique(pat$p_id)){
  if(! i %in% unique(dat0$p_id)){
    print(i)
  }
}

# are all table IDs available as csv files?
for(i in unique(dat0$p_id)){
  if(! i %in% unique(pat$p_id)){
    print(i)
  }
}


# Add the patient data: merge the two dataframe by patient id
dat = merge(dat0, pat, by="p_id", all = F)
length(unique(dat$p_id))
# [1] 222


# make sure that all the variables have the correct 
str(dat)
dat$p_id = as.factor(dat$p_id)
dat$sex = as.factor(dat$sex)

dat$independent_pre_stroke = as.factor(dat$independent_pre_stroke)
dat$death_after_3months = as.factor(dat$death_after_3months)
dat$intracranial_bleeding = as.factor(dat$intracranial_bleeding)
dat$tah_pre_stroke = as.factor(dat$tah_pre_stroke)
dat$antikoagulation_pre_stroke = as.factor(dat$antikoagulation_pre_stroke)
dat$statin_pre_stroke = as.factor(dat$statin_pre_stroke)
dat$antihypertensiva_pre_stroke = as.factor(dat$antihypertensiva_pre_stroke)
dat$toast = as.factor(dat$toast)
dat$atrial_fibrillation = as.factor(dat$atrial_fibrillation)
dat$rf_diabetes = as.factor(dat$rf_diabetes)
dat$rf_hypertonia = as.factor(dat$rf_hypertonia)
dat$rf_hypercholesterinaemie = as.factor(dat$rf_hypercholesterinaemie)
dat$rf_smoker = as.factor(dat$rf_smoker)
dat$rf_chd = as.factor(dat$rf_chd)
dat$rf_pavk = as.factor(dat$rf_pavk)
dat$rf_tia_stroke = as.factor(dat$rf_tia_stroke)
dat$infarct_side = as.factor(dat$infarct_side)
dat$additional_occlusion = as.factor(dat$additional_occlusion)  
dat$additional_occlusion_side = as.factor(dat$additional_occlusion_side) 
dat$additional_occlusion_ica_excranial = as.factor(dat$additional_occlusion_ica_excranial)
dat$additional_occlusion_ica_intracranial = as.factor(dat$additional_occlusion_ica_intracranial)
dat$additional_occlusion_ica_t = as.factor(dat$additional_occlusion_ica_t)
dat$additional_occlusion_mca_m2 = as.factor(dat$additional_occlusion_mca_m2)
dat$additional_occlusion_mca_m2_m3 = as.factor(dat$additional_occlusion_mca_m2_m3)   
dat$collateralization = as.factor(dat$collateralization)

dat$anaesthesia = as.factor(dat$anaesthesia)

dat$lyse = as.factor(dat$lyse)
dat$angio = as.factor(dat$angio)
dat$thrombectomy = as.factor(dat$thrombectomy)

dat$time_to_imaging = as.character(dat$time_to_imaging)
dat$time_to_needle = as.character(dat$time_to_needle)
dat$time_to_groin_puncture = as.character(dat$time_to_groin_puncture)

dat$vessel_open_before_therapy = as.factor(dat$vessel_open_before_therapy)
dat$tici = as.factor(dat$tici)
dat$tici_binary = as.factor(dat$tici_binary)

dat$mean = as.numeric(dat$mean)
dat$sd = as.numeric(dat$sd)
dat$min = as.numeric(dat$min)
dat$max = as.numeric(dat$max)
dat$n_pixels = as.numeric(dat$n_pixels)
dat$surface_mm2 = as.numeric(dat$surface_mm2)
dat$volume_adc = as.numeric(dat$volume_adc)
dat$volume_tmax = as.numeric(dat$volume_tmax)
dat$volume_rbf_adc = as.numeric(dat$volume_rbf_adc)
dat$volume_rbf_tmax = as.numeric(dat$volume_rbf_tmax)
dat$volume_rbv_adc = as.numeric(dat$volume_rbv_adc)
dat$volume_rbv_tmax = as.numeric(dat$volume_rbv_tmax)
dat$measure = as.factor(dat$measure)

str(dat)





########################
#### Exclude slices ####
########################

#### define the different regions/slices

# split the variable ROI in region and slices
# this is necessar to group the dataframe by the region, to get the  mean easily
dat$region = c()
dat$slice = c()
for(i in 1:length(dat$roi)){
  splt_roi = strsplit(dat$roi[i],"_")
  # [[1]]
  # [1] "5" "0"
  dat$slice[i] = splt_roi[[1]][1] # 5
  dat$region[i] = splt_roi[[1]][2] # 0
}

# for ROIs 1,2,3,4:
dat$region[dat$roi==1] = -1
dat$region[dat$roi==2] = -2
dat$region[dat$roi==3] = -3
dat$region[dat$roi==4] = -4

head(dat)
dat$region = as.factor(dat$region)
dat$slice = as.factor(dat$slice)

# We exclude the 12th slice
slice12 = which(dat$slice==12)
length(slice12)
dat = dat[-slice12,]

# We exclude region 4 and 5 of slice 11
slice11_4 = which(dat$roi=="11_4")
length(slice11_4)
dat = dat[-slice11_4,]
slice11_5 = which(dat$roi=="11_5")
length(slice11_5)
dat = dat[-slice11_5,]

slice5_4 = which(dat$roi=="5_4")
length(slice5_4)
dat = dat[-slice5_4,]
slice5_5 = which(dat$roi=="5_5")
length(slice5_5)
dat = dat[-slice5_5,]

# 







##########################
#### Get mean and std ####
##########################


#### Get the pooled mean and variance

# calculate the pooled mean across the slices for each ROI
# N = number of pixels per region
# pooled mean = (N1*Mean1+N2*Mean2+...+N9*Mean9)/(N1+N2+...+N9)
# --> means of regions with more pixels get more weight
# pooled sd = ((N1-1)*Sd1+(N2-1)*Sd2+...+(N9-1)Sd9/(N1+N2+...+N9-9))

# calculate the mean across slices for each ROI
# we apply the formular to each region (group_by(region))
library(dplyr)
dat = dat %>% group_by(p_id,measure,region) %>% mutate(pooled_mean = sum(n_pixels*mean)/(sum(n_pixels))) %>% ungroup()
dat = dat %>% group_by(p_id,measure,region) %>% mutate(pooled_sd = sum((n_pixels-1)*sd)/(sum(n_pixels)-length(n_pixels))) %>% ungroup()
dat = as.data.frame(dat)

# assign the mean to the 4 regions (before it takes the mean across all regions 1, 2, 3 and 4 respectively)
dat$pooled_mean[dat$roi==1] = dat$mean[dat$roi==1]
dat$pooled_mean[dat$roi==2] = dat$mean[dat$roi==2]
dat$pooled_mean[dat$roi==3] = dat$mean[dat$roi==3]
dat$pooled_mean[dat$roi==4] = dat$mean[dat$roi==4]
dat$pooled_sd[dat$roi==1] = dat$sd[dat$roi==1]
dat$pooled_sd[dat$roi==2] = dat$sd[dat$roi==2]
dat$pooled_sd[dat$roi==3] = dat$sd[dat$roi==3]
dat$pooled_sd[dat$roi==4] = dat$sd[dat$roi==4]


# check if it does what it should (mean): consider region 5
dat5 = dat[which(dat$region==5 & dat$measure=="rBF" & dat$p_id==unique(dat$p_id)[5]),]
dat5
# numerator
dat5$mean*dat5$n_pixels
dat5$mean[1]*dat5$n_pixels[1]
sum(dat5$mean*dat5$n_pixels)
# denominator
sum(dat5$n_pixels)
# result
sum(dat5$mean*dat5$n_pixels)/sum(dat5$n_pixels)

# 
sum((dat5$n_pixels-1)*dat5$sd)/(sum(dat5$n_pixels)-length(dat5$n_pixels))

# check if we have all the measurments: 
# We have 13 patients with 3 measurements a 84 regions/slices
# and 1 patient with 3 measurements a 74 regions/slices
# = 13*3*84 + 1*3*74 = 2332
dim(dat)



##########################################################
#### Define the regions for stroke/contralateral side ####
##########################################################

# It might be easier to learn when we directly define the sides
# consider only region 0-9
# region 0-4 and roi 1/3: right
# region 5-9 and roi 2/4: left
dat0 = dat
# create a new variable that tells if a region is on the stroke or the contralateral side
dat$region2 = c()
dat_left = dat[dat$infarct_side==1,] # stroke on the left
dat_left$region2[dat_left$region == -1] = "C_LN"
dat_left$region2[dat_left$region == -2] = "S_LN"
dat_left$region2[dat_left$region == -3] = "C_Th"
dat_left$region2[dat_left$region == -4] = "S_Th"
dat_left$region2[dat_left$region == 0] = "C_Occ"
dat_left$region2[dat_left$region == 9] = "S_Occ"
dat_left$region2[dat_left$region == 1] = "C_Medp"
dat_left$region2[dat_left$region == 8] = "S_Medp"
dat_left$region2[dat_left$region == 2] = "C_Medm"
dat_left$region2[dat_left$region == 7] = "S_Medm"
dat_left$region2[dat_left$region == 3] = "C_Meda"
dat_left$region2[dat_left$region == 6] = "S_Meda"
dat_left$region2[dat_left$region == 4] = "C_Ant"
dat_left$region2[dat_left$region == 5] = "S_Ant"
dat_right = dat[dat$infarct_side==2,] # stroke on the right
dat_right$region2[dat_right$region == -1] = "S_LN"
dat_right$region2[dat_right$region == -2] = "C_LN"
dat_right$region2[dat_right$region == -3] = "S_Th"
dat_right$region2[dat_right$region == -4] = "C_Th"
dat_right$region2[dat_right$region == 0] = "S_Occ"
dat_right$region2[dat_right$region == 9] = "C_Occ"
dat_right$region2[dat_right$region == 1] = "S_Medp"
dat_right$region2[dat_right$region == 8] = "C_Medp"
dat_right$region2[dat_right$region == 2] = "S_Medm"
dat_right$region2[dat_right$region == 7] = "C_Medm"
dat_right$region2[dat_right$region == 3] = "S_Meda"
dat_right$region2[dat_right$region == 6] = "C_Meda"
dat_right$region2[dat_right$region == 4] = "S_Ant"
dat_right$region2[dat_right$region == 5] = "C_Ant"

dat = rbind(dat_left,dat_right)
head(dat)







###################
#### Save data ####
###################


#### Create new variable mrs_3months_binary
# define new outcome variable mrs binary: 1 = good, 0 = bad
dat$mrs_3months_binary = 1
dat$mrs_3months_binary[dat$mrs_3months>=3] = 0
dat$mrs_3months_binary = as.factor(dat$mrs_3months_binary)
# dat$mrs_3months_binary = revalue(dat$mrs_3months_binary, c("1"="good", "0"="bad"))

# Tissue at risk
dat$volume_tar = dat$volume_tmax - dat$volume_adc

# save the final dataset
dat_final = unique(dat[,!(names(dat) %in% c("mean","sd","min","max","n_pixels","surface_mm2","roi","slice"))])
dat_final$region = factor(dat_final$region, levels=c("-1","-2","-3","-4","0","1","2","3","4","5","6","7","8","9"))
dat_final = dat_final[order(dat_final$p_id,dat_final$measure,dat_final$region),]
dim(dat_final)
write.table(dat_final, paste0(dir, "data/data.csv"),  sep=",", row.names = F)
save(dat_final, file=paste0(dir, "data/data.R"))

# save rBF
dat = dat_final[dat_final$measure=="rBF",]
write.table(dat, paste0(dir, "data/data_rbf.csv"),  sep=",", row.names = F)
save(dat, file=paste0(dir, "data/data_rbf.R"))

# save rBV
dat = dat_final[dat_final$measure=="rBV",]
write.table(dat, paste0(dir, "data/data_rbv.csv"),  sep=",", row.names = F)
save(dat, file=paste0(dir, "data/data_rbv.R"))

# save rBV
dat = dat_final[dat_final$measure=="TTP",]
write.table(dat, paste0(dir, "data/data_ttp.csv"),  sep=",", row.names = F)
save(dat, file=paste0(dir, "data/data_ttp.R"))

# save TMAX
dat = dat_final[dat_final$measure=="TMAX",]
write.table(dat, paste0(dir, "data/data_tmax.csv"),  sep=",", row.names = F)
save(dat, file=paste0(dir, "data/data_tmax.R"))

# save MTT
dat = dat_final[dat_final$measure=="MTT",]
write.table(dat, paste0(dir, "data/data_mtt.csv"),  sep=",", row.names = F)
save(dat, file=paste0(dir, "data/data_mtt.R"))

# save rBV
dat = dat_final[dat_final$measure=="tMIP",]
write.table(dat, paste0(dir, "data/data_tmip.csv"),  sep=",", row.names = F)
save(dat, file=paste0(dir, "data/data_tmip.R"))










########## Wird direkt in der CV gemacht, hier nicht mehr gebraucht !! ##########

# #########################################
# #### Impute variables before therapy ####
# #########################################
# 
# 
# library(reshape2)
# library(missForest)
# 
# 
# #### All parameters in one model
# 
# # We now want all variables within out dataset:
# 
# load(paste0(dir, "/data/data.R"))
# dat = dat_final
# 
# # save the original data in dat0
# dat0 = dat
# 
# # change the names to identify each measure
# dat0$region2[dat0$measure=="rBF"] = paste0(dat0$region2[dat0$measure=="rBF"], "_rbf")
# dat0$region2[dat0$measure=="rBV"] = paste0(dat0$region2[dat0$measure=="rBV"], "_rbv")
# dat0$region2[dat0$measure=="TTP"] = paste0(dat0$region2[dat0$measure=="TTP"], "_ttp")
# dat0$region2[dat0$measure=="TMAX"] = paste0(dat0$region2[dat0$measure=="TMAX"], "_tmax")
# dat0$region2[dat0$measure=="MTT"] = paste0(dat0$region2[dat0$measure=="MTT"], "_mtt")
# dat0$region2[dat0$measure=="tMIP"] = paste0(dat0$region2[dat0$measure=="tMIP"], "_tmip")
# 
# # transform data
# # mrs, risk_factors, ..., region1_mean_volume, region2_mean_volume, ...
# dat = dcast(dat0, p_id+age+sex+independent_pre_stroke+nihss_bl
#             +intracranial_bleeding+tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
#             +sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr
#             +atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie+rf_smoker+rf_chd+rf_pavk+rf_tia_stroke
#             +infarct_side+additional_occlusion+lyse+time_to_imaging+time_to_groin_puncture
#             +volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar
#             +mrs_3months_binary+collateralization+anaesthesia
#             ~ region2, value.var="pooled_mean")
# 
# save(dat, file=paste0(dir,"/data/data_wide_all_before_therapy.R"))
# 
# 
# 
# #### Missing data
# 
# # consider the number of missings per column
# n_missing = apply(dat, 2, function(x) length(which(is.na(x))))
# n_missing
# barplot((n_missing/nrow(dat))*100, las=2, ylab="Missing values (%)",
#         ylim=c(0,100), cex.names=0.6, col="royalblue")
# 
# 
# #### Impute the missing data
# 
# set.seed(3004)
# dat$time_to_imaging = as.numeric(dat$time_to_imaging)
# dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
# 
# dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
# dat$time_to_imaging = as.numeric(dat$time_to_imaging)
# imp = missForest(dat[,c("mrs_3months_binary","age","sex","nihss_bl","independent_pre_stroke","intracranial_bleeding","tah_pre_stroke","antikoagulation_pre_stroke","statin_pre_stroke",
#                         "antihypertensiva_pre_stroke","sys_bloodpressure_bl","dias_bloodpressure_bl","glucose_bl","hba1c","ldl","hdl","triglyceride","crp","inr",
#                         "atrial_fibrillation","rf_diabetes","rf_hypertonia","rf_hypercholesterinaemie","rf_smoker","rf_chd","rf_pavk","rf_tia_stroke","infarct_side",
#                         "additional_occlusion","lyse","time_to_imaging","rf_pavk","time_to_groin_puncture","collateralization","anaesthesia",
#                         "volume_adc","volume_tmax","volume_rbf_adc","volume_rbf_tmax","volume_rbv_adc","volume_rbv_tmax","volume_tar",
#                         "C_LN_rbf","C_Th_rbf","C_Occ_rbf","C_Medp_rbf","C_Medm_rbf","C_Meda_rbf","C_Ant_rbf","S_LN_rbf","S_Th_rbf","S_Occ_rbf","S_Medp_rbf","S_Medm_rbf","S_Meda_rbf","S_Ant_rbf",
#                         "C_LN_rbv","C_Th_rbv","C_Occ_rbv","C_Medp_rbv","C_Medm_rbv","C_Meda_rbv","C_Ant_rbv","S_LN_rbv","S_Th_rbv","S_Occ_rbv","S_Medp_rbv","S_Medm_rbv","S_Meda_rbv","S_Ant_rbv",
#                         "C_LN_ttp","C_Th_ttp","C_Occ_ttp","C_Medp_ttp","C_Medm_ttp","C_Meda_ttp","C_Ant_ttp","S_LN_ttp","S_Th_ttp","S_Occ_ttp","S_Medp_ttp","S_Medm_ttp","S_Meda_ttp","S_Ant_ttp",
#                         "C_LN_tmax","C_Th_tmax","C_Occ_tmax","C_Medp_tmax","C_Medm_tmax","C_Meda_tmax","C_Ant_tmax","S_LN_tmax","S_Th_tmax","S_Occ_tmax","S_Medp_tmax","S_Medm_tmax","S_Meda_tmax","S_Ant_tmax",
#                         "C_LN_mtt","C_Th_mtt","C_Occ_mtt","C_Medp_mtt","C_Medm_mtt","C_Meda_mtt","C_Ant_mtt","S_LN_mtt","S_Th_mtt","S_Occ_mtt","S_Medp_mtt","S_Medm_mtt","S_Meda_mtt","S_Ant_mtt",
#                         "C_LN_tmip","C_Th_tmip","C_Occ_tmip","C_Medp_tmip","C_Medm_tmip","C_Meda_tmip","C_Ant_tmip","S_LN_tmip","S_Th_tmip","S_Occ_tmip","S_Medp_tmip","S_Medm_tmip","S_Meda_tmip","S_Ant_tmip")],
#                  verbose = T, variablewise = T)
# 
# 
# # consider the error
# imp$OOBerror
# dat_imp = imp$ximp
# 
# dat_imp = cbind(p_id=dat$p_id,dat_imp)
# 
# head(dat_imp)
# 
# save(dat_imp, file=paste0(dir, "data/data_wide_imp_all_before_therapy.R"))
# 
# 
# #### Train, Valid, Test
# 
# # sample train and test set (from the imputed data, can't predict otherwise)
# # for cross validation use different sets
# set.seed(123)
# idx = sample(1:nrow(dat_imp),nrow(dat_imp))
# idx
# test = list()
# seq(1,nrow(dat_imp),round(nrow(dat_imp)/5))
# test[[1]] = dat_imp[idx[1:45],]
# test[[2]] = dat_imp[idx[46:90],]
# test[[3]] = dat_imp[idx[91:135],]
# test[[4]] = dat_imp[idx[136:180],]
# test[[5]] = dat_imp[idx[181:nrow(dat_imp)],]
# train = list()
# train[[1]] = dat_imp[-idx[1:45],]
# train[[2]] = dat_imp[-idx[46:90],]
# train[[3]] = dat_imp[-idx[91:135],]
# train[[4]] = dat_imp[-idx[136:180],]
# train[[5]] = dat_imp[-idx[181:nrow(dat_imp)],]
# 
# 
# save(test, file=paste0(dir, "data/test_imp_all_before_therapy.R"))
# save(train, file=paste0(dir, "data/train_imp_all_before_therapy.R"))
# 
# # save as csv
# for(i in 1:5){
#   write.table(test[[i]], paste0(dir, "data/test",i,"_imp_all_before_therapy.csv"),  sep=",", row.names = F)
#   write.table(train[[i]], paste0(dir, "data/train",i,"_imp_all_before_therapy.csv"),  sep=",", row.names = F)
# }
# 
# 
# 
# 
# 
# 
# #### save the data from alle measures separately
# 
# var_names = c("rbf","rbv","ttp","tmax","mtt","tmip")
# 
# for(var in var_names){
#   load(paste0(dir, "/data/data_",var,".R"))
#   
#   # save the original data in dat0
#   dat0 = dat
#   
#   # transform data
#   dat = dcast(dat0, p_id+age+sex+independent_pre_stroke+nihss_bl
#               +intracranial_bleeding+tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
#               +sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr
#               +atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie+rf_smoker+rf_chd+rf_pavk+rf_tia_stroke
#               +infarct_side+additional_occlusion+lyse+time_to_imaging+time_to_groin_puncture
#               +volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar
#               +mrs_3months_binary+collateralization+anaesthesia
#               ~ region2, value.var="pooled_mean")
#   save(dat, file=paste0(dir,"/data/data_",var,"_wide_before_therapy.R"))
#   
#   set.seed(3004)
#   dat$time_to_imaging = as.numeric(dat$time_to_imaging)
#   dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
#   
#   dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
#   dat$time_to_imaging = as.numeric(dat$time_to_imaging)
#   imp = missForest(dat[,c("mrs_3months_binary","age","sex","nihss_bl","independent_pre_stroke","intracranial_bleeding","tah_pre_stroke","antikoagulation_pre_stroke","statin_pre_stroke",
#                           "antihypertensiva_pre_stroke","sys_bloodpressure_bl","dias_bloodpressure_bl","glucose_bl","hba1c","ldl","hdl","triglyceride","crp","inr",
#                           "atrial_fibrillation","rf_diabetes","rf_hypertonia","rf_hypercholesterinaemie","rf_smoker","rf_chd","rf_pavk","rf_tia_stroke","infarct_side",
#                           "additional_occlusion","lyse","time_to_imaging","rf_pavk","time_to_groin_puncture","collateralization","anaesthesia",
#                           "volume_adc","volume_tmax","volume_rbf_adc","volume_rbf_tmax","volume_rbv_adc","volume_rbv_tmax","volume_tar",
#                           "C_LN","C_Th","C_Occ","C_Medp","C_Medm","C_Meda","C_Ant",
#                           "S_LN","S_Th","S_Occ","S_Medp","S_Medm","S_Meda","S_Ant")],
#                    verbose = T, variablewise = T)
#   
#   
#   # consider the error
#   imp$OOBerror
#   dat_imp = imp$ximp
#   dat_imp = cbind(p_id=dat$p_id,dat_imp)
#   save(dat_imp, file=paste0(dir, "data/data_",var,"_wide_imp_before_therapy.R"))
#   
#   
#   #### Train, Valid, Test
#   
#   # sample train and test set (from the imputed data, can't predict otherwise)
#   # for cross validation use different sets
#   set.seed(123)
#   idx = sample(1:nrow(dat_imp),nrow(dat_imp))
#   idx
#   test = list()
#   seq(1,nrow(dat_imp),round(nrow(dat_imp)/5))
#   test[[1]] = dat_imp[idx[1:45],]
#   test[[2]] = dat_imp[idx[46:90],]
#   test[[3]] = dat_imp[idx[91:135],]
#   test[[4]] = dat_imp[idx[136:180],]
#   test[[5]] = dat_imp[idx[181:nrow(dat_imp)],]
#   train = list()
#   train[[1]] = dat_imp[-idx[1:45],]
#   train[[2]] = dat_imp[-idx[46:90],]
#   train[[3]] = dat_imp[-idx[91:135],]
#   train[[4]] = dat_imp[-idx[136:180],]
#   train[[5]] = dat_imp[-idx[181:nrow(dat_imp)],]
#   
#   save(test, file=paste0(dir, "data/test_",var,"_imp_before_therapy.R"))
#   save(train, file=paste0(dir, "data/train_",var,"_imp_before_therapy.R"))
#   
#   # save as csv
#   for(i in 1:5){
#     write.table(test[[i]], paste0(dir, "data/test",i,"_",var,"_imp_before_therapy.csv"),  sep=",", row.names = F)
#     write.table(train[[i]], paste0(dir, "data/train",i,"_",var,"_imp_before_therapy.csv"),  sep=",", row.names = F)
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# ########################################
# #### Impute variables after therapy ####
# ########################################
# 
# library(plyr)
# #### All parameters in one model
# 
# # We now want all variables within out dataset:
# 
# load(paste0(dir, "/data/data.R"))
# dat = dat_final
# 
# # combine the TOAST values
# # levels(dat$toast)
# # dat$toast = revalue(dat$toast, c("4"="4", "5"="3", "6"="3", "7"="3"))
# # 1 = makroangiopathie
# # 2 = cardioembolisch
# # 3 = ungekl?rt
# # 4 = ?
# # save the original data in dat0
# dat0 = dat
# 
# # change the names to identify each measure
# dat0$region2[dat0$measure=="rBF"] = paste0(dat0$region2[dat0$measure=="rBF"], "_rbf")
# dat0$region2[dat0$measure=="rBV"] = paste0(dat0$region2[dat0$measure=="rBV"], "_rbv")
# dat0$region2[dat0$measure=="TTP"] = paste0(dat0$region2[dat0$measure=="TTP"], "_ttp")
# dat0$region2[dat0$measure=="TMAX"] = paste0(dat0$region2[dat0$measure=="TMAX"], "_tmax")
# dat0$region2[dat0$measure=="MTT"] = paste0(dat0$region2[dat0$measure=="MTT"], "_mtt")
# dat0$region2[dat0$measure=="tMIP"] = paste0(dat0$region2[dat0$measure=="tMIP"], "_tmip")
# 
# # transform data
# # mrs, risk_factors, ..., region1_mean_volume, region2_mean_volume, ...
# dat = dcast(dat0, p_id+age+sex+independent_pre_stroke+nihss_bl
#             +intracranial_bleeding+tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
#             +sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr+anaesthesia
#             +toast+atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie+rf_smoker+rf_chd+rf_pavk+rf_tia_stroke+infarct_side+additional_occlusion
#             +lyse+time_to_imaging+time_to_groin_puncture+vessel_open_before_therapy+tici+tici_binary
#             +volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar
#             +mrs_3months_binary+collateralization
#             ~ region2, value.var="pooled_mean")
# 
# save(dat, file=paste0(dir,"/data/data_wide_all_after_therapy.R"))
# 
# 
# 
# #### Missing data
# 
# # consider the number of missings per column
# n_missing = apply(dat, 2, function(x) length(which(is.na(x))))
# n_missing
# barplot((n_missing/nrow(dat))*100, las=2, ylab="Missing values (%)",
#         ylim=c(0,100), cex.names=0.6, col="royalblue")
# 
# 
# #### Impute the missing data
# 
# set.seed(3004)
# dat$time_to_imaging = as.numeric(dat$time_to_imaging)
# dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
# 
# dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
# dat$time_to_imaging = as.numeric(dat$time_to_imaging)
# imp = missForest(dat[,c("mrs_3months_binary","age","sex","nihss_bl","independent_pre_stroke","intracranial_bleeding",
#                         "tah_pre_stroke","antikoagulation_pre_stroke","statin_pre_stroke",
#                         "antihypertensiva_pre_stroke","sys_bloodpressure_bl","dias_bloodpressure_bl","glucose_bl","hba1c","ldl","hdl","triglyceride","crp","inr","anaesthesia",
#                         "atrial_fibrillation","rf_diabetes","rf_hypertonia","rf_hypercholesterinaemie","rf_smoker","rf_pavk",
#                         "rf_chd","rf_pavk","rf_tia_stroke","infarct_side","additional_occlusion",
#                         "lyse","time_to_imaging","time_to_groin_puncture","vessel_open_before_therapy","tici","collateralization",
#                         "volume_adc","volume_tmax","volume_rbf_adc","volume_rbf_tmax","volume_rbv_adc","volume_rbv_tmax","volume_tar",
#                         "C_LN_rbf","C_Th_rbf","C_Occ_rbf","C_Medp_rbf","C_Medm_rbf","C_Meda_rbf","C_Ant_rbf","S_LN_rbf","S_Th_rbf","S_Occ_rbf","S_Medp_rbf","S_Medm_rbf","S_Meda_rbf","S_Ant_rbf",
#                         "C_LN_rbv","C_Th_rbv","C_Occ_rbv","C_Medp_rbv","C_Medm_rbv","C_Meda_rbv","C_Ant_rbv","S_LN_rbv","S_Th_rbv","S_Occ_rbv","S_Medp_rbv","S_Medm_rbv","S_Meda_rbv","S_Ant_rbv",
#                         "C_LN_ttp","C_Th_ttp","C_Occ_ttp","C_Medp_ttp","C_Medm_ttp","C_Meda_ttp","C_Ant_ttp","S_LN_ttp","S_Th_ttp","S_Occ_ttp","S_Medp_ttp","S_Medm_ttp","S_Meda_ttp","S_Ant_ttp",
#                         "C_LN_tmax","C_Th_tmax","C_Occ_tmax","C_Medp_tmax","C_Medm_tmax","C_Meda_tmax","C_Ant_tmax","S_LN_tmax","S_Th_tmax","S_Occ_tmax","S_Medp_tmax","S_Medm_tmax","S_Meda_tmax","S_Ant_tmax",
#                         "C_LN_mtt","C_Th_mtt","C_Occ_mtt","C_Medp_mtt","C_Medm_mtt","C_Meda_mtt","C_Ant_mtt","S_LN_mtt","S_Th_mtt","S_Occ_mtt","S_Medp_mtt","S_Medm_mtt","S_Meda_mtt","S_Ant_mtt",
#                         "C_LN_tmip","C_Th_tmip","C_Occ_tmip","C_Medp_tmip","C_Medm_tmip","C_Meda_tmip","C_Ant_tmip","S_LN_tmip","S_Th_tmip","S_Occ_tmip","S_Medp_tmip","S_Medm_tmip","S_Meda_tmip","S_Ant_tmip")],
#                  verbose = T, variablewise = T)
# 
# 
# # consider the error
# imp$OOBerror
# dat_imp = imp$ximp
# 
# dat_imp = cbind(p_id=dat$p_id,dat_imp)
# 
# head(dat_imp)
# 
# save(dat_imp, file=paste0(dir, "data/data_wide_imp_all_after_therapy.R"))
# 
# 
# #### Train, Valid, Test
# 
# # sample train and test set (from the imputed data, can't predict otherwise)
# # for cross validation use different sets
# set.seed(123)
# idx = sample(1:nrow(dat_imp),nrow(dat_imp))
# idx
# test = list()
# seq(1,nrow(dat_imp),round(nrow(dat_imp)/5))
# test[[1]] = dat_imp[idx[1:45],]
# test[[2]] = dat_imp[idx[46:90],]
# test[[3]] = dat_imp[idx[91:135],]
# test[[4]] = dat_imp[idx[136:180],]
# test[[5]] = dat_imp[idx[181:nrow(dat_imp)],]
# train = list()
# train[[1]] = dat_imp[-idx[1:45],]
# train[[2]] = dat_imp[-idx[46:90],]
# train[[3]] = dat_imp[-idx[91:135],]
# train[[4]] = dat_imp[-idx[136:180],]
# train[[5]] = dat_imp[-idx[181:nrow(dat_imp)],]
# 
# 
# save(test, file=paste0(dir, "data/test_imp_all_after_therapy.R"))
# save(train, file=paste0(dir, "data/train_imp_all_after_therapy.R"))
# 
# # save as csv
# for(i in 1:5){
#   write.table(test[[i]], paste0(dir, "data/test",i,"_imp_all_after_therapy.csv"),  sep=",", row.names = F)
#   write.table(train[[i]], paste0(dir, "data/train",i,"_imp_all_after_therapy.csv"),  sep=",", row.names = F)
# }
# 
# 
# 
# 
# 
# 
# #### save the data from alle measures separately
# 
# var_names = c("rbf","rbv","ttp","tmax","mtt","tmip")
# 
# for(var in var_names){
#   load(paste0(dir, "/data/data_",var,".R"))
#   
#   dat$toast = revalue(dat$toast, c("4"="4", "5"="3", "6"="3", "7"="3"))
#   # save the original data in dat0
#   dat0 = dat
#   
#   # transform data
#   # mrs, risk_factors, ..., region1_mean_volume, region2_mean_volume, ...
#   dat = dcast(dat0, p_id+age+sex+independent_pre_stroke+nihss_bl
#               +intracranial_bleeding+tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
#               +sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr+anaesthesia
#               +atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie+rf_smoker+rf_chd+rf_pavk+rf_tia_stroke+infarct_side+additional_occlusion
#               +lyse+time_to_imaging+time_to_groin_puncture+vessel_open_before_therapy+tici+tici_binary
#               +volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar
#               +mrs_3months_binary+collateralization
#               ~ region2, value.var="pooled_mean")
#   save(dat, file=paste0(dir,"/data/data_",var,"_wide_after_therapy.R"))
#   
#   set.seed(3004)
#   dat$time_to_imaging = as.numeric(dat$time_to_imaging)
#   dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
#   
#   dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
#   dat$time_to_imaging = as.numeric(dat$time_to_imaging)
#   imp = missForest(dat[,c("mrs_3months_binary","age","sex","nihss_bl",
#                           "independent_pre_stroke","intracranial_bleeding","tah_pre_stroke","antikoagulation_pre_stroke","statin_pre_stroke",
#                           "antihypertensiva_pre_stroke","sys_bloodpressure_bl","dias_bloodpressure_bl","glucose_bl","hba1c","ldl","hdl","triglyceride","crp","inr","anaesthesia",
#                           "atrial_fibrillation","rf_diabetes","rf_hypertonia","rf_hypercholesterinaemie","rf_smoker","rf_pavk",
#                           "rf_chd","rf_pavk","rf_tia_stroke","infarct_side","additional_occlusion",
#                           "lyse","time_to_imaging","time_to_groin_puncture","vessel_open_before_therapy","tici","collateralization",
#                           "volume_adc","volume_tmax","volume_rbf_adc","volume_rbf_tmax","volume_rbv_adc","volume_rbv_tmax","volume_tar",
#                           "C_LN","C_Th","C_Occ","C_Medp","C_Medm","C_Meda","C_Ant",
#                           "S_LN","S_Th","S_Occ","S_Medp","S_Medm","S_Meda","S_Ant")],
#                    verbose = T, variablewise = T)
#   
#   
#   # consider the error
#   imp$OOBerror
#   dat_imp = imp$ximp
#   dat_imp = cbind(p_id=dat$p_id,dat_imp)
#   save(dat_imp, file=paste0(dir, "data/data_",var,"_wide_imp_after_therapy.R"))
#   
#   
#   #### Train, Valid, Test
#   
#   # sample train and test set (from the imputed data, can't predict otherwise)
#   # for cross validation use different sets
#   set.seed(123)
#   idx = sample(1:nrow(dat_imp),nrow(dat_imp))
#   idx
#   test = list()
#   seq(1,nrow(dat_imp),round(nrow(dat_imp)/5))
#   test[[1]] = dat_imp[idx[1:45],]
#   test[[2]] = dat_imp[idx[46:90],]
#   test[[3]] = dat_imp[idx[91:135],]
#   test[[4]] = dat_imp[idx[136:180],]
#   test[[5]] = dat_imp[idx[181:nrow(dat_imp)],]
#   train = list()
#   train[[1]] = dat_imp[-idx[1:45],]
#   train[[2]] = dat_imp[-idx[46:90],]
#   train[[3]] = dat_imp[-idx[91:135],]
#   train[[4]] = dat_imp[-idx[136:180],]
#   train[[5]] = dat_imp[-idx[181:nrow(dat_imp)],]
#   
#   save(test, file=paste0(dir, "data/test_",var,"_imp_after_therapy.R"))
#   save(train, file=paste0(dir, "data/train_",var,"_imp_after_therapy.R"))
#   
#   # save as csv
#   for(i in 1:5){
#     write.table(test[[i]], paste0(dir, "data/test",i,"_",var,"_imp_after_therapy.csv"),  sep=",", row.names = F)
#     write.table(train[[i]], paste0(dir, "data/train",i,"_",var,"_imp_after_therapy.csv"),  sep=",", row.names = F)
#   }
# }
# 
# 
# 
# 
# ################################
# #### Impute variables NIHSS ####
# ################################
# 
# library(plyr)
# #### All parameters in one model
# 
# # We now want all variables within out dataset:
# 
# load(paste0(dir, "/data/data.R"))
# dat = dat_final
# 
# # combine the TOAST values
# # levels(dat$toast)
# # dat$toast = revalue(dat$toast, c("4"="4", "5"="3", "6"="3", "7"="3"))
# # 1 = makroangiopathie
# # 2 = cardioembolisch
# # 3 = ungekl?rt
# # 4 = ?
# # save the original data in dat0
# dat0 = dat
# 
# # change the names to identify each measure
# dat0$region2[dat0$measure=="rBF"] = paste0(dat0$region2[dat0$measure=="rBF"], "_rbf")
# dat0$region2[dat0$measure=="rBV"] = paste0(dat0$region2[dat0$measure=="rBV"], "_rbv")
# dat0$region2[dat0$measure=="TTP"] = paste0(dat0$region2[dat0$measure=="TTP"], "_ttp")
# dat0$region2[dat0$measure=="TMAX"] = paste0(dat0$region2[dat0$measure=="TMAX"], "_tmax")
# dat0$region2[dat0$measure=="MTT"] = paste0(dat0$region2[dat0$measure=="MTT"], "_mtt")
# dat0$region2[dat0$measure=="tMIP"] = paste0(dat0$region2[dat0$measure=="tMIP"], "_tmip")
# 
# # transform data
# # mrs, risk_factors, ..., region1_mean_volume, region2_mean_volume, ...
# dat = dcast(dat0, p_id+age+sex+independent_pre_stroke+nihss_bl+nihss_24h+nihss_diff_bl_24h
#             +intracranial_bleeding+tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
#             +sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr+anaesthesia
#             +atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie+rf_smoker+rf_chd+rf_pavk+rf_tia_stroke+infarct_side+additional_occlusion
#             +lyse+time_to_imaging+time_to_groin_puncture+vessel_open_before_therapy+tici+tici_binary
#             +volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar
#             +mrs_3months_binary+collateralization
#             ~ region2, value.var="pooled_mean")
# 
# save(dat, file=paste0(dir,"/data/data_wide_all_nihss.R"))
# 
# 
# 
# #### Missing data
# 
# # consider the number of missings per column
# n_missing = apply(dat, 2, function(x) length(which(is.na(x))))
# n_missing
# barplot((n_missing/nrow(dat))*100, las=2, ylab="Missing values (%)",
#         ylim=c(0,100), cex.names=0.6, col="royalblue")
# 
# 
# #### Impute the missing data
# 
# set.seed(3004)
# dat$time_to_imaging = as.numeric(dat$time_to_imaging)
# dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
# 
# dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
# dat$time_to_imaging = as.numeric(dat$time_to_imaging)
# imp = missForest(dat[,c("mrs_3months_binary","age","sex","nihss_bl","nihss_24h","nihss_diff_bl_24h","independent_pre_stroke","intracranial_bleeding",
#                         "tah_pre_stroke","antikoagulation_pre_stroke","statin_pre_stroke",
#                         "antihypertensiva_pre_stroke","sys_bloodpressure_bl","dias_bloodpressure_bl","glucose_bl","hba1c","ldl","hdl","triglyceride","crp","inr","anaesthesia",
#                         "atrial_fibrillation","rf_diabetes","rf_hypertonia","rf_hypercholesterinaemie","rf_smoker","rf_pavk",
#                         "rf_chd","rf_pavk","rf_tia_stroke","infarct_side","additional_occlusion",
#                         "lyse","time_to_imaging","time_to_groin_puncture","vessel_open_before_therapy","tici","collateralization",
#                         "volume_adc","volume_tmax","volume_rbf_adc","volume_rbf_tmax","volume_rbv_adc","volume_rbv_tmax","volume_tar",
#                         "C_LN_rbf","C_Th_rbf","C_Occ_rbf","C_Medp_rbf","C_Medm_rbf","C_Meda_rbf","C_Ant_rbf","S_LN_rbf","S_Th_rbf","S_Occ_rbf","S_Medp_rbf","S_Medm_rbf","S_Meda_rbf","S_Ant_rbf",
#                         "C_LN_rbv","C_Th_rbv","C_Occ_rbv","C_Medp_rbv","C_Medm_rbv","C_Meda_rbv","C_Ant_rbv","S_LN_rbv","S_Th_rbv","S_Occ_rbv","S_Medp_rbv","S_Medm_rbv","S_Meda_rbv","S_Ant_rbv",
#                         "C_LN_ttp","C_Th_ttp","C_Occ_ttp","C_Medp_ttp","C_Medm_ttp","C_Meda_ttp","C_Ant_ttp","S_LN_ttp","S_Th_ttp","S_Occ_ttp","S_Medp_ttp","S_Medm_ttp","S_Meda_ttp","S_Ant_ttp",
#                         "C_LN_tmax","C_Th_tmax","C_Occ_tmax","C_Medp_tmax","C_Medm_tmax","C_Meda_tmax","C_Ant_tmax","S_LN_tmax","S_Th_tmax","S_Occ_tmax","S_Medp_tmax","S_Medm_tmax","S_Meda_tmax","S_Ant_tmax",
#                         "C_LN_mtt","C_Th_mtt","C_Occ_mtt","C_Medp_mtt","C_Medm_mtt","C_Meda_mtt","C_Ant_mtt","S_LN_mtt","S_Th_mtt","S_Occ_mtt","S_Medp_mtt","S_Medm_mtt","S_Meda_mtt","S_Ant_mtt",
#                         "C_LN_tmip","C_Th_tmip","C_Occ_tmip","C_Medp_tmip","C_Medm_tmip","C_Meda_tmip","C_Ant_tmip","S_LN_tmip","S_Th_tmip","S_Occ_tmip","S_Medp_tmip","S_Medm_tmip","S_Meda_tmip","S_Ant_tmip")],
#                  verbose = T, variablewise = T)
# 
# 
# # consider the error
# imp$OOBerror
# dat_imp = imp$ximp
# 
# dat_imp = cbind(p_id=dat$p_id,dat_imp)
# 
# head(dat_imp)
# 
# save(dat_imp, file=paste0(dir, "data/data_wide_imp_all_nihss.R"))
# 
# 
# #### Train, Valid, Test
# 
# # sample train and test set (from the imputed data, can't predict otherwise)
# # for cross validation use different sets
# set.seed(123)
# idx = sample(1:nrow(dat_imp),nrow(dat_imp))
# idx
# test = list()
# seq(1,nrow(dat_imp),round(nrow(dat_imp)/5))
# test[[1]] = dat_imp[idx[1:45],]
# test[[2]] = dat_imp[idx[46:90],]
# test[[3]] = dat_imp[idx[91:135],]
# test[[4]] = dat_imp[idx[136:180],]
# test[[5]] = dat_imp[idx[181:nrow(dat_imp)],]
# train = list()
# train[[1]] = dat_imp[-idx[1:45],]
# train[[2]] = dat_imp[-idx[46:90],]
# train[[3]] = dat_imp[-idx[91:135],]
# train[[4]] = dat_imp[-idx[136:180],]
# train[[5]] = dat_imp[-idx[181:nrow(dat_imp)],]
# 
# 
# save(test, file=paste0(dir, "data/test_imp_all_nihss.R"))
# save(train, file=paste0(dir, "data/train_imp_all_nihss.R"))
# 
# # save as csv
# for(i in 1:5){
#   write.table(test[[i]], paste0(dir, "data/test",i,"_imp_all_nihss.csv"),  sep=",", row.names = F)
#   write.table(train[[i]], paste0(dir, "data/train",i,"_imp_all_nihss.csv"),  sep=",", row.names = F)
# }
# 
# 
# 
# 
# 
# 
# #### save the data from alle measures separately
# 
# var_names = c("rbf","rbv","ttp","tmax","mtt","tmip")
# 
# for(var in var_names){
#   load(paste0(dir, "/data/data_",var,".R"))
#   
#   # dat$toast = revalue(dat$toast, c("4"="4", "5"="3", "6"="3", "7"="3"))
#   # save the original data in dat0
#   dat0 = dat
#   
#   # transform data
#   # mrs, risk_factors, ..., region1_mean_volume, region2_mean_volume, ...
#   dat = dcast(dat0, p_id+age+sex+independent_pre_stroke+nihss_bl+nihss_24h+nihss_diff_bl_24h
#               +intracranial_bleeding+tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
#               +sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr+anaesthesia
#               +atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie+rf_smoker+rf_chd+rf_pavk+rf_tia_stroke+infarct_side+additional_occlusion
#               +lyse+time_to_imaging+time_to_groin_puncture+vessel_open_before_therapy+tici+tici_binary
#               +volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar
#               +mrs_3months_binary+collateralization
#               ~ region2, value.var="pooled_mean")
#   save(dat, file=paste0(dir,"/data/data_",var,"_wide_nihss.R"))
#   
#   set.seed(3004)
#   dat$time_to_imaging = as.numeric(dat$time_to_imaging)
#   dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
#   
#   dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
#   dat$time_to_imaging = as.numeric(dat$time_to_imaging)
#   imp = missForest(dat[,c("mrs_3months_binary","age","sex","nihss_bl","nihss_24h","nihss_diff_bl_24h",
#                           "independent_pre_stroke","intracranial_bleeding","tah_pre_stroke","antikoagulation_pre_stroke","statin_pre_stroke",
#                           "antihypertensiva_pre_stroke","sys_bloodpressure_bl","dias_bloodpressure_bl","glucose_bl","hba1c","ldl","hdl","triglyceride","crp","inr","anaesthesia",
#                           "atrial_fibrillation","rf_diabetes","rf_hypertonia","rf_hypercholesterinaemie","rf_smoker","rf_pavk",
#                           "rf_chd","rf_pavk","rf_tia_stroke","infarct_side","additional_occlusion",
#                           "lyse","time_to_imaging","time_to_groin_puncture","vessel_open_before_therapy","tici","collateralization",
#                           "volume_adc","volume_tmax","volume_rbf_adc","volume_rbf_tmax","volume_rbv_adc","volume_rbv_tmax","volume_tar",
#                           "C_LN","C_Th","C_Occ","C_Medp","C_Medm","C_Meda","C_Ant",
#                           "S_LN","S_Th","S_Occ","S_Medp","S_Medm","S_Meda","S_Ant")],
#                    verbose = T, variablewise = T)
#   
#   
#   # consider the error
#   imp$OOBerror
#   dat_imp = imp$ximp
#   dat_imp = cbind(p_id=dat$p_id,dat_imp)
#   save(dat_imp, file=paste0(dir, "data/data_",var,"_wide_imp_nihss.R"))
#   
#   
#   #### Train, Valid, Test
#   
#   # sample train and test set (from the imputed data, can't predict otherwise)
#   # for cross validation use different sets
#   set.seed(123)
#   idx = sample(1:nrow(dat_imp),nrow(dat_imp))
#   idx
#   test = list()
#   seq(1,nrow(dat_imp),round(nrow(dat_imp)/5))
#   test[[1]] = dat_imp[idx[1:45],]
#   test[[2]] = dat_imp[idx[46:90],]
#   test[[3]] = dat_imp[idx[91:135],]
#   test[[4]] = dat_imp[idx[136:180],]
#   test[[5]] = dat_imp[idx[181:nrow(dat_imp)],]
#   train = list()
#   train[[1]] = dat_imp[-idx[1:45],]
#   train[[2]] = dat_imp[-idx[46:90],]
#   train[[3]] = dat_imp[-idx[91:135],]
#   train[[4]] = dat_imp[-idx[136:180],]
#   train[[5]] = dat_imp[-idx[181:nrow(dat_imp)],]
#   
#   save(test, file=paste0(dir, "data/test_",var,"_imp_nihss.R"))
#   save(train, file=paste0(dir, "data/train_",var,"_imp_nihss.R"))
#   
#   # save as csv
#   for(i in 1:5){
#     write.table(test[[i]], paste0(dir, "data/test",i,"_",var,"_imp_nihss.csv"),  sep=",", row.names = F)
#     write.table(train[[i]], paste0(dir, "data/train",i,"_",var,"_imp_nihss.csv"),  sep=",", row.names = F)
#   }
# }
# 
# 
# 
# 
