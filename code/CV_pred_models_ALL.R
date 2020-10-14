rm(list=ls())
# We exclude mTICI and contralaters because they are measured AFTER therapy

library(randomForest)
library(ggplot2)
library(pdp)
library(keras)
library(gridExtra)
library(lattice)
library(latticeExtra)
library(ellipse)
library(missForest)

# set the path to the files
dir = "C:/Users/user/Dropbox/PhD/Stroke/PowerAnalyse_GewichteteMittelwerte_MixedModelSD_und_PredictionModel/"
# dir = "/Users/janne.hamann/Dropbox/StatistikOtherSideProjekt/"
setwd(dir)

# set the name for the variable of interest
var_name = "all"

# source the files with the functions we need
source(paste0(dir, "analyses/functions/roc_data.R"))
# source(paste0(dir, "analyses/functions/get_partial_plots.R"))
source(paste0(dir, "analyses/functions/change_var_names.R"))





#####################
#### Preparation ####
#####################

# Load the data with all variables included (with TICI and NIHSS 24h)
load(paste0(dir,"/data/data_wide_",var_name,"_nihss.R"))

n_missing = apply(dat, 2, function(x) length(which(is.na(x))))
n_missing
barplot((n_missing/nrow(dat))*100, las=2, ylab="Missing values (%)",
        ylim=c(0,100), cex.names=0.6, col="royalblue")


# How many complete cases?
vars_of_interest = c("mrs_3months_binary","age","sex","independent_pre_stroke","nihss_bl","tah_pre_stroke","antikoagulation_pre_stroke","statin_pre_stroke","antihypertensiva_pre_stroke",
                     "sys_bloodpressure_bl","dias_bloodpressure_bl","glucose_bl","hba1c","ldl","hdl","triglyceride","crp","inr",
                     "atrial_fibrillation","rf_diabetes","rf_hypertonia","rf_hypercholesterinaemie",
                     "rf_smoker","rf_chd","rf_pavk","rf_tia_stroke","infarct_side","additional_occlusion",
                     "lyse","time_to_imaging","time_to_groin_puncture","collateralization","anaesthesia","tici","nihss_24h",
                     "C_LN_rbf","C_Th_rbf","C_Occ_rbf","C_Medp_rbf","C_Medm_rbf","C_Meda_rbf","C_Ant_rbf",
                     "C_LN_rbv","C_Th_rbv","C_Occ_rbv","C_Medp_rbv","C_Medm_rbv","C_Meda_rbv","C_Ant_rbv",
                     "C_LN_tmax","C_Th_tmax","C_Occ_tmax","C_Medp_tmax","C_Medm_tmax","C_Meda_tmax","C_Ant_tmax",
                     "C_LN_tmip","C_Th_tmip","C_Occ_tmip","C_Medp_tmip","C_Medm_tmip","C_Meda_tmip","C_Ant_tmip",
                     "C_LN_ttp","C_Th_ttp","C_Occ_ttp","C_Medp_ttp","C_Medm_ttp","C_Meda_ttp","C_Ant_ttp",
                     "C_LN_mtt","C_Th_mtt","C_Occ_mtt","C_Medp_mtt","C_Medm_mtt","C_Meda_mtt","C_Ant_mtt",
                     "S_LN_rbf","S_Th_rbf","S_Occ_rbf","S_Medp_rbf","S_Medm_rbf","S_Meda_rbf","S_Ant_rbf",
                     "S_LN_rbv","S_Th_rbv","S_Occ_rbv","S_Medp_rbv","S_Medm_rbv","S_Meda_rbv","S_Ant_rbv",
                     "S_LN_tmax","S_Th_tmax","S_Occ_tmax","S_Medp_tmax","S_Medm_tmax","S_Meda_tmax","S_Ant_tmax",
                     "S_LN_tmip","S_Th_tmip","S_Occ_tmip","S_Medp_tmip","S_Medm_tmip","S_Meda_tmip","S_Ant_tmip",
                     "S_LN_ttp","S_Th_ttp","S_Occ_ttp","S_Medp_ttp","S_Medm_ttp","S_Meda_ttp","S_Ant_ttp",
                     "S_LN_mtt","S_Th_mtt","S_Occ_mtt","S_Medp_mtt","S_Medm_mtt","S_Meda_mtt","S_Ant_mtt",
                     "volume_adc","volume_tmax","volume_rbf_adc","volume_rbf_tmax","volume_rbv_adc","volume_rbv_tmax","volume_tar")
dat_compl=na.omit(dat[,vars_of_interest])

nrow(dat_compl) # n=108 complete cases



#### Define training and test sets for CV

# If a validation set is needed, the respective training set is split
# 222/5 --> 44+44+44+45+45=222
set.seed(123)
n = sample(1:nrow(dat),nrow(dat),replace=F)
idx_test1 = n[1:44]
idx_test2 = n[45:88]
idx_test3 = n[89:132]
idx_test4 = n[133:177]
idx_test5 = n[178:222]
# length(idx_test1)
# length(idx_test2)
# length(idx_test3)
# length(idx_test4)
# length(idx_test5)
paste("Total number of observations:", sum(length(idx_test1),length(idx_test2),length(idx_test3),length(idx_test4),length(idx_test5)))

# get training data
idx_train1 = which(!seq(1,nrow(dat),1) %in% idx_test1)
idx_train2 = which(!seq(1,nrow(dat),1) %in% idx_test2)
idx_train3 = which(!seq(1,nrow(dat),1) %in% idx_test3)
idx_train4 = which(!seq(1,nrow(dat),1) %in% idx_test4)
idx_train5 = which(!seq(1,nrow(dat),1) %in% idx_test5)


# impute data based on training sets with the missForest package
dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)
dat$time_to_imaging = as.numeric(dat$time_to_imaging)

train_na = list(train1=dat[idx_train1,],
                train2=dat[idx_train2,],
                train3=dat[idx_train3,],
                train4=dat[idx_train4,],
                train5=dat[idx_train5,])

test_na = list(test1=dat[idx_test1,],
               test2=dat[idx_test2,],
               test3=dat[idx_test3,],
               test4=dat[idx_test4,],
               test5=dat[idx_test5,])


# when the files do not exist already, run the imputation
if(!paste0("train_imputed_",var_name,".R") %in% list.files(paste0(dir,"data/CV/"))){
  train = list()
  test = list()
  for(i in 1:length(train_na)){
    # We can not use TICI and NIHSS when imputing at baseline
    test_tmp = test_na[[i]][,vars_of_interest]
    train_tmp = train_na[[i]][,vars_of_interest]
    
    # 1. Impute at baseline without TICI and NIHSS
    train_tmp_bl = train_na[[i]][,vars_of_interest[!vars_of_interest %in% c("tici","nihss_24h")]]
    test_tmp_bl = test_na[[i]][,vars_of_interest[!vars_of_interest %in% c("tici","nihss_24h")]]
    
    imp_bl = missForest(train_tmp_bl)
    # Impute each of the test samples based on the imputed training data
    for(j in 1:nrow(test_tmp_bl)){
      imp_bl_test = missForest(rbind(imp_bl$ximp,test_tmp_bl[j,]))
      test_tmp_bl[j,] = imp_bl_test$ximp[nrow(imp_bl_test$ximp),]
    }
    
    # 2. Use the imputed baseline dataset to predict missing values in TICI
    train_tmp_tici = data.frame(imp_bl$ximp, tici=train_na[[i]]$tici)
    test_tmp_tici = data.frame(test_tmp_bl, tici=test_na[[i]]$tici)
    
    imp_tici = missForest(train_tmp_tici)
    
    for(j in 1:nrow(test_tmp_tici)){
      imp_tici_test = missForest(rbind(imp_tici$ximp,test_tmp_tici[j,]))
      test_tmp_tici[j,] = imp_tici_test$ximp[nrow(imp_tici_test$ximp),]
    }
    
    # 3. Use the imputed dataset to predict missing values in NIHSS 24h
    train_tmp_nihss = data.frame(imp_tici$ximp, nihss_24h=train_na[[i]]$nihss_24h)
    test_tmp_nihss = data.frame(test_tmp_tici, nihss_24h=test_na[[i]]$nihss_24h)
    
    imp_nihss = missForest(train_tmp_nihss)
    
    for(j in 1:nrow(test_tmp_nihss)){
      imp_nihss_test = missForest(rbind(imp_nihss$ximp,test_tmp_nihss[j,]))
      test_tmp_nihss[j,] = imp_nihss_test$ximp[nrow(imp_nihss_test$ximp),]
    }
    
    # Save the imputed data in a list
    train[[i]] = imp_nihss$ximp
    test[[i]] = test_tmp_nihss
  }
  
  save(train, file=paste0(dir,"/data/CV/train_imputed_",var_name,".R"))
  save(test, file=paste0(dir,"data/CV/test_imputed_",var_name,".R"))
  
} else{
  load(paste0(dir,"/data/CV/train_imputed_",var_name,".R"))
  load(paste0(dir,"/data/CV/test_imputed_",var_name,".R"))
}

#### Define the formulars we want to look at
form_mri_c = formula(mrs_3months_binary ~ C_LN_rbf+C_Th_rbf+C_Occ_rbf+C_Medp_rbf+C_Medm_rbf+C_Meda_rbf+C_Ant_rbf
                     + C_LN_rbv+C_Th_rbv+C_Occ_rbv+C_Medp_rbv+C_Medm_rbv+C_Meda_rbv+C_Ant_rbv
                     + C_LN_ttp+C_Th_ttp+C_Occ_ttp+C_Medp_ttp+C_Medm_ttp+C_Meda_ttp+C_Ant_ttp
                     + C_LN_tmax+C_Th_tmax+C_Occ_tmax+C_Medp_tmax+C_Medm_tmax+C_Meda_tmax+C_Ant_tmax
                     + C_LN_mtt+C_Th_mtt+C_Occ_mtt+C_Medp_mtt+C_Medm_mtt+C_Meda_mtt+C_Ant_mtt
                     + C_LN_tmip+C_Th_tmip+C_Occ_tmip+C_Medp_tmip+C_Medm_tmip+C_Meda_tmip+C_Ant_tmip)

form_mri_s = formula(mrs_3months_binary ~ S_LN_rbf+S_Th_rbf+S_Occ_rbf+S_Medp_rbf+S_Medm_rbf+S_Meda_rbf+S_Ant_rbf
                     + S_LN_rbv+S_Th_rbv+S_Occ_rbv+S_Medp_rbv+S_Medm_rbv+S_Meda_rbv+S_Ant_rbv
                     + S_LN_ttp+S_Th_ttp+S_Occ_ttp+S_Medp_ttp+S_Medm_ttp+S_Meda_ttp+S_Ant_ttp
                     + S_LN_tmax+S_Th_tmax+S_Occ_tmax+S_Medp_tmax+S_Medm_tmax+S_Meda_tmax+S_Ant_tmax
                     + S_LN_mtt+S_Th_mtt+S_Occ_mtt+S_Medp_mtt+S_Medm_mtt+S_Meda_mtt+S_Ant_mtt
                     + S_LN_tmip+S_Th_tmip+S_Occ_tmip+S_Medp_tmip+S_Medm_tmip+S_Meda_tmip+S_Ant_tmip)

form_mri_sv = formula(mrs_3months_binary ~ S_LN_rbf+S_Th_rbf+S_Occ_rbf+S_Medp_rbf+S_Medm_rbf+S_Meda_rbf+S_Ant_rbf
                      + S_LN_rbv+S_Th_rbv+S_Occ_rbv+S_Medp_rbv+S_Medm_rbv+S_Meda_rbv+S_Ant_rbv
                      + S_LN_ttp+S_Th_ttp+S_Occ_ttp+S_Medp_ttp+S_Medm_ttp+S_Meda_ttp+S_Ant_ttp
                      + S_LN_tmax+S_Th_tmax+S_Occ_tmax+S_Medp_tmax+S_Medm_tmax+S_Meda_tmax+S_Ant_tmax
                      + S_LN_mtt+S_Th_mtt+S_Occ_mtt+S_Medp_mtt+S_Medm_mtt+S_Meda_mtt+S_Ant_mtt
                      + S_LN_tmip+S_Th_tmip+S_Occ_tmip+S_Medp_tmip+S_Medm_tmip+S_Meda_tmip+S_Ant_tmip
                      + volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar)

form_mri = formula(mrs_3months_binary ~ C_LN_rbf+C_Th_rbf+C_Occ_rbf+C_Medp_rbf+C_Medm_rbf+C_Meda_rbf+C_Ant_rbf
                   + C_LN_rbv+C_Th_rbv+C_Occ_rbv+C_Medp_rbv+C_Medm_rbv+C_Meda_rbv+C_Ant_rbv
                   + C_LN_ttp+C_Th_ttp+C_Occ_ttp+C_Medp_ttp+C_Medm_ttp+C_Meda_ttp+C_Ant_ttp
                   + C_LN_tmax+C_Th_tmax+C_Occ_tmax+C_Medp_tmax+C_Medm_tmax+C_Meda_tmax+C_Ant_tmax
                   + C_LN_mtt+C_Th_mtt+C_Occ_mtt+C_Medp_mtt+C_Medm_mtt+C_Meda_mtt+C_Ant_mtt
                   + C_LN_tmip+C_Th_tmip+C_Occ_tmip+C_Medp_tmip+C_Medm_tmip+C_Meda_tmip+C_Ant_tmip
                   + S_LN_rbf+S_Th_rbf+S_Occ_rbf+S_Medp_rbf+S_Medm_rbf+S_Meda_rbf+S_Ant_rbf
                   + S_LN_rbv+S_Th_rbv+S_Occ_rbv+S_Medp_rbv+S_Medm_rbv+S_Meda_rbv+S_Ant_rbv
                   + S_LN_ttp+S_Th_ttp+S_Occ_ttp+S_Medp_ttp+S_Medm_ttp+S_Meda_ttp+S_Ant_ttp
                   + S_LN_tmax+S_Th_tmax+S_Occ_tmax+S_Medp_tmax+S_Medm_tmax+S_Meda_tmax+S_Ant_tmax
                   + S_LN_mtt+S_Th_mtt+S_Occ_mtt+S_Medp_mtt+S_Medm_mtt+S_Meda_mtt+S_Ant_mtt
                   + S_LN_tmip+S_Th_tmip+S_Occ_tmip+S_Medp_tmip+S_Medm_tmip+S_Meda_tmip+S_Ant_tmip
                   + volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar)

form_bl = formula(mrs_3months_binary ~ age+sex+independent_pre_stroke+nihss_bl
                  + tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
                  + sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr
                  + atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie
                  + rf_smoker+rf_chd+rf_pavk+rf_tia_stroke+infarct_side+additional_occlusion
                  + lyse+time_to_imaging+time_to_groin_puncture+collateralization+anaesthesia)

form_bl_mri_c = formula(mrs_3months_binary ~ age+sex+independent_pre_stroke+nihss_bl
                        + tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
                        + sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr
                        + atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie
                        + rf_smoker+rf_chd+rf_pavk+rf_tia_stroke+infarct_side+additional_occlusion
                        + lyse+time_to_imaging+time_to_groin_puncture+collateralization+anaesthesia
                        + C_LN_rbf+C_Th_rbf+C_Occ_rbf+C_Medp_rbf+C_Medm_rbf+C_Meda_rbf+C_Ant_rbf
                        + C_LN_rbv+C_Th_rbv+C_Occ_rbv+C_Medp_rbv+C_Medm_rbv+C_Meda_rbv+C_Ant_rbv
                        + C_LN_ttp+C_Th_ttp+C_Occ_ttp+C_Medp_ttp+C_Medm_ttp+C_Meda_ttp+C_Ant_ttp
                        + C_LN_tmax+C_Th_tmax+C_Occ_tmax+C_Medp_tmax+C_Medm_tmax+C_Meda_tmax+C_Ant_tmax
                        + C_LN_mtt+C_Th_mtt+C_Occ_mtt+C_Medp_mtt+C_Medm_mtt+C_Meda_mtt+C_Ant_mtt
                        + C_LN_tmip+C_Th_tmip+C_Occ_tmip+C_Medp_tmip+C_Medm_tmip+C_Meda_tmip+C_Ant_tmip)

form_bl_mri_sv = formula(mrs_3months_binary ~ age+sex+independent_pre_stroke+nihss_bl
                         + tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
                         + sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr
                         + atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie
                         + rf_smoker+rf_chd+rf_pavk+rf_tia_stroke+infarct_side+additional_occlusion
                         + lyse+time_to_imaging+time_to_groin_puncture+collateralization+anaesthesia
                         + S_LN_rbf+S_Th_rbf+S_Occ_rbf+S_Medp_rbf+S_Medm_rbf+S_Meda_rbf+S_Ant_rbf
                         + S_LN_rbv+S_Th_rbv+S_Occ_rbv+S_Medp_rbv+S_Medm_rbv+S_Meda_rbv+S_Ant_rbv
                         + S_LN_ttp+S_Th_ttp+S_Occ_ttp+S_Medp_ttp+S_Medm_ttp+S_Meda_ttp+S_Ant_ttp
                         + S_LN_tmax+S_Th_tmax+S_Occ_tmax+S_Medp_tmax+S_Medm_tmax+S_Meda_tmax+S_Ant_tmax
                         + S_LN_mtt+S_Th_mtt+S_Occ_mtt+S_Medp_mtt+S_Medm_mtt+S_Meda_mtt+S_Ant_mtt
                         + S_LN_tmip+S_Th_tmip+S_Occ_tmip+S_Medp_tmip+S_Medm_tmip+S_Meda_tmip+S_Ant_tmip
                         + volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar)

form_bl_mri = formula(mrs_3months_binary ~ age+sex+independent_pre_stroke+nihss_bl
                      + tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
                      + sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr
                      + atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie
                      + rf_smoker+rf_chd+rf_pavk+rf_tia_stroke+infarct_side+additional_occlusion
                      + lyse+time_to_imaging+time_to_groin_puncture+collateralization+anaesthesia
                      + C_LN_rbf+C_Th_rbf+C_Occ_rbf+C_Medp_rbf+C_Medm_rbf+C_Meda_rbf+C_Ant_rbf
                      + C_LN_rbv+C_Th_rbv+C_Occ_rbv+C_Medp_rbv+C_Medm_rbv+C_Meda_rbv+C_Ant_rbv
                      + C_LN_ttp+C_Th_ttp+C_Occ_ttp+C_Medp_ttp+C_Medm_ttp+C_Meda_ttp+C_Ant_ttp
                      + C_LN_tmax+C_Th_tmax+C_Occ_tmax+C_Medp_tmax+C_Medm_tmax+C_Meda_tmax+C_Ant_tmax
                      + C_LN_mtt+C_Th_mtt+C_Occ_mtt+C_Medp_mtt+C_Medm_mtt+C_Meda_mtt+C_Ant_mtt
                      + C_LN_tmip+C_Th_tmip+C_Occ_tmip+C_Medp_tmip+C_Medm_tmip+C_Meda_tmip+C_Ant_tmip
                      + S_LN_rbf+S_Th_rbf+S_Occ_rbf+S_Medp_rbf+S_Medm_rbf+S_Meda_rbf+S_Ant_rbf
                      + S_LN_rbv+S_Th_rbv+S_Occ_rbv+S_Medp_rbv+S_Medm_rbv+S_Meda_rbv+S_Ant_rbv
                      + S_LN_ttp+S_Th_ttp+S_Occ_ttp+S_Medp_ttp+S_Medm_ttp+S_Meda_ttp+S_Ant_ttp
                      + S_LN_tmax+S_Th_tmax+S_Occ_tmax+S_Medp_tmax+S_Medm_tmax+S_Meda_tmax+S_Ant_tmax
                      + S_LN_mtt+S_Th_mtt+S_Occ_mtt+S_Medp_mtt+S_Medm_mtt+S_Meda_mtt+S_Ant_mtt
                      + S_LN_tmip+S_Th_tmip+S_Occ_tmip+S_Medp_tmip+S_Medm_tmip+S_Meda_tmip+S_Ant_tmip
                      + volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar)

form_expert = formula(mrs_3months_binary ~ age+nihss_bl+rf_hypertonia+rf_diabetes+time_to_groin_puncture+
                        sys_bloodpressure_bl+collateralization+S_Medm_rbf+volume_adc+volume_tar+rf_smoker+lyse+rf_tia_stroke)


for(r in 1:5){
  ##########################################
  #### Variable Selction: Random Forest ####
  ##########################################
  
  # We use the random forest for variable selection.
  # It tells us how important the different variables are
  # and we take the 10 variables which are most important for prediction
  
  
  set.seed(3004)
  
  
  #### Define RF models
  
  # We fit multiple trees.Try all possible splits and calculate a score (e.g. Gini). Take the split where the score is minimal and repeat 
  # the procedure. The splits can only be horizontal or vertical lines. 
  # In the random forest, each tree uses a bootstrap sample of the training data. The remaining data is used for testing. 
  # At each split, we choose a prespecified number of variables randomly to construct the tree. The trees are overfitted (to reduce bias). 
  # The final outcome is the class which the trees predicted most often (reduces variance).
  
  
  # MRI data: Information from contralteral stroke side
  rf_mri_c = randomForest(form_mri_c, data=train[[r]], ntree=500, importance=T)
  
  # MRI data: Information from stroke side
  rf_mri_s = randomForest(form_mri_s, data=train[[r]], ntree=500, importance=T)
  
  # MRI data: Information from stroke side + volume
  rf_mri_sv = randomForest(form_mri_sv, data=train[[r]], ntree=500, importance=T)
  
  # MRI data: Information from stroke + contralateral side
  rf_mri = randomForest(form_mri, data=train[[r]], ntree=500, importance=T)
  
  # Baseline data: Information from stroke + contralateral side
  rf_bl = randomForest(form_bl, data=train[[r]], ntree=500, importance=T)
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri_c = randomForest(form_bl_mri_c, data=train[[r]], ntree=500, importance=T)
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri_sv = randomForest(form_bl_mri_sv, data=train[[r]], ntree=500, importance=T)
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri = randomForest(form_bl_mri, data=train[[r]], ntree=500, importance=T)
  
  # Expert
  rf_expert = randomForest(form_expert, data=train[[r]], ntree=500, importance=T)
  
  
  
  # tab = c(mri_c_test, mri_s_test, mri_sv_test, mri_test, bl_test, bl_mri_c_test, bl_mri_sv_test, bl_mri_test)
  # names(tab) = c("MRI (contralat)", "MRI (stroke wo vol)", "MRI (stroke)","MRI (stroke+contralat)","Baseline","Baseline + MRI (contralat)",
  #                "Baseline + MRI (stroke)","Baseline + MRI (stroke+contralat)")
  # tab
  
  
  
  #### Save level names for plotting
  rf_bl_plot = change_var_names_RF(rf_bl)
  rf_bl_mri_c_plot = change_var_names_RF(rf_bl_mri_c)
  rf_bl_mri_sv_plot = change_var_names_RF(rf_bl_mri_sv)
  rf_bl_mri_plot = change_var_names_RF(rf_bl_mri)
  train_plot = change_var_names_DF(train[[r]])
  
  
  
  #### Variance importance plots
  
  # MeanDecreaseAccuracy: Get OOb performance of tree, than permute variable of interest randomly and measure the OOB performance again
  # Then, calculate the difference between the two
  # Negative: Random permutation of the variable is better than the variable --> unimportant
  
  # MeanDecreaseGini: The second measure is the total decrease in node impurities from splitting on the variable, averaged over all trees. 
  # For classification, the node impurity is measured by the Gini index. For regression, it is measured by residual sum of squares.
  # LH: Improvement of the score at the split where the variable is used --> average over all trees that include the variable
  
  par(mfrow=c(1,1))
  pdf(paste0(dir,"images/CV/run",r,"/var_imp_plots_RF_VAR_SELECTION_",var_name,".pdf"), width=20, height=10)
  varImpPlot(rf_mri_c, main="MRI (contralat)", cex=1.8, pch=16)
  varImpPlot(rf_mri_s, main="MRI (stroke wo vol)", cex=1.8, pch=16)
  varImpPlot(rf_mri_sv, main="MRI (stroke)", cex=1.8, pch=16)
  varImpPlot(rf_mri, main="MRI (stroke+contralat)", cex=1.8, pch=16)
  varImpPlot(rf_bl, main="Baseline", cex=1.8, pch=16)
  varImpPlot(rf_bl_mri_c, main="Baseline + MRI (contralat)", cex=1.8, pch=16)
  varImpPlot(rf_bl_mri_sv, main="Baseline + MRI (stroke)", cex=1.8, pch=16)
  varImpPlot(rf_bl_mri, main="Baseline + MRI (stroke+contralat)", cex=1.8, pch=16)
  varImpPlot(rf_expert, main="Expert", cex=1.8, pch=16)
  dev.off()
  
  
  xlabels = data.frame(var=c("C_LN_rbf","C_Th_rbf","C_Occ_rbf","C_Medp_rbf","C_Medm_rbf","C_Meda_rbf","C_Ant_rbf",
                             "C_LN_rbv","C_Th_rbv","C_Occ_rbv","C_Medp_rbv","C_Medm_rbv","C_Meda_rbv","C_Ant_rbv",
                             "C_LN_tmax","C_Th_tmax","C_Occ_tmax","C_Medp_tmax","C_Medm_tmax","C_Meda_tmax","C_Ant_tmax",
                             "C_LN_tmip","C_Th_tmip","C_Occ_tmip","C_Medp_tmip","C_Medm_tmip","C_Meda_tmip","C_Ant_tmip",
                             "C_LN_ttp","C_Th_ttp","C_Occ_ttp","C_Medp_ttp","C_Medm_ttp","C_Meda_ttp","C_Ant_ttp",
                             "C_LN_mtt","C_Th_mtt","C_Occ_mtt","C_Medp_mtt","C_Medm_mtt","C_Meda_mtt","C_Ant_mtt",
                             "S_LN_rbf","S_Th_rbf","S_Occ_rbf","S_Medp_rbf","S_Medm_rbf","S_Meda_rbf","S_Ant_rbf",
                             "S_LN_rbv","S_Th_rbv","S_Occ_rbv","S_Medp_rbv","S_Medm_rbv","S_Meda_rbv","S_Ant_rbv",
                             "S_LN_tmax","S_Th_tmax","S_Occ_tmax","S_Medp_tmax","S_Medm_tmax","S_Meda_tmax","S_Ant_tmax",
                             "S_LN_tmip","S_Th_tmip","S_Occ_tmip","S_Medp_tmip","S_Medm_tmip","S_Meda_tmip","S_Ant_tmip",
                             "S_LN_ttp","S_Th_ttp","S_Occ_ttp","S_Medp_ttp","S_Medm_ttp","S_Meda_ttp","S_Ant_ttp",
                             "S_LN_mtt","S_Th_mtt","S_Occ_mtt","S_Medp_mtt","S_Medm_mtt","S_Meda_mtt","S_Ant_mtt",
                             "age","sex","independent_pre_stroke","nihss_bl",
                             "tah_pre_stroke","antikoagulation_pre_stroke",
                             "statin_pre_stroke","antihypertensiva_pre_stroke",
                             "sys_bloodpressure_bl","dias_bloodpressure_bl",
                             "glucose_bl","hba1c","ldl","hdl","triglyceride","crp","inr",
                             "atrial_fibrillation","rf_diabetes","rf_hypertonia","rf_hypercholesterinaemie",
                             "rf_smoker","rf_chd","rf_pavk",
                             "rf_tia_stroke","infarct_side","additional_occlusion",
                             "lyse","time_to_imaging","time_to_groin_puncture",
                             "collateralization","anaesthesia"),
                       name=c("LN - contralateral side (rCBF)","Th - contralateral side (rCBF)","Ant - contralateral side (rCBF)","M ant - contralateral side (rCBF)","M med - contralateral side (rCBF)","M post - contralateral side (rCBF)","Post - contralateral side (rCBF)",
                              "LN - contralateral side (rCBV)","Th - contralateral side (rCBV)","Ant - contralateral side (rCBV)","M ant - contralateral side (rCBV)","M med - contralateral side (rCBV)","M post - contralateral side (rCBV)","Post - contralateral side (rCBV)",
                              "LN - contralateral side (TMAX)","Th - contralateral side (TMAX)","Ant - contralateral side (TMAX)","M ant - contralateral side (TMAX)","M med - contralateral side (TMAX)","M post - contralateral side (TMAX)","Post - contralateral side (TMAX)",
                              "LN - contralateral side (tMIP)","Th - contralateral side (tMIP)","Ant - contralateral side (tMIP)","M ant - contralateral side (tMIP)","M med - contralateral side (tMIP)","M post - contralateral side (tMIP)","Post - contralateral side (tMIP)",
                              "LN - contralateral side (TTP)","Th - contralateral side (TTP)","Ant - contralateral side (TTP)","M ant - contralateral side (TTP)","M med - contralateral side (TTP)","M post - contralateral side (TTP)","Post - contralateral side (TTP)",
                              "LN - contralateral side (MTT)","Th - contralateral side (MTT)","Ant - contralateral side (MTT)","M ant - contralateral side (MTT)","M med - contralateral side (MTT)","M post - contralateral side (MTT)","Post - contralateral side (MTT)",
                              "LN - stroke side (rCBF)","Th - stroke side (rCBF)","Ant - stroke side (rCBF)","M ant - stroke side (rCBF)","M med - stroke side (rCBF)","M post - stroke side (rCBF)","Post - stroke side (rCBF)",
                              "LN - stroke side (rCBV)","Th - stroke side (rCBV)","Ant - stroke side (rCBV)","M ant - stroke side (rCBV)","M med - stroke side (rCBV)","M post - stroke side (rCBV)","Post - stroke side (rCBV)",
                              "LN - stroke side (TMAX)","Th - stroke side (TMAX)","Ant - stroke side (TMAX)","M ant - stroke side (TMAX)","M med - stroke side (TMAX)","M post - stroke side (TMAX)","Post - stroke side (TMAX)",
                              "LN - stroke side (tMIP)","Th - stroke side (tMIP)","Ant - stroke side (tMIP)","M ant - stroke side (tMIP)","M med - stroke side (tMIP)","M post - stroke side (tMIP)","Post - stroke side (tMIP)",
                              "LN - stroke side (TTP)","Th - stroke side (TTP)","Ant - stroke side (TTP)","M ant - stroke side (TTP)","M med - stroke side (TTP)","M post - stroke side (TTP)","Post - stroke side (TTP)",
                              "LN - stroke side (MTT)","Th - stroke side (MTT)","Ant - stroke side (MTT)","M ant - stroke side (MTT)","M med - stroke side (MTT)","M post - stroke side (MTT)","Post - stroke side (MTT)",
                              "Age (y)","Sex","Independent before stroke","NIHSS baseline",
                              "Previous antiplatelet therapy","Previous oral anticoagulation",
                              "Previous statin therapy","Previous antihypertensive therapy",
                              "Systolic blood pressure (mmHg)","Diastolic blood pressure (mmHg)",
                              "Glucose (mmol/l)","HbA1c (%)","LDL (mmol/l)","HDL (mmol/l)","TG (mmol/l)","CRP (mg/l)","INR",
                              "Atrial Fibrillation","Diabetes","Hypertension","Dyslipidemia",
                              "Smoker","Coronary heart disease","Peripheral arterial occlusive disease",
                              "Past ischemic events","Infarct side","DO NOT SHOW ICA-M1 or M1-M2-M3",
                              "IVT","Onset to imaging (min)","Onset to groin puncture (min)",
                              "Collateralization status","General anaesthesia"))
  
  
  
  
  # #### Partial dependency plots
  # if(r==1){
  #   models = list(rf_mri_c, rf_mri_s, rf_mri_sv, rf_mri, 
  #                 rf_bl_plot, rf_bl_mri_c_plot, rf_bl_mri_sv_plot, rf_bl_mri_plot)
  #   img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_RF_mri_contralat_VAR_SELECTION_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_mri_stroke_wo_vol_VAR_SELECTION_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_mri_stroke_VAR_SELECTION_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_mri_stroke_contralat_VAR_SELECTION_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_VAR_SELECTION_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_contralat_VAR_SELECTION_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_stroke_VAR_SELECTION_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_stroke_contralat_VAR_SELECTION_",var_name,".pdf"))
  #   data = list(train[[r]],train[[r]],train[[r]],train[[r]],train_plot,train_plot,train_plot,train_plot)
  #   
  #   for(l in 1:length(img_names)){
  #     print(l)
  #     pdf(paste0(dir,img_names[l]))
  #     
  #     for(i in attributes(models[[l]]$terms)$term.labels){
  #       print(i)
  #       info = partial(models[[l]], pred.var = i, ice = T, which.class = 2, rug=T, prob=T, train=data[[l]])
  #       data[[l]]$no = seq(1,dim(data[[l]])[1],1)
  #       dat_plot = merge(info, data[[l]], by.x="yhat.id", by.y="no")
  #       
  #       # find yhat which is closest to the predicted variables
  #       for(j in unique(dat_plot$yhat.id)){
  #         pat = dat_plot[dat_plot$yhat.id == j,]
  #         if(class(dat_plot[,paste0(i,".x")])=="factor"){
  #           k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
  #         } else{
  #           k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #         }
  #         dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #       }
  #       
  #       p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                       ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==i)]),
  #                       alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #       print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       
  #     }
  #     dev.off()
  #   }
  # }
  
  
  
  
  
  
  
  
  
  #############################################
  #### Prediction Models BL: Random Forest ####
  #############################################
  
  
  # Get the formular with the most important predictors as input
  get_formula = function(mod, n){
    return(as.formula(paste("mrs_3months_binary", paste(names(sort(importance(mod)[,"MeanDecreaseAccuracy"], decreasing=T))[1:n],collapse="+"), sep="~")))
  }
  
  
  #### Define the formulars we want to look at
  form_mri_c_bl = get_formula(rf_mri_c, 7)
  form_mri_s_bl = get_formula(rf_mri_s, 7)
  form_mri_sv_bl = get_formula(rf_mri_sv, 10)
  form_mri_bl = get_formula(rf_mri, 10)
  form_bl_bl = get_formula(rf_bl, 10)
  form_bl_mri_c_bl = get_formula(rf_bl_mri_c, 10)
  form_bl_mri_sv_bl = get_formula(rf_bl_mri_sv, 10)
  form_bl_mri_bl = get_formula(rf_bl_mri, 10)
  form_expert_bl = form_expert
  
  
  set.seed(3004)
  
  
  #### Models
  
  # MRI data: Information from contralteral stroke side
  rf_mri_c = randomForest(form_mri_c_bl, data=train[[r]], ntree=500, importance=T)
  rf_pred_mri_c_test = predict(rf_mri_c, newdata = test[[r]])
  rf_pred_mri_c_test_prob = predict(rf_mri_c, type="prob", newdata=test[[r]])
  (mri_c_test = mean(test[[r]]$mrs_3months_binary == rf_pred_mri_c_test))
  
  
  # MRI data: Information from stroke side
  rf_mri_s = randomForest(form_mri_s_bl, data=train[[r]], ntree=500, importance=T)
  rf_pred_mri_s_test = predict(rf_mri_s, newdata = test[[r]])
  rf_pred_mri_s_test_prob = predict(rf_mri_s, type="prob", newdata = test[[r]])
  (mri_s_test = mean(test[[r]]$mrs_3months_binary == rf_pred_mri_s_test))
  
  # MRI data: Information from stroke side + volume
  rf_mri_sv = randomForest(form_mri_sv_bl, data=train[[r]], ntree=500, importance=T)
  rf_pred_mri_sv_test = predict(rf_mri_sv, newdata = test[[r]])
  rf_pred_mri_sv_test_prob = predict(rf_mri_sv, type="prob", newdata = test[[r]])
  (mri_sv_test = mean(test[[r]]$mrs_3months_binary == rf_pred_mri_sv_test))
  
  # MRI data: Information from stroke + contralateral side
  rf_mri = randomForest(form_mri_bl, data=train[[r]], ntree=500, importance=T)
  rf_pred_mri_test = predict(rf_mri, newdata = test[[r]])
  rf_pred_mri_test_prob = predict(rf_mri, type="prob", newdata = test[[r]])
  (mri_test = mean(test[[r]]$mrs_3months_binary == rf_pred_mri_test))
  
  # Baseline data: Information from stroke + contralateral side
  rf_bl = randomForest(form_bl_bl, data=train[[r]], ntree=500, importance=T)
  rf_pred_bl_test = predict(rf_bl, newdata = test[[r]])
  rf_pred_bl_test_prob = predict(rf_bl, type="prob", newdata = test[[r]])
  (bl_test = mean(test[[r]]$mrs_3months_binary == rf_pred_bl_test))
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri_c = randomForest(form_bl_mri_c_bl, data=train[[r]], ntree=500, importance=T)
  rf_pred_bl_mri_c_test = predict(rf_bl_mri_c, newdata = test[[r]])
  rf_pred_bl_mri_c_test_prob = predict(rf_bl_mri_c, type="prob", newdata = test[[r]])
  (bl_mri_c_test = mean(test[[r]]$mrs_3months_binary == rf_pred_bl_mri_c_test))
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri_sv = randomForest(form_bl_mri_sv_bl, data=train[[r]], ntree=500, importance=T)
  rf_pred_bl_mri_sv_test = predict(rf_bl_mri_sv, newdata = test[[r]])
  rf_pred_bl_mri_sv_test_prob = predict(rf_bl_mri_sv, type="prob", newdata = test[[r]])
  (bl_mri_sv_test = mean(test[[r]]$mrs_3months_binary == rf_pred_bl_mri_sv_test))
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri = randomForest(form_bl_mri_bl, data=train[[r]], ntree=500, importance=T)
  rf_pred_bl_mri_test = predict(rf_bl_mri, newdata = test[[r]])
  rf_pred_bl_mri_test_prob = predict(rf_bl_mri, type="prob", newdata = test[[r]])
  (bl_mri_test = 1-mean(test[[r]]$mrs_3months_binary != rf_pred_bl_mri_test))
  
  # Expert
  rf_expert = randomForest(form_expert_bl, data=train[[r]], ntree=500, importance=T)
  rf_pred_expert_test = predict(rf_expert, newdata = test[[r]])
  rf_pred_expert_test_prob = predict(rf_expert, type="prob", newdata = test[[r]])
  (expert_test = 1-mean(test[[r]]$mrs_3months_binary != rf_pred_expert_test))
  
  
  
  tab = c(mri_c_test, mri_s_test, mri_sv_test, mri_test, bl_test, bl_mri_c_test, bl_mri_sv_test, bl_mri_test, expert_test)
  names(tab) = c("MRI (contralat)", "MRI (stroke wo vol)", "MRI (stroke)","MRI (stroke+contralat)","Baseline","Baseline + MRI (contralat)",
                 "Baseline + MRI (stroke)","Baseline + MRI (stroke+contralat)", "Expert")
  tab
  
  test_pred_rf = data.frame(test[[r]], mri_c = rf_pred_mri_c_test_prob[,2], mri_s = rf_pred_mri_s_test_prob[,2], 
                            mri_sv = rf_pred_mri_sv_test_prob[,2], mri=rf_pred_mri_test_prob[,2], 
                            bl = rf_pred_bl_test_prob[,2], bl_mri_c = rf_pred_bl_mri_c_test_prob[,2], 
                            bl_mri_sv = rf_pred_bl_mri_sv_test_prob[,2], bl_mri = rf_pred_bl_mri_test_prob[,2],
                            expert = rf_pred_expert_test_prob[,2])
  save(test_pred_rf, file=paste0(dir,"data/CV/run",r,"/test_pred_rf_BL_",var_name,".R"))
  
  
  
  #### Save level names for plotting
  rf_bl_plot = change_var_names_RF(rf_bl)
  rf_bl_mri_c_plot = change_var_names_RF(rf_bl_mri_c)
  rf_bl_mri_sv_plot = change_var_names_RF(rf_bl_mri_sv)
  rf_bl_mri_plot = change_var_names_RF(rf_bl_mri)
  rf_expert_plot = change_var_names_RF(rf_expert)
  train_plot = change_var_names_DF(train[[r]])
  
  
  #### Partial Dependency Plots:
  # if(r==1){
  #   models = list(rf_mri_c, rf_mri_s, rf_mri_sv, rf_mri,
  #                 rf_bl_plot, rf_bl_mri_c_plot, rf_bl_mri_sv_plot, rf_bl_mri_plot, rf_expert_plot)
  #   img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_RF_mri_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_mri_stroke_wo_vol_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_mri_stroke_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_mri_stroke_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_stroke_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_stroke_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_expert_BL_",var_name,".pdf"))
  #   data = list(train[[r]],train[[r]],train[[r]],train[[r]],train_plot,train_plot,train_plot,train_plot,train_plot)
  #   
  #   for(l in 1:length(img_names)){
  #     
  #     pdf(paste0(dir,img_names[l]))
  #     
  #     for(i in attributes(models[[l]]$terms)$term.labels){
  #       print(i)
  #       info = partial(models[[l]], pred.var = i, ice = T, which.class = 2, rug=T, prob=T, train=data[[l]])
  #       data[[l]]$no = seq(1,dim(data[[l]])[1],1)
  #       dat_plot = merge(info, data[[l]], by.x="yhat.id", by.y="no")
  #       
  #       # find yhat which is closest to the predicted variables
  #       for(j in unique(dat_plot$yhat.id)){
  #         pat = dat_plot[dat_plot$yhat.id == j,]
  #         if(class(dat_plot[,paste0(i,".x")])=="factor"){
  #           k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
  #         } else{
  #           k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #         }
  #         dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #       }
  #       
  #       p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                       ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==i)]),
  #                       alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #       print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       
  #     }
  #     dev.off()
  #   }
  # }
  
  
  
  
  
  
  
  
  
  # ###################################################
  # #### Prediction Models BL: Logistic regression ####
  # ###################################################
  # 
  # 
  # # run models
  # logreg_mri_c = glm(form_mri_c_bl, data=train[[r]], family = binomial)
  # logreg_mri_s = glm(form_mri_s_bl, data=train[[r]], family = binomial)
  # logreg_mri_sv = glm(form_mri_sv_bl, data=train[[r]], family = binomial)
  # logreg_mri = glm(form_mri_bl, data=train[[r]], family = binomial)
  # logreg_bl = glm(form_bl_bl, data=train[[r]], family = binomial)
  # logreg_bl_mri_c = glm(form_bl_mri_c_bl, data=train[[r]], family = binomial)
  # logreg_bl_mri_sv = glm(form_bl_mri_sv_bl, data=train[[r]], family = binomial)
  # logreg_bl_mri = glm(form_bl_mri_bl, data=train[[r]], family = binomial)
  # logreg_expert = glm(form_expert_bl, data=train[[r]], family = binomial)
  # 
  # # predictions
  # pred_mri_c = predict(logreg_mri_c, newdata=test[[r]], type="response")
  # pred_mri_s = predict(logreg_mri_s, newdata=test[[r]], type="response")  
  # pred_mri_sv = predict(logreg_mri_sv, newdata=test[[r]], type="response")  
  # pred_mri = predict(logreg_mri, newdata=test[[r]], type="response")
  # pred_bl = predict(logreg_bl, newdata=test[[r]], type="response")
  # pred_bl_mri_c = predict(logreg_bl_mri_c, newdata=test[[r]], type="response")
  # pred_bl_mri_sv = predict(logreg_bl_mri_sv, newdata=test[[r]], type="response")
  # pred_bl_mri = predict(logreg_bl_mri, newdata=test[[r]], type="response")
  # pred_expert = predict(logreg_expert, newdata=test[[r]], type="response")
  # 
  # test_pred_logreg = data.frame(test[[r]], mri_c = pred_mri_c, mri_s = pred_mri_s, mri_sv = pred_mri_sv,
  #                               mri=pred_mri, bl = pred_bl, bl_mri_c = pred_bl_mri_c, 
  #                               bl_mri_sv = pred_bl_mri_sv, bl_mri = pred_bl_mri,
  #                               expert = pred_expert)
  # save(test_pred_logreg, file=paste0(dir,"data/CV/run",r,"/test_pred_logreg_BL_",var_name,".R"))
  # 
  # 
  # # probabilities to 0,1
  # pred_mri_c_class = ifelse(pred_mri_c>0.5,1,0)
  # pred_mri_s_class = ifelse(pred_mri_s>0.5,1,0)
  # pred_mri_sv_class = ifelse(pred_mri_sv>0.5,1,0)
  # pred_mri_class = ifelse(pred_mri>0.5,1,0)
  # pred_bl_class = ifelse(pred_bl>0.5,1,0)
  # pred_bl_mri_c_class = ifelse(pred_bl_mri_c>0.5,1,0)
  # pred_bl_mri_sv_class = ifelse(pred_bl_mri_sv>0.5,1,0)
  # pred_bl_mri_class = ifelse(pred_bl_mri>0.5,1,0)
  # pred_expert_class = ifelse(pred_expert>0.5,1,0)
  # 
  # tab = c(mean(pred_mri_c_class == test[[r]]$mrs_3months_binary), mean(pred_mri_s_class == test[[r]]$mrs_3months_binary), mean(pred_mri_sv_class == test[[r]]$mrs_3months_binary), mean(pred_mri_class == test[[r]]$mrs_3months_binary),
  #         mean(pred_bl_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_c_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_sv_class == test[[r]]$mrs_3months_binary),
  #         mean(pred_bl_mri_class == test[[r]]$mrs_3months_binary), mean(pred_expert_class == test[[r]]$mrs_3months_binary))
  # names(tab) = c("MRI (contralat)", "MRI (stroke wo vol)", "MRI (stroke)","MRI (stroke+contralat)","Baseline","Baseline + MRI (contralat)",
  #                "Baseline + MRI (stroke)","Baseline + MRI (stroke+contralat)", "Expert")
  # tab
  # 
  # 
  # #### Change level names for plotting
  # logreg_bl_plot = change_var_names_RF(logreg_bl)
  # logreg_bl_mri_c_plot = change_var_names_RF(logreg_bl_mri_c)
  # logreg_bl_mri_sv_plot = change_var_names_RF(logreg_bl_mri)
  # logreg_bl_mri_plot = change_var_names_RF(logreg_bl_mri)
  # logreg_expert_plot = change_var_names_RF(logreg_expert)
  # train_plot = change_var_names_RF(train[[r]])
  # 
  # 
  # 
  # #### Partial Dependency Plots:
  # 
  # models = list(logreg_mri_c, logreg_mri_s, logreg_mri_sv, logreg_mri,
  #               logreg_bl_plot, logreg_bl_mri_c_plot, logreg_bl_mri_sv_plot, logreg_bl_mri_plot, logreg_expert_plot)
  # img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_logreg_mri_contralat_BL_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_mri_stroke_wo_vol_BL_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_mri_stroke_BL_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_mri_stroke_contralat_BL_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_BL_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_mri_contralat_BL_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_mri_stroke_BL_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_mri_stroke_contralat_BL_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_expert_BL_",var_name,".pdf"))
  # data = list(train[[r]],train[[r]],train[[r]],train_plot,train_plot,train_plot,train_plot,train_plot)
  # 
  # for(l in 1:length(img_names)){
  #   
  #   pdf(paste0(dir,img_names[l]))
  #   
  #   for(i in attributes(models[[l]]$terms)$term.labels){
  #     print(i)
  #     info = partial(models[[l]], pred.var = i, ice = T, which.class = 2, rug=T, prob=T, train=data[[l]])
  #     data[[l]]$no = seq(1,dim(data[[l]])[1],1)
  #     dat_plot = merge(info, data[[l]], by.x="yhat.id", by.y="no")
  #     
  #     # find yhat which is closest to the predicted variables
  #     for(j in unique(dat_plot$yhat.id)){
  #       pat = dat_plot[dat_plot$yhat.id == j,]
  #       if(class(dat_plot[,paste0(i,".x")])=="factor"){
  #         k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
  #       } else{
  #         k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #       }
  #       dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #     }
  #     
  #     p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                     ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==i)]),
  #                     alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #     print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #     
  #   }
  #   dev.off()
  # }
  # 
  
  
  
  
  
  
  #####################################
  #### Prediction Models BL: LASSO ####
  #####################################
  
  
  library(glmnet)
  
  # Test how it works
  lambda = 10^seq(10, -2, length = 100)
  i=1
  lasso_mri_c = glmnet(model.matrix(form_bl_mri_c_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out = cv.glmnet(model.matrix(form_bl_mri_c_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  predict(lasso_mri_c, s = cv_out$lambda.min, newx = model.matrix(form_bl_mri_c_bl, test[[r]])[,-1], type="response")
  
  
  # Run the prediction models
  lasso_mri_c = glmnet(model.matrix(form_mri_c_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_mri_c = cv.glmnet(model.matrix(form_mri_c_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_mri_c = predict(lasso_mri_c, s = cv_out_mri_c$lambda.min, newx = model.matrix(form_mri_c_bl, test[[r]])[,-1], type="response")
  
  lasso_mri_s = glmnet(model.matrix(form_mri_s_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_mri_s = cv.glmnet(model.matrix(form_mri_s_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_mri_s = predict(lasso_mri_s, s = cv_out_mri_s$lambda.min, newx = model.matrix(form_mri_s_bl, test[[r]])[,-1], type="response")
  
  lasso_mri_sv = glmnet(model.matrix(form_mri_sv_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_mri_sv = cv.glmnet(model.matrix(form_mri_sv_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, family="binomial")
  pred_mri_sv = predict(lasso_mri_sv, s = cv_out_mri_sv$lambda.min, newx = model.matrix(form_mri_sv_bl, test[[r]])[,-1], type="response")
  
  lasso_mri = glmnet(model.matrix(form_mri_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_mri = cv.glmnet(model.matrix(form_mri_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_mri = predict(lasso_mri, s = cv_out_mri$lambda.min, newx = model.matrix(form_mri_bl, test[[r]])[,-1], type="response")
  
  lasso_bl = glmnet(model.matrix(form_bl_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_bl = cv.glmnet(model.matrix(form_bl_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_bl = predict(lasso_bl, s = cv_out_bl$lambda.min, newx = model.matrix(form_bl_bl, test[[r]])[,-1], type="response")
  
  lasso_bl_mri_c = glmnet(model.matrix(form_bl_mri_c_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_bl_mri_c = cv.glmnet(model.matrix(form_bl_mri_c_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_bl_mri_c = predict(lasso_bl_mri_c, s = cv_out_bl_mri_c$lambda.min, newx = model.matrix(form_bl_mri_c_bl, test[[r]])[,-1], type="response")
  
  lasso_bl_mri_sv = glmnet(model.matrix(form_bl_mri_sv_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_bl_mri_sv = cv.glmnet(model.matrix(form_bl_mri_sv_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_bl_mri_sv = predict(lasso_bl_mri_sv, s = cv_out_bl_mri_sv$lambda.min, newx = model.matrix(form_bl_mri_sv_bl, test[[r]])[,-1], type="response")
  
  lasso_bl_mri = glmnet(model.matrix(form_bl_mri_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_bl_mri = cv.glmnet(model.matrix(form_bl_mri_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_bl_mri = predict(lasso_bl_mri, s = cv_out_bl_mri$lambda.min, newx = model.matrix(form_bl_mri_bl, test[[r]])[,-1], type="response")
  
  lasso_expert = glmnet(model.matrix(form_expert_bl, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_expert = cv.glmnet(model.matrix(form_expert, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_expert = predict(lasso_expert, s = cv_out_expert$lambda.min, newx = model.matrix(form_expert, test[[r]])[,-1], type="response")
  
  # Save the result on the test set
  test_pred_lasso = data.frame(test[[r]], mri_c = as.numeric(pred_mri_c), mri_s = as.numeric(pred_mri_s), mri_sv = as.numeric(pred_mri_sv),
                               mri=as.numeric(pred_mri), bl = as.numeric(pred_bl), bl_mri_c = as.numeric(pred_bl_mri_c), 
                               bl_mri_sv = as.numeric(pred_bl_mri_sv), bl_mri = as.numeric(pred_bl_mri),
                               expert = as.numeric(pred_expert))
  save(test_pred_lasso, file=paste0(dir,"data/CV/run",r,"/test_pred_lasso_BL_",var_name,".R"))
  
  
  # probabilities to 0,1
  pred_mri_c_class = ifelse(as.numeric(pred_mri_c)>0.5,1,0)
  pred_mri_s_class = ifelse(as.numeric(pred_mri_s)>0.5,1,0)
  pred_mri_sv_class = ifelse(as.numeric(pred_mri_sv)>0.5,1,0)
  pred_mri_class = ifelse(as.numeric(pred_mri)>0.5,1,0)
  pred_bl_class = ifelse(as.numeric(pred_bl)>0.5,1,0)
  pred_bl_mri_c_class = ifelse(as.numeric(pred_bl_mri_c)>0.5,1,0)
  pred_bl_mri_sv_class = ifelse(as.numeric(pred_bl_mri_sv)>0.5,1,0)
  pred_bl_mri_class = ifelse(as.numeric(pred_bl_mri)>0.5,1,0)
  pred_expert_class = ifelse(as.numeric(pred_expert)>0.5,1,0)
  
  tab = c(mean(pred_mri_c_class == test[[r]]$mrs_3months_binary), mean(pred_mri_s_class == test[[r]]$mrs_3months_binary), mean(pred_mri_sv_class == test[[r]]$mrs_3months_binary), mean(pred_mri_class == test[[r]]$mrs_3months_binary),
          mean(pred_bl_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_c_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_sv_class == test[[r]]$mrs_3months_binary),
          mean(pred_bl_mri_class == test[[r]]$mrs_3months_binary), mean(pred_expert_class == test[[r]]$mrs_3months_binary))
  names(tab) = c("MRI (contralat)", "MRI (stroke wo vol)", "MRI (stroke)","MRI (stroke+contralat)","Baseline","Baseline + MRI (contralat)",
                 "Baseline + MRI (stroke)","Baseline + MRI (stroke+contralat)", "Expert")
  tab
  
  
  #### Change level names for plotting
  lasso_bl_plot = change_var_names_RF(lasso_bl)
  lasso_bl_mri_c_plot = change_var_names_RF(lasso_bl_mri_c)
  lasso_bl_mri_sv_plot = change_var_names_RF(lasso_bl_mri)
  lasso_bl_mri_plot = change_var_names_RF(lasso_bl_mri)
  lasso_expert_plot = change_var_names_RF(lasso_expert)
  train_plot = change_var_names_RF(train[[r]])
  
  
  
  # #### Partial Dependency Plots:
  # if(r==1){
  #   models = list(lasso_mri_c, lasso_mri_s, lasso_mri_sv, lasso_mri,
  #                 lasso_bl_plot, lasso_bl_mri_c_plot, lasso_bl_mri_sv_plot, lasso_bl_mri_plot, lasso_expert_plot)
  #   img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_lasso_mri_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_mri_stroke_wo_vol_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_mri_stroke_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_mri_stroke_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_mri_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_mri_stroke_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_mri_stroke_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_expert_BL_",var_name,".pdf"))
  #   data = list(train[[r]],train[[r]],train[[r]],train[[r]],train_plot,train_plot,train_plot,train_plot,train_plot)
  #   formulas = list(form_mri_c_bl,form_mri_s_bl,form_mri_sv_bl,form_mri_bl,
  #                   form_bl_bl,form_bl_mri_c_bl,form_bl_mri_sv_bl,form_bl_mri_bl,form_expert_bl)
  #   cv_outs = list(cv_out_mri_c, cv_out_mri_s, cv_out_mri_sv, cv_out_mri, 
  #                  cv_out_bl, cv_out_bl_mri_c, cv_out_bl_mri_sv, cv_out_bl_mri, cv_out_expert)
  #   
  #   # pred_wrapper = function(object,newdata){ 
  #   #   return(predict(object, newx=newdata, s=cv_out_mri_c$lambda.min)[,1])
  #   # }
  #   # 
  #   # info = partial(lasso_mri_c, pred.var = "C_Occ", ice = T, which.class = 1, prob=T, 
  #   #                train=model.matrix(form_mri_c_bl, train[[r]])[,-1], 
  #   #                pred.fun=pred_wrapper)
  #   
  #   
  #   for(l in 1:length(img_names)){
  #     
  #     pdf(paste0(dir,img_names[l]))
  #     vars_model = models[[l]]$beta@Dimnames[[1]]
  #     vars_data = attributes(terms(formulas[[l]]))$term.labels
  #     
  #     for(i in vars_model){
  #       print(i)
  #       
  #       pred_wrapper = function(object,newdata){ 
  #         return(predict(object, newx=newdata, s=cv_outs[[l]]$lambda.min, type="response")[,1])
  #       }
  #       
  #       info = partial(models[[l]], pred.var = i, ice = T, which.class = 1, prob=T, 
  #                      train = model.matrix(formulas[[l]], train[[r]])[,-1], 
  #                      pred.fun=pred_wrapper)
  #       data[[l]]$no = seq(1,dim(data[[l]])[1],1)
  #       dat_plot = merge(info, data[[l]], by.x="yhat.id", by.y="no")
  #       
  #       # find yhat which is closest to the predicted variables
  #       for(j in unique(dat_plot$yhat.id)){
  #         pat = dat_plot[dat_plot$yhat.id == j,]
  #         if(nrow(pat)<5){
  #           # find the equivalent name in the dataframe
  #           k = which(pat[,i] == pat[,vars_data[which(vars_model==i)]][1])
  #           dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #           
  #         } else{
  #           k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #           dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #           
  #         }
  #       }
  #       
  #       if(is.factor(train[[r]][,vars_data[which(vars_model==i)]])){
  #         p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                         ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==vars_data[which(vars_model==i)])]),
  #                         alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #         print(p + layer(lpoints(dat_plot[,vars_data[which(vars_model==i)]], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       } else{
  #         p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                         ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==vars_data[which(vars_model==i)])]),
  #                         alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #         print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       }
  #     }
  #     dev.off()
  #   }
  #   
  # }
  
  
  
  
  
  
  
  
  
  ##############################################
  #### Prediction Models BL: Neural Network ####
  ##############################################
  
  
  # we want a matrix with [n_obs,n_features]
  # 
  # # Some tries
  # X_train = as.matrix(train[[r]][,paste(names(sort(importance(rf_mri_c)[,"MeanDecreaseAccuracy"], decreasing=T))[1:7])])
  # Y_train = train[[r]][,"mrs_3months_binary"]
  # X_test = as.matrix(test[[r]][,paste(names(sort(importance(rf_mri_c)[,"MeanDecreaseAccuracy"], decreasing=T))[1:7])])
  # Y_test = test[[r]][,"mrs_3months_binary"]
  # str(X_train)
  # 
  # # normalize data
  # X_train = apply(X_train, 2, as.numeric)
  # X_test = apply(X_test, 2, as.numeric)
  # X_train_mean = apply(X_train, 2, mean)
  # X_train_sd = sqrt(apply(X_train, 2, var))
  # X_train_norm = X_train
  # X_test_norm = X_test
  # for(j in 1:length(X_train_mean)){
  #   X_train_norm[,j] = (X_train_norm[,j]-X_train_mean[j])/X_train_sd[j]
  #   X_test_norm[,j] = (X_test_norm[,j]-X_train_mean[j])/X_train_sd[j]
  # }
  # 
  # # one hot encoding
  # Y_train = to_categorical(Y_train, 2)
  # Y_test = to_categorical(Y_test, 2)
  # str(Y_train)
  # 
  # # define the model
  # model = keras_model_sequential()
  # model %>% layer_dense(units = 8, activation = 'relu',  input_shape = c(7)) %>%
  #   # layer_dropout(rate=0.3) %>%
  #   layer_dense(units = 8, activation = 'relu') %>%
  #   layer_dropout(rate=0.3) %>%
  #   layer_dense(units = 8, activation = 'relu') %>%
  #   layer_dropout(rate=0.3) %>%
  #   layer_dense(units = 8, activation = 'relu') %>%
  #   layer_dropout(rate=0.3) %>%
  #   layer_dense(units = 8, activation = 'relu') %>%
  #   layer_dropout(rate=0.3) %>%
  #   # layer_dense(units = 8, activation = 'relu', kernel_regularizer = regularizer_l2(reg)) %>%
  #   layer_dense(units = 2, activation = 'softmax')
  # 
  # # compile the model
  # model %>% compile(
  #   loss = 'categorical_crossentropy',
  #   optimizer = optimizer_adam(),
  #   metrics = c('accuracy')
  # )
  # 
  # name = "mri_c"
  # folder = paste0("C:/Users/hezo/Documents/Stroke/pred_models_before_therapy/",var_name,"/run1/", name)
  # # folder = paste0("/Users/janne.hamann/Desktop/Other Side Mac/CNN/after/",var_name,"/run1/", name)
  # set.seed(123)
  # history = model %>% fit(X_train_norm,
  #                         Y_train,
  #                         epochs = 15,
  #                         batch_size = 2,
  #                         validation_split = 0.1,
  #                         view_metrics=F,
  #                         callbacks = callback_model_checkpoint(paste0(folder,"/model-{epoch:02d}.hdf5"),
  #                                                               monitor = "val_loss",
  #                                                               verbose = 0,
  #                                                               save_best_only = T,
  #                                                               save_weights_only = F,
  #                                                               mode = "min",
  #                                                               period = 1))
  # 
  # # find best epoch
  # epoch_k = which(history$metrics$val_loss == min(history$metrics$val_loss))
  # # load model
  # if(epoch_k<10){
  #   epoch_k = paste0("0",epoch_k)
  # } else{
  #   epoch_k = epoch_k
  # }
  # epoch_k
  # best_model = load_model_hdf5(paste0(folder,"/model-",epoch_k,".hdf5"), custom_objects = NULL, compile = TRUE)
  # 
  # # evaluate on the test set
  # pred = predict(best_model,X_train_norm)[,2]
  # predict_classes(best_model, X_test_norm)
  # evaluate(best_model, X_train_norm, Y_train)
  # evaluate(model, x=X_test_norm, y=Y_test)
  
  
  # Define the Neural Network for prediction
  NN = function(form, train, test, epochs, bs, reg, name, var_name, folder_name){
    
    # form = formular to get the variables
    # n_var = how many variables
    # train = list of training sets for CV
    # test = list of test sets for CV
    # reg = regularization to the model
    
    # how many variables
    vars = attributes(terms(form))$term.labels
    n_var = length(vars)
    
    # save predictions from test set
    pred = c()
    pred_class = c()
    models = list()
    
    # start to train with train[1]
    X_train = as.matrix(train[,vars])
    Y_train = train[,"mrs_3months_binary"]
    X_test = as.matrix(test[,vars])
    Y_test = test[,"mrs_3months_binary"]
    # str(X_train)
    
    # normalize data
    X_train = apply(X_train, 2, as.numeric)
    X_test = apply(X_test, 2, as.numeric)
    X_train_mean = apply(X_train, 2, mean)
    X_train_sd = sqrt(apply(X_train, 2, var))
    X_train_norm = X_train
    X_test_norm = X_test
    for(j in 1:length(X_train_mean)){
      X_train_norm[,j] = (X_train_norm[,j]-X_train_mean[j])/X_train_sd[j]
      X_test_norm[,j] = (X_test_norm[,j]-X_train_mean[j])/X_train_sd[j]
    }
    
    # one hot encoding
    Y_train = to_categorical(Y_train, 2)
    Y_test = to_categorical(Y_test, 2)
    # str(Y_train)
    
    # define the model
    model = keras_model_sequential()
    model %>% layer_dense(units = 8, activation = 'relu', kernel_regularizer = regularizer_l2(reg), input_shape = c(n_var)) %>%
      layer_dropout(rate=0.3) %>%
      layer_dense(units = 8, activation = 'relu', kernel_regularizer = regularizer_l2(reg)) %>%
      layer_dropout(rate=0.3) %>%
      layer_dense(units = 8, activation = 'relu', kernel_regularizer = regularizer_l2(reg)) %>%
      layer_dropout(rate=0.3) %>%
      layer_dense(units = 8, activation = 'relu', kernel_regularizer = regularizer_l2(reg)) %>%
      layer_dropout(rate=0.3) %>%
      # layer_dense(units = 8, activation = 'relu', kernel_regularizer = regularizer_l2(reg)) %>%
      # layer_dropout(rate=0.3) %>%
      # layer_dense(units = 8, activation = 'relu', kernel_regularizer = regularizer_l2(reg)) %>%
      layer_dense(units = 2, activation = 'softmax')
    
    # compile the model
    model %>% compile(
      loss = 'categorical_crossentropy',
      optimizer = optimizer_adam(),
      metrics = c('accuracy')
    )
    
    set.seed(123)
    # folder to save the model weights in epoch k
    folder = paste0("C:/Users/user/Documents/Stroke/",folder_name,"/",var_name,"/run",r,"/", name)
    history = model %>% fit(X_train_norm,
                            Y_train,
                            epochs = epochs,
                            batch_size = bs,
                            view_metrics=F,
                            validation_split = 0.1,
                            callbacks = list(callback_model_checkpoint(paste0(folder,"/model-{epoch:02d}.hdf5"),
                                                                       monitor = "val_loss", verbose = 0,
                                                                       save_best_only = T, save_weights_only = F,
                                                                       mode = "min", period = 1)))
    
    # find the best epoch of the model and load it
    epoch_k = which(history$metrics$val_loss == min(history$metrics$val_loss))
    # load model
    if(epoch_k<10){
      epoch_k = paste0("0",epoch_k)
    } else{
      epoch_k = epoch_k
    }
    best_model = load_model_hdf5(paste0(folder,"/model-",epoch_k,".hdf5"), custom_objects = NULL, compile = TRUE)
    
    # evaluate on the test set
    pred_tmp = predict(best_model, X_test_norm)[,2] # predictions for class 1
    pred_class_tmp = predict_classes(best_model, X_test_norm)
    print(evaluate(best_model, x=X_test_norm, y=Y_test))
    
    pred = c(pred, pred_tmp)
    pred_class = c(pred_class, pred_class_tmp)
    
    return(list(pred=pred, pred_class = pred_class, models=best_model))
  }
  
  
  nn_mri_c = NN(form_mri_c_bl, train[[r]], test[[r]], 150, 2, 0.001, "mri_c", var_name, "pred_models_before_therapy")
  nn_mri_s = NN(form_mri_s_bl, train[[r]], test[[r]], 150, 2, 0.001, "mri_s", var_name, "pred_models_before_therapy")
  nn_mri_sv = NN(form_mri_sv_bl, train[[r]], test[[r]], 150, 2, 0.002, "mri_sv", var_name, "pred_models_before_therapy")
  nn_mri = NN(form_mri_bl, train[[r]], test[[r]], 150, 2, 0.002, "mri", var_name, "pred_models_before_therapy")
  nn_bl = NN(form_bl_bl, train[[r]], test[[r]], 150, 2, 0.002, "bl", var_name, "pred_models_before_therapy")
  nn_bl_mri_c = NN(form_bl_mri_c_bl, train[[r]], test[[r]], 150, 2, 0.002, "bl_mri_c", var_name, "pred_models_before_therapy")
  nn_bl_mri_sv = NN(form_bl_mri_sv_bl, train[[r]], test[[r]], 150, 2, 0.002, "bl_mri_sv", var_name, "pred_models_before_therapy")
  nn_bl_mri = NN(form_bl_mri_bl, train[[r]], test[[r]], 150, 2, 0.002, "bl_mri", var_name, "pred_models_before_therapy")
  nn_expert = NN(form_expert_bl, train[[r]], test[[r]], 150, 2, 0.002, "expert", var_name, "pred_models_before_therapy")
  
  mean(test[[r]]$mrs_3months_binary == nn_mri_c$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_mri_s$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_mri_sv$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_mri$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_bl$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_bl_mri_c$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_bl_mri_sv$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_bl_mri$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_expert$pred_class)
  
  test_pred_nn = data.frame(test[[r]], mri_c = nn_mri_c$pred, mri_s = nn_mri_s$pred, mri_sv = nn_mri_sv$pred,
                            mri=nn_mri$pred, bl = nn_bl$pred, bl_mri_c = nn_bl_mri_c$pred, 
                            bl_mri_sv = nn_bl_mri_sv$pred, bl_mri = nn_bl_mri$pred, expert = nn_expert$pred)
  save(test_pred_nn, file=paste0(dir,"data/CV/run",r,"/test_pred_nn_BL_",var_name,".R"))
  
  
  
  
  # #### Partial Dependency Plots:
  # if(r==1){
  #   formulas = list(form_mri_c_bl, form_mri_s_bl, form_mri_sv_bl, form_mri_bl,
  #                   form_bl_bl, form_bl_mri_c_bl, form_bl_mri_sv_bl, form_bl_mri_bl, form_expert_bl)
  #   models = list(nn_mri_c$models, nn_mri_s$models, nn_mri_sv$models, nn_mri$models,
  #                 nn_bl$models, nn_bl_mri_c$models, nn_bl_mri_sv$models, nn_bl_mri$models, nn_expert$models)
  #   img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_NN_mri_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_mri_stroke_wo_volume_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_mri_stroke_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_mri_stroke_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_mri_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_mri_stroke_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_mri_stroke_contralat_BL_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_expert_BL_",var_name,".pdf"))
  #   data = list(train[[r]],train[[r]],train[[r]],train[[r]],train[[r]],train[[r]],train[[r]],train[[r]],train[[r]])
  #   n_var = c(length(attributes(terms(form_mri_c_bl))$term.labels),
  #             length(attributes(terms(form_mri_s_bl))$term.labels),
  #             length(attributes(terms(form_mri_sv_bl))$term.labels),
  #             length(attributes(terms(form_mri_bl))$term.labels),
  #             length(attributes(terms(form_bl_bl))$term.labels),
  #             length(attributes(terms(form_bl_mri_c_bl))$term.labels),
  #             length(attributes(terms(form_bl_mri_sv_bl))$term.labels),
  #             length(attributes(terms(form_bl_mri_bl))$term.labels),
  #             length(attributes(terms(form_expert_bl))$term.labels))
  #   
  #   library(gridExtra)
  #   
  #   # partial plot for the model: https://bgreenwell.github.io/pdp/articles/pdp-example-tensorflow.html#fn1
  #   pred_wrapper = function(object, newdata) {
  #     pred = predict(object, x = as.matrix(newdata))
  #     return(as.vector(pred[,2]))
  #   }
  #   
  #   for(l in 1:length(img_names)){
  #     
  #     pdf(paste0(dir,img_names[l]))
  #     
  #     vars = attributes(terms(formulas[l][[1]]))$term.labels
  #     
  #     for(i in vars){
  #       print(i)
  #       X_train_tmp = as.data.frame(data[[l]][,vars])
  #       
  #       info = partial(models[[l]], pred.var = i, ice = T, which.class = 1, prob=T, train=X_train_tmp, pred.fun = pred_wrapper)
  #       
  #       X_train_tmp$no = seq(1,dim(X_train_tmp)[1],1)
  #       dat_plot = merge(info, X_train_tmp, by.x="yhat.id", by.y="no")
  #       
  #       # find yhat which is closest to the predicted variables
  #       for(j in unique(dat_plot$yhat.id)){
  #         pat = dat_plot[dat_plot$yhat.id == j,]
  #         if(class(dat_plot[,paste0(i,".x")])=="factor"){
  #           k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
  #         } else{
  #           k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #         }
  #         dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #       }
  #       
  #       p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=X_train_tmp,
  #                       ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==i)]),
  #                       alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #       print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       
  #     }
  #     dev.off()
  #   }
  # }
  
  
  
  
  
  
  
  # ####################################################
  # #### Prediction Models BL + TICI: Random Forest ####
  # ####################################################
  # 
  # # Include TICI
  # get_formula_tici = function(form){
  #   return(as.formula(paste("mrs_3months_binary", paste(paste(form)[3],"+ tici"),sep="~")))
  # }
  # 
  # #### Define the formulars we want to look at
  # form_bl_tici = get_formula_tici(form_bl_bl)
  # form_bl_mri_c_tici = get_formula_tici(form_bl_mri_c_bl)
  # form_bl_mri_sv_tici = get_formula_tici(form_bl_mri_sv_bl)
  # form_bl_mri_tici = get_formula_tici(form_bl_mri_bl)
  # form_expert_tici = get_formula_tici(form_expert_bl)
  # 
  # 
  # set.seed(3004)
  # 
  # 
  # #### Models
  # 
  # # Baseline data: Information from stroke + contralateral side
  # rf_bl = randomForest(form_bl_tici, data = train[[r]], ntree=500, importance=T)
  # rf_pred_bl_test = predict(rf_bl, newdata=test[[r]])
  # rf_pred_bl_test_prob = predict(rf_bl, type="prob", newdata=test[[r]])
  # (bl_test = mean(test[[r]]$mrs_3months_binary == rf_pred_bl_test))
  # 
  # # Baseline + MRI data: Information from stroke + contralateral side
  # rf_bl_mri_c = randomForest(form_bl_mri_c_tici, data = train[[r]], ntree=500, importance=T)
  # rf_pred_bl_mri_c_test = predict(rf_bl_mri_c, newdata=test[[r]])
  # rf_pred_bl_mri_c_test_prob = predict(rf_bl_mri_c, type="prob", newdata=test[[r]])
  # (bl_mri_c_test = mean(test[[r]]$mrs_3months_binary == rf_pred_bl_mri_c_test))
  # 
  # # Baseline + MRI data: Information from stroke + contralateral side
  # rf_bl_mri_sv = randomForest(form_bl_mri_sv_tici, data = train[[r]], ntree=500, importance=T)
  # rf_pred_bl_mri_sv_test = predict(rf_bl_mri_sv, newdata=test[[r]])
  # rf_pred_bl_mri_sv_test_prob = predict(rf_bl_mri_sv, type="prob", newdata=test[[r]])
  # (bl_mri_sv_test = mean(test[[r]]$mrs_3months_binary == rf_pred_bl_mri_sv_test))
  # 
  # # Baseline + MRI data: Information from stroke + contralateral side
  # rf_bl_mri = randomForest(form_bl_mri_tici, data = train[[r]], ntree=500, importance=T)
  # rf_pred_bl_mri_test = predict(rf_bl_mri, newdata=test[[r]])
  # rf_pred_bl_mri_test_prob = predict(rf_bl_mri, type="prob", newdata=test[[r]])
  # (bl_mri_test = 1-mean(test[[r]]$mrs_3months_binary != rf_pred_bl_mri_test))
  # 
  # # Expert
  # rf_expert = randomForest(form_expert_tici, data = train[[r]], ntree=500, importance=T)
  # rf_pred_expert_test = predict(rf_expert, newdata=test[[r]])
  # rf_pred_expert_test_prob = predict(rf_expert, type="prob", newdata=test[[r]])
  # (expert_test = 1-mean(test[[r]]$mrs_3months_binary != rf_pred_expert_test))
  # 
  # 
  # tab = c(bl_test, bl_mri_c_test, bl_mri_sv_test, bl_mri_test, expert_test)
  # names(tab) = c("Baseline","Baseline + MRI (contralat)",
  #                "Baseline + MRI (stroke)","Baseline + MRI (stroke+contralat)", "Expert")
  # tab
  # 
  # 
  # # load the MRI information from the model "before"
  # test_pred_rf = data.frame(test[[r]], bl = rf_pred_bl_test_prob[,2], 
  #                           bl_mri_c = rf_pred_bl_mri_c_test_prob[,2], 
  #                           bl_mri_sv = rf_pred_bl_mri_sv_test_prob[,2], 
  #                           bl_mri = rf_pred_bl_mri_test_prob[,2],
  #                           expert = rf_pred_expert_test_prob[,2])
  # save(test_pred_rf, file=paste0(dir,"data/CV/run",r,"/test_pred_rf_BL_TICI_",var_name,".R"))
  # 
  # 
  # 
  # #### Save level names for plotting
  # rf_bl_plot = change_var_names_RF(rf_bl)
  # rf_bl_mri_c_plot = change_var_names_RF(rf_bl_mri_c)
  # rf_bl_mri_sv_plot = change_var_names_RF(rf_bl_mri_sv)
  # rf_bl_mri_plot = change_var_names_RF(rf_bl_mri)
  # rf_expert_plot = change_var_names_RF(rf_expert)
  # train_plot = change_var_names_DF(train[[r]])
  # 
  # 
  # #### Partial dependency plots
  # if(r==1){
  #   models = list(rf_bl_plot, rf_bl_mri_c_plot, rf_bl_mri_sv_plot, rf_bl_mri_plot, rf_expert_plot)
  #   img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_contralat_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_stroke_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_stroke_contralat_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_RF_expert_BL_TICI_",var_name,".pdf"))
  #   data = list(train_plot,train_plot,train_plot,train_plot,train_plot)
  #   
  #   
  #   for(l in 1:length(img_names)){
  #     
  #     pdf(paste0(dir,img_names[l]))
  #     
  #     for(i in attributes(models[[l]]$terms)$term.labels){
  #       print(i)
  #       info = partial(models[[l]], pred.var = i, ice = T, which.class = 2, rug=T, prob=T, train=data[[l]])
  #       data[[l]]$no = seq(1,dim(data[[l]])[1],1)
  #       dat_plot = merge(info, data[[l]], by.x="yhat.id", by.y="no")
  #       
  #       # find yhat which is closest to the predicted variables
  #       for(j in unique(dat_plot$yhat.id)){
  #         pat = dat_plot[dat_plot$yhat.id == j,]
  #         if(class(dat_plot[,paste0(i,".x")])=="factor"){
  #           k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
  #         } else{
  #           k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #         }
  #         dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #       }
  #       
  #       p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                       ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==i)]),
  #                       alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #       print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       
  #     }
  #     dev.off()
  #   }
  # }
  # 
  # 
  # 
  # 
  # 
  # 
  # # ##########################################################
  # # #### Prediction Models BL + TICI: Logistic regression ####
  # # ##########################################################
  # # 
  # # 
  # # # run models
  # # logreg_bl = glm(form_bl_tici, data=train[[r]], family = binomial)
  # # logreg_bl_mri_c = glm(form_bl_mri_c_tici, data=train[[r]], family = binomial)
  # # logreg_bl_mri_sv = glm(form_bl_mri_sv_tici, data=train[[r]], family = binomial)
  # # logreg_bl_mri = glm(form_bl_mri_tici, data=train[[r]], family = binomial)
  # # logreg_expert = glm(form_expert_tici, data=train[[r]], family = binomial)
  # # 
  # # # predictions
  # # pred_bl = predict(logreg_bl, newdata=test[[r]], type="response")
  # # pred_bl_mri_c = predict(logreg_bl_mri_c, newdata=test[[r]], type="response")
  # # pred_bl_mri_sv = predict(logreg_bl_mri_sv, newdata=test[[r]], type="response")
  # # pred_bl_mri = predict(logreg_bl_mri, newdata=test[[r]], type="response")
  # # pred_expert = predict(logreg_expert, newdata=test[[r]], type="response")
  # # 
  # # test_pred_logreg = data.frame(test[[r]], bl = pred_bl, bl_mri_c = pred_bl_mri_c, 
  # #                               bl_mri_sv = pred_bl_mri_sv, bl_mri = pred_bl_mri,
  # #                               expert = pred_expert)
  # # save(test_pred_logreg, file=paste0(dir,"data/CV/run",r,"/test_pred_logreg_BL_TICI_",var_name,".R"))
  # # 
  # # 
  # # # probabilities to 0,1
  # # pred_bl_class = ifelse(pred_bl>0.5,1,0)
  # # pred_bl_mri_c_class = ifelse(pred_bl_mri_c>0.5,1,0)
  # # pred_bl_mri_sv_class = ifelse(pred_bl_mri_sv>0.5,1,0)
  # # pred_bl_mri_class = ifelse(pred_bl_mri>0.5,1,0)
  # # pred_expert_class = ifelse(pred_expert>0.5,1,0)
  # # 
  # # tab = c(mean(pred_bl_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_c_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_sv_class == test[[r]]$mrs_3months_binary),
  # #         mean(pred_bl_mri_class == test[[r]]$mrs_3months_binary), mean(pred_expert_class == test[[r]]$mrs_3months_binary))
  # # names(tab) = c("Baseline","Baseline + MRI (contralat)",
  # #                "Baseline + MRI (stroke)","Baseline + MRI (stroke+contralat)", "Expert")
  # # tab
  # # 
  # # 
  # # #### Change level names for plotting
  # # logreg_bl_plot = change_var_names_RF(logreg_bl)
  # # logreg_bl_mri_c_plot = change_var_names_RF(logreg_bl_mri_c)
  # # logreg_bl_mri_sv_plot = change_var_names_RF(logreg_bl_mri)
  # # logreg_bl_mri_plot = change_var_names_RF(logreg_bl_mri)
  # # logreg_expert_plot = change_var_names_RF(logreg_expert)
  # # train_plot = change_var_names_RF(train[[r]])
  # # 
  # # 
  # # 
  # # #### Partial Dependency Plots:
  # # 
  # # models = list(logreg_bl_plot, logreg_bl_mri_c_plot, logreg_bl_mri_sv_plot, logreg_bl_mri_plot, logreg_expert_plot)
  # # img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_BL_TICI_",var_name,".pdf"),
  # #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_mri_contralat_BL_TICI_",var_name,".pdf"),
  # #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_mri_stroke_BL_TICI_",var_name,".pdf"),
  # #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_mri_stroke_contralat_BL_TICI_",var_name,".pdf"),
  # #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_expert_BL_TICI_",var_name,".pdf"))
  # # data = list(train_plot,train_plot,train_plot,train_plot,train_plot)
  # # 
  # # for(l in 1:length(img_names)){
  # #   
  # #   pdf(paste0(dir,img_names[l]))
  # #   
  # #   for(i in attributes(models[[l]]$terms)$term.labels){
  # #     print(i)
  # #     info = partial(models[[l]], pred.var = i, ice = T, which.class = 2, rug=T, prob=T, train=data[[l]])
  # #     data[[l]]$no = seq(1,dim(data[[l]])[1],1)
  # #     dat_plot = merge(info, data[[l]], by.x="yhat.id", by.y="no")
  # #     
  # #     # find yhat which is closest to the predicted variables
  # #     for(j in unique(dat_plot$yhat.id)){
  # #       pat = dat_plot[dat_plot$yhat.id == j,]
  # #       if(class(dat_plot[,paste0(i,".x")])=="factor"){
  # #         k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
  # #       } else{
  # #         k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  # #       }
  # #       dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  # #     }
  # #     
  # #     p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  # #                     ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==i)]),
  # #                     alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  # #     print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  # #     
  # #   }
  # #   dev.off()
  # # }
  # # 
  # # 
  # # 
  # # 
  # # 
  # 
  # 
  # ############################################
  # #### Prediction Models BL + TICI: LASSO ####
  # ############################################
  # 
  # 
  # library(glmnet)
  # 
  # lambda = 10^seq(10, -2, length = 100)
  # 
  # lasso_bl = glmnet(model.matrix(form_bl_tici, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  # cv_out_bl = cv.glmnet(model.matrix(form_bl_tici, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  # pred_bl = predict(lasso_bl, s = cv_out_bl$lambda.min, newx = model.matrix(form_bl_tici, test[[r]])[,-1], type="response")
  # 
  # lasso_bl_mri_c = glmnet(model.matrix(form_bl_mri_c_tici, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  # cv_out_bl_mri_c = cv.glmnet(model.matrix(form_bl_mri_c_tici, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  # pred_bl_mri_c = predict(lasso_bl_mri_c, s = cv_out_bl_mri_c$lambda.min, newx = model.matrix(form_bl_mri_c_tici, test[[r]])[,-1], type="response")
  # 
  # lasso_bl_mri_sv = glmnet(model.matrix(form_bl_mri_sv_tici, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  # cv_out_bl_mri_sv = cv.glmnet(model.matrix(form_bl_mri_sv_tici, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  # pred_bl_mri_sv = predict(lasso_bl_mri_sv, s = cv_out_bl_mri_sv$lambda.min, newx = model.matrix(form_bl_mri_sv_tici, test[[r]])[,-1], type="response")
  # 
  # lasso_bl_mri = glmnet(model.matrix(form_bl_mri_tici, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  # cv_out_bl_mri = cv.glmnet(model.matrix(form_bl_mri_tici, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  # pred_bl_mri = predict(lasso_bl_mri, s = cv_out_bl_mri$lambda.min, newx = model.matrix(form_bl_mri_tici, test[[r]])[,-1], type="response")
  # 
  # lasso_expert = glmnet(model.matrix(form_expert_tici, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  # cv_out_expert = cv.glmnet(model.matrix(form_expert_tici, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  # pred_expert = predict(lasso_expert, s = cv_out_expert$lambda.min, newx = model.matrix(form_expert_tici, test[[r]])[,-1], type="response")
  # 
  # 
  # # Save the result on the test set
  # test_pred_lasso = data.frame(test[[r]], bl = as.numeric(pred_bl), bl_mri_c = as.numeric(pred_bl_mri_c), 
  #                              bl_mri_sv = as.numeric(pred_bl_mri_sv), bl_mri = as.numeric(pred_bl_mri),
  #                              expert = as.numeric(pred_expert))
  # save(test_pred_lasso, file=paste0(dir,"data/CV/run",r,"/test_pred_lasso_BL_TICI_",var_name,".R"))
  # 
  # 
  # # probabilities to 0,1
  # pred_bl_class = ifelse(pred_bl>0.5,1,0)
  # pred_bl_mri_c_class = ifelse(pred_bl_mri_c>0.5,1,0)
  # pred_bl_mri_sv_class = ifelse(pred_bl_mri_sv>0.5,1,0)
  # pred_bl_mri_class = ifelse(pred_bl_mri>0.5,1,0)
  # pred_expert_class = ifelse(pred_expert>0.5,1,0)
  # 
  # tab = c(mean(pred_bl_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_c_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_sv_class == test[[r]]$mrs_3months_binary),
  #         mean(pred_bl_mri_class == test[[r]]$mrs_3months_binary), mean(pred_expert_class == test[[r]]$mrs_3months_binary))
  # names(tab) = c("Baseline","Baseline + MRI (contralat)",
  #                "Baseline + MRI (stroke)","Baseline + MRI (stroke+contralat)", "Expert")
  # tab
  # 
  # 
  # #### Change level names for plotting
  # lasso_bl_plot = change_var_names_RF(lasso_bl)
  # lasso_bl_mri_c_plot = change_var_names_RF(lasso_bl_mri_c)
  # lasso_bl_mri_sv_plot = change_var_names_RF(lasso_bl_mri)
  # lasso_bl_mri_plot = change_var_names_RF(lasso_bl_mri)
  # lasso_expert_plot = change_var_names_RF(lasso_expert)
  # train_plot = change_var_names_RF(train[[r]])
  # 
  # 
  # 
  # #### Partial Dependency Plots:
  # if(r==1){
  #   models = list(lasso_bl_plot, lasso_bl_mri_c_plot, lasso_bl_mri_sv_plot, lasso_bl_mri_plot, lasso_expert_plot)
  #   img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_mri_contralat_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_mri_stroke_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_mri_stroke_contralat_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_expert_BL_TICI_",var_name,".pdf"))
  #   data = list(train_plot,train_plot,train_plot,train_plot,train_plot)
  #   formulas = list(form_bl_tici,form_bl_mri_c_tici,form_bl_mri_sv_tici,form_bl_mri_tici,form_expert_tici)
  #   cv_outs = list(cv_out_bl, cv_out_bl_mri_c, cv_out_bl_mri_sv, cv_out_bl_mri, cv_out_expert)
  #   
  #   
  #   for(l in 1:length(img_names)){
  #     
  #     pdf(paste0(dir,img_names[l]))
  #     vars_model = models[[l]]$beta@Dimnames[[1]]
  #     vars_model = vars_model[!grepl("tici",vars_model)]
  #     vars_data = attributes(terms(formulas[[l]]))$term.labels
  #     vars_data = vars_data[!grepl("tici",vars_data)]
  #     
  #     for(i in vars_model){
  #       print(i)
  #       
  #       pred_wrapper = function(object,newdata){ 
  #         return(predict(object, newx=newdata, s=cv_outs[[l]]$lambda.min, type="response")[,1])
  #       }
  #       
  #       info = partial(models[[l]], pred.var = i, ice = T, which.class = 1, prob=T, 
  #                      train = model.matrix(formulas[[l]], train[[r]])[,-1], 
  #                      pred.fun=pred_wrapper)
  #       data[[l]]$no = seq(1,dim(data[[l]])[1],1)
  #       dat_plot = merge(info, data[[l]], by.x="yhat.id", by.y="no")
  #       
  #       # find yhat which is closest to the predicted variables
  #       for(j in unique(dat_plot$yhat.id)){
  #         pat = dat_plot[dat_plot$yhat.id == j,]
  #         if(nrow(pat)<5){
  #           # find the equivalent name in the dataframe
  #           k = which(pat[,i] == pat[,vars_data[which(vars_model==i)]][1])
  #           dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #           
  #         } else{
  #           k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #           dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #           
  #         }
  #       }
  #       
  #       if(is.factor(train[[r]][,vars_data[which(vars_model==i)]])){
  #         p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                         ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==vars_data[which(vars_model==i)])]),
  #                         alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #         print(p + layer(lpoints(dat_plot[,vars_data[which(vars_model==i)]], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       } else{
  #         p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                         ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==vars_data[which(vars_model==i)])]),
  #                         alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #         print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       }
  #       
  #       
  #     }
  #     dev.off()
  #   }
  # }
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #####################################################
  # #### Prediction Models BL + TICI: Neural Network ####
  # #####################################################
  # 
  # nn_bl = NN(form_bl_tici, train[[r]], test[[r]], 150, 2, 0.002, "bl", var_name, "pred_models_after_therapy")
  # nn_bl_mri_c = NN(form_bl_mri_c_tici, train[[r]], test[[r]], 150, 2, 0.002, "bl_mri_c", var_name, "pred_models_after_therapy")
  # nn_bl_mri_sv = NN(form_bl_mri_sv_tici, train[[r]], test[[r]], 150, 2, 0.002, "bl_mri_sv", var_name, "pred_models_after_therapy")
  # nn_bl_mri = NN(form_bl_mri_tici, train[[r]], test[[r]], 150, 2, 0.002, "bl_mri", var_name, "pred_models_after_therapy")
  # nn_expert = NN(form_expert_tici, train[[r]], test[[r]], 150, 2, 0.002, "expert", var_name, "pred_models_after_therapy")
  # 
  # mean(test[[r]]$mrs_3months_binary == nn_bl$pred_class)
  # mean(test[[r]]$mrs_3months_binary == nn_bl_mri_c$pred_class)
  # mean(test[[r]]$mrs_3months_binary == nn_bl_mri_sv$pred_class)
  # mean(test[[r]]$mrs_3months_binary == nn_bl_mri$pred_class)
  # mean(test[[r]]$mrs_3months_binary == nn_expert$pred_class)
  # 
  # test_pred_nn = data.frame(test[[r]], bl = nn_bl$pred, bl_mri_c = nn_bl_mri_c$pred, 
  #                           bl_mri_sv = nn_bl_mri_sv$pred, bl_mri = nn_bl_mri$pred, expert = nn_expert$pred)
  # save(test_pred_nn, file=paste0(dir,"data/CV/run",r,"/test_pred_nn_BL_TICI_",var_name,".R"))
  # 
  # 
  # 
  # 
  # #### Partial Dependency Plots:
  # if(r==1){
  #   formulas = list(form_bl_tici, form_bl_mri_c_tici, form_bl_mri_sv_tici, form_bl_mri_tici, form_expert_tici)
  #   models = list(nn_bl$models, nn_bl_mri_c$models, nn_bl_mri_sv$models, nn_bl_mri$models, nn_expert$models)
  #   img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_mri_contralat_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_mri_stroke_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_mri_stroke_contralat_BL_TICI_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_expert_BL_TICI_",var_name,".pdf"))
  #   data = list(train[[r]],train[[r]],train[[r]],train[[r]],train[[r]])
  #   n_var = c(length(attributes(terms(form_bl_tici))$term.labels),
  #             length(attributes(terms(form_bl_mri_c_tici))$term.labels),
  #             length(attributes(terms(form_bl_mri_sv_tici))$term.labels),
  #             length(attributes(terms(form_bl_mri_tici))$term.labels),
  #             length(attributes(terms(form_expert_tici))$term.labels))
  #   
  #   
  #   # partial plot for the model: https://bgreenwell.github.io/pdp/articles/pdp-example-tensorflow.html#fn1
  #   pred_wrapper = function(object, newdata) {
  #     pred = predict(object, x = as.matrix(newdata))
  #     return(as.vector(pred[,2]))
  #   }
  #   
  #   for(l in 1:length(img_names)){
  #     
  #     pdf(paste0(dir,img_names[l]))
  #     
  #     vars = attributes(terms(formulas[l][[1]]))$term.labels
  #     
  #     for(i in vars){
  #       print(i)
  #       X_train_tmp = as.data.frame(data[[l]][,vars])
  #       
  #       info = partial(models[[l]], pred.var = i, ice = T, which.class = 1, prob=T, train=X_train_tmp, pred.fun = pred_wrapper)
  #       
  #       X_train_tmp$no = seq(1,dim(X_train_tmp)[1],1)
  #       dat_plot = merge(info, X_train_tmp, by.x="yhat.id", by.y="no")
  #       
  #       # find yhat which is closest to the predicted variables
  #       for(j in unique(dat_plot$yhat.id)){
  #         pat = dat_plot[dat_plot$yhat.id == j,]
  #         if(class(dat_plot[,paste0(i,".x")])=="factor"){
  #           k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
  #         } else{
  #           k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #         }
  #         dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #       }
  #       
  #       p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=X_train_tmp,
  #                       ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==i)]),
  #                       alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #       print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       
  #     }
  #     dev.off()
  #   }
  #   
  # }
  
  
  
  
  
  ################################################################
  #### Prediction Models BL + TICI + NIHSS 24h: Random Forest ####
  ################################################################
  
  # Include TICI + NIHSS 24h
  get_formula_nihss = function(form){
    return(as.formula(paste("mrs_3months_binary", paste(paste(form)[3],"+ tici + nihss_24h"),sep="~")))
  }
  
  #### Define the formulars we want to look at
  form_bl_nihss = get_formula_nihss(form_bl_bl)
  form_bl_mri_c_nihss = get_formula_nihss(form_bl_mri_c_bl)
  form_bl_mri_sv_nihss = get_formula_nihss(form_bl_mri_sv_bl)
  form_bl_mri_nihss = get_formula_nihss(form_bl_mri_bl)
  form_expert_nihss = get_formula_nihss(form_expert_bl)
  
  
  set.seed(3004)
  
  
  #### Models
  
  # Baseline data: Information from stroke + contralateral side
  rf_bl = randomForest(form_bl_nihss, data = train[[r]], ntree=500, importance=T)
  rf_pred_bl_test = predict(rf_bl, newdata=test[[r]])
  rf_pred_bl_test_prob = predict(rf_bl, type="prob", newdata=test[[r]])
  (bl_test = mean(test[[r]]$mrs_3months_binary == rf_pred_bl_test))
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri_c = randomForest(form_bl_mri_c_nihss, data = train[[r]], ntree=500, importance=T)
  rf_pred_bl_mri_c_test = predict(rf_bl_mri_c, newdata=test[[r]])
  rf_pred_bl_mri_c_test_prob = predict(rf_bl_mri_c, type="prob", newdata=test[[r]])
  (bl_mri_c_test = mean(test[[r]]$mrs_3months_binary == rf_pred_bl_mri_c_test))
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri_sv = randomForest(form_bl_mri_sv_nihss, data = train[[r]], ntree=500, importance=T)
  rf_pred_bl_mri_sv_test = predict(rf_bl_mri_sv, newdata=test[[r]])
  rf_pred_bl_mri_sv_test_prob = predict(rf_bl_mri_sv, type="prob", newdata=test[[r]])
  (bl_mri_sv_test = mean(test[[r]]$mrs_3months_binary == rf_pred_bl_mri_sv_test))
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri = randomForest(form_bl_mri_nihss, data = train[[r]], ntree=500, importance=T)
  rf_pred_bl_mri_test = predict(rf_bl_mri, newdata=test[[r]])
  rf_pred_bl_mri_test_prob = predict(rf_bl_mri, type="prob", newdata=test[[r]])
  (bl_mri_test = 1-mean(test[[r]]$mrs_3months_binary != rf_pred_bl_mri_test))
  
  # Expert
  rf_expert = randomForest(form_expert_nihss, data = train[[r]], ntree=500, importance=T)
  rf_pred_expert_test = predict(rf_expert, newdata=test[[r]])
  rf_pred_expert_test_prob = predict(rf_expert, type="prob", newdata=test[[r]])
  (expert_test = 1-mean(test[[r]]$mrs_3months_binary != rf_pred_expert_test))
  
  
  tab = c(bl_test, bl_mri_c_test, bl_mri_sv_test, bl_mri_test, expert_test)
  names(tab) = c("Baseline","Baseline + MRI (contralat)",
                 "Baseline + MRI (stroke)","Baseline + MRI (stroke+contralat)", "Expert")
  tab
  
  
  # load the MRI information from the model "before"
  test_pred_rf = data.frame(test[[r]], bl = rf_pred_bl_test_prob[,2], 
                            bl_mri_c = rf_pred_bl_mri_c_test_prob[,2], 
                            bl_mri_sv = rf_pred_bl_mri_sv_test_prob[,2], 
                            bl_mri = rf_pred_bl_mri_test_prob[,2],
                            expert = rf_pred_expert_test_prob[,2])
  save(test_pred_rf, file=paste0(dir,"data/CV/run",r,"/test_pred_rf_BL_TICI_NIHSS_",var_name,".R"))
  
  
  
  #### Save level names for plotting
  rf_bl_plot = change_var_names_RF(rf_bl)
  rf_bl_mri_c_plot = change_var_names_RF(rf_bl_mri_c)
  rf_bl_mri_sv_plot = change_var_names_RF(rf_bl_mri_sv)
  rf_bl_mri_plot = change_var_names_RF(rf_bl_mri)
  rf_expert_plot = change_var_names_RF(rf_expert)
  train_plot = change_var_names_DF(train[[r]])
  
  
  # #### Partial dependency plots
  # if(r==1){
  #   models = list(rf_bl_plot, rf_bl_mri_c_plot, rf_bl_mri_sv_plot, rf_bl_mri_plot, rf_expert_plot)
  #   img_names = c( paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_BL_TICI_NIHSS_",var_name,".pdf"),
  #                  paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_contralat_BL_TICI_NIHSS_",var_name,".pdf"),
  #                  paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_stroke_BL_TICI_NIHSS_",var_name,".pdf"),
  #                  paste0("images/CV/run",r,"/partial_dep_plots_RF_baseline_mri_stroke_contralat_BL_TICI_NIHSS_",var_name,".pdf"),
  #                  paste0("images/CV/run",r,"/partial_dep_plots_RF_expert_BL_TICI_NIHSS_",var_name,".pdf"))
  #   data = list(train_plot,train_plot,train_plot,train_plot,train_plot)
  #   
  #   
  #   for(l in 1:length(img_names)){
  #     
  #     pdf(paste0(dir,img_names[l]))
  #     
  #     for(i in attributes(models[[l]]$terms)$term.labels){
  #       print(i)
  #       info = partial(models[[l]], pred.var = i, ice = T, which.class = 2, rug=T, prob=T, train=data[[l]])
  #       data[[l]]$no = seq(1,dim(data[[l]])[1],1)
  #       dat_plot = merge(info, data[[l]], by.x="yhat.id", by.y="no")
  #       
  #       # find yhat which is closest to the predicted variables
  #       for(j in unique(dat_plot$yhat.id)){
  #         pat = dat_plot[dat_plot$yhat.id == j,]
  #         if(class(dat_plot[,paste0(i,".x")])=="factor"){
  #           k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
  #         } else{
  #           k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #         }
  #         dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #       }
  #       
  #       p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                       ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==i)]),
  #                       alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #       print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       
  #     }
  #     dev.off()
  #   }
  # }
  
  
  
  
  
  # ######################################################################
  # #### Prediction Models BL + TICI + NIHSS 24h: Logistic regression ####
  # ######################################################################
  # 
  # 
  # # run models
  # logreg_bl = glm(form_bl_nihss, data=train[[r]], family = binomial)
  # logreg_bl_mri_c = glm(form_bl_mri_c_nihss, data=train[[r]], family = binomial)
  # logreg_bl_mri_sv = glm(form_bl_mri_sv_nihss, data=train[[r]], family = binomial)
  # logreg_bl_mri = glm(form_bl_mri_nihss, data=train[[r]], family = binomial)
  # logreg_expert = glm(form_expert_nihss, data=train[[r]], family = binomial)
  # 
  # # predictions
  # pred_bl = predict(logreg_bl, newdata=test[[r]], type="response")
  # pred_bl_mri_c = predict(logreg_bl_mri_c, newdata=test[[r]], type="response")
  # pred_bl_mri_sv = predict(logreg_bl_mri_sv, newdata=test[[r]], type="response")
  # pred_bl_mri = predict(logreg_bl_mri, newdata=test[[r]], type="response")
  # pred_expert = predict(logreg_expert, newdata=test[[r]], type="response")
  # 
  # test_pred_logreg = data.frame(test[[r]], bl = pred_bl, bl_mri_c = pred_bl_mri_c, 
  #                               bl_mri_sv = pred_bl_mri_sv, bl_mri = pred_bl_mri,
  #                               expert = pred_expert)
  # save(test_pred_logreg, file=paste0(dir,"data/CV/run",r,"/test_pred_logreg_BL_TICI_NIHSS_",var_name,".R"))
  # 
  # 
  # # probabilities to 0,1
  # pred_bl_class = ifelse(pred_bl>0.5,1,0)
  # pred_bl_mri_c_class = ifelse(pred_bl_mri_c>0.5,1,0)
  # pred_bl_mri_sv_class = ifelse(pred_bl_mri_sv>0.5,1,0)
  # pred_bl_mri_class = ifelse(pred_bl_mri>0.5,1,0)
  # pred_expert_class = ifelse(pred_expert>0.5,1,0)
  # 
  # tab = c(mean(pred_bl_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_c_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_sv_class == test[[r]]$mrs_3months_binary),
  #         mean(pred_bl_mri_class == test[[r]]$mrs_3months_binary), mean(pred_expert_class == test[[r]]$mrs_3months_binary))
  # names(tab) = c("Baseline","Baseline + MRI (contralat)",
  #                "Baseline + MRI (stroke)","Baseline + MRI (stroke+contralat)", "Expert")
  # tab
  # 
  # 
  # #### Change level names for plotting
  # logreg_bl_plot = change_var_names_RF(logreg_bl)
  # logreg_bl_mri_c_plot = change_var_names_RF(logreg_bl_mri_c)
  # logreg_bl_mri_sv_plot = change_var_names_RF(logreg_bl_mri)
  # logreg_bl_mri_plot = change_var_names_RF(logreg_bl_mri)
  # logreg_expert_plot = change_var_names_RF(logreg_expert)
  # train_plot = change_var_names_RF(train[[r]])
  # 
  # 
  # 
  # #### Partial Dependency Plots:
  # 
  # models = list(logreg_bl_plot, logreg_bl_mri_c_plot, logreg_bl_mri_sv_plot, logreg_bl_mri_plot, logreg_expert_plot)
  # img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_BL_TICI_NIHSS_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_mri_contralat_BL_TICI_NIHSS_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_mri_stroke_BL_TICI_NIHSS_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_baseline_mri_stroke_contralat_BL_TICI_NIHSS_",var_name,".pdf"),
  #               paste0("images/CV/run",r,"/partial_dep_plots_logreg_expert_BL_TICI_NIHSS_",var_name,".pdf"))
  # data = list(train_plot,train_plot,train_plot,train_plot,train_plot)
  # 
  # for(l in 1:length(img_names)){
  #   
  #   pdf(paste0(dir,img_names[l]))
  #   
  #   for(i in attributes(models[[l]]$terms)$term.labels){
  #     print(i)
  #     info = partial(models[[l]], pred.var = i, ice = T, which.class = 2, rug=T, prob=T, train=data[[l]])
  #     data[[l]]$no = seq(1,dim(data[[l]])[1],1)
  #     dat_plot = merge(info, data[[l]], by.x="yhat.id", by.y="no")
  #     
  #     # find yhat which is closest to the predicted variables
  #     for(j in unique(dat_plot$yhat.id)){
  #       pat = dat_plot[dat_plot$yhat.id == j,]
  #       if(class(dat_plot[,paste0(i,".x")])=="factor"){
  #         k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
  #       } else{
  #         k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #       }
  #       dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #     }
  #     
  #     p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                     ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==i)]),
  #                     alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #     print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #     
  #   }
  #   dev.off()
  # }
  # 
  
  
  
  
  
  
  ########################################################
  #### Prediction Models BL + TICI + NIHSS 24h: LASSO ####
  ########################################################
  
  
  library(glmnet)
  
  lambda = 10^seq(10, -2, length = 100)
  
  lasso_bl = glmnet(model.matrix(form_bl_nihss, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_bl = cv.glmnet(model.matrix(form_bl_nihss, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_bl = predict(lasso_bl, s = cv_out_bl$lambda.min, newx = model.matrix(form_bl_nihss, test[[r]])[,-1], type="response")
  
  lasso_bl_mri_c = glmnet(model.matrix(form_bl_mri_c_nihss, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_bl_mri_c = cv.glmnet(model.matrix(form_bl_mri_c_nihss, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_bl_mri_c = predict(lasso_bl_mri_c, s = cv_out_bl_mri_c$lambda.min, newx = model.matrix(form_bl_mri_c_nihss, test[[r]])[,-1], type="response")
  
  lasso_bl_mri_sv = glmnet(model.matrix(form_bl_mri_sv_nihss, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_bl_mri_sv = cv.glmnet(model.matrix(form_bl_mri_sv_nihss, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_bl_mri_sv = predict(lasso_bl_mri_sv, s = cv_out_bl_mri_sv$lambda.min, newx = model.matrix(form_bl_mri_sv_nihss, test[[r]])[,-1], type="response")
  
  lasso_bl_mri = glmnet(model.matrix(form_bl_mri_nihss, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_bl_mri = cv.glmnet(model.matrix(form_bl_mri_nihss, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_bl_mri = predict(lasso_bl_mri, s = cv_out_bl_mri$lambda.min, newx = model.matrix(form_bl_mri_nihss, test[[r]])[,-1], type="response")
  
  lasso_expert = glmnet(model.matrix(form_expert_nihss, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha=1, lambda=lambda, family = "binomial")
  cv_out_expert = cv.glmnet(model.matrix(form_expert_nihss, train[[r]])[,-1], train[[r]]$mrs_3months_binary, alpha = 1, lambda=lambda, family="binomial")
  pred_expert = predict(lasso_expert, s = cv_out_expert$lambda.min, newx = model.matrix(form_expert_nihss, test[[r]])[,-1], type="response")
  
  
  # Save the result on the test set
  test_pred_lasso = data.frame(test[[r]], bl = as.numeric(pred_bl), bl_mri_c = as.numeric(pred_bl_mri_c), 
                               bl_mri_sv = as.numeric(pred_bl_mri_sv), bl_mri = as.numeric(pred_bl_mri),
                               expert = as.numeric(pred_expert))
  save(test_pred_lasso, file=paste0(dir,"data/CV/run",r,"/test_pred_lasso_BL_TICI_NIHSS_",var_name,".R"))
  
  
  # probabilities to 0,1
  pred_bl_class = ifelse(pred_bl>0.5,1,0)
  pred_bl_mri_c_class = ifelse(pred_bl_mri_c>0.5,1,0)
  pred_bl_mri_sv_class = ifelse(pred_bl_mri_sv>0.5,1,0)
  pred_bl_mri_class = ifelse(pred_bl_mri>0.5,1,0)
  pred_expert_class = ifelse(pred_expert>0.5,1,0)
  
  tab = c(mean(pred_bl_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_c_class == test[[r]]$mrs_3months_binary), mean(pred_bl_mri_sv_class == test[[r]]$mrs_3months_binary),
          mean(pred_bl_mri_class == test[[r]]$mrs_3months_binary), mean(pred_expert_class == test[[r]]$mrs_3months_binary))
  names(tab) = c("Baseline","Baseline + MRI (contralat)",
                 "Baseline + MRI (stroke)","Baseline + MRI (stroke+contralat)", "Expert")
  tab
  
  
  #### Change level names for plotting
  lasso_bl_plot = change_var_names_RF(lasso_bl)
  lasso_bl_mri_c_plot = change_var_names_RF(lasso_bl_mri_c)
  lasso_bl_mri_sv_plot = change_var_names_RF(lasso_bl_mri)
  lasso_bl_mri_plot = change_var_names_RF(lasso_bl_mri)
  lasso_expert_plot = change_var_names_RF(lasso_expert)
  train_plot = change_var_names_RF(train[[r]])
  
  
  
  # #### Partial Dependency Plots:
  # if(r==1){
  #   models = list(lasso_bl_plot, lasso_bl_mri_c_plot, lasso_bl_mri_sv_plot, lasso_bl_mri_plot, lasso_expert_plot)
  #   img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_BL_TICI_NIHSS_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_mri_contralat_BL_TICI_NIHSS_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_mri_stroke_BL_TICI_NIHSS_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_baseline_mri_stroke_contralat_BL_TICI_NIHSS_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_lasso_expert_BL_TICI_NIHSS_",var_name,".pdf"))
  #   data = list(train_plot,train_plot,train_plot,train_plot,train_plot)
  #   formulas = list(form_bl_nihss,form_bl_mri_c_nihss,form_bl_mri_sv_nihss,form_bl_mri_nihss,form_expert_nihss)
  #   cv_outs = list(cv_out_bl, cv_out_bl_mri_c, cv_out_bl_mri_sv, cv_out_bl_mri, cv_out_expert)
  #   
  #   
  #   for(l in 1:length(img_names)){
  #     
  #     pdf(paste0(dir,img_names[l]))
  #     # was not able to get TICI run so far
  #     vars_model = models[[l]]$beta@Dimnames[[1]]
  #     vars_model = vars_model[!grepl("tici",vars_model)]
  #     vars_data = attributes(terms(formulas[[l]]))$term.labels
  #     vars_data = vars_data[!grepl("tici",vars_data)]
  #     
  #     for(i in vars_model){
  #       print(i)
  #       
  #       pred_wrapper = function(object,newdata){ 
  #         return(predict(object, newx=newdata, s=cv_outs[[l]]$lambda.min, type="response")[,1])
  #       }
  #       
  #       info = partial(models[[l]], pred.var = i, ice = T, which.class = 1, prob=T, 
  #                      train = model.matrix(formulas[[l]], train[[r]])[,-1], 
  #                      pred.fun=pred_wrapper)
  #       data[[l]]$no = seq(1,dim(data[[l]])[1],1)
  #       dat_plot = merge(info, data[[l]], by.x="yhat.id", by.y="no")
  #       
  #       # find yhat which is closest to the predicted variables
  #       for(j in unique(dat_plot$yhat.id)){
  #         pat = dat_plot[dat_plot$yhat.id == j,]
  #         if(nrow(pat)<5){
  #           # find the equivalent name in the dataframe
  #           k = which(pat[,i] == pat[,vars_data[which(vars_model==i)]][1])
  #           dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #           
  #         } else{
  #           k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #           dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #           
  #         }
  #       }
  #       
  #       if(is.factor(train[[r]][,vars_data[which(vars_model==i)]])){
  #         p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                         ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==vars_data[which(vars_model==i)])]),
  #                         alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #         print(p + layer(lpoints(dat_plot[,vars_data[which(vars_model==i)]], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       } else{
  #         p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=data[[l]],
  #                         ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==vars_data[which(vars_model==i)])]),
  #                         alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #         print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       }
  #       
  #       
  #     }
  #     dev.off()
  #   }
  # }
  
  
  
  
  
  
  
  
  
  
  
  #################################################################
  #### Prediction Models BL + TICI + NIHSS 24h: Neural Network ####
  #################################################################
  
  nn_bl = NN(form_bl_nihss, train[[r]], test[[r]], 150, 2, 0.002, "bl", var_name, "pred_models_nihss")
  nn_bl_mri_c = NN(form_bl_mri_c_nihss, train[[r]], test[[r]], 150, 2, 0.002, "bl_mri_c", var_name, "pred_models_nihss")
  nn_bl_mri_sv = NN(form_bl_mri_sv_nihss, train[[r]], test[[r]], 150, 2, 0.002, "bl_mri_sv", var_name, "pred_models_nihss")
  nn_bl_mri = NN(form_bl_mri_nihss, train[[r]], test[[r]], 150, 2, 0.002, "bl_mri", var_name, "pred_models_nihss")
  nn_expert = NN(form_expert_nihss, train[[r]], test[[r]], 150, 2, 0.002, "expert", var_name, "pred_models_nihss")
  
  mean(test[[r]]$mrs_3months_binary == nn_bl$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_bl_mri_c$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_bl_mri_sv$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_bl_mri$pred_class)
  mean(test[[r]]$mrs_3months_binary == nn_expert$pred_class)
  
  test_pred_nn = data.frame(test[[r]], bl = nn_bl$pred, bl_mri_c = nn_bl_mri_c$pred, 
                            bl_mri_sv = nn_bl_mri_sv$pred, bl_mri = nn_bl_mri$pred, expert = nn_expert$pred)
  save(test_pred_nn, file=paste0(dir,"data/CV/run",r,"/test_pred_nn_BL_TICI_NIHSS_",var_name,".R"))
  
  
  
  
  #### Partial Dependency Plots:
  # if(r==1){
  #   formulas = list(form_bl_nihss, form_bl_mri_c_nihss, form_bl_mri_sv_nihss, form_bl_mri_nihss, form_expert_nihss)
  #   models = list(nn_bl$models, nn_bl_mri_c$models, nn_bl_mri_sv$models, nn_bl_mri$models,  nn_expert$models)
  #   img_names = c(paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_BL_TICI_NIHSS_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_mri_contralat_BL_TICI_NIHSS_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_mri_stroke_BL_TICI_NIHSS_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_baseline_mri_stroke_contralat_BL_TICI_NIHSS_",var_name,".pdf"),
  #                 paste0("images/CV/run",r,"/partial_dep_plots_NN_expert_BL_TICI_NIHSS_",var_name,".pdf"))
  #   data = list(train[[r]],train[[r]],train[[r]],train[[r]],train[[r]])
  #   n_var = c(length(attributes(terms(form_bl_nihss))$term.labels),
  #             length(attributes(terms(form_bl_mri_c_nihss))$term.labels),
  #             length(attributes(terms(form_bl_mri_sv_nihss))$term.labels),
  #             length(attributes(terms(form_bl_mri_nihss))$term.labels),
  #             length(attributes(terms(form_expert_nihss))$term.labels))
  #   
  #   
  #   # partial plot for the model: https://bgreenwell.github.io/pdp/articles/pdp-example-tensorflow.html#fn1
  #   pred_wrapper = function(object, newdata) {
  #     pred = predict(object, x = as.matrix(newdata))
  #     return(as.vector(pred[,2]))
  #   }
  #   
  #   for(l in 1:length(img_names)){
  #     
  #     pdf(paste0(dir,img_names[l]))
  #     
  #     vars = attributes(terms(formulas[l][[1]]))$term.labels
  #     
  #     for(i in vars){
  #       print(i)
  #       X_train_tmp = as.data.frame(data[[l]][,vars])
  #       
  #       info = partial(models[[l]], pred.var = i, ice = T, which.class = 1, prob=T, train=X_train_tmp, pred.fun = pred_wrapper)
  #       
  #       X_train_tmp$no = seq(1,dim(X_train_tmp)[1],1)
  #       dat_plot = merge(info, X_train_tmp, by.x="yhat.id", by.y="no")
  #       
  #       # find yhat which is closest to the predicted variables
  #       for(j in unique(dat_plot$yhat.id)){
  #         pat = dat_plot[dat_plot$yhat.id == j,]
  #         if(class(dat_plot[,paste0(i,".x")])=="factor"){
  #           k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
  #         } else{
  #           k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  #         }
  #         dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
  #       }
  #       
  #       p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=X_train_tmp,
  #                       ylab="Probability for favorable outcome", xlab = paste0(xlabels$name[which(xlabels$var==i)]),
  #                       alpha=0.2, ylim=c(0,1), scales=list(tck=c(1,0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0"))))
  #       print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5)))
  #       
  #     }
  #     dev.off()
  #   }
  # }
  # 
  
  
}



