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
library(dplyr)

# set the path to the files
dir = "C:/Users/hezo/Dropbox/PhD/Stroke/PowerAnalyse_GewichteteMittelwerte_MixedModelSD_und_PredictionModel/"
# dir = "/Users/janne.hamann/Dropbox/StatistikOtherSideProjekt/"
setwd(dir)

# set the name for the variable of interest
var_name = "rbv"

# source the files with the functions we need
source(paste0(dir, "analyses/functions/roc_data.R"))
# source(paste0(dir, "analyses/functions/get_partial_plots.R"))
source(paste0(dir, "analyses/functions/change_var_names.R"))





#####################
#### Preparation ####
#####################

# Load the data with all variables included (with TICI and NIHSS 24h)
load(paste0(dir,"/data/data_",var_name,"_wide_nihss.R"))

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
                     "C_LN","C_Th","C_Occ","C_Medp","C_Medm","C_Meda","C_Ant",
                     "S_LN","S_Th","S_Occ","S_Medp","S_Medm","S_Meda","S_Ant",
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
form_mri_c = formula(mrs_3months_binary ~ C_LN+C_Th+C_Occ+C_Medp+C_Medm+C_Meda+C_Ant)

form_mri_s = formula(mrs_3months_binary ~ S_LN+S_Th+S_Occ+S_Medp+S_Medm+S_Meda+S_Ant)

form_mri_sv = formula(mrs_3months_binary ~ S_LN+S_Th+S_Occ+S_Medp+S_Medm+S_Meda+S_Ant
                      + volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar)

form_mri = formula(mrs_3months_binary ~ C_LN+C_Th+C_Occ+C_Medp+C_Medm+C_Meda+C_Ant
                   + S_LN+S_Th+S_Occ+S_Medp+S_Medm+S_Meda+S_Ant
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
                        + C_LN+C_Th+C_Occ+C_Medp+C_Medm+C_Meda+C_Ant)

form_bl_mri_sv = formula(mrs_3months_binary ~ age+sex+independent_pre_stroke+nihss_bl
                         + tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
                         + sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr
                         + atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie
                         + rf_smoker+rf_chd+rf_pavk+rf_tia_stroke+infarct_side+additional_occlusion
                         + lyse+time_to_imaging+time_to_groin_puncture+collateralization+anaesthesia
                         + S_LN+S_Th+S_Occ+S_Medp+S_Medm+S_Meda+S_Ant
                         + volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar)

form_bl_mri = formula(mrs_3months_binary ~ age+sex+independent_pre_stroke+nihss_bl
                      + tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
                      + sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr
                      + atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie
                      + rf_smoker+rf_chd+rf_pavk+rf_tia_stroke+infarct_side+additional_occlusion
                      + lyse+time_to_imaging+time_to_groin_puncture+collateralization+anaesthesia
                      + C_LN+C_Th+C_Occ+C_Medp+C_Medm+C_Meda+C_Ant
                      + S_LN+S_Th+S_Occ+S_Medp+S_Medm+S_Meda+S_Ant
                      + volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar)

form_expert = formula(mrs_3months_binary ~ age+nihss_bl+rf_hypertonia+rf_diabetes+time_to_groin_puncture+
                        sys_bloodpressure_bl+collateralization+S_Medm+volume_adc+volume_tar+rf_smoker+lyse+rf_tia_stroke)


rf_mri_sv = list()
rf_mri = list()
rf_bl = list()
rf_bl_mri_sv = list()
rf_bl_mri = list()
rf_expert = list()

for(r in 1:5){
  
  ##########################################
  #### Variable Selction: Random Forest ####
  ##########################################
  
  set.seed(3004)

  # MRI data: Information from stroke side + volume
  rf_mri_sv[[r]] = randomForest(form_mri_sv, data=train[[r]], ntree=500, importance=T)
  
  # MRI data: Information from stroke + contralateral side
  rf_mri[[r]] = randomForest(form_mri, data=train[[r]], ntree=500, importance=T)
  
  # Baseline data: Information from stroke + contralateral side
  rf_bl[[r]] = randomForest(form_bl, data=train[[r]], ntree=500, importance=T)
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri_sv[[r]] = randomForest(form_bl_mri_sv, data=train[[r]], ntree=500, importance=T)
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri[[r]] = randomForest(form_bl_mri, data=train[[r]], ntree=500, importance=T)
  
  # Expert
  rf_expert[[r]] = randomForest(form_expert, data=train[[r]], ntree=500, importance=T)
}





#### Variance importance plots

# MeanDecreaseAccuracy: Get OOb performance of tree, than permute variable of interest randomly and measure the OOB performance again
# Then, calculate the difference between the two
# Negative: Random permutation of the variable is better than the variable --> unimportant

xlabels = data.frame(var=c("C_LN","C_Th","C_Ant","C_Meda","C_Medm","C_Medp","C_Occ",
                           "S_LN","S_Th","S_Ant","S_Meda","S_Medm","S_Medp","S_Occ",
                           "volume_adc", "volume_tmax", "volume_tar",
                           "volume_rbf_adc","volume_rbf_tmax","volume_rbv_adc","volume_rbv_tmax",
                           "age","sex","independent_pre_stroke","nihss_bl",
                           "tah_pre_stroke","antikoagulation_pre_stroke",
                           "statin_pre_stroke","antihypertensiva_pre_stroke",
                           "sys_bloodpressure_bl","dias_bloodpressure_bl",
                           "glucose_bl","hba1c","ldl","hdl","triglyceride","crp","inr",
                           "atrial_fibrillation","rf_diabetes","rf_hypertonia","rf_hypercholesterinaemie",
                           "rf_smoker","rf_chd","rf_pavk",
                           "rf_tia_stroke","infarct_side","additional_occlusion",
                           "lyse","time_to_imaging","time_to_groin_puncture",
                           "collateralization","anaesthesia",
                           "tici","nihss_24h"),
                     name=c("LN - contralateral side","Th - contralateral side","Ant - contralateral side","M ant - contralateral side",
                            "M med - contralateral side","M post - contralateral side","Post - contralateral side","LN - stroke side",
                            "Th - stroke side","Ant - stroke side","M ant - stroke side","M med - stroke side",
                            "M post - stroke side","Post - stroke side",
                            "Ischaemic core", "Area of hypoperfusion", "Tissue at risk",
                            "CBF in core","CBF in area of hypoperfusion","CBV in core", "CBV in area of hypoperfusion",
                            "Age (y)","Sex","Independent before stroke","NIHSS baseline",
                            "Previous antiplatelet therapy","Previous oral anticoagulation",
                            "Previous statin therapy","Previous antihypertensive therapy",
                            "Systolic blood pressure (mmHg)","Diastolic blood pressure (mmHg)",
                            "Glucose (mmol/l)","HbA1c (%)","LDL (mmol/l)","HDL (mmol/l)","TG (mmol/l)","CRP (mg/l)","INR",
                            "Atrial Fibrillation","Diabetes","Hypertension","Dyslipidemia",
                            "Smoker","Coronary heart disease","Peripheral arterial occlusive disease",
                            "Past ischemic events","Infarct side","Additional occlusion",
                            "IVT","Onset to imaging (min)","Onset to groin puncture (min)",
                            "Collateralization status","General anaesthesia",
                            "TICI","NIHSS 24h"))

get_imp_plot = function(rf){
  
  imp_plot = varImpPlot(rf[[1]], cex=1.8, pch=16)
  nmax = ifelse(nrow(imp_plot)>=10, 10, nrow(imp_plot))
  imp_plot = imp_plot[order(imp_plot[,"MeanDecreaseAccuracy"], decreasing = T), ]
  imp_plot_df = data.frame(mean_decrease_accuracy = imp_plot[1:nmax, "MeanDecreaseAccuracy"],
                           variables = names(imp_plot[1:nmax, "MeanDecreaseAccuracy"]),
                           run = rep(1, nmax))
  
  for(r in 1:5){
    imp_plot = varImpPlot(rf[[r]], main="MRI (contralat)", cex=1.8, pch=16)
    imp_plot = imp_plot[order(imp_plot[,"MeanDecreaseAccuracy"], decreasing = T), ]
    imp_plot_df = rbind(imp_plot_df, data.frame(mean_decrease_accuracy = imp_plot[1:nmax, "MeanDecreaseAccuracy"],
                                                variables = names(imp_plot[1:nmax, "MeanDecreaseAccuracy"]),
                                                run = rep(r, nmax)))
  }
  imp_plot_df = imp_plot_df %>% group_by(variables) %>% mutate(mean = mean(mean_decrease_accuracy))
  imp_plot_df = as.data.frame(imp_plot_df)
  
  plot_rf = imp_plot_df
  plot_rf = merge(plot_rf, xlabels, by.x = "variables", by.y = "var", all.x = T, all.y = F)
  plot_rf$name = droplevels(plot_rf$name)
  plot_rf$name = reorder(plot_rf$name, plot_rf$mean)
  plot_rf$name
  p = ggplot(plot_rf, aes(x = name, y = mean_decrease_accuracy)) + 
    geom_point(size = 2.5) +
    geom_point(aes(x = name, y = mean), col = "darksalmon", shape = 18, size = 3) +
    coord_flip() + 
    ylab("Mean Decrease in Accuracy") +
    xlab("") +
    theme_bw() + 
    theme(axis.line = element_line(colour = "black"),
          # panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.border = element_blank(),
          panel.background = element_blank(), #) +
          axis.text.x = element_text(color = "black", size = 12, angle=0, hjust = .5, vjust = 0, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle=0), #, hjust = .5, vjust = 0, face = "plain"),
          axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
          legend.background = element_blank(),
          legend.text = element_text(color = "black", size = 16),
          legend.title = element_text(color = "black", size = 16))
  
  return(p)
}



plot_rf_mri_sv = get_imp_plot(rf_mri_sv)
plot_rf_mri = get_imp_plot(rf_mri)
plot_rf_bl = get_imp_plot(rf_bl)
plot_rf_bl_mri_sv = get_imp_plot(rf_bl_mri_sv)
plot_rf_bl_mri = get_imp_plot(rf_bl_mri)
plot_rf_expert = get_imp_plot(rf_expert)


pdf(paste0(dir,"images/var_imp_plots_RF_VAR_SELECTION_",var_name,"_MRI_S.pdf"), width=12, height = 10)
plot_rf_mri_sv
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_VAR_SELECTION_",var_name,"_MRI.pdf"), width=12, height = 10)
plot_rf_mri
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_VAR_SELECTION_",var_name,"_BL.pdf"), width=12, height = 10)
plot_rf_bl
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_VAR_SELECTION_",var_name,"_BL_MRI_S.pdf"), width=12, height = 10)
plot_rf_bl_mri_sv
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_VAR_SELECTION_",var_name,"_BL_MRI.pdf"), width=12, height = 10)
plot_rf_bl_mri
dev.off()


# Generate one plot
rf_var_sel = rbind(data.frame(plot_rf_mri_sv$data, model = "mri_sv"), 
                   data.frame(plot_rf_mri$data, model = "mri"),
                   data.frame(plot_rf_bl$data, model = "bl"),
                   data.frame(plot_rf_bl_mri_sv$data, model = "bl_mri_sv"),
                   data.frame(plot_rf_bl_mri$data, model = "bl_mri"),
                   data.frame(plot_rf_expert$data, model = "expert"))

rf_var_sel$name = reorder(rf_var_sel$name, rf_var_sel$mean)
rf_var_sel$name

p = ggplot(rf_var_sel, aes(x = name, y = mean_decrease_accuracy)) + 
  geom_point(size = 0.8) +
  geom_point(aes(x = name, y = mean, colour = model), shape = 18, size = 3) +
  coord_flip() + 
  ylab("Mean Decrease in Accuracy") +
  xlab("") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 12, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle=0), #, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) + 
  scale_colour_manual(values=c(mri_sv = "darkseagreen", mri = "dodgerblue4", 
                               bl = "darkred", bl_mri_sv = "darkorchid4",
                               bl_mri = "darksalmon", expert = "mediumorchid2"),
                      breaks = c("mri_sv", "mri", "bl", "bl_mri_sv", "bl_mri", "expert"),
                      labels = c("MRI(S)", "MRI", "PtC", "PtC + MRI(S)", "PtC + MRI", "Expert"),
                      name = "Predictor set") # +
p

pdf(paste0(dir,"images/var_imp_plots_RF_VAR_SELECTION_",var_name,"_ALL_MODELS.pdf"), width=12, height = 10)
p
dev.off()



#############################################
#### Prediction Models BL: Random Forest ####
#############################################


# Get the formular with the most important predictors as input
get_formula = function(mod, n){
  return(as.formula(paste("mrs_3months_binary", paste(names(sort(importance(mod)[,"MeanDecreaseAccuracy"], decreasing=T))[1:n],collapse="+"), sep="~")))
}


#### Define the formulars we want to look at
form_mri_sv_bl = list()
form_mri_bl = list()
form_bl_bl = list()
form_bl_mri_sv_bl = list()
form_bl_mri_bl = list()
form_expert_bl = list()

for(r in 1:5){

  form_mri_sv_bl[[r]] = get_formula(rf_mri_sv[[r]], 10)
  form_mri_bl[[r]] = get_formula(rf_mri[[r]], 10)
  form_bl_bl[[r]] = get_formula(rf_bl[[r]], 10)
  form_bl_mri_sv_bl[[r]] = get_formula(rf_bl_mri_sv[[r]], 10)
  form_bl_mri_bl[[r]] = get_formula(rf_bl_mri[[r]], 10)
  form_expert_bl[[r]] = form_expert
}



for(r in 1:5){
  
  set.seed(3004)
  
  # MRI data: Information from stroke side + volume
  rf_mri_sv[[r]] = randomForest(form_mri_sv_bl[[r]], data=train[[r]], ntree=500, importance=T)

  # MRI data: Information from stroke + contralateral side
  rf_mri[[r]] = randomForest(form_mri_bl[[r]], data=train[[r]], ntree=500, importance=T)

  # Baseline data: Information from stroke + contralateral side
  rf_bl[[r]] = randomForest(form_bl_bl[[r]], data=train[[r]], ntree=500, importance=T)

  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri_sv[[r]] = randomForest(form_bl_mri_sv_bl[[r]], data=train[[r]], ntree=500, importance=T)

  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri[[r]] = randomForest(form_bl_mri_bl[[r]], data=train[[r]], ntree=500, importance=T)

  # Expert
  rf_expert[[r]] = randomForest(form_expert_bl[[r]], data=train[[r]], ntree=500, importance=T)

}


plot_rf_mri_sv = get_imp_plot(rf_mri_sv)
plot_rf_mri = get_imp_plot(rf_mri)
plot_rf_bl = get_imp_plot(rf_bl)
plot_rf_bl_mri_sv = get_imp_plot(rf_bl_mri_sv)
plot_rf_bl_mri = get_imp_plot(rf_bl_mri)
plot_rf_expert = get_imp_plot(rf_expert)

pdf(paste0(dir,"images/var_imp_plots_RF_BL_",var_name,"_MRI_S.pdf"), width=12, height = 10)
plot_rf_mri_sv
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_BL_",var_name,"_MRI.pdf"), width=12, height = 10)
plot_rf_mri
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_BL_",var_name,"_BL.pdf"), width=12, height = 10)
plot_rf_bl
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_BL_",var_name,"_BL_MRI_S.pdf"), width=12, height = 10)
plot_rf_bl_mri_sv
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_BL_",var_name,"_BL_MRI.pdf"), width=12, height = 10)
plot_rf_bl_mri
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_BL_",var_name,"_Expert.pdf"), width=12, height = 10)
plot_rf_expert
dev.off()


# Generate one plot
rf_tp_bl = rbind(data.frame(plot_rf_mri_sv$data, model = "mri_sv"), 
                   data.frame(plot_rf_mri$data, model = "mri"),
                   data.frame(plot_rf_bl$data, model = "bl"),
                   data.frame(plot_rf_bl_mri_sv$data, model = "bl_mri_sv"),
                   data.frame(plot_rf_bl_mri$data, model = "bl_mri"),
                   data.frame(plot_rf_expert$data, model = "expert"))

rf_tp_bl$name = reorder(rf_tp_bl$name, rf_tp_bl$mean)
rf_tp_bl$name

p = ggplot(rf_tp_bl, aes(x = name, y = mean_decrease_accuracy)) + 
  geom_point(size = 0.8) +
  geom_point(aes(x = name, y = mean, colour = model), shape = 18, size = 3) +
  coord_flip() + 
  ylab("Mean Decrease in Accuracy") +
  xlab("") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 12, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle=0), #, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) + 
  scale_colour_manual(values=c(mri_sv = "darkseagreen", mri = "dodgerblue4", 
                               bl = "darkred", bl_mri_sv = "darkorchid4",
                               bl_mri = "darksalmon", expert = "mediumorchid2"),
                      breaks = c("mri_sv", "mri", "bl", "bl_mri_sv", "bl_mri", "expert"),
                      labels = c("MRI(S)", "MRI", "PtC", "PtC + MRI(S)", "PtC + MRI", "Expert"),
                      name = "Predictor set") # +
p

pdf(paste0(dir,"images/var_imp_plots_RF_BL_",var_name,"_ALL_MODELS.pdf"), width=12, height = 10)
p
dev.off()



####################################################
#### Prediction Models BL + TICI: Random Forest ####
####################################################

# Include TICI
get_formula_tici = function(form){
  return(as.formula(paste("mrs_3months_binary", paste(paste(form)[3],"+ tici"),sep="~")))
}

#### Define the formulars we want to look at
form_bl_tici = list()
form_bl_mri_sv_tici = list()
form_bl_mri_tici = list()
form_expert_tici = list()

for(r in 1:5){
  form_bl_tici[[r]] = get_formula_tici(form_bl_bl[[r]])
  form_bl_mri_sv_tici[[r]] = get_formula_tici(form_bl_mri_sv_bl[[r]])
  form_bl_mri_tici[[r]] = get_formula_tici(form_bl_mri_bl[[r]])
  form_expert_tici[[r]] = get_formula_tici(form_expert_bl[[r]])
}



for(r in 1:5){
  
  set.seed(3004)
  
  # Baseline data: Information from stroke + contralateral side
  rf_bl[[r]] = randomForest(form_bl_tici[[r]], data = train[[r]], ntree=500, importance=T)
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri_sv[[r]] = randomForest(form_bl_mri_sv_tici[[r]], data = train[[r]], ntree=500, importance=T)
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri[[r]] = randomForest(form_bl_mri_tici[[r]], data = train[[r]], ntree=500, importance=T)
  
  # Expert
  rf_expert[[r]] = randomForest(form_expert_tici[[r]], data = train[[r]], ntree=500, importance=T)
  
}


plot_rf_bl = get_imp_plot(rf_bl)
plot_rf_bl_mri_sv = get_imp_plot(rf_bl_mri_sv)
plot_rf_bl_mri = get_imp_plot(rf_bl_mri)
plot_rf_expert = get_imp_plot(rf_expert)


pdf(paste0(dir,"images/var_imp_plots_RF_BL_TICI_",var_name,"_BL.pdf"), width=12, height = 10)
plot_rf_bl
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_BL_TICI_",var_name,"_BL_MRI_S.pdf"), width=12, height = 10)
plot_rf_bl_mri_sv
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_BL_TICI_",var_name,"_BL_MRI.pdf"), width=12, height = 10)
plot_rf_bl_mri
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_BL_TICI_",var_name,"_Expert.pdf"), width=12, height = 10)
plot_rf_expert
dev.off()


# Generate one plot
rf_tp_tici = rbind(data.frame(plot_rf_mri_sv$data, model = "mri_sv"), 
                 data.frame(plot_rf_mri$data, model = "mri"),
                 data.frame(plot_rf_bl$data, model = "bl"),
                 data.frame(plot_rf_bl_mri_sv$data, model = "bl_mri_sv"),
                 data.frame(plot_rf_bl_mri$data, model = "bl_mri"),
                 data.frame(plot_rf_expert$data, model = "expert"))

rf_tp_tici$name = reorder(rf_tp_tici$name, rf_tp_tici$mean)
rf_tp_tici$name

p = ggplot(rf_tp_tici, aes(x = name, y = mean_decrease_accuracy)) + 
  geom_point(size = 0.8) +
  geom_point(aes(x = name, y = mean, colour = model), shape = 18, size = 3) +
  coord_flip() + 
  ylab("Mean Decrease in Accuracy") +
  xlab("") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 12, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle=0), #, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) + 
  scale_colour_manual(values=c(mri_sv = "darkseagreen", mri = "dodgerblue4", 
                               bl = "darkred", bl_mri_sv = "darkorchid4",
                               bl_mri = "darksalmon", expert = "mediumorchid2"),
                      breaks = c("mri_sv", "mri", "bl", "bl_mri_sv", "bl_mri", "expert"),
                      labels = c("MRI(S)", "MRI", "PtC", "PtC + MRI(S)", "PtC + MRI", "Expert"),
                      name = "Predictor set") # +
p

pdf(paste0(dir,"images/var_imp_plots_RF_TICI_",var_name,"_ALL_MODELS.pdf"), width=12, height = 10)
p
dev.off()



################################################################
#### Prediction Models BL + TICI + NIHSS 24h: Random Forest ####
################################################################


# Include TICI + NIHSS 24h
get_formula_nihss = function(form){
  return(as.formula(paste("mrs_3months_binary", paste(paste(form)[3],"+ tici + nihss_24h"),sep="~")))
}

#### Define the formulars we want to look at
form_bl_nihss = list()
form_bl_mri_sv_nihss = list()
form_bl_mri_nihss = list()
form_expert_nihss = list()
for(r in 1:5){
  form_bl_nihss[[r]] = get_formula_nihss(form_bl_bl[[r]])
  form_bl_mri_sv_nihss[[r]] = get_formula_nihss(form_bl_mri_sv_bl[[r]])
  form_bl_mri_nihss[[r]] = get_formula_nihss(form_bl_mri_bl[[r]])
  form_expert_nihss[[r]] = get_formula_nihss(form_expert_bl[[r]])
}


for(r in 1:5){
  
  set.seed(3004)
  
  # Baseline data: Information from stroke + contralateral side
  rf_bl[[r]] = randomForest(form_bl_nihss[[r]], data = train[[r]], ntree=500, importance=T)
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri_sv[[r]] = randomForest(form_bl_mri_sv_nihss[[r]], data = train[[r]], ntree=500, importance=T)
  
  # Baseline + MRI data: Information from stroke + contralateral side
  rf_bl_mri[[r]] = randomForest(form_bl_mri_nihss[[r]], data = train[[r]], ntree=500, importance=T)
  
  # Expert
  rf_expert[[r]] = randomForest(form_expert_nihss[[r]], data = train[[r]], ntree=500, importance=T)
}


plot_rf_bl = get_imp_plot(rf_bl)
plot_rf_bl_mri_sv = get_imp_plot(rf_bl_mri_sv)
plot_rf_bl_mri = get_imp_plot(rf_bl_mri)
plot_rf_expert = get_imp_plot(rf_expert)


pdf(paste0(dir,"images/var_imp_plots_RF_BL_TICI_NIHSS_",var_name,"_BL.pdf"), width=12, height = 10)
plot_rf_bl
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_BL_TICI_NIHSS_",var_name,"_BL_MRI_S.pdf"), width=12, height = 10)
plot_rf_bl_mri_sv
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_BL_TICI_NIHSS_",var_name,"_BL_MRI.pdf"), width=12, height = 10)
plot_rf_bl_mri
dev.off()

pdf(paste0(dir,"images/var_imp_plots_RF_BL_TICI_NIHSS_",var_name,"_Expert.pdf"), width=12, height = 10)
plot_rf_expert
dev.off()


# Generate one plot
rf_tp_nihss = rbind(data.frame(plot_rf_mri_sv$data, model = "mri_sv"), 
                 data.frame(plot_rf_mri$data, model = "mri"),
                 data.frame(plot_rf_bl$data, model = "bl"),
                 data.frame(plot_rf_bl_mri_sv$data, model = "bl_mri_sv"),
                 data.frame(plot_rf_bl_mri$data, model = "bl_mri"),
                 data.frame(plot_rf_expert$data, model = "expert"))

rf_tp_nihss$name = reorder(rf_tp_nihss$name, rf_tp_nihss$mean)
rf_tp_nihss$name

p = ggplot(rf_tp_nihss, aes(x = name, y = mean_decrease_accuracy)) + 
  geom_point(size = 0.8) +
  geom_point(aes(x = name, y = mean, colour = model), shape = 18, size = 3) +
  coord_flip() + 
  ylab("Mean Decrease in Accuracy") +
  xlab("") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 12, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle=0), #, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) + 
  scale_colour_manual(values=c(mri_sv = "darkseagreen", mri = "dodgerblue4", 
                               bl = "darkred", bl_mri_sv = "darkorchid4",
                               bl_mri = "darksalmon", expert = "mediumorchid2"),
                      breaks = c("mri_sv", "mri", "bl", "bl_mri_sv", "bl_mri", "expert"),
                      labels = c("MRI(S)", "MRI", "PtC", "PtC + MRI(S)", "PtC + MRI", "Expert"),
                      name = "Predictor set") # +
p

pdf(paste0(dir,"images/var_imp_plots_RF_NIHSS_",var_name,"_ALL_MODELS.pdf"), width=12, height = 10)
p
dev.off()
