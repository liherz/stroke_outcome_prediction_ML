rm(list=ls())
# We exclude mTICI and contralaters because they are measured AFTER therapy

library(plyr)
# set the path to the files
dir = "C:/Users/user/Dropbox/PhD/Stroke/PowerAnalyse_GewichteteMittelwerte_MixedModelSD_und_PredictionModel/"
# dir = "/Users/janne.hamann/Dropbox/StatistikOtherSideProjekt/"
setwd(dir)

# Function to get AUCs
source(paste0(dir, "analyses/functions/roc_data.R"))




# variable selection -----------------------------------------------------------

var_names = c("rbv","rbf","mtt","tmax","ttp","tmip")


# Load data from the different runs:
rf_bl_all = list()
rf_tici_all = list()
rf_nihss_all = list()
lasso_bl_all = list()
lasso_tici_all = list()
lasso_nihss_all = list()
nn_bl_all = list()
nn_tici_all = list()
nn_nihss_all = list()

for(v in 1:length(var_names)){
  rf_bl = data.frame()
  rf_tici = data.frame()
  rf_nihss = data.frame()
  lasso_bl = data.frame()
  lasso_tici = data.frame()
  lasso_nihss = data.frame()
  nn_bl = data.frame()
  nn_tici = data.frame()
  nn_nihss = data.frame()
  for(r in 1:5){
    # RF
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_rf_BL_",var_names[v],".R"))
    rf_bl = rbind(rf_bl, test_pred_rf)
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_rf_BL_TICI_",var_names[v],".R"))
    rf_tici = rbind(rf_tici, test_pred_rf)
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_rf_BL_TICI_NIHSS_",var_names[v],".R"))
    rf_nihss = rbind(rf_nihss, test_pred_rf)
    
    # LASSO
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_lasso_BL_",var_names[v],".R"))
    test_pred_lasso = rename(test_pred_lasso, replace=c("X1"="mri_c","X1.1"="mri_s","X1.2"="mri_sv","X1.3"="mri","X1.4"="bl","X1.5"="bl_mri_c","X1.6"="bl_mri_sv","X1.7"="bl_mri","X1.8"="expert"))
    lasso_bl = rbind(lasso_bl, test_pred_lasso)
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_lasso_BL_TICI_",var_names[v],".R"))
    test_pred_lasso = rename(test_pred_lasso, replace=c("X1"="bl","X1.1"="bl_mri_c","X1.2"="bl_mri_sv","X1.3"="bl_mri","X1.4"="expert"))
    lasso_tici = rbind(lasso_tici, test_pred_lasso)  
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_lasso_BL_TICI_NIHSS_",var_names[v],".R"))
    test_pred_lasso = rename(test_pred_lasso, replace=c("X1"="bl","X1.1"="bl_mri_c","X1.2"="bl_mri_sv","X1.3"="bl_mri","X1.4"="expert"))
    lasso_nihss = rbind(lasso_nihss, test_pred_lasso)
    
    # NN
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_nn_BL_",var_names[v],".R"))
    nn_bl = rbind(nn_bl, test_pred_nn)
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_nn_BL_TICI_",var_names[v],".R"))
    nn_tici = rbind(nn_tici, test_pred_nn)  
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_nn_BL_TICI_NIHSS_",var_names[v],".R"))
    nn_nihss = rbind(nn_nihss, test_pred_nn)
  }

  # RF
  rf_bl_all[[v]] = rf_bl
  rf_tici_all[[v]] = rf_tici
  rf_nihss_all[[v]] = rf_nihss
  save(rf_bl, file=paste0(dir,"data/CV/test_pred_rf_BL_",var_names[v],".R"))
  save(rf_tici, file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_",var_names[v],".R"))
  save(rf_nihss, file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_",var_names[v],".R"))
  
  # LASSO
  lasso_bl_all[[v]] = lasso_bl
  lasso_tici_all[[v]] = lasso_tici
  lasso_nihss_all[[v]] = lasso_nihss
  save(lasso_bl, file=paste0(dir,"data/CV/test_pred_lasso_BL_",var_names[v],".R"))
  save(lasso_tici, file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_",var_names[v],".R"))
  save(lasso_nihss, file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_",var_names[v],".R"))
  
  # NN
  nn_bl_all[[v]] = nn_bl
  nn_tici_all[[v]] = nn_tici
  nn_nihss_all[[v]] = nn_nihss
  save(nn_bl, file=paste0(dir,"data/CV/test_pred_nn_BL_",var_names[v],".R"))
  save(nn_tici, file=paste0(dir,"data/CV/test_pred_nn_BL_TICI_",var_names[v],".R"))
  save(nn_nihss, file=paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_",var_names[v],".R"))

}





# no variable selection --------------------------------------------------------

var_names = c("rbv","rbf","mtt","tmax","ttp","tmip")


# Load data from the different runs:
rf_bl_all = list()
rf_tici_all = list()
rf_nihss_all = list()
lasso_bl_all = list()
lasso_tici_all = list()
lasso_nihss_all = list()
nn_bl_all = list()
nn_tici_all = list()
nn_nihss_all = list()

for(v in 1:length(var_names)){
  rf_bl = data.frame()
  rf_tici = data.frame()
  rf_nihss = data.frame()
  lasso_bl = data.frame()
  lasso_tici = data.frame()
  lasso_nihss = data.frame()
  for(r in 1:5){
    # RF
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_rf_BL_",var_names[v],"_NO_VAR_SELECTION.R"))
    rf_bl = rbind(rf_bl, test_pred_rf)
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_rf_BL_TICI_",var_names[v],"_NO_VAR_SELECTION.R"))
    rf_tici = rbind(rf_tici, test_pred_rf)  
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_rf_BL_TICI_NIHSS_",var_names[v],"_NO_VAR_SELECTION.R"))
    rf_nihss = rbind(rf_nihss, test_pred_rf)
    
    # LASSO
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_lasso_BL_",var_names[v],"_NO_VAR_SELECTION.R"))
    test_pred_lasso = rename(test_pred_lasso, replace=c("X1"="mri_c","X1.1"="mri_s","X1.2"="mri_sv","X1.3"="mri","X1.4"="bl","X1.5"="bl_mri_c","X1.6"="bl_mri_sv","X1.7"="bl_mri","X1.8"="expert"))
    lasso_bl = rbind(lasso_bl, test_pred_lasso)
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_lasso_BL_TICI_",var_names[v],"_NO_VAR_SELECTION.R"))
    test_pred_lasso = rename(test_pred_lasso, replace=c("X1"="bl","X1.1"="bl_mri_c","X1.2"="bl_mri_sv","X1.3"="bl_mri","X1.4"="expert"))
    lasso_tici = rbind(lasso_tici, test_pred_lasso)  
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_lasso_BL_TICI_NIHSS_",var_names[v],"_NO_VAR_SELECTION.R"))
    test_pred_lasso = rename(test_pred_lasso, replace=c("X1"="bl","X1.1"="bl_mri_c","X1.2"="bl_mri_sv","X1.3"="bl_mri","X1.4"="expert"))
    lasso_nihss = rbind(lasso_nihss, test_pred_lasso)
  }
  
  # RF
  rf_bl_all[[v]] = rf_bl
  rf_tici_all[[v]] = rf_tici
  rf_nihss_all[[v]] = rf_nihss
  save(rf_bl, file=paste0(dir,"data/CV/test_pred_rf_BL_",var_names[v],"_NO_VAR_SELECTION.R"))
  save(rf_tici, file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_",var_names[v],"_NO_VAR_SELECTION.R"))
  save(rf_nihss, file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_",var_names[v],"_NO_VAR_SELECTION.R"))
  
  # LASSO
  lasso_bl_all[[v]] = lasso_bl
  lasso_tici_all[[v]] = lasso_tici
  lasso_nihss_all[[v]] = lasso_nihss
  save(lasso_bl, file=paste0(dir,"data/CV/test_pred_lasso_BL_",var_names[v],"_NO_VAR_SELECTION.R"))
  save(lasso_tici, file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_",var_names[v],"_NO_VAR_SELECTION.R"))
  save(lasso_nihss, file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_",var_names[v],"_NO_VAR_SELECTION.R"))
}





# All values -------------------------------------------------------------------

# Only done for BL & BL+TICI+NIHSS

# set the name for the variable of interest
var_names = c("all")


# Load data from the different runs:
rf_bl_all = list()
rf_nihss_all = list()
lasso_bl_all = list()
lasso_nihss_all = list()
nn_bl_all = list()
nn_nihss_all = list()

for(v in 1:length(var_names)){
  rf_bl = data.frame()
  rf_nihss = data.frame()
  lasso_bl = data.frame()
  lasso_nihss = data.frame()
  nn_bl = data.frame()
  nn_nihss = data.frame()
  for(r in 1:5){
    # RF
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_rf_BL_",var_names[v],".R"))
    rf_bl = rbind(rf_bl, test_pred_rf)
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_rf_BL_TICI_NIHSS_",var_names[v],".R"))
    rf_nihss = rbind(rf_nihss, test_pred_rf)
    
    # LASSO
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_lasso_BL_",var_names[v],".R"))
    test_pred_lasso = rename(test_pred_lasso, replace=c("X1"="mri_c","X1.1"="mri_s","X1.2"="mri_sv","X1.3"="mri","X1.4"="bl","X1.5"="bl_mri_c","X1.6"="bl_mri_sv","X1.7"="bl_mri","X1.8"="expert"))
    lasso_bl = rbind(lasso_bl, test_pred_lasso)
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_lasso_BL_TICI_NIHSS_",var_names[v],".R"))
    test_pred_lasso = rename(test_pred_lasso, replace=c("X1"="bl","X1.1"="bl_mri_c","X1.2"="bl_mri_sv","X1.3"="bl_mri","X1.4"="expert"))
    lasso_nihss = rbind(lasso_nihss, test_pred_lasso)
    
    # NN
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_nn_BL_",var_names[v],".R"))
    nn_bl = rbind(nn_bl, test_pred_nn)
    load(file=paste0(dir,"data/CV/run",r,"/test_pred_nn_BL_TICI_NIHSS_",var_names[v],".R"))
    nn_nihss = rbind(nn_nihss, test_pred_nn)
  }
  
  # RF
  rf_bl_all[[v]] = rf_bl
  rf_nihss_all[[v]] = rf_nihss
  save(rf_bl, file=paste0(dir,"data/CV/test_pred_rf_BL_",var_names[v],".R"))
  save(rf_nihss, file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_",var_names[v],".R"))
  
  # LASSO
  lasso_bl_all[[v]] = lasso_bl
  lasso_nihss_all[[v]] = lasso_nihss
  save(lasso_bl, file=paste0(dir,"data/CV/test_pred_lasso_BL_",var_names[v],".R"))
  save(lasso_nihss, file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_",var_names[v],".R"))
  
  # NN
  nn_bl_all[[v]] = nn_bl
  nn_nihss_all[[v]] = nn_nihss
  save(nn_bl, file=paste0(dir,"data/CV/test_pred_nn_BL_",var_names[v],".R"))
  save(nn_nihss, file=paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_",var_names[v],".R"))
  
}
