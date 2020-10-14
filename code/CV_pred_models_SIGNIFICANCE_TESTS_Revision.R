rm(list=ls())

library(pROC)
library(openxlsx)

# set the path to the files
dir = "C:/Users/user/Dropbox/PhD/Stroke/PowerAnalyse_GewichteteMittelwerte_MixedModelSD_und_PredictionModel/"
# dir = "/Users/janne.hamann/Dropbox/StatistikOtherSideProjekt/"
setwd(dir)

source(paste0(dir, "analyses/functions/roc_data.R"))




# rCBV -------------------------------------------------------------------------

# Need to analyse THRIVE only once
dat0 = read.xlsx(paste0(dir,"data/Data_THRIVE_20190312.xlsx"), sheet=1, na.strings="NA")
head(dat0)
load(paste0(dir,"/data/data_rbv_wide_nihss.R"))
thrive = merge(dat0, dat, by.x="PID", by.y="p_id")

load(file=paste0(dir,"data/CV/test_pred_rf_BL_rbv.R"))
load(file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_rbv.R"))

load(file=paste0(dir,"data/CV/test_pred_lasso_BL_rbv.R"))
load(file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_rbv.R"))

load(file=paste0(dir,"data/CV/test_pred_nn_BL_rbv.R"))
load(file=paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_rbv.R"))

thrive_bl = roc(thrive$mrs_3months_binary, 1-thrive$p_good_outcome)

rf_bl_rbv = rf_bl
lasso_bl_rbv = lasso_bl
nn_bl_rbv = nn_bl

rf_nihss_rbv = rf_nihss
lasso_nihss_rbv = lasso_nihss
nn_nihss_rbv = nn_nihss


# Stroke MRI only: No baseline data, always the same
rf_bl_mri_sv = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$mri_sv)
lasso_bl_mri_sv = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$mri_sv)
nn_bl_mri_sv = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$mri_sv)

# MRI only
rf_bl_mri = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$mri)
lasso_bl_mri = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$mri)
nn_bl_mri = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$mri)

# Baseline
rf_bl_bl = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$bl)
rf_nihss_bl = roc(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$bl)

lasso_bl_bl = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$bl)
lasso_nihss_bl = roc(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$bl)

nn_bl_bl = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$bl)
nn_nihss_bl = roc(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$bl)


# Baseline + stroke MRI
rf_bl_bl_mri_sv = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$bl_mri_sv)
rf_nihss_bl_mri_sv = roc(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$bl_mri_sv)

lasso_bl_bl_mri_sv = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$bl_mri_sv)
lasso_nihss_bl_mri_sv = roc(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$bl_mri_sv)

nn_bl_bl_mri_sv = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$bl_mri_sv)
nn_nihss_bl_mri_sv = roc(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$bl_mri_sv)


# Baseline + MRI
rf_bl_bl_mri = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$bl_mri)
rf_nihss_bl_mri = roc(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$bl_mri)

lasso_bl_bl_mri = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$bl_mri)
lasso_nihss_bl_mri = roc(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$bl_mri)

nn_bl_bl_mri = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$bl_mri)
nn_nihss_bl_mri = roc(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$bl_mri)


# Expert
rf_bl_expert = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$expert)
rf_nihss_expert = roc(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$expert)

lasso_bl_expert = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$expert)
lasso_nihss_expert = roc(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$expert)

nn_bl_expert = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$expert)
nn_nihss_expert = roc(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$expert)


#Tests

# MRI(SV) vs. MRI
rf_bl_mri_sv_mri = roc.test(rf_bl_mri_sv, rf_bl_mri, method="bootstrap")
lasso_bl_mri_sv_mri = roc.test(lasso_bl_mri_sv, lasso_bl_mri, method="bootstrap")
nn_bl_mri_sv_mri = roc.test(nn_bl_mri_sv, nn_bl_mri, method="bootstrap")

# MRI vs. Baseline
rf_bl_mri_bl = roc.test(rf_bl_mri, rf_bl_bl, method="bootstrap")
lasso_bl_mri_bl = roc.test(lasso_bl_mri, lasso_bl_bl, method="bootstrap")
nn_bl_mri_bl = roc.test(nn_bl_mri, nn_bl_bl, method="bootstrap")

# Baseline vs. Baseline + MRI(sv)
rf_bl_bl_bl_mri_sv = roc.test(rf_bl_bl, rf_bl_bl_mri_sv, method="bootstrap")
lasso_bl_bl_bl_mri_sv = roc.test(lasso_bl_bl, lasso_bl_bl_mri_sv, method="bootstrap")
nn_bl_bl_bl_mri_sv = roc.test(nn_bl_bl, nn_bl_bl_mri_sv, method="bootstrap")

rf_nihss_bl_bl_mri_sv = roc.test(rf_nihss_bl, rf_nihss_bl_mri_sv, method="bootstrap")
lasso_nihss_bl_bl_mri_sv = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri_sv, method="bootstrap")
nn_nihss_bl_bl_mri_sv = roc.test(nn_nihss_bl, nn_nihss_bl_mri_sv, method="bootstrap")

# Baseline vs. Baseline + MRI
rf_bl_bl_bl_mri = roc.test(rf_bl_bl, rf_bl_bl_mri, method="bootstrap")
lasso_bl_bl_bl_mri = roc.test(lasso_bl_bl, lasso_bl_bl_mri, method="bootstrap")
nn_bl_bl_bl_mri = roc.test(nn_bl_bl, nn_bl_bl_mri, method="bootstrap")

rf_nihss_bl_bl_mri = roc.test(rf_nihss_bl, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_bl_bl_mri = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_bl_bl_mri = roc.test(nn_nihss_bl, nn_nihss_bl_mri, method="bootstrap")

# Thrive vs. Baseline
rf_bl_thrive_bl = roc.test(thrive_bl, rf_bl_bl, method="bootstrap")
lasso_bl_thrive_bl = roc.test(thrive_bl, lasso_bl_bl, method="bootstrap")
nn_bl_thrive_bl = roc.test(thrive_bl, nn_bl_bl, method="bootstrap")

# Expert vs. Baseline (MRI) as Expert contains MRI data
rf_bl_expert_bl_mri = roc.test(rf_bl_expert, rf_bl_bl_mri, method="bootstrap")
lasso_bl_expert_bl_mri = roc.test(lasso_bl_expert, lasso_bl_bl_mri, method="bootstrap")
nn_bl_expert_bl_mri = roc.test(nn_bl_expert, nn_bl_bl_mri, method="bootstrap")

rf_nihss_expert_bl_mri = roc.test(rf_nihss_expert, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_expert_bl_mri = roc.test(lasso_nihss_expert, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_expert_bl_mri = roc.test(nn_nihss_expert, nn_nihss_bl_mri, method="bootstrap")

# Expert vs. Baseline
rf_bl_bl_expert = roc.test(rf_bl_bl, rf_bl_expert, method="bootstrap")
lasso_bl_bl_expert = roc.test(lasso_bl_bl, lasso_bl_expert, method="bootstrap")
nn_bl_bl_expert = roc.test(nn_bl_bl, nn_bl_expert, method="bootstrap")

rf_nihss_bl_expert = roc.test(rf_nihss_bl, rf_nihss_expert, method="bootstrap")
lasso_nihss_bl_expert = roc.test(lasso_nihss_bl, lasso_nihss_expert, method="bootstrap")
nn_nihss_bl_expert = roc.test(nn_nihss_bl, nn_nihss_expert, method="bootstrap")


# Create tables
get_values = function(dat){
  return(c("AUC1"=dat$roc1$auc, "AUC2"=dat$roc2$auc, "pvalue"=dat$p.value))
}

##### bl
# MRI sv vs. MRI
# MRI vs. BL
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# THRIVE vs. BL
# BL vs. Expert
tab_rf_bl = rbind(get_values(rf_bl_mri_sv_mri),
                  get_values(rf_bl_mri_bl),
                  get_values(rf_bl_bl_bl_mri_sv),
                  get_values(rf_bl_bl_bl_mri),
                  get_values(rf_bl_thrive_bl),
                  get_values(rf_bl_expert_bl_mri),
                  get_values(rf_bl_bl_expert))
row.names(tab_rf_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_lasso_bl = rbind(get_values(lasso_bl_mri_sv_mri),
                     get_values(lasso_bl_mri_bl),
                     get_values(lasso_bl_bl_bl_mri_sv),
                     get_values(lasso_bl_bl_bl_mri),
                     get_values(lasso_bl_thrive_bl),
                     get_values(lasso_bl_expert_bl_mri),
                     get_values(lasso_bl_bl_expert))
row.names(tab_lasso_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                            "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_nn_bl = rbind(get_values(nn_bl_mri_sv_mri),
                  get_values(nn_bl_mri_bl),
                  get_values(nn_bl_bl_bl_mri_sv),
                  get_values(nn_bl_bl_bl_mri),
                  get_values(nn_bl_thrive_bl),
                  get_values(nn_bl_expert_bl_mri),
                  get_values(nn_bl_bl_expert))
row.names(tab_nn_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")


##### nihss
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# BL vs. Expert
tab_rf_nihss = rbind(get_values(rf_nihss_bl_bl_mri_sv),
                   get_values(rf_nihss_bl_bl_mri),
                   get_values(rf_nihss_expert_bl_mri),
                   get_values(rf_nihss_bl_expert))
row.names(tab_rf_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_lasso_nihss = rbind(get_values(lasso_nihss_bl_bl_mri_sv),
                      get_values(lasso_nihss_bl_bl_mri),
                      get_values(lasso_nihss_expert_bl_mri),
                      get_values(lasso_nihss_bl_expert))
row.names(tab_lasso_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                               "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_nn_nihss = rbind(get_values(nn_nihss_bl_bl_mri_sv),
                   get_values(nn_nihss_bl_bl_mri),
                   get_values(nn_nihss_expert_bl_mri),
                   get_values(nn_nihss_bl_expert))
row.names(tab_nn_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")


tab_rf_bl
tab_lasso_bl
tab_nn_bl

tab_rf_nihss
tab_lasso_nihss
tab_nn_nihss

tab_rbv = rbind(data.frame(tab_rf_bl,model="rf",comp=row.names(tab_rf_bl),time="bl"),
            data.frame(tab_rf_nihss,model="rf",comp=row.names(tab_rf_nihss),time="nihss"),
            data.frame(tab_lasso_bl,model="lasso",comp=row.names(tab_lasso_bl),time="bl"),
            data.frame(tab_lasso_nihss,model="lasso",comp=row.names(tab_lasso_nihss),time="nihss"),
            data.frame(tab_nn_bl,model="nn",comp=row.names(tab_nn_bl),time="bl"),
            data.frame(tab_nn_nihss,model="nn",comp=row.names(tab_nn_nihss),time="nihss"))
tab_rbv = data.frame(tab_rbv, row.names=NULL)



# rCBF -------------------------------------------------------------------------

load(file=paste0(dir,"data/CV/test_pred_rf_BL_rbf.R"))
load(file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_rbf.R"))

load(file=paste0(dir,"data/CV/test_pred_lasso_BL_rbf.R"))
load(file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_rbf.R"))

load(file=paste0(dir,"data/CV/test_pred_nn_BL_rbf.R"))
load(file=paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_rbf.R"))

rf_bl_rbf = rf_bl
lasso_bl_rbf = lasso_bl
nn_bl_rbf = nn_bl

rf_nihss_rbf = rf_nihss
lasso_nihss_rbf = lasso_nihss
nn_nihss_rbf = nn_nihss


# Stroke MRI only: No baseline data, always the same
rf_bl_mri_sv = roc(rf_bl_rbf$mrs_3months_binary, rf_bl_rbf$mri_sv)
lasso_bl_mri_sv = roc(lasso_bl_rbf$mrs_3months_binary, lasso_bl_rbf$mri_sv)
nn_bl_mri_sv = roc(nn_bl_rbf$mrs_3months_binary, nn_bl_rbf$mri_sv)

# MRI only
rf_bl_mri = roc(rf_bl_rbf$mrs_3months_binary, rf_bl_rbf$mri)
lasso_bl_mri = roc(lasso_bl_rbf$mrs_3months_binary, lasso_bl_rbf$mri)
nn_bl_mri = roc(nn_bl_rbf$mrs_3months_binary, nn_bl_rbf$mri)

# Baseline: consider always rbv for baseline
rf_bl_bl = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$bl)
rf_nihss_bl = roc(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$bl)

lasso_bl_bl = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$bl)
lasso_nihss_bl = roc(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$bl)

nn_bl_bl = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$bl)
nn_nihss_bl = roc(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$bl)


# Baseline + stroke MRI
rf_bl_bl_mri_sv = roc(rf_bl_rbf$mrs_3months_binary, rf_bl_rbf$bl_mri_sv)
rf_nihss_bl_mri_sv = roc(rf_nihss_rbf$mrs_3months_binary, rf_nihss_rbf$bl_mri_sv)

lasso_bl_bl_mri_sv = roc(lasso_bl_rbf$mrs_3months_binary, lasso_bl_rbf$bl_mri_sv)
lasso_nihss_bl_mri_sv = roc(lasso_nihss_rbf$mrs_3months_binary, lasso_nihss_rbf$bl_mri_sv)

nn_bl_bl_mri_sv = roc(nn_bl_rbf$mrs_3months_binary, nn_bl_rbf$bl_mri_sv)
nn_nihss_bl_mri_sv = roc(nn_nihss_rbf$mrs_3months_binary, nn_nihss_rbf$bl_mri_sv)


# Baseline + MRI
rf_bl_bl_mri = roc(rf_bl_rbf$mrs_3months_binary, rf_bl_rbf$bl_mri)
rf_nihss_bl_mri = roc(rf_nihss_rbf$mrs_3months_binary, rf_nihss_rbf$bl_mri)

lasso_bl_bl_mri = roc(lasso_bl_rbf$mrs_3months_binary, lasso_bl_rbf$bl_mri)
lasso_nihss_bl_mri = roc(lasso_nihss_rbf$mrs_3months_binary, lasso_nihss_rbf$bl_mri)

nn_bl_bl_mri = roc(nn_bl_rbf$mrs_3months_binary, nn_bl_rbf$bl_mri)
nn_nihss_bl_mri = roc(nn_nihss_rbf$mrs_3months_binary, nn_nihss_rbf$bl_mri)


# Expert
rf_bl_expert = roc(rf_bl_rbf$mrs_3months_binary, rf_bl_rbf$expert)
rf_nihss_expert = roc(rf_nihss_rbf$mrs_3months_binary, rf_nihss_rbf$expert)

lasso_bl_expert = roc(lasso_bl_rbf$mrs_3months_binary, lasso_bl_rbf$expert)
lasso_nihss_expert = roc(lasso_nihss_rbf$mrs_3months_binary, lasso_nihss_rbf$expert)

nn_bl_expert = roc(nn_bl_rbf$mrs_3months_binary, nn_bl_rbf$expert)
nn_nihss_expert = roc(nn_nihss_rbf$mrs_3months_binary, nn_nihss_rbf$expert)


#Tests

# MRI(SV) vs. MRI
rf_bl_mri_sv_mri = roc.test(rf_bl_mri_sv, rf_bl_mri, method="bootstrap")
lasso_bl_mri_sv_mri = roc.test(lasso_bl_mri_sv, lasso_bl_mri, method="bootstrap")
nn_bl_mri_sv_mri = roc.test(nn_bl_mri_sv, nn_bl_mri, method="bootstrap")

# MRI vs. Baseline
rf_bl_mri_bl = roc.test(rf_bl_mri, rf_bl_bl, method="bootstrap")
lasso_bl_mri_bl = roc.test(lasso_bl_mri, lasso_bl_bl, method="bootstrap")
nn_bl_mri_bl = roc.test(nn_bl_mri, nn_bl_bl, method="bootstrap")

# Baseline vs. Baseline + MRI(sv)
rf_bl_bl_bl_mri_sv = roc.test(rf_bl_bl, rf_bl_bl_mri_sv, method="bootstrap")
lasso_bl_bl_bl_mri_sv = roc.test(lasso_bl_bl, lasso_bl_bl_mri_sv, method="bootstrap")
nn_bl_bl_bl_mri_sv = roc.test(nn_bl_bl, nn_bl_bl_mri_sv, method="bootstrap")

rf_nihss_bl_bl_mri_sv = roc.test(rf_nihss_bl, rf_nihss_bl_mri_sv, method="bootstrap")
lasso_nihss_bl_bl_mri_sv = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri_sv, method="bootstrap")
nn_nihss_bl_bl_mri_sv = roc.test(nn_nihss_bl, nn_nihss_bl_mri_sv, method="bootstrap")

# Baseline vs. Baseline + MRI
rf_bl_bl_bl_mri = roc.test(rf_bl_bl, rf_bl_bl_mri, method="bootstrap")
lasso_bl_bl_bl_mri = roc.test(lasso_bl_bl, lasso_bl_bl_mri, method="bootstrap")
nn_bl_bl_bl_mri = roc.test(nn_bl_bl, nn_bl_bl_mri, method="bootstrap")

rf_nihss_bl_bl_mri = roc.test(rf_nihss_bl, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_bl_bl_mri = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_bl_bl_mri = roc.test(nn_nihss_bl, nn_nihss_bl_mri, method="bootstrap")

# Thrive vs. Baseline
rf_bl_thrive_bl = roc.test(thrive_bl, rf_bl_bl, method="bootstrap")
lasso_bl_thrive_bl = roc.test(thrive_bl, lasso_bl_bl, method="bootstrap")
nn_bl_thrive_bl = roc.test(thrive_bl, nn_bl_bl, method="bootstrap")

# Expert vs. Baseline (MRI) as Expert contains MRI data
rf_bl_expert_bl_mri = roc.test(rf_bl_expert, rf_bl_bl_mri, method="bootstrap")
lasso_bl_expert_bl_mri = roc.test(lasso_bl_expert, lasso_bl_bl_mri, method="bootstrap")
nn_bl_expert_bl_mri = roc.test(nn_bl_expert, nn_bl_bl_mri, method="bootstrap")

rf_nihss_expert_bl_mri = roc.test(rf_nihss_expert, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_expert_bl_mri = roc.test(lasso_nihss_expert, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_expert_bl_mri = roc.test(nn_nihss_expert, nn_nihss_bl_mri, method="bootstrap")

# Expert vs. Baseline
rf_bl_bl_expert = roc.test(rf_bl_bl, rf_bl_expert, method="bootstrap")
lasso_bl_bl_expert = roc.test(lasso_bl_bl, lasso_bl_expert, method="bootstrap")
nn_bl_bl_expert = roc.test(nn_bl_bl, nn_bl_expert, method="bootstrap")

rf_nihss_bl_expert = roc.test(rf_nihss_bl, rf_nihss_expert, method="bootstrap")
lasso_nihss_bl_expert = roc.test(lasso_nihss_bl, lasso_nihss_expert, method="bootstrap")
nn_nihss_bl_expert = roc.test(nn_nihss_bl, nn_nihss_expert, method="bootstrap")


# Create tables
get_values = function(dat){
  return(c("AUC1"=dat$roc1$auc, "AUC2"=dat$roc2$auc, "pvalue"=dat$p.value))
}

##### bl
# MRI sv vs. MRI
# MRI vs. BL
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# THRIVE vs. BL
# BL vs. Expert
tab_rf_bl = rbind(get_values(rf_bl_mri_sv_mri),
                  get_values(rf_bl_mri_bl),
                  get_values(rf_bl_bl_bl_mri_sv),
                  get_values(rf_bl_bl_bl_mri),
                  get_values(rf_bl_thrive_bl),
                  get_values(rf_bl_expert_bl_mri),
                  get_values(rf_bl_bl_expert))
row.names(tab_rf_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_lasso_bl = rbind(get_values(lasso_bl_mri_sv_mri),
                     get_values(lasso_bl_mri_bl),
                     get_values(lasso_bl_bl_bl_mri_sv),
                     get_values(lasso_bl_bl_bl_mri),
                     get_values(lasso_bl_thrive_bl),
                     get_values(lasso_bl_expert_bl_mri),
                     get_values(lasso_bl_bl_expert))
row.names(tab_lasso_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                            "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_nn_bl = rbind(get_values(nn_bl_mri_sv_mri),
                  get_values(nn_bl_mri_bl),
                  get_values(nn_bl_bl_bl_mri_sv),
                  get_values(nn_bl_bl_bl_mri),
                  get_values(nn_bl_thrive_bl),
                  get_values(nn_bl_expert_bl_mri),
                  get_values(nn_bl_bl_expert))
row.names(tab_nn_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")


##### nihss
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# BL vs. Expert
tab_rf_nihss = rbind(get_values(rf_nihss_bl_bl_mri_sv),
                     get_values(rf_nihss_bl_bl_mri),
                     get_values(rf_nihss_expert_bl_mri),
                     get_values(rf_nihss_bl_expert))
row.names(tab_rf_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_lasso_nihss = rbind(get_values(lasso_nihss_bl_bl_mri_sv),
                        get_values(lasso_nihss_bl_bl_mri),
                        get_values(lasso_nihss_expert_bl_mri),
                        get_values(lasso_nihss_bl_expert))
row.names(tab_lasso_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                               "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_nn_nihss = rbind(get_values(nn_nihss_bl_bl_mri_sv),
                     get_values(nn_nihss_bl_bl_mri),
                     get_values(nn_nihss_expert_bl_mri),
                     get_values(nn_nihss_bl_expert))
row.names(tab_nn_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")


tab_rf_bl
tab_lasso_bl
tab_nn_bl

tab_rf_nihss
tab_lasso_nihss
tab_nn_nihss

tab_rbf = rbind(data.frame(tab_rf_bl,model="rf",comp=row.names(tab_rf_bl),time="bl"),
                data.frame(tab_rf_nihss,model="rf",comp=row.names(tab_rf_nihss),time="nihss"),
                data.frame(tab_lasso_bl,model="lasso",comp=row.names(tab_lasso_bl),time="bl"),
                data.frame(tab_lasso_nihss,model="lasso",comp=row.names(tab_lasso_nihss),time="nihss"),
                data.frame(tab_nn_bl,model="nn",comp=row.names(tab_nn_bl),time="bl"),
                data.frame(tab_nn_nihss,model="nn",comp=row.names(tab_nn_nihss),time="nihss"))
tab_rbf = data.frame(tab_rbf, row.names=NULL)




# TMAX -------------------------------------------------------------------------

load(file=paste0(dir,"data/CV/test_pred_rf_BL_tmax.R"))
load(file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_tmax.R"))

load(file=paste0(dir,"data/CV/test_pred_lasso_BL_tmax.R"))
load(file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_tmax.R"))

load(file=paste0(dir,"data/CV/test_pred_nn_BL_tmax.R"))
load(file=paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_tmax.R"))

rf_bl_tmax = rf_bl
lasso_bl_tmax = lasso_bl
nn_bl_tmax = nn_bl

rf_nihss_tmax = rf_nihss
lasso_nihss_tmax = lasso_nihss
nn_nihss_tmax = nn_nihss


# Stroke MRI only: No baseline data, always the same
rf_bl_mri_sv = roc(rf_bl_tmax$mrs_3months_binary, rf_bl_tmax$mri_sv)
lasso_bl_mri_sv = roc(lasso_bl_tmax$mrs_3months_binary, lasso_bl_tmax$mri_sv)
nn_bl_mri_sv = roc(nn_bl_tmax$mrs_3months_binary, nn_bl_tmax$mri_sv)

# MRI only
rf_bl_mri = roc(rf_bl_tmax$mrs_3months_binary, rf_bl_tmax$mri)
lasso_bl_mri = roc(lasso_bl_tmax$mrs_3months_binary, lasso_bl_tmax$mri)
nn_bl_mri = roc(nn_bl_tmax$mrs_3months_binary, nn_bl_tmax$mri)

# Baseline: consider always rbv for baseline
rf_bl_bl = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$bl)
rf_nihss_bl = roc(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$bl)

lasso_bl_bl = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$bl)
lasso_nihss_bl = roc(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$bl)

nn_bl_bl = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$bl)
nn_nihss_bl = roc(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$bl)


# Baseline + stroke MRI
rf_bl_bl_mri_sv = roc(rf_bl_tmax$mrs_3months_binary, rf_bl_tmax$bl_mri_sv)
rf_nihss_bl_mri_sv = roc(rf_nihss_tmax$mrs_3months_binary, rf_nihss_tmax$bl_mri_sv)

lasso_bl_bl_mri_sv = roc(lasso_bl_tmax$mrs_3months_binary, lasso_bl_tmax$bl_mri_sv)
lasso_nihss_bl_mri_sv = roc(lasso_nihss_tmax$mrs_3months_binary, lasso_nihss_tmax$bl_mri_sv)

nn_bl_bl_mri_sv = roc(nn_bl_tmax$mrs_3months_binary, nn_bl_tmax$bl_mri_sv)
nn_nihss_bl_mri_sv = roc(nn_nihss_tmax$mrs_3months_binary, nn_nihss_tmax$bl_mri_sv)


# Baseline + MRI
rf_bl_bl_mri = roc(rf_bl_tmax$mrs_3months_binary, rf_bl_tmax$bl_mri)
rf_nihss_bl_mri = roc(rf_nihss_tmax$mrs_3months_binary, rf_nihss_tmax$bl_mri)

lasso_bl_bl_mri = roc(lasso_bl_tmax$mrs_3months_binary, lasso_bl_tmax$bl_mri)
lasso_nihss_bl_mri = roc(lasso_nihss_tmax$mrs_3months_binary, lasso_nihss_tmax$bl_mri)

nn_bl_bl_mri = roc(nn_bl_tmax$mrs_3months_binary, nn_bl_tmax$bl_mri)
nn_nihss_bl_mri = roc(nn_nihss_tmax$mrs_3months_binary, nn_nihss_tmax$bl_mri)


# Expert
rf_bl_expert = roc(rf_bl_tmax$mrs_3months_binary, rf_bl_tmax$expert)
rf_nihss_expert = roc(rf_nihss_tmax$mrs_3months_binary, rf_nihss_tmax$expert)

lasso_bl_expert = roc(lasso_bl_tmax$mrs_3months_binary, lasso_bl_tmax$expert)
lasso_nihss_expert = roc(lasso_nihss_tmax$mrs_3months_binary, lasso_nihss_tmax$expert)

nn_bl_expert = roc(nn_bl_tmax$mrs_3months_binary, nn_bl_tmax$expert)
nn_nihss_expert = roc(nn_nihss_tmax$mrs_3months_binary, nn_nihss_tmax$expert)


#Tests

# MRI(SV) vs. MRI
rf_bl_mri_sv_mri = roc.test(rf_bl_mri_sv, rf_bl_mri, method="bootstrap")
lasso_bl_mri_sv_mri = roc.test(lasso_bl_mri_sv, lasso_bl_mri, method="bootstrap")
nn_bl_mri_sv_mri = roc.test(nn_bl_mri_sv, nn_bl_mri, method="bootstrap")

# MRI vs. Baseline
rf_bl_mri_bl = roc.test(rf_bl_mri, rf_bl_bl, method="bootstrap")
lasso_bl_mri_bl = roc.test(lasso_bl_mri, lasso_bl_bl, method="bootstrap")
nn_bl_mri_bl = roc.test(nn_bl_mri, nn_bl_bl, method="bootstrap")

# Baseline vs. Baseline + MRI(sv)
rf_bl_bl_bl_mri_sv = roc.test(rf_bl_bl, rf_bl_bl_mri_sv, method="bootstrap")
lasso_bl_bl_bl_mri_sv = roc.test(lasso_bl_bl, lasso_bl_bl_mri_sv, method="bootstrap")
nn_bl_bl_bl_mri_sv = roc.test(nn_bl_bl, nn_bl_bl_mri_sv, method="bootstrap")

rf_nihss_bl_bl_mri_sv = roc.test(rf_nihss_bl, rf_nihss_bl_mri_sv, method="bootstrap")
lasso_nihss_bl_bl_mri_sv = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri_sv, method="bootstrap")
nn_nihss_bl_bl_mri_sv = roc.test(nn_nihss_bl, nn_nihss_bl_mri_sv, method="bootstrap")

# Baseline vs. Baseline + MRI
rf_bl_bl_bl_mri = roc.test(rf_bl_bl, rf_bl_bl_mri, method="bootstrap")
lasso_bl_bl_bl_mri = roc.test(lasso_bl_bl, lasso_bl_bl_mri, method="bootstrap")
nn_bl_bl_bl_mri = roc.test(nn_bl_bl, nn_bl_bl_mri, method="bootstrap")

rf_nihss_bl_bl_mri = roc.test(rf_nihss_bl, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_bl_bl_mri = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_bl_bl_mri = roc.test(nn_nihss_bl, nn_nihss_bl_mri, method="bootstrap")

# Thrive vs. Baseline
rf_bl_thrive_bl = roc.test(thrive_bl, rf_bl_bl, method="bootstrap")
lasso_bl_thrive_bl = roc.test(thrive_bl, lasso_bl_bl, method="bootstrap")
nn_bl_thrive_bl = roc.test(thrive_bl, nn_bl_bl, method="bootstrap")

# Expert vs. Baseline (MRI) as Expert contains MRI data
rf_bl_expert_bl_mri = roc.test(rf_bl_expert, rf_bl_bl_mri, method="bootstrap")
lasso_bl_expert_bl_mri = roc.test(lasso_bl_expert, lasso_bl_bl_mri, method="bootstrap")
nn_bl_expert_bl_mri = roc.test(nn_bl_expert, nn_bl_bl_mri, method="bootstrap")

rf_nihss_expert_bl_mri = roc.test(rf_nihss_expert, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_expert_bl_mri = roc.test(lasso_nihss_expert, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_expert_bl_mri = roc.test(nn_nihss_expert, nn_nihss_bl_mri, method="bootstrap")

# Expert vs. Baseline
rf_bl_bl_expert = roc.test(rf_bl_bl, rf_bl_expert, method="bootstrap")
lasso_bl_bl_expert = roc.test(lasso_bl_bl, lasso_bl_expert, method="bootstrap")
nn_bl_bl_expert = roc.test(nn_bl_bl, nn_bl_expert, method="bootstrap")

rf_nihss_bl_expert = roc.test(rf_nihss_bl, rf_nihss_expert, method="bootstrap")
lasso_nihss_bl_expert = roc.test(lasso_nihss_bl, lasso_nihss_expert, method="bootstrap")
nn_nihss_bl_expert = roc.test(nn_nihss_bl, nn_nihss_expert, method="bootstrap")


# Create tables
get_values = function(dat){
  return(c("AUC1"=dat$roc1$auc, "AUC2"=dat$roc2$auc, "pvalue"=dat$p.value))
}

##### bl
# MRI sv vs. MRI
# MRI vs. BL
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# THRIVE vs. BL
# BL vs. Expert
tab_rf_bl = rbind(get_values(rf_bl_mri_sv_mri),
                  get_values(rf_bl_mri_bl),
                  get_values(rf_bl_bl_bl_mri_sv),
                  get_values(rf_bl_bl_bl_mri),
                  get_values(rf_bl_thrive_bl),
                  get_values(rf_bl_expert_bl_mri),
                  get_values(rf_bl_bl_expert))
row.names(tab_rf_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_lasso_bl = rbind(get_values(lasso_bl_mri_sv_mri),
                     get_values(lasso_bl_mri_bl),
                     get_values(lasso_bl_bl_bl_mri_sv),
                     get_values(lasso_bl_bl_bl_mri),
                     get_values(lasso_bl_thrive_bl),
                     get_values(lasso_bl_expert_bl_mri),
                     get_values(lasso_bl_bl_expert))
row.names(tab_lasso_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                            "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_nn_bl = rbind(get_values(nn_bl_mri_sv_mri),
                  get_values(nn_bl_mri_bl),
                  get_values(nn_bl_bl_bl_mri_sv),
                  get_values(nn_bl_bl_bl_mri),
                  get_values(nn_bl_thrive_bl),
                  get_values(nn_bl_expert_bl_mri),
                  get_values(nn_bl_bl_expert))
row.names(tab_nn_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")


##### nihss
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# BL vs. Expert
tab_rf_nihss = rbind(get_values(rf_nihss_bl_bl_mri_sv),
                     get_values(rf_nihss_bl_bl_mri),
                     get_values(rf_nihss_expert_bl_mri),
                     get_values(rf_nihss_bl_expert))
row.names(tab_rf_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_lasso_nihss = rbind(get_values(lasso_nihss_bl_bl_mri_sv),
                        get_values(lasso_nihss_bl_bl_mri),
                        get_values(lasso_nihss_expert_bl_mri),
                        get_values(lasso_nihss_bl_expert))
row.names(tab_lasso_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                               "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_nn_nihss = rbind(get_values(nn_nihss_bl_bl_mri_sv),
                     get_values(nn_nihss_bl_bl_mri),
                     get_values(nn_nihss_expert_bl_mri),
                     get_values(nn_nihss_bl_expert))
row.names(tab_nn_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")


tab_rf_bl
tab_lasso_bl
tab_nn_bl

tab_rf_nihss
tab_lasso_nihss
tab_nn_nihss

tab_tmax = rbind(data.frame(tab_rf_bl,model="rf",comp=row.names(tab_rf_bl),time="bl"),
                data.frame(tab_rf_nihss,model="rf",comp=row.names(tab_rf_nihss),time="nihss"),
                data.frame(tab_lasso_bl,model="lasso",comp=row.names(tab_lasso_bl),time="bl"),
                data.frame(tab_lasso_nihss,model="lasso",comp=row.names(tab_lasso_nihss),time="nihss"),
                data.frame(tab_nn_bl,model="nn",comp=row.names(tab_nn_bl),time="bl"),
                data.frame(tab_nn_nihss,model="nn",comp=row.names(tab_nn_nihss),time="nihss"))
tab_tmax = data.frame(tab_tmax, row.names=NULL)




# MTT --------------------------------------------------------------------------

load(file=paste0(dir,"data/CV/test_pred_rf_BL_mtt.R"))
load(file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_mtt.R"))

load(file=paste0(dir,"data/CV/test_pred_lasso_BL_mtt.R"))
load(file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_mtt.R"))

load(file=paste0(dir,"data/CV/test_pred_nn_BL_mtt.R"))
load(file=paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_mtt.R"))

rf_bl_mtt = rf_bl
lasso_bl_mtt = lasso_bl
nn_bl_mtt = nn_bl

rf_nihss_mtt = rf_nihss
lasso_nihss_mtt = lasso_nihss
nn_nihss_mtt = nn_nihss


# Stroke MRI only: No baseline data, always the same
rf_bl_mri_sv = roc(rf_bl_mtt$mrs_3months_binary, rf_bl_mtt$mri_sv)
lasso_bl_mri_sv = roc(lasso_bl_mtt$mrs_3months_binary, lasso_bl_mtt$mri_sv)
nn_bl_mri_sv = roc(nn_bl_mtt$mrs_3months_binary, nn_bl_mtt$mri_sv)

# MRI only
rf_bl_mri = roc(rf_bl_mtt$mrs_3months_binary, rf_bl_mtt$mri)
lasso_bl_mri = roc(lasso_bl_mtt$mrs_3months_binary, lasso_bl_mtt$mri)
nn_bl_mri = roc(nn_bl_mtt$mrs_3months_binary, nn_bl_mtt$mri)

# Baseline: consider always rbv for baseline
rf_bl_bl = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$bl)
rf_nihss_bl = roc(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$bl)

lasso_bl_bl = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$bl)
lasso_nihss_bl = roc(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$bl)

nn_bl_bl = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$bl)
nn_nihss_bl = roc(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$bl)


# Baseline + stroke MRI
rf_bl_bl_mri_sv = roc(rf_bl_mtt$mrs_3months_binary, rf_bl_mtt$bl_mri_sv)
rf_nihss_bl_mri_sv = roc(rf_nihss_mtt$mrs_3months_binary, rf_nihss_mtt$bl_mri_sv)

lasso_bl_bl_mri_sv = roc(lasso_bl_mtt$mrs_3months_binary, lasso_bl_mtt$bl_mri_sv)
lasso_nihss_bl_mri_sv = roc(lasso_nihss_mtt$mrs_3months_binary, lasso_nihss_mtt$bl_mri_sv)

nn_bl_bl_mri_sv = roc(nn_bl_mtt$mrs_3months_binary, nn_bl_mtt$bl_mri_sv)
nn_nihss_bl_mri_sv = roc(nn_nihss_mtt$mrs_3months_binary, nn_nihss_mtt$bl_mri_sv)


# Baseline + MRI
rf_bl_bl_mri = roc(rf_bl_mtt$mrs_3months_binary, rf_bl_mtt$bl_mri)
rf_nihss_bl_mri = roc(rf_nihss_mtt$mrs_3months_binary, rf_nihss_mtt$bl_mri)

lasso_bl_bl_mri = roc(lasso_bl_mtt$mrs_3months_binary, lasso_bl_mtt$bl_mri)
lasso_nihss_bl_mri = roc(lasso_nihss_mtt$mrs_3months_binary, lasso_nihss_mtt$bl_mri)

nn_bl_bl_mri = roc(nn_bl_mtt$mrs_3months_binary, nn_bl_mtt$bl_mri)
nn_nihss_bl_mri = roc(nn_nihss_mtt$mrs_3months_binary, nn_nihss_mtt$bl_mri)


# Expert
rf_bl_expert = roc(rf_bl_mtt$mrs_3months_binary, rf_bl_mtt$expert)
rf_nihss_expert = roc(rf_nihss_mtt$mrs_3months_binary, rf_nihss_mtt$expert)

lasso_bl_expert = roc(lasso_bl_mtt$mrs_3months_binary, lasso_bl_mtt$expert)
lasso_nihss_expert = roc(lasso_nihss_mtt$mrs_3months_binary, lasso_nihss_mtt$expert)

nn_bl_expert = roc(nn_bl_mtt$mrs_3months_binary, nn_bl_mtt$expert)
nn_nihss_expert = roc(nn_nihss_mtt$mrs_3months_binary, nn_nihss_mtt$expert)


#Tests

# MRI(SV) vs. MRI
rf_bl_mri_sv_mri = roc.test(rf_bl_mri_sv, rf_bl_mri, method="bootstrap")
lasso_bl_mri_sv_mri = roc.test(lasso_bl_mri_sv, lasso_bl_mri, method="bootstrap")
nn_bl_mri_sv_mri = roc.test(nn_bl_mri_sv, nn_bl_mri, method="bootstrap")

# MRI vs. Baseline
rf_bl_mri_bl = roc.test(rf_bl_mri, rf_bl_bl, method="bootstrap")
lasso_bl_mri_bl = roc.test(lasso_bl_mri, lasso_bl_bl, method="bootstrap")
nn_bl_mri_bl = roc.test(nn_bl_mri, nn_bl_bl, method="bootstrap")

# Baseline vs. Baseline + MRI(sv)
rf_bl_bl_bl_mri_sv = roc.test(rf_bl_bl, rf_bl_bl_mri_sv, method="bootstrap")
lasso_bl_bl_bl_mri_sv = roc.test(lasso_bl_bl, lasso_bl_bl_mri_sv, method="bootstrap")
nn_bl_bl_bl_mri_sv = roc.test(nn_bl_bl, nn_bl_bl_mri_sv, method="bootstrap")

rf_nihss_bl_bl_mri_sv = roc.test(rf_nihss_bl, rf_nihss_bl_mri_sv, method="bootstrap")
lasso_nihss_bl_bl_mri_sv = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri_sv, method="bootstrap")
nn_nihss_bl_bl_mri_sv = roc.test(nn_nihss_bl, nn_nihss_bl_mri_sv, method="bootstrap")

# Baseline vs. Baseline + MRI
rf_bl_bl_bl_mri = roc.test(rf_bl_bl, rf_bl_bl_mri, method="bootstrap")
lasso_bl_bl_bl_mri = roc.test(lasso_bl_bl, lasso_bl_bl_mri, method="bootstrap")
nn_bl_bl_bl_mri = roc.test(nn_bl_bl, nn_bl_bl_mri, method="bootstrap")

rf_nihss_bl_bl_mri = roc.test(rf_nihss_bl, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_bl_bl_mri = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_bl_bl_mri = roc.test(nn_nihss_bl, nn_nihss_bl_mri, method="bootstrap")

# Thrive vs. Baseline
rf_bl_thrive_bl = roc.test(thrive_bl, rf_bl_bl, method="bootstrap")
lasso_bl_thrive_bl = roc.test(thrive_bl, lasso_bl_bl, method="bootstrap")
nn_bl_thrive_bl = roc.test(thrive_bl, nn_bl_bl, method="bootstrap")

# Expert vs. Baseline (MRI) as Expert contains MRI data
rf_bl_expert_bl_mri = roc.test(rf_bl_expert, rf_bl_bl_mri, method="bootstrap")
lasso_bl_expert_bl_mri = roc.test(lasso_bl_expert, lasso_bl_bl_mri, method="bootstrap")
nn_bl_expert_bl_mri = roc.test(nn_bl_expert, nn_bl_bl_mri, method="bootstrap")

rf_nihss_expert_bl_mri = roc.test(rf_nihss_expert, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_expert_bl_mri = roc.test(lasso_nihss_expert, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_expert_bl_mri = roc.test(nn_nihss_expert, nn_nihss_bl_mri, method="bootstrap")

# Expert vs. Baseline
rf_bl_bl_expert = roc.test(rf_bl_bl, rf_bl_expert, method="bootstrap")
lasso_bl_bl_expert = roc.test(lasso_bl_bl, lasso_bl_expert, method="bootstrap")
nn_bl_bl_expert = roc.test(nn_bl_bl, nn_bl_expert, method="bootstrap")

rf_nihss_bl_expert = roc.test(rf_nihss_bl, rf_nihss_expert, method="bootstrap")
lasso_nihss_bl_expert = roc.test(lasso_nihss_bl, lasso_nihss_expert, method="bootstrap")
nn_nihss_bl_expert = roc.test(nn_nihss_bl, nn_nihss_expert, method="bootstrap")


# Create tables
get_values = function(dat){
  return(c("AUC1"=dat$roc1$auc, "AUC2"=dat$roc2$auc, "pvalue"=dat$p.value))
}

##### bl
# MRI sv vs. MRI
# MRI vs. BL
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# THRIVE vs. BL
# BL vs. Expert
tab_rf_bl = rbind(get_values(rf_bl_mri_sv_mri),
                  get_values(rf_bl_mri_bl),
                  get_values(rf_bl_bl_bl_mri_sv),
                  get_values(rf_bl_bl_bl_mri),
                  get_values(rf_bl_thrive_bl),
                  get_values(rf_bl_expert_bl_mri),
                  get_values(rf_bl_bl_expert))
row.names(tab_rf_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_lasso_bl = rbind(get_values(lasso_bl_mri_sv_mri),
                     get_values(lasso_bl_mri_bl),
                     get_values(lasso_bl_bl_bl_mri_sv),
                     get_values(lasso_bl_bl_bl_mri),
                     get_values(lasso_bl_thrive_bl),
                     get_values(lasso_bl_expert_bl_mri),
                     get_values(lasso_bl_bl_expert))
row.names(tab_lasso_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                            "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_nn_bl = rbind(get_values(nn_bl_mri_sv_mri),
                  get_values(nn_bl_mri_bl),
                  get_values(nn_bl_bl_bl_mri_sv),
                  get_values(nn_bl_bl_bl_mri),
                  get_values(nn_bl_thrive_bl),
                  get_values(nn_bl_expert_bl_mri),
                  get_values(nn_bl_bl_expert))
row.names(tab_nn_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")


##### nihss
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# BL vs. Expert
tab_rf_nihss = rbind(get_values(rf_nihss_bl_bl_mri_sv),
                     get_values(rf_nihss_bl_bl_mri),
                     get_values(rf_nihss_expert_bl_mri),
                     get_values(rf_nihss_bl_expert))
row.names(tab_rf_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_lasso_nihss = rbind(get_values(lasso_nihss_bl_bl_mri_sv),
                        get_values(lasso_nihss_bl_bl_mri),
                        get_values(lasso_nihss_expert_bl_mri),
                        get_values(lasso_nihss_bl_expert))
row.names(tab_lasso_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                               "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_nn_nihss = rbind(get_values(nn_nihss_bl_bl_mri_sv),
                     get_values(nn_nihss_bl_bl_mri),
                     get_values(nn_nihss_expert_bl_mri),
                     get_values(nn_nihss_bl_expert))
row.names(tab_nn_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")


tab_rf_bl
tab_lasso_bl
tab_nn_bl

tab_rf_nihss
tab_lasso_nihss
tab_nn_nihss

tab_mtt = rbind(data.frame(tab_rf_bl,model="rf",comp=row.names(tab_rf_bl),time="bl"),
                data.frame(tab_rf_nihss,model="rf",comp=row.names(tab_rf_nihss),time="nihss"),
                data.frame(tab_lasso_bl,model="lasso",comp=row.names(tab_lasso_bl),time="bl"),
                data.frame(tab_lasso_nihss,model="lasso",comp=row.names(tab_lasso_nihss),time="nihss"),
                data.frame(tab_nn_bl,model="nn",comp=row.names(tab_nn_bl),time="bl"),
                data.frame(tab_nn_nihss,model="nn",comp=row.names(tab_nn_nihss),time="nihss"))
tab_mtt = data.frame(tab_mtt, row.names=NULL)



# tMIP -------------------------------------------------------------------------

load(file=paste0(dir,"data/CV/test_pred_rf_BL_tmip.R"))
load(file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_tmip.R"))

load(file=paste0(dir,"data/CV/test_pred_lasso_BL_tmip.R"))
load(file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_tmip.R"))

load(file=paste0(dir,"data/CV/test_pred_nn_BL_tmip.R"))
load(file=paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_tmip.R"))

rf_bl_tmip = rf_bl
lasso_bl_tmip = lasso_bl
nn_bl_tmip = nn_bl

rf_nihss_tmip = rf_nihss
lasso_nihss_tmip = lasso_nihss
nn_nihss_tmip = nn_nihss


# Stroke MRI only: No baseline data, always the same
rf_bl_mri_sv = roc(rf_bl_tmip$mrs_3months_binary, rf_bl_tmip$mri_sv)
lasso_bl_mri_sv = roc(lasso_bl_tmip$mrs_3months_binary, lasso_bl_tmip$mri_sv)
nn_bl_mri_sv = roc(nn_bl_tmip$mrs_3months_binary, nn_bl_tmip$mri_sv)

# MRI only
rf_bl_mri = roc(rf_bl_tmip$mrs_3months_binary, rf_bl_tmip$mri)
lasso_bl_mri = roc(lasso_bl_tmip$mrs_3months_binary, lasso_bl_tmip$mri)
nn_bl_mri = roc(nn_bl_tmip$mrs_3months_binary, nn_bl_tmip$mri)

# Baseline: consider always rbv for baseline
rf_bl_bl = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$bl)
rf_nihss_bl = roc(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$bl)

lasso_bl_bl = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$bl)
lasso_nihss_bl = roc(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$bl)

nn_bl_bl = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$bl)
nn_nihss_bl = roc(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$bl)


# Baseline + stroke MRI
rf_bl_bl_mri_sv = roc(rf_bl_tmip$mrs_3months_binary, rf_bl_tmip$bl_mri_sv)
rf_nihss_bl_mri_sv = roc(rf_nihss_tmip$mrs_3months_binary, rf_nihss_tmip$bl_mri_sv)

lasso_bl_bl_mri_sv = roc(lasso_bl_tmip$mrs_3months_binary, lasso_bl_tmip$bl_mri_sv)
lasso_nihss_bl_mri_sv = roc(lasso_nihss_tmip$mrs_3months_binary, lasso_nihss_tmip$bl_mri_sv)

nn_bl_bl_mri_sv = roc(nn_bl_tmip$mrs_3months_binary, nn_bl_tmip$bl_mri_sv)
nn_nihss_bl_mri_sv = roc(nn_nihss_tmip$mrs_3months_binary, nn_nihss_tmip$bl_mri_sv)


# Baseline + MRI
rf_bl_bl_mri = roc(rf_bl_tmip$mrs_3months_binary, rf_bl_tmip$bl_mri)
rf_nihss_bl_mri = roc(rf_nihss_tmip$mrs_3months_binary, rf_nihss_tmip$bl_mri)

lasso_bl_bl_mri = roc(lasso_bl_tmip$mrs_3months_binary, lasso_bl_tmip$bl_mri)
lasso_nihss_bl_mri = roc(lasso_nihss_tmip$mrs_3months_binary, lasso_nihss_tmip$bl_mri)

nn_bl_bl_mri = roc(nn_bl_tmip$mrs_3months_binary, nn_bl_tmip$bl_mri)
nn_nihss_bl_mri = roc(nn_nihss_tmip$mrs_3months_binary, nn_nihss_tmip$bl_mri)


# Expert
rf_bl_expert = roc(rf_bl_tmip$mrs_3months_binary, rf_bl_tmip$expert)
rf_nihss_expert = roc(rf_nihss_tmip$mrs_3months_binary, rf_nihss_tmip$expert)

lasso_bl_expert = roc(lasso_bl_tmip$mrs_3months_binary, lasso_bl_tmip$expert)
lasso_nihss_expert = roc(lasso_nihss_tmip$mrs_3months_binary, lasso_nihss_tmip$expert)

nn_bl_expert = roc(nn_bl_tmip$mrs_3months_binary, nn_bl_tmip$expert)
nn_nihss_expert = roc(nn_nihss_tmip$mrs_3months_binary, nn_nihss_tmip$expert)


#Tests

# MRI(SV) vs. MRI
rf_bl_mri_sv_mri = roc.test(rf_bl_mri_sv, rf_bl_mri, method="bootstrap")
lasso_bl_mri_sv_mri = roc.test(lasso_bl_mri_sv, lasso_bl_mri, method="bootstrap")
nn_bl_mri_sv_mri = roc.test(nn_bl_mri_sv, nn_bl_mri, method="bootstrap")

# MRI vs. Baseline
rf_bl_mri_bl = roc.test(rf_bl_mri, rf_bl_bl, method="bootstrap")
lasso_bl_mri_bl = roc.test(lasso_bl_mri, lasso_bl_bl, method="bootstrap")
nn_bl_mri_bl = roc.test(nn_bl_mri, nn_bl_bl, method="bootstrap")

# Baseline vs. Baseline + MRI(sv)
rf_bl_bl_bl_mri_sv = roc.test(rf_bl_bl, rf_bl_bl_mri_sv, method="bootstrap")
lasso_bl_bl_bl_mri_sv = roc.test(lasso_bl_bl, lasso_bl_bl_mri_sv, method="bootstrap")
nn_bl_bl_bl_mri_sv = roc.test(nn_bl_bl, nn_bl_bl_mri_sv, method="bootstrap")

rf_nihss_bl_bl_mri_sv = roc.test(rf_nihss_bl, rf_nihss_bl_mri_sv, method="bootstrap")
lasso_nihss_bl_bl_mri_sv = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri_sv, method="bootstrap")
nn_nihss_bl_bl_mri_sv = roc.test(nn_nihss_bl, nn_nihss_bl_mri_sv, method="bootstrap")

# Baseline vs. Baseline + MRI
rf_bl_bl_bl_mri = roc.test(rf_bl_bl, rf_bl_bl_mri, method="bootstrap")
lasso_bl_bl_bl_mri = roc.test(lasso_bl_bl, lasso_bl_bl_mri, method="bootstrap")
nn_bl_bl_bl_mri = roc.test(nn_bl_bl, nn_bl_bl_mri, method="bootstrap")

rf_nihss_bl_bl_mri = roc.test(rf_nihss_bl, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_bl_bl_mri = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_bl_bl_mri = roc.test(nn_nihss_bl, nn_nihss_bl_mri, method="bootstrap")

# Thrive vs. Baseline
rf_bl_thrive_bl = roc.test(thrive_bl, rf_bl_bl, method="bootstrap")
lasso_bl_thrive_bl = roc.test(thrive_bl, lasso_bl_bl, method="bootstrap")
nn_bl_thrive_bl = roc.test(thrive_bl, nn_bl_bl, method="bootstrap")

# Expert vs. Baseline (MRI) as Expert contains MRI data
rf_bl_expert_bl_mri = roc.test(rf_bl_expert, rf_bl_bl_mri, method="bootstrap")
lasso_bl_expert_bl_mri = roc.test(lasso_bl_expert, lasso_bl_bl_mri, method="bootstrap")
nn_bl_expert_bl_mri = roc.test(nn_bl_expert, nn_bl_bl_mri, method="bootstrap")

rf_nihss_expert_bl_mri = roc.test(rf_nihss_expert, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_expert_bl_mri = roc.test(lasso_nihss_expert, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_expert_bl_mri = roc.test(nn_nihss_expert, nn_nihss_bl_mri, method="bootstrap")

# Expert vs. Baseline
rf_bl_bl_expert = roc.test(rf_bl_bl, rf_bl_expert, method="bootstrap")
lasso_bl_bl_expert = roc.test(lasso_bl_bl, lasso_bl_expert, method="bootstrap")
nn_bl_bl_expert = roc.test(nn_bl_bl, nn_bl_expert, method="bootstrap")

rf_nihss_bl_expert = roc.test(rf_nihss_bl, rf_nihss_expert, method="bootstrap")
lasso_nihss_bl_expert = roc.test(lasso_nihss_bl, lasso_nihss_expert, method="bootstrap")
nn_nihss_bl_expert = roc.test(nn_nihss_bl, nn_nihss_expert, method="bootstrap")


# Create tables
get_values = function(dat){
  return(c("AUC1"=dat$roc1$auc, "AUC2"=dat$roc2$auc, "pvalue"=dat$p.value))
}

##### bl
# MRI sv vs. MRI
# MRI vs. BL
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# THRIVE vs. BL
# BL vs. Expert
tab_rf_bl = rbind(get_values(rf_bl_mri_sv_mri),
                  get_values(rf_bl_mri_bl),
                  get_values(rf_bl_bl_bl_mri_sv),
                  get_values(rf_bl_bl_bl_mri),
                  get_values(rf_bl_thrive_bl),
                  get_values(rf_bl_expert_bl_mri),
                  get_values(rf_bl_bl_expert))
row.names(tab_rf_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_lasso_bl = rbind(get_values(lasso_bl_mri_sv_mri),
                     get_values(lasso_bl_mri_bl),
                     get_values(lasso_bl_bl_bl_mri_sv),
                     get_values(lasso_bl_bl_bl_mri),
                     get_values(lasso_bl_thrive_bl),
                     get_values(lasso_bl_expert_bl_mri),
                     get_values(lasso_bl_bl_expert))
row.names(tab_lasso_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                            "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_nn_bl = rbind(get_values(nn_bl_mri_sv_mri),
                  get_values(nn_bl_mri_bl),
                  get_values(nn_bl_bl_bl_mri_sv),
                  get_values(nn_bl_bl_bl_mri),
                  get_values(nn_bl_thrive_bl),
                  get_values(nn_bl_expert_bl_mri),
                  get_values(nn_bl_bl_expert))
row.names(tab_nn_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")


##### nihss
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# BL vs. Expert
tab_rf_nihss = rbind(get_values(rf_nihss_bl_bl_mri_sv),
                     get_values(rf_nihss_bl_bl_mri),
                     get_values(rf_nihss_expert_bl_mri),
                     get_values(rf_nihss_bl_expert))
row.names(tab_rf_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_lasso_nihss = rbind(get_values(lasso_nihss_bl_bl_mri_sv),
                        get_values(lasso_nihss_bl_bl_mri),
                        get_values(lasso_nihss_expert_bl_mri),
                        get_values(lasso_nihss_bl_expert))
row.names(tab_lasso_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                               "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_nn_nihss = rbind(get_values(nn_nihss_bl_bl_mri_sv),
                     get_values(nn_nihss_bl_bl_mri),
                     get_values(nn_nihss_expert_bl_mri),
                     get_values(nn_nihss_bl_expert))
row.names(tab_nn_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")


tab_rf_bl
tab_lasso_bl
tab_nn_bl

tab_rf_nihss
tab_lasso_nihss
tab_nn_nihss

tab_tmip = rbind(data.frame(tab_rf_bl,model="rf",comp=row.names(tab_rf_bl),time="bl"),
                data.frame(tab_rf_nihss,model="rf",comp=row.names(tab_rf_nihss),time="nihss"),
                data.frame(tab_lasso_bl,model="lasso",comp=row.names(tab_lasso_bl),time="bl"),
                data.frame(tab_lasso_nihss,model="lasso",comp=row.names(tab_lasso_nihss),time="nihss"),
                data.frame(tab_nn_bl,model="nn",comp=row.names(tab_nn_bl),time="bl"),
                data.frame(tab_nn_nihss,model="nn",comp=row.names(tab_nn_nihss),time="nihss"))
tab_tmip = data.frame(tab_tmip, row.names=NULL)




# TTP --------------------------------------------------------------------------

load(file=paste0(dir,"data/CV/test_pred_rf_BL_ttp.R"))
load(file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_ttp.R"))

load(file=paste0(dir,"data/CV/test_pred_lasso_BL_ttp.R"))
load(file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_ttp.R"))

load(file=paste0(dir,"data/CV/test_pred_nn_BL_ttp.R"))
load(file=paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_ttp.R"))

rf_bl_ttp = rf_bl
lasso_bl_ttp = lasso_bl
nn_bl_ttp = nn_bl

rf_nihss_ttp = rf_nihss
lasso_nihss_ttp = lasso_nihss
nn_nihss_ttp = nn_nihss


# Stroke MRI only: No baseline data, always the same
rf_bl_mri_sv = roc(rf_bl_ttp$mrs_3months_binary, rf_bl_ttp$mri_sv)
lasso_bl_mri_sv = roc(lasso_bl_ttp$mrs_3months_binary, lasso_bl_ttp$mri_sv)
nn_bl_mri_sv = roc(nn_bl_ttp$mrs_3months_binary, nn_bl_ttp$mri_sv)

# MRI only
rf_bl_mri = roc(rf_bl_ttp$mrs_3months_binary, rf_bl_ttp$mri)
lasso_bl_mri = roc(lasso_bl_ttp$mrs_3months_binary, lasso_bl_ttp$mri)
nn_bl_mri = roc(nn_bl_ttp$mrs_3months_binary, nn_bl_ttp$mri)

# Baseline: consider always rbv for baseline
rf_bl_bl = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$bl)
rf_nihss_bl = roc(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$bl)

lasso_bl_bl = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$bl)
lasso_nihss_bl = roc(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$bl)

nn_bl_bl = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$bl)
nn_nihss_bl = roc(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$bl)


# Baseline + stroke MRI
rf_bl_bl_mri_sv = roc(rf_bl_ttp$mrs_3months_binary, rf_bl_ttp$bl_mri_sv)
rf_nihss_bl_mri_sv = roc(rf_nihss_ttp$mrs_3months_binary, rf_nihss_ttp$bl_mri_sv)

lasso_bl_bl_mri_sv = roc(lasso_bl_ttp$mrs_3months_binary, lasso_bl_ttp$bl_mri_sv)
lasso_nihss_bl_mri_sv = roc(lasso_nihss_ttp$mrs_3months_binary, lasso_nihss_ttp$bl_mri_sv)

nn_bl_bl_mri_sv = roc(nn_bl_ttp$mrs_3months_binary, nn_bl_ttp$bl_mri_sv)
nn_nihss_bl_mri_sv = roc(nn_nihss_ttp$mrs_3months_binary, nn_nihss_ttp$bl_mri_sv)


# Baseline + MRI
rf_bl_bl_mri = roc(rf_bl_ttp$mrs_3months_binary, rf_bl_ttp$bl_mri)
rf_nihss_bl_mri = roc(rf_nihss_ttp$mrs_3months_binary, rf_nihss_ttp$bl_mri)

lasso_bl_bl_mri = roc(lasso_bl_ttp$mrs_3months_binary, lasso_bl_ttp$bl_mri)
lasso_nihss_bl_mri = roc(lasso_nihss_ttp$mrs_3months_binary, lasso_nihss_ttp$bl_mri)

nn_bl_bl_mri = roc(nn_bl_ttp$mrs_3months_binary, nn_bl_ttp$bl_mri)
nn_nihss_bl_mri = roc(nn_nihss_ttp$mrs_3months_binary, nn_nihss_ttp$bl_mri)


# Expert
rf_bl_expert = roc(rf_bl_ttp$mrs_3months_binary, rf_bl_ttp$expert)
rf_nihss_expert = roc(rf_nihss_ttp$mrs_3months_binary, rf_nihss_ttp$expert)

lasso_bl_expert = roc(lasso_bl_ttp$mrs_3months_binary, lasso_bl_ttp$expert)
lasso_nihss_expert = roc(lasso_nihss_ttp$mrs_3months_binary, lasso_nihss_ttp$expert)

nn_bl_expert = roc(nn_bl_ttp$mrs_3months_binary, nn_bl_ttp$expert)
nn_nihss_expert = roc(nn_nihss_ttp$mrs_3months_binary, nn_nihss_ttp$expert)


#Tests

# MRI(SV) vs. MRI
rf_bl_mri_sv_mri = roc.test(rf_bl_mri_sv, rf_bl_mri, method="bootstrap")
lasso_bl_mri_sv_mri = roc.test(lasso_bl_mri_sv, lasso_bl_mri, method="bootstrap")
nn_bl_mri_sv_mri = roc.test(nn_bl_mri_sv, nn_bl_mri, method="bootstrap")

# MRI vs. Baseline
rf_bl_mri_bl = roc.test(rf_bl_mri, rf_bl_bl, method="bootstrap")
lasso_bl_mri_bl = roc.test(lasso_bl_mri, lasso_bl_bl, method="bootstrap")
nn_bl_mri_bl = roc.test(nn_bl_mri, nn_bl_bl, method="bootstrap")

# Baseline vs. Baseline + MRI(sv)
rf_bl_bl_bl_mri_sv = roc.test(rf_bl_bl, rf_bl_bl_mri_sv, method="bootstrap")
lasso_bl_bl_bl_mri_sv = roc.test(lasso_bl_bl, lasso_bl_bl_mri_sv, method="bootstrap")
nn_bl_bl_bl_mri_sv = roc.test(nn_bl_bl, nn_bl_bl_mri_sv, method="bootstrap")

rf_nihss_bl_bl_mri_sv = roc.test(rf_nihss_bl, rf_nihss_bl_mri_sv, method="bootstrap")
lasso_nihss_bl_bl_mri_sv = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri_sv, method="bootstrap")
nn_nihss_bl_bl_mri_sv = roc.test(nn_nihss_bl, nn_nihss_bl_mri_sv, method="bootstrap")

# Baseline vs. Baseline + MRI
rf_bl_bl_bl_mri = roc.test(rf_bl_bl, rf_bl_bl_mri, method="bootstrap")
lasso_bl_bl_bl_mri = roc.test(lasso_bl_bl, lasso_bl_bl_mri, method="bootstrap")
nn_bl_bl_bl_mri = roc.test(nn_bl_bl, nn_bl_bl_mri, method="bootstrap")

rf_nihss_bl_bl_mri = roc.test(rf_nihss_bl, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_bl_bl_mri = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_bl_bl_mri = roc.test(nn_nihss_bl, nn_nihss_bl_mri, method="bootstrap")

# Thrive vs. Baseline
rf_bl_thrive_bl = roc.test(thrive_bl, rf_bl_bl, method="bootstrap")
lasso_bl_thrive_bl = roc.test(thrive_bl, lasso_bl_bl, method="bootstrap")
nn_bl_thrive_bl = roc.test(thrive_bl, nn_bl_bl, method="bootstrap")

# Expert vs. Baseline (MRI) as Expert contains MRI data
rf_bl_expert_bl_mri = roc.test(rf_bl_expert, rf_bl_bl_mri, method="bootstrap")
lasso_bl_expert_bl_mri = roc.test(lasso_bl_expert, lasso_bl_bl_mri, method="bootstrap")
nn_bl_expert_bl_mri = roc.test(nn_bl_expert, nn_bl_bl_mri, method="bootstrap")

rf_nihss_expert_bl_mri = roc.test(rf_nihss_expert, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_expert_bl_mri = roc.test(lasso_nihss_expert, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_expert_bl_mri = roc.test(nn_nihss_expert, nn_nihss_bl_mri, method="bootstrap")

# Expert vs. Baseline
rf_bl_bl_expert = roc.test(rf_bl_bl, rf_bl_expert, method="bootstrap")
lasso_bl_bl_expert = roc.test(lasso_bl_bl, lasso_bl_expert, method="bootstrap")
nn_bl_bl_expert = roc.test(nn_bl_bl, nn_bl_expert, method="bootstrap")

rf_nihss_bl_expert = roc.test(rf_nihss_bl, rf_nihss_expert, method="bootstrap")
lasso_nihss_bl_expert = roc.test(lasso_nihss_bl, lasso_nihss_expert, method="bootstrap")
nn_nihss_bl_expert = roc.test(nn_nihss_bl, nn_nihss_expert, method="bootstrap")


# Create tables
get_values = function(dat){
  return(c("AUC1"=dat$roc1$auc, "AUC2"=dat$roc2$auc, "pvalue"=dat$p.value))
}

##### bl
# MRI sv vs. MRI
# MRI vs. BL
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# THRIVE vs. BL
# BL vs. Expert
tab_rf_bl = rbind(get_values(rf_bl_mri_sv_mri),
                  get_values(rf_bl_mri_bl),
                  get_values(rf_bl_bl_bl_mri_sv),
                  get_values(rf_bl_bl_bl_mri),
                  get_values(rf_bl_thrive_bl),
                  get_values(rf_bl_expert_bl_mri),
                  get_values(rf_bl_bl_expert))
row.names(tab_rf_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_lasso_bl = rbind(get_values(lasso_bl_mri_sv_mri),
                     get_values(lasso_bl_mri_bl),
                     get_values(lasso_bl_bl_bl_mri_sv),
                     get_values(lasso_bl_bl_bl_mri),
                     get_values(lasso_bl_thrive_bl),
                     get_values(lasso_bl_expert_bl_mri),
                     get_values(lasso_bl_bl_expert))
row.names(tab_lasso_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                            "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_nn_bl = rbind(get_values(nn_bl_mri_sv_mri),
                  get_values(nn_bl_mri_bl),
                  get_values(nn_bl_bl_bl_mri_sv),
                  get_values(nn_bl_bl_bl_mri),
                  get_values(nn_bl_thrive_bl),
                  get_values(nn_bl_expert_bl_mri),
                  get_values(nn_bl_bl_expert))
row.names(tab_nn_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")


##### nihss
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# BL vs. Expert
tab_rf_nihss = rbind(get_values(rf_nihss_bl_bl_mri_sv),
                     get_values(rf_nihss_bl_bl_mri),
                     get_values(rf_nihss_expert_bl_mri),
                     get_values(rf_nihss_bl_expert))
row.names(tab_rf_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_lasso_nihss = rbind(get_values(lasso_nihss_bl_bl_mri_sv),
                        get_values(lasso_nihss_bl_bl_mri),
                        get_values(lasso_nihss_expert_bl_mri),
                        get_values(lasso_nihss_bl_expert))
row.names(tab_lasso_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                               "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_nn_nihss = rbind(get_values(nn_nihss_bl_bl_mri_sv),
                     get_values(nn_nihss_bl_bl_mri),
                     get_values(nn_nihss_expert_bl_mri),
                     get_values(nn_nihss_bl_expert))
row.names(tab_nn_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")


tab_rf_bl
tab_lasso_bl
tab_nn_bl

tab_rf_nihss
tab_lasso_nihss
tab_nn_nihss

tab_ttp = rbind(data.frame(tab_rf_bl,model="rf",comp=row.names(tab_rf_bl),time="bl"),
                data.frame(tab_rf_nihss,model="rf",comp=row.names(tab_rf_nihss),time="nihss"),
                data.frame(tab_lasso_bl,model="lasso",comp=row.names(tab_lasso_bl),time="bl"),
                data.frame(tab_lasso_nihss,model="lasso",comp=row.names(tab_lasso_nihss),time="nihss"),
                data.frame(tab_nn_bl,model="nn",comp=row.names(tab_nn_bl),time="bl"),
                data.frame(tab_nn_nihss,model="nn",comp=row.names(tab_nn_nihss),time="nihss"))
tab_ttp = data.frame(tab_ttp, row.names=NULL)




# All --------------------------------------------------------------------------

load(file=paste0(dir,"data/CV/test_pred_rf_BL_all.R"))
load(file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_all.R"))

load(file=paste0(dir,"data/CV/test_pred_lasso_BL_all.R"))
load(file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_all.R"))

load(file=paste0(dir,"data/CV/test_pred_nn_BL_all.R"))
load(file=paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_all.R"))

rf_bl_all = rf_bl
lasso_bl_all = lasso_bl
nn_bl_all = nn_bl

rf_nihss_all = rf_nihss
lasso_nihss_all = lasso_nihss
nn_nihss_all = nn_nihss


# Stroke MRI only: No baseline data, always the same
rf_bl_mri_sv = roc(rf_bl_all$mrs_3months_binary, rf_bl_all$mri_sv)
lasso_bl_mri_sv = roc(lasso_bl_all$mrs_3months_binary, lasso_bl_all$mri_sv)
nn_bl_mri_sv = roc(nn_bl_all$mrs_3months_binary, nn_bl_all$mri_sv)

# MRI only
rf_bl_mri = roc(rf_bl_all$mrs_3months_binary, rf_bl_all$mri)
lasso_bl_mri = roc(lasso_bl_all$mrs_3months_binary, lasso_bl_all$mri)
nn_bl_mri = roc(nn_bl_all$mrs_3months_binary, nn_bl_all$mri)

# Baseline: consider always rbv for baseline
rf_bl_bl = roc(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$bl)
rf_nihss_bl = roc(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$bl)

lasso_bl_bl = roc(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$bl)
lasso_nihss_bl = roc(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$bl)

nn_bl_bl = roc(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$bl)
nn_nihss_bl = roc(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$bl)


# Baseline + stroke MRI
rf_bl_bl_mri_sv = roc(rf_bl_all$mrs_3months_binary, rf_bl_all$bl_mri_sv)
rf_nihss_bl_mri_sv = roc(rf_nihss_all$mrs_3months_binary, rf_nihss_all$bl_mri_sv)

lasso_bl_bl_mri_sv = roc(lasso_bl_all$mrs_3months_binary, lasso_bl_all$bl_mri_sv)
lasso_nihss_bl_mri_sv = roc(lasso_nihss_all$mrs_3months_binary, lasso_nihss_all$bl_mri_sv)

nn_bl_bl_mri_sv = roc(nn_bl_all$mrs_3months_binary, nn_bl_all$bl_mri_sv)
nn_nihss_bl_mri_sv = roc(nn_nihss_all$mrs_3months_binary, nn_nihss_all$bl_mri_sv)


# Baseline + MRI
rf_bl_bl_mri = roc(rf_bl_all$mrs_3months_binary, rf_bl_all$bl_mri)
rf_nihss_bl_mri = roc(rf_nihss_all$mrs_3months_binary, rf_nihss_all$bl_mri)

lasso_bl_bl_mri = roc(lasso_bl_all$mrs_3months_binary, lasso_bl_all$bl_mri)
lasso_nihss_bl_mri = roc(lasso_nihss_all$mrs_3months_binary, lasso_nihss_all$bl_mri)

nn_bl_bl_mri = roc(nn_bl_all$mrs_3months_binary, nn_bl_all$bl_mri)
nn_nihss_bl_mri = roc(nn_nihss_all$mrs_3months_binary, nn_nihss_all$bl_mri)


# Expert
rf_bl_expert = roc(rf_bl_all$mrs_3months_binary, rf_bl_all$expert)
rf_nihss_expert = roc(rf_nihss_all$mrs_3months_binary, rf_nihss_all$expert)

lasso_bl_expert = roc(lasso_bl_all$mrs_3months_binary, lasso_bl_all$expert)
lasso_nihss_expert = roc(lasso_nihss_all$mrs_3months_binary, lasso_nihss_all$expert)

nn_bl_expert = roc(nn_bl_all$mrs_3months_binary, nn_bl_all$expert)
nn_nihss_expert = roc(nn_nihss_all$mrs_3months_binary, nn_nihss_all$expert)


#Tests

# MRI(SV) vs. MRI
rf_bl_mri_sv_mri = roc.test(rf_bl_mri_sv, rf_bl_mri, method="bootstrap")
lasso_bl_mri_sv_mri = roc.test(lasso_bl_mri_sv, lasso_bl_mri, method="bootstrap")
nn_bl_mri_sv_mri = roc.test(nn_bl_mri_sv, nn_bl_mri, method="bootstrap")

# MRI vs. Baseline
rf_bl_mri_bl = roc.test(rf_bl_mri, rf_bl_bl, method="bootstrap")
lasso_bl_mri_bl = roc.test(lasso_bl_mri, lasso_bl_bl, method="bootstrap")
nn_bl_mri_bl = roc.test(nn_bl_mri, nn_bl_bl, method="bootstrap")

# Baseline vs. Baseline + MRI(sv)
rf_bl_bl_bl_mri_sv = roc.test(rf_bl_bl, rf_bl_bl_mri_sv, method="bootstrap")
lasso_bl_bl_bl_mri_sv = roc.test(lasso_bl_bl, lasso_bl_bl_mri_sv, method="bootstrap")
nn_bl_bl_bl_mri_sv = roc.test(nn_bl_bl, nn_bl_bl_mri_sv, method="bootstrap")

rf_nihss_bl_bl_mri_sv = roc.test(rf_nihss_bl, rf_nihss_bl_mri_sv, method="bootstrap")
lasso_nihss_bl_bl_mri_sv = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri_sv, method="bootstrap")
nn_nihss_bl_bl_mri_sv = roc.test(nn_nihss_bl, nn_nihss_bl_mri_sv, method="bootstrap")

# Baseline vs. Baseline + MRI
rf_bl_bl_bl_mri = roc.test(rf_bl_bl, rf_bl_bl_mri, method="bootstrap")
lasso_bl_bl_bl_mri = roc.test(lasso_bl_bl, lasso_bl_bl_mri, method="bootstrap")
nn_bl_bl_bl_mri = roc.test(nn_bl_bl, nn_bl_bl_mri, method="bootstrap")

rf_nihss_bl_bl_mri = roc.test(rf_nihss_bl, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_bl_bl_mri = roc.test(lasso_nihss_bl, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_bl_bl_mri = roc.test(nn_nihss_bl, nn_nihss_bl_mri, method="bootstrap")

# Thrive vs. Baseline
rf_bl_thrive_bl = roc.test(thrive_bl, rf_bl_bl, method="bootstrap")
lasso_bl_thrive_bl = roc.test(thrive_bl, lasso_bl_bl, method="bootstrap")
nn_bl_thrive_bl = roc.test(thrive_bl, nn_bl_bl, method="bootstrap")

# Expert vs. Baseline (MRI) as Expert contains MRI data
rf_bl_expert_bl_mri = roc.test(rf_bl_expert, rf_bl_bl_mri, method="bootstrap")
lasso_bl_expert_bl_mri = roc.test(lasso_bl_expert, lasso_bl_bl_mri, method="bootstrap")
nn_bl_expert_bl_mri = roc.test(nn_bl_expert, nn_bl_bl_mri, method="bootstrap")

rf_nihss_expert_bl_mri = roc.test(rf_nihss_expert, rf_nihss_bl_mri, method="bootstrap")
lasso_nihss_expert_bl_mri = roc.test(lasso_nihss_expert, lasso_nihss_bl_mri, method="bootstrap")
nn_nihss_expert_bl_mri = roc.test(nn_nihss_expert, nn_nihss_bl_mri, method="bootstrap")

# Expert vs. Baseline
rf_bl_bl_expert = roc.test(rf_bl_bl, rf_bl_expert, method="bootstrap")
lasso_bl_bl_expert = roc.test(lasso_bl_bl, lasso_bl_expert, method="bootstrap")
nn_bl_bl_expert = roc.test(nn_bl_bl, nn_bl_expert, method="bootstrap")

rf_nihss_bl_expert = roc.test(rf_nihss_bl, rf_nihss_expert, method="bootstrap")
lasso_nihss_bl_expert = roc.test(lasso_nihss_bl, lasso_nihss_expert, method="bootstrap")
nn_nihss_bl_expert = roc.test(nn_nihss_bl, nn_nihss_expert, method="bootstrap")


# Create tables
get_values = function(dat){
  return(c("AUC1"=dat$roc1$auc, "AUC2"=dat$roc2$auc, "pvalue"=dat$p.value))
}

##### bl
# MRI sv vs. MRI
# MRI vs. BL
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# THRIVE vs. BL
# BL vs. Expert
tab_rf_bl = rbind(get_values(rf_bl_mri_sv_mri),
                  get_values(rf_bl_mri_bl),
                  get_values(rf_bl_bl_bl_mri_sv),
                  get_values(rf_bl_bl_bl_mri),
                  get_values(rf_bl_thrive_bl),
                  get_values(rf_bl_expert_bl_mri),
                  get_values(rf_bl_bl_expert))
row.names(tab_rf_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_lasso_bl = rbind(get_values(lasso_bl_mri_sv_mri),
                     get_values(lasso_bl_mri_bl),
                     get_values(lasso_bl_bl_bl_mri_sv),
                     get_values(lasso_bl_bl_bl_mri),
                     get_values(lasso_bl_thrive_bl),
                     get_values(lasso_bl_expert_bl_mri),
                     get_values(lasso_bl_bl_expert))
row.names(tab_lasso_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                            "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")
tab_nn_bl = rbind(get_values(nn_bl_mri_sv_mri),
                  get_values(nn_bl_mri_bl),
                  get_values(nn_bl_bl_bl_mri_sv),
                  get_values(nn_bl_bl_bl_mri),
                  get_values(nn_bl_thrive_bl),
                  get_values(nn_bl_expert_bl_mri),
                  get_values(nn_bl_bl_expert))
row.names(tab_nn_bl) = c("MRI(S) vs. MRI", "MRI vs. PtC", "PtC vs. PtC+MRI(S)", 
                         "PtC vs. PtC+MRI", "THRIVE vs. PtC", 
                         "Expert vs. PtC+MRI", "PtC vs. Expert")


##### nihss
# BL vs. BL+MRI sv
# BL vs. BL+MRI
# BL vs. Expert
tab_rf_nihss = rbind(get_values(rf_nihss_bl_bl_mri_sv),
                     get_values(rf_nihss_bl_bl_mri),
                     get_values(rf_nihss_expert_bl_mri),
                     get_values(rf_nihss_bl_expert))
row.names(tab_rf_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_lasso_nihss = rbind(get_values(lasso_nihss_bl_bl_mri_sv),
                        get_values(lasso_nihss_bl_bl_mri),
                        get_values(lasso_nihss_expert_bl_mri),
                        get_values(lasso_nihss_bl_expert))
row.names(tab_lasso_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                               "Expert vs. PtC+MRI", "PtC vs. Expert")

tab_nn_nihss = rbind(get_values(nn_nihss_bl_bl_mri_sv),
                     get_values(nn_nihss_bl_bl_mri),
                     get_values(nn_nihss_expert_bl_mri),
                     get_values(nn_nihss_bl_expert))
row.names(tab_nn_nihss) = c("PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                            "Expert vs. PtC+MRI", "PtC vs. Expert")


tab_rf_bl
tab_lasso_bl
tab_nn_bl

tab_rf_nihss
tab_lasso_nihss
tab_nn_nihss

tab_all = rbind(data.frame(tab_rf_bl,model="rf",comp=row.names(tab_rf_bl),time="bl"),
                data.frame(tab_rf_nihss,model="rf",comp=row.names(tab_rf_nihss),time="nihss"),
                data.frame(tab_lasso_bl,model="lasso",comp=row.names(tab_lasso_bl),time="bl"),
                data.frame(tab_lasso_nihss,model="lasso",comp=row.names(tab_lasso_nihss),time="nihss"),
                data.frame(tab_nn_bl,model="nn",comp=row.names(tab_nn_bl),time="bl"),
                data.frame(tab_nn_nihss,model="nn",comp=row.names(tab_nn_nihss),time="nihss"))
tab_all = data.frame(tab_all, row.names=NULL)




# All results ------------------------------------------------------------------

tab_comb = rbind(data.frame(tab_rbv, perfusion = "rbv"),
                data.frame(tab_rbf, perfusion = "rbf"),
                data.frame(tab_mtt, perfusion = "mtt"),
                data.frame(tab_tmax, perfusion = "tmax"),
                data.frame(tab_ttp, perfusion = "ttp"),
                data.frame(tab_tmip, perfusion = "tmip"),
                data.frame(tab_all, perfusion = "all"))

tab_comb$pvalue = round(tab_comb$pvalue, 3)
tab_comb$AUC1 = round(tab_comb$AUC1, 3)
tab_comb$AUC2 = round(tab_comb$AUC2, 3)
tab_comb$perfusion = factor(tab_comb$perfusion, levels=c("rbv","rbf","tmax","mtt","tmip","ttp","all"))
tab_comb = tab_comb[tab_comb$comp %in% c("MRI(S) vs. MRI", "MRI vs. PtC", "THRIVE vs. PtC", 
                                         "PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                                         "PtC vs. Expert"), ]
tab_comb$comp = factor(tab_comb$comp, levels=c("MRI(S) vs. MRI", "MRI vs. PtC", "THRIVE vs. PtC", 
                                               "PtC vs. PtC+MRI(S)", "PtC vs. PtC+MRI", 
                                               "PtC vs. Expert"))

tab_bl = tab_comb[tab_comb$time=="bl",]
tab_nihss = tab_comb[tab_comb$time=="nihss",]

tab_bl = tab_bl[order(tab_bl$model, tab_bl$comp, tab_bl$perfusion),]
tab_nihss = tab_nihss[order(tab_nihss$model, tab_nihss$comp, tab_nihss$perfusion),]

write.xlsx(tab_bl, file=paste0(dir,"/data/CV/Tests_BL_Revision.xlsx"))
write.xlsx(tab_nihss, file=paste0(dir,"/data/CV/Tests_BL_TICI_NIHSS_Revision.xlsx"))
