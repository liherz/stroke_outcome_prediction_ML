rm(list=ls())

library(ggplot2)
library(gridExtra)
library(openxlsx)

# set the path to the files
dir = "C:/Users/hezo/Dropbox/PhD/Stroke/PowerAnalyse_GewichteteMittelwerte_MixedModelSD_und_PredictionModel/"
# dir = "/Users/janne.hamann/Dropbox/StatistikOtherSideProjekt/"
setwd(dir)

source(paste0(dir,"analyses/functions/wilson_ci.R"))
source(paste0(dir,"analyses/functions/accuracy.R"))
source(paste0(dir,"analyses/functions/sensitivity.R"))
source(paste0(dir,"analyses/functions/specificity.R"))


dat = read.xlsx(paste0(dir,"data/Data_THRIVE_20190312.xlsx"), sheet=1, na.strings="NA")
head(dat)
load(file=paste0(dir, "data/data_","rbv","_wide_imp_before_therapy.R"))
dat = merge(dat,dat_imp, by.x="PID", by.y="p_id")

# change thrive outcome to 0,1: good outcome = 0, bad outcome = 1
dat$outcome_thrive = ifelse(dat$p_good_outcome<0.5,1,0)

# exclude NA (1 thrive score not available)
table(dat$outcome_thrive, dat$mrs_3months_binary)
which(is.na(dat$outcome_thrive))
dat = dat[-which(is.na(dat$outcome_thrive)),]

# Get the accuracy and the corresponding CI
tab = table(dat$mrs_3months_binary, dat$outcome_thrive)
results = c("acc"=accuracy(dat$mrs_3months_binary, dat$outcome_thrive),
            "acc_ci" = wilson_ci(n_successes = sum(tab['0','0'], tab['1','1']), n_obs = sum(tab)),
            "spec"=specificity(dat$mrs_3months_binary, dat$outcome_thrive),
            "spec_ci" = wilson_ci(n_successes = tab['0','0'], n_obs = sum(tab['0',])),
            "sens"=sensitivity(dat$mrs_3months_binary, dat$outcome_thrive),
            "sens_ci" = wilson_ci(n_successes = tab['1','1'], n_obs = sum(tab['1',])))

results
# acc  acc_ci1  acc_ci2     spec spec_ci1 spec_ci2     sens sens_ci1 sens_ci2 
# 61.99    55.44    68.13    61.02    52.00    69.34    63.11    53.47    71.80 

# AUC
source(paste0(dir, "analyses/functions/roc_data.R"))
roc = roc_data(dat$mrs_3months_binary, 1-dat$p_good_outcome)
c(round(roc$auc["auc"],3), round(roc$auc["lower_ci"],3), round(roc$auc["upper_ci"],3))
