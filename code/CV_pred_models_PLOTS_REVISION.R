# Plots for publication
rm(list=ls())

library(ggplot2)
library(gridExtra)
library(openxlsx)
library(lemon)

# set the path to the files
dir = "C:/Users/user/Dropbox/PhD/Stroke/PowerAnalyse_GewichteteMittelwerte_MixedModelSD_und_PredictionModel/"
# dir = "/Users/janne.hamann/Dropbox/StatistikOtherSideProjekt/"
setwd(dir)

source(paste0(dir, "analyses/functions/roc_data.R"))

# options(OutDec = ".")
# 3.5
# options(OutDec = ".")
# 3.5


# The baseline model is always taken from parameter rCBV:
# We ran the baseline model for each parameter but there are no parameters
# included in the baseline model (same model, multiple RF runs...)




# Model comparison: AUC --------------------------------------------------------

# Get the data
get_plot_data = function(dat, measure_name, model_name, selected){
  
  if("mri_sv" %in% colnames(dat)){
    dat_mri_sv = roc_data(dat$mrs_3months_binary ,dat$mri_sv)
    dat_mri = roc_data(dat$mrs_3months_binary ,dat$mri)
    dat_bl = roc_data(dat$mrs_3months_binary ,dat$bl)
    dat_bl_mri_sv = roc_data(dat$mrs_3months_binary ,dat$bl_mri_sv)
    dat_bl_mri = roc_data(dat$mrs_3months_binary ,dat$bl_mri)
    dat_expert = roc_data(dat$mrs_3months_binary ,dat$expert)
    
    dat_plot = data.frame(acc = c(dat_mri_sv$auc["auc"], dat_mri$auc["auc"],
                                  dat_bl$auc["auc"], dat_bl_mri_sv$auc["auc"], 
                                  dat_bl_mri$auc["auc"], dat_expert$auc["auc"]),
                          lower = c(dat_mri_sv$auc["lower_ci"], dat_mri$auc["lower_ci"],
                                    dat_bl$auc["lower_ci"], dat_bl_mri_sv$auc["lower_ci"], 
                                    dat_bl_mri$auc["lower_ci"], dat_expert$auc["lower_ci"]),
                          upper = c(dat_mri_sv$auc["upper_ci"], dat_mri$auc["upper_ci"],
                                    dat_bl$auc["upper_ci"], dat_bl_mri_sv$auc["upper_ci"], 
                                    dat_bl_mri$auc["upper_ci"], dat_expert$auc["upper_ci"]),
                          measure=measure_name,
                          stat_model=model_name,
                          model = c("MRI(S)","MRI","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
  } else{
    dat_bl = roc_data(dat$mrs_3months_binary ,dat$bl)
    dat_bl_mri_sv = roc_data(dat$mrs_3months_binary ,dat$bl_mri_sv)
    dat_bl_mri = roc_data(dat$mrs_3months_binary ,dat$bl_mri)
    dat_expert = roc_data(dat$mrs_3months_binary ,dat$expert)
    
    dat_plot = data.frame(acc = c(dat_bl$auc["auc"], dat_bl_mri_sv$auc["auc"], 
                                  dat_bl_mri$auc["auc"], dat_expert$auc["auc"]),
                          lower = c(dat_bl$auc["lower_ci"], dat_bl_mri_sv$auc["lower_ci"], 
                                    dat_bl_mri$auc["lower_ci"], dat_expert$auc["lower_ci"]),
                          upper = c(dat_bl$auc["upper_ci"], dat_bl_mri_sv$auc["upper_ci"], 
                                    dat_bl_mri$auc["upper_ci"], dat_expert$auc["upper_ci"]),
                          measure=measure_name,
                          stat_model=model_name,
                          model = c("PtC","PtC+MRI(S)","PtC+MRI","Expert"))
  }
  return(dat_plot)
}



# BL: Random Forest
load(paste0(dir,"data/CV/test_pred_rf_BL_rbf.R"))
rf_bl_rbf = get_plot_data(rf_bl, "rCBF", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_rbv.R"))
rf_bl_rbv = get_plot_data(rf_bl, "rCBV", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_tmax.R"))
rf_bl_tmax = get_plot_data(rf_bl, "TMAX", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_mtt.R"))
rf_bl_mtt = get_plot_data(rf_bl, "MTT", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_tmip.R"))
rf_bl_tmip = get_plot_data(rf_bl, "tMIP", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_ttp.R"))
rf_bl_ttp = get_plot_data(rf_bl, "TTP", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_all.R"))
rf_bl_all = get_plot_data(rf_bl, "All", "rf")
rf_bl = rbind(rf_bl_rbf, rf_bl_rbv, rf_bl_tmax, rf_bl_mtt, rf_bl_tmip, rf_bl_ttp, rf_bl_all)

# BL: LASSO
load(paste0(dir,"data/CV/test_pred_lasso_BL_rbf.R"))
lasso_bl_rbf = get_plot_data(lasso_bl, "rCBF", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_rbv.R"))
lasso_bl_rbv = get_plot_data(lasso_bl, "rCBV", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_tmax.R"))
lasso_bl_tmax = get_plot_data(lasso_bl, "TMAX", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_mtt.R"))
lasso_bl_mtt = get_plot_data(lasso_bl, "MTT", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_tmip.R"))
lasso_bl_tmip = get_plot_data(lasso_bl, "tMIP", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_ttp.R"))
lasso_bl_ttp = get_plot_data(lasso_bl, "TTP", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_all.R"))
lasso_bl_all = get_plot_data(lasso_bl, "All", "lasso")
lasso_bl = rbind(lasso_bl_rbf, lasso_bl_rbv, lasso_bl_tmax, lasso_bl_mtt, lasso_bl_tmip, lasso_bl_ttp, lasso_bl_all)

# BL: Neural Network
load(paste0(dir,"data/CV/test_pred_nn_BL_rbf.R"))
nn_bl_rbf = get_plot_data(nn_bl, "rCBF", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_rbv.R"))
nn_bl_rbv = get_plot_data(nn_bl, "rCBV", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_tmax.R"))
nn_bl_tmax = get_plot_data(nn_bl, "TMAX", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_mtt.R"))
nn_bl_mtt = get_plot_data(nn_bl, "MTT", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_tmip.R"))
nn_bl_tmip = get_plot_data(nn_bl, "tMIP", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_ttp.R"))
nn_bl_ttp = get_plot_data(nn_bl, "TTP", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_all.R"))
nn_bl_all = get_plot_data(nn_bl, "All", "nn")
nn_bl = rbind(nn_bl_rbf, nn_bl_rbv, nn_bl_tmax, nn_bl_mtt, nn_bl_tmip, nn_bl_ttp, nn_bl_all)

# Create a dataframe
dat_bl = rbind(rf_bl, lasso_bl, nn_bl)
dat_bl$model = factor(dat_bl$model, levels=c("MRI(S)","MRI","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
# levels(dat_bl$model)

# for the baseline model we consider only model rCBV
k = which(dat_bl$model=="PtC" & dat_bl$measure %in% c("rCBF","TTP","TMAX","MTT","tMIP","All"))
dat_bl = dat_bl[-k,]
dat_bl$measure = as.character(dat_bl$measure)
dat_bl[dat_bl$model=="PtC","measure"] = "None"
dat_bl$measure = as.factor(dat_bl$measure)




# BL + TICI + NIHSS: Random Forest
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_rbf.R"))
rf_nihss_rbf = get_plot_data(rf_nihss, "rCBF", "rf")
rf_nihss_rbf = rbind(rf_bl_rbf[rf_bl_rbf$model %in% c("MRI(S)","MRI"),], rf_nihss_rbf)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_rbv.R"))
rf_nihss_rbv = get_plot_data(rf_nihss, "rCBV", "rf")
rf_nihss_rbv = rbind(rf_bl_rbv[rf_bl_rbv$model %in% c("MRI(S)","MRI"),], rf_nihss_rbv)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_tmax.R"))
rf_nihss_tmax = get_plot_data(rf_nihss, "TMAX", "rf")
rf_nihss_tmax = rbind(rf_bl_tmax[rf_bl_tmax$model %in% c("MRI(S)","MRI"),], rf_nihss_tmax)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_mtt.R"))
rf_nihss_mtt = get_plot_data(rf_nihss, "MTT", "rf")
rf_nihss_mtt = rbind(rf_bl_mtt[rf_bl_mtt$model %in% c("MRI(S)","MRI"),], rf_nihss_mtt)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_tmip.R"))
rf_nihss_tmip = get_plot_data(rf_nihss, "tMIP", "rf")
rf_nihss_tmip = rbind(rf_bl_tmip[rf_bl_tmip$model %in% c("MRI(S)","MRI"),], rf_nihss_tmip)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_ttp.R"))
rf_nihss_ttp = get_plot_data(rf_nihss, "TTP", "rf")
rf_nihss_ttp = rbind(rf_bl_ttp[rf_bl_ttp$model %in% c("MRI(S)","MRI"),], rf_nihss_ttp)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_all.R"))
rf_nihss_all = get_plot_data(rf_nihss, "All", "rf")
rf_nihss_all = rbind(rf_bl_all[rf_bl_all$model %in% c("MRI(S)","MRI"),], rf_nihss_all)
rf_nihss = rbind(rf_nihss_rbf, rf_nihss_rbv, rf_nihss_tmax, rf_nihss_mtt, rf_nihss_tmip, rf_nihss_ttp, rf_nihss_all)

# BL + TICI + NIHSS: LASSO
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_rbf.R"))
lasso_nihss_rbf = get_plot_data(lasso_nihss, "rCBF", "lasso")
lasso_nihss_rbf = rbind(lasso_bl_rbf[lasso_bl_rbf$model %in% c("MRI(S)","MRI"),], lasso_nihss_rbf)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_rbv.R"))
lasso_nihss_rbv = get_plot_data(lasso_nihss, "rCBV", "lasso")
lasso_nihss_rbv = rbind(lasso_bl_rbv[lasso_bl_rbv$model %in% c("MRI(S)","MRI"),], lasso_nihss_rbv)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_tmax.R"))
lasso_nihss_tmax = get_plot_data(lasso_nihss, "TMAX", "lasso")
lasso_nihss_tmax = rbind(lasso_bl_tmax[lasso_bl_tmax$model %in% c("MRI(S)","MRI"),], lasso_nihss_tmax)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_mtt.R"))
lasso_nihss_mtt = get_plot_data(lasso_nihss, "MTT", "lasso")
lasso_nihss_mtt = rbind(lasso_bl_mtt[lasso_bl_mtt$model %in% c("MRI(S)","MRI"),], lasso_nihss_mtt)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_tmip.R"))
lasso_nihss_tmip = get_plot_data(lasso_nihss, "tMIP", "lasso")
lasso_nihss_tmip = rbind(lasso_bl_tmip[lasso_bl_tmip$model %in% c("MRI(S)","MRI"),], lasso_nihss_tmip)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_ttp.R"))
lasso_nihss_ttp = get_plot_data(lasso_nihss, "TTP", "lasso")
lasso_nihss_ttp = rbind(lasso_bl_ttp[lasso_bl_ttp$model %in% c("MRI(S)","MRI"),], lasso_nihss_ttp)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_all.R"))
lasso_nihss_all = get_plot_data(lasso_nihss, "All", "lasso")
lasso_nihss_all = rbind(lasso_bl_all[lasso_bl_all$model %in% c("MRI(S)","MRI"),], lasso_nihss_all)
lasso_nihss = rbind(lasso_nihss_rbf, lasso_nihss_rbv, lasso_nihss_tmax, lasso_nihss_mtt, lasso_nihss_tmip, lasso_nihss_ttp, lasso_nihss_all)

# BL + TICI + NIHSS: Neural Network
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_rbf.R"))
nn_nihss_rbf = get_plot_data(nn_nihss, "rCBF", "nn")
nn_nihss_rbf = rbind(nn_bl_rbf[nn_bl_rbf$model %in% c("MRI(S)","MRI"),], nn_nihss_rbf)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_rbv.R"))
nn_nihss_rbv = get_plot_data(nn_nihss, "rCBV", "nn")
nn_nihss_rbv = rbind(nn_bl_rbv[nn_bl_rbv$model %in% c("MRI(S)","MRI"),], nn_nihss_rbv)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_tmax.R"))
nn_nihss_tmax = get_plot_data(nn_nihss, "TMAX", "nn")
nn_nihss_tmax = rbind(nn_bl_tmax[nn_bl_tmax$model %in% c("MRI(S)","MRI"),], nn_nihss_tmax)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_mtt.R"))
nn_nihss_mtt = get_plot_data(nn_nihss, "MTT", "nn")
nn_nihss_mtt = rbind(nn_bl_mtt[nn_bl_mtt$model %in% c("MRI(S)","MRI"),], nn_nihss_mtt)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_tmip.R"))
nn_nihss_tmip = get_plot_data(nn_nihss, "tMIP", "nn")
nn_nihss_tmip = rbind(nn_bl_tmip[nn_bl_tmip$model %in% c("MRI(S)","MRI"),], nn_nihss_tmip)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_ttp.R"))
nn_nihss_ttp = get_plot_data(nn_nihss, "TTP", "nn")
nn_nihss_ttp = rbind(nn_bl_ttp[nn_bl_ttp$model %in% c("MRI(S)","MRI"),], nn_nihss_ttp)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_all.R"))
nn_nihss_all = get_plot_data(nn_nihss, "All", "nn")
nn_nihss_all = rbind(nn_bl_all[nn_bl_all$model %in% c("MRI(S)","MRI"),], nn_nihss_all)
nn_nihss = rbind(nn_nihss_rbf, nn_nihss_rbv, nn_nihss_tmax, nn_nihss_mtt, nn_nihss_tmip, nn_nihss_ttp, nn_nihss_all)

# Create a dataframe
dat_nihss = rbind(rf_nihss, lasso_nihss, nn_nihss)
dat_nihss$model = factor(dat_nihss$model, levels=c("MRI(S)","MRI","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
levels(dat_nihss$model)

# for the baseline model we consider only model rCBV
k = which(dat_nihss$model=="PtC" & dat_nihss$measure %in% c("rCBF","TTP","TMAX","MTT","tMIP","All"))
dat_nihss = dat_nihss[-k,]
dat_nihss$measure = as.character(dat_nihss$measure)
dat_nihss[dat_nihss$model=="PtC","measure"] = "None"
dat_nihss$measure = as.factor(dat_nihss$measure)

dat_bl$measure = factor(dat_bl$measure, levels = c("rCBV","rCBF","TTP","TMAX","MTT","tMIP","All","None"))
dat_nihss$measure = factor(dat_nihss$measure, levels = c("rCBV","rCBF","TTP","TMAX","MTT","tMIP","All","None"))


# # Save the tables
# rf_bl_out = rf_bl[order(rf_bl$model),c("model","measure","acc","lower","upper")]
# rf_bl_out$acc = round(rf_bl_out$acc,3)
# rf_bl_out$lower = round(rf_bl_out$lower,3)
# rf_bl_out$upper = round(rf_bl_out$upper,3)
# rf_bl_out$model = factor(rf_bl_out$model, levels=c("MRI","MRI(S)","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
# write.xlsx(rf_bl_out, file=paste0(dir,"data/CV/Results_RF_BL_Revision.xlsx"))
# rf_nihss_out = rf_nihss[order(rf_nihss$model),c("model","measure","acc","lower","upper")]
# rf_nihss_out$acc = round(rf_nihss_out$acc,3)
# rf_nihss_out$lower = round(rf_nihss_out$lower,3)
# rf_nihss_out$upper = round(rf_nihss_out$upper,3)
# rf_nihss_out$model = factor(rf_nihss_out$model, levels=c("MRI","MRI(S)","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
# write.xlsx(rf_nihss_out, file=paste0(dir,"data/CV/Results_RF_BL_TICI_NIHSS_Revision.xlsx"))
# 
# lasso_bl_out = lasso_bl[order(lasso_bl$model),c("model","measure","acc","lower","upper")]
# lasso_bl_out$acc = round(lasso_bl_out$acc,3)
# lasso_bl_out$lower = round(lasso_bl_out$lower,3)
# lasso_bl_out$upper = round(lasso_bl_out$upper,3)
# lasso_bl_out$model = factor(lasso_bl_out$model, levels=c("MRI","MRI(S)","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
# write.xlsx(lasso_bl_out, file=paste0(dir,"data/CV/Results_LASSO_BL_Revision.xlsx"))
# lasso_nihss_out = lasso_nihss[order(lasso_nihss$model),c("model","measure","acc","lower","upper")]
# lasso_nihss_out$acc = round(lasso_nihss_out$acc,3)
# lasso_nihss_out$lower = round(lasso_nihss_out$lower,3)
# lasso_nihss_out$upper = round(lasso_nihss_out$upper,3)
# lasso_nihss_out$model = factor(lasso_nihss_out$model, levels=c("MRI","MRI(S)","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
# write.xlsx(lasso_nihss_out, file=paste0(dir,"data/CV/Results_LASSO_BL_TICI_NIHSS_Revision.xlsx"))
# 
# nn_bl_out = nn_bl[order(nn_bl$model),c("model","measure","acc","lower","upper")]
# nn_bl_out$acc = round(nn_bl_out$acc,3)
# nn_bl_out$lower = round(nn_bl_out$lower,3)
# nn_bl_out$upper = round(nn_bl_out$upper,3)
# nn_bl_out$model = factor(nn_bl_out$model, levels=c("MRI","MRI(S)","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
# write.xlsx(nn_bl_out, file=paste0(dir,"data/CV/Results_NN_BL_Revision.xlsx"))
# nn_nihss_out = nn_nihss[order(nn_nihss$model),c("model","measure","acc","lower","upper")]
# nn_nihss_out$acc = round(nn_nihss_out$acc,3)
# nn_nihss_out$lower = round(nn_nihss_out$lower,3)
# nn_nihss_out$upper = round(nn_nihss_out$upper,3)
# nn_nihss_out$model = factor(nn_nihss_out$model, levels=c("MRI","MRI(S)","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
# write.xlsx(nn_nihss_out, file=paste0(dir,"data/CV/Results_NN_BL_TICI_NIHSS_Revision.xlsx"))


# Plot
pd = position_dodge(0.6) # move them .05 to the left and right

dat0 = dat_bl[dat_bl$stat_model=="rf",]
p_rf_bl = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd) +
  ylab("AUC") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.35,0.9) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))

dat0 = dat_bl[dat_bl$stat_model=="lasso",]
p_lasso_bl = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd) +
  ylab("AUC") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.35,0.9) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))

dat0 = dat_bl[dat_bl$stat_model=="nn",]
p_nn_bl = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd) +
  ylab("AUC") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.35,0.9) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))



dat0 = dat_nihss[dat_nihss$stat_model=="rf",]
p_rf_nihss = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd) +
  ylab("AUC") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.35,0.9) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))


dat0 = dat_nihss[dat_nihss$stat_model=="lasso",]
p_lasso_nihss = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd) +
  ylab("AUC") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.35,0.9) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))


dat0 = dat_nihss[dat_nihss$stat_model=="nn",]
p_nn_nihss = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd) +
  ylab("AUC") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.35,0.9) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))


library(ggpubr)
legend = g_legend(p_rf_bl + theme(legend.position='bottom'))
pdf(paste0(dir,"for publication/Measure_comparison_AUC_BL_TICI_NIHSS_Revision.pdf"), width=20, height=15)
# postscript(paste0(dir,"for publication/Measure_comparison_AUC.eps"))
grid.arrange(arrangeGrob(p_rf_bl+theme(legend.position = "hidden", axis.title = element_text(size=15)), 
                         top=text_grob("      Baseline Prediction", face="bold", size=22), 
                         left=text_grob("Random Forest", face="bold", size=22, rot=90)),
             arrangeGrob(p_rf_nihss+theme(legend.position = "hidden", axis.title = element_text(size=15)),
                         top=text_grob("      + TICI + NIHSS 24h", face="bold", size=22), 
                         left=text_grob("", size=5)),
             arrangeGrob(p_lasso_bl+theme(legend.position = "hidden", axis.title = element_text(size=15)), 
                         top=text_grob("", size=5),
                         left=text_grob("Logistic Regression", face="bold", size=22, rot=90)),
             arrangeGrob(p_lasso_nihss+theme(legend.position = "hidden", axis.title = element_text(size=15)),
                         top=text_grob("", size=5),
                         left=text_grob("", size=5)),
             arrangeGrob(p_nn_bl+theme(legend.position = "hidden", axis.title = element_text(size=15)),
                         top=text_grob("", size=5),
                         left=text_grob("Neural Network", face="bold", size=22, rot=90)),
             arrangeGrob(p_nn_nihss+theme(legend.position = "hidden", axis.title = element_text(size=15)),
                         top=text_grob("", size=5),
                         left=text_grob("", size=5)),
             bottom=legend$grobs[[1]],
             nrow=3)
dev.off()



# Model comparison: Brier Score ------------------------------------------------

brier_score = function(true, pred){
  # true =  as.numeric(as.character(dat$mrs_3months_binary))
  pred =  as.numeric(pred)
  bs = (true - pred)^2
  avg_bs = mean(bs)
  return(list(bs = bs, avg_bs = avg_bs))
}

# Get the data
get_plot_data = function(dat, measure_name, model_name, selected){
  
  dat$mrs_3months_binary = as.numeric(as.character(dat$mrs_3months_binary))
  if("mri_sv" %in% colnames(dat)){
    dat_mri_sv = brier_score(dat$mrs_3months_binary, dat$mri_sv)
    dat_mri = brier_score(dat$mrs_3months_binary, dat$mri)
    dat_bl = brier_score(dat$mrs_3months_binary, dat$bl)
    dat_bl_mri_sv = brier_score(dat$mrs_3months_binary, dat$bl_mri_sv)
    dat_bl_mri = brier_score(dat$mrs_3months_binary, dat$bl_mri)
    dat_expert = brier_score(dat$mrs_3months_binary, dat$expert)
    
    dat_plot = data.frame(acc = c(dat_mri_sv$avg_bs, dat_mri$avg_bs,
                                  dat_bl$avg_bs, dat_bl_mri_sv$avg_bs, 
                                  dat_bl_mri$avg_bs, dat_expert$avg_bs),
                          measure=measure_name,
                          stat_model=model_name,
                          model = c("MRI(S)","MRI","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
  } else{
    dat_bl = brier_score(dat$mrs_3months_binary ,dat$bl)
    dat_bl_mri_sv = brier_score(dat$mrs_3months_binary ,dat$bl_mri_sv)
    dat_bl_mri = brier_score(dat$mrs_3months_binary ,dat$bl_mri)
    dat_expert = brier_score(dat$mrs_3months_binary ,dat$expert)
    
    dat_plot = data.frame(acc = c(dat_bl$avg_bs, dat_bl_mri_sv$avg_bs, 
                                  dat_bl_mri$avg_bs, dat_expert$avg_bs),
                          measure=measure_name,
                          stat_model=model_name,
                          model = c("PtC","PtC+MRI(S)","PtC+MRI","Expert"))
  }
  return(dat_plot)
}



# BL: Random Forest
load(paste0(dir,"data/CV/test_pred_rf_BL_rbf.R"))
rf_bl_rbf = get_plot_data(rf_bl, "rCBF", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_rbv.R"))
rf_bl_rbv = get_plot_data(rf_bl, "rCBV", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_tmax.R"))
rf_bl_tmax = get_plot_data(rf_bl, "TMAX", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_mtt.R"))
rf_bl_mtt = get_plot_data(rf_bl, "MTT", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_tmip.R"))
rf_bl_tmip = get_plot_data(rf_bl, "tMIP", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_ttp.R"))
rf_bl_ttp = get_plot_data(rf_bl, "TTP", "rf")
load(paste0(dir,"data/CV/test_pred_rf_BL_all.R"))
rf_bl_all = get_plot_data(rf_bl, "All", "rf")
rf_bl = rbind(rf_bl_rbf, rf_bl_rbv, rf_bl_tmax, rf_bl_mtt, rf_bl_tmip, rf_bl_ttp, rf_bl_all)

# BL: LASSO
load(paste0(dir,"data/CV/test_pred_lasso_BL_rbf.R"))
lasso_bl_rbf = get_plot_data(lasso_bl, "rCBF", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_rbv.R"))
lasso_bl_rbv = get_plot_data(lasso_bl, "rCBV", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_tmax.R"))
lasso_bl_tmax = get_plot_data(lasso_bl, "TMAX", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_mtt.R"))
lasso_bl_mtt = get_plot_data(lasso_bl, "MTT", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_tmip.R"))
lasso_bl_tmip = get_plot_data(lasso_bl, "tMIP", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_ttp.R"))
lasso_bl_ttp = get_plot_data(lasso_bl, "TTP", "lasso")
load(paste0(dir,"data/CV/test_pred_lasso_BL_all.R"))
lasso_bl_all = get_plot_data(lasso_bl, "All", "lasso")
lasso_bl = rbind(lasso_bl_rbf, lasso_bl_rbv, lasso_bl_tmax, lasso_bl_mtt, lasso_bl_tmip, lasso_bl_ttp, lasso_bl_all)

# BL: Neural Network
load(paste0(dir,"data/CV/test_pred_nn_BL_rbf.R"))
nn_bl_rbf = get_plot_data(nn_bl, "rCBF", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_rbv.R"))
nn_bl_rbv = get_plot_data(nn_bl, "rCBV", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_tmax.R"))
nn_bl_tmax = get_plot_data(nn_bl, "TMAX", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_mtt.R"))
nn_bl_mtt = get_plot_data(nn_bl, "MTT", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_tmip.R"))
nn_bl_tmip = get_plot_data(nn_bl, "tMIP", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_ttp.R"))
nn_bl_ttp = get_plot_data(nn_bl, "TTP", "nn")
load(paste0(dir,"data/CV/test_pred_nn_BL_all.R"))
nn_bl_all = get_plot_data(nn_bl, "All", "nn")
nn_bl = rbind(nn_bl_rbf, nn_bl_rbv, nn_bl_tmax, nn_bl_mtt, nn_bl_tmip, nn_bl_ttp, nn_bl_all)

# Create a dataframe
dat_bl = rbind(rf_bl, lasso_bl, nn_bl)
dat_bl$model = factor(dat_bl$model, levels=c("MRI(S)","MRI","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
# levels(dat_bl$model)

# for the baseline model we consider only model rCBV
k = which(dat_bl$model=="PtC" & dat_bl$measure %in% c("rCBF","TTP","TMAX","MTT","tMIP","All"))
dat_bl = dat_bl[-k,]
dat_bl$measure = as.character(dat_bl$measure)
dat_bl[dat_bl$model=="PtC","measure"] = "None"
dat_bl$measure = as.factor(dat_bl$measure)




# BL + TICI + NIHSS: Random Forest
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_rbf.R"))
rf_nihss_rbf = get_plot_data(rf_nihss, "rCBF", "rf")
rf_nihss_rbf = rbind(rf_bl_rbf[rf_bl_rbf$model %in% c("MRI(S)","MRI"),], rf_nihss_rbf)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_rbv.R"))
rf_nihss_rbv = get_plot_data(rf_nihss, "rCBV", "rf")
rf_nihss_rbv = rbind(rf_bl_rbv[rf_bl_rbv$model %in% c("MRI(S)","MRI"),], rf_nihss_rbv)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_tmax.R"))
rf_nihss_tmax = get_plot_data(rf_nihss, "TMAX", "rf")
rf_nihss_tmax = rbind(rf_bl_tmax[rf_bl_tmax$model %in% c("MRI(S)","MRI"),], rf_nihss_tmax)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_mtt.R"))
rf_nihss_mtt = get_plot_data(rf_nihss, "MTT", "rf")
rf_nihss_mtt = rbind(rf_bl_mtt[rf_bl_mtt$model %in% c("MRI(S)","MRI"),], rf_nihss_mtt)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_tmip.R"))
rf_nihss_tmip = get_plot_data(rf_nihss, "tMIP", "rf")
rf_nihss_tmip = rbind(rf_bl_tmip[rf_bl_tmip$model %in% c("MRI(S)","MRI"),], rf_nihss_tmip)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_ttp.R"))
rf_nihss_ttp = get_plot_data(rf_nihss, "TTP", "rf")
rf_nihss_ttp = rbind(rf_bl_ttp[rf_bl_ttp$model %in% c("MRI(S)","MRI"),], rf_nihss_ttp)
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_all.R"))
rf_nihss_all = get_plot_data(rf_nihss, "All", "rf")
rf_nihss_all = rbind(rf_bl_all[rf_bl_all$model %in% c("MRI(S)","MRI"),], rf_nihss_all)
rf_nihss = rbind(rf_nihss_rbf, rf_nihss_rbv, rf_nihss_tmax, rf_nihss_mtt, rf_nihss_tmip, rf_nihss_ttp, rf_nihss_all)

# BL + TICI + NIHSS: LASSO
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_rbf.R"))
lasso_nihss_rbf = get_plot_data(lasso_nihss, "rCBF", "lasso")
lasso_nihss_rbf = rbind(lasso_bl_rbf[lasso_bl_rbf$model %in% c("MRI(S)","MRI"),], lasso_nihss_rbf)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_rbv.R"))
lasso_nihss_rbv = get_plot_data(lasso_nihss, "rCBV", "lasso")
lasso_nihss_rbv = rbind(lasso_bl_rbv[lasso_bl_rbv$model %in% c("MRI(S)","MRI"),], lasso_nihss_rbv)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_tmax.R"))
lasso_nihss_tmax = get_plot_data(lasso_nihss, "TMAX", "lasso")
lasso_nihss_tmax = rbind(lasso_bl_tmax[lasso_bl_tmax$model %in% c("MRI(S)","MRI"),], lasso_nihss_tmax)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_mtt.R"))
lasso_nihss_mtt = get_plot_data(lasso_nihss, "MTT", "lasso")
lasso_nihss_mtt = rbind(lasso_bl_mtt[lasso_bl_mtt$model %in% c("MRI(S)","MRI"),], lasso_nihss_mtt)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_tmip.R"))
lasso_nihss_tmip = get_plot_data(lasso_nihss, "tMIP", "lasso")
lasso_nihss_tmip = rbind(lasso_bl_tmip[lasso_bl_tmip$model %in% c("MRI(S)","MRI"),], lasso_nihss_tmip)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_ttp.R"))
lasso_nihss_ttp = get_plot_data(lasso_nihss, "TTP", "lasso")
lasso_nihss_ttp = rbind(lasso_bl_ttp[lasso_bl_ttp$model %in% c("MRI(S)","MRI"),], lasso_nihss_ttp)
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_all.R"))
lasso_nihss_all = get_plot_data(lasso_nihss, "All", "lasso")
lasso_nihss_all = rbind(lasso_bl_all[lasso_bl_all$model %in% c("MRI(S)","MRI"),], lasso_nihss_all)
lasso_nihss = rbind(lasso_nihss_rbf, lasso_nihss_rbv, lasso_nihss_tmax, lasso_nihss_mtt, lasso_nihss_tmip, lasso_nihss_ttp, lasso_nihss_all)

# BL + TICI + NIHSS: Neural Network
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_rbf.R"))
nn_nihss_rbf = get_plot_data(nn_nihss, "rCBF", "nn")
nn_nihss_rbf = rbind(nn_bl_rbf[nn_bl_rbf$model %in% c("MRI(S)","MRI"),], nn_nihss_rbf)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_rbv.R"))
nn_nihss_rbv = get_plot_data(nn_nihss, "rCBV", "nn")
nn_nihss_rbv = rbind(nn_bl_rbv[nn_bl_rbv$model %in% c("MRI(S)","MRI"),], nn_nihss_rbv)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_tmax.R"))
nn_nihss_tmax = get_plot_data(nn_nihss, "TMAX", "nn")
nn_nihss_tmax = rbind(nn_bl_tmax[nn_bl_tmax$model %in% c("MRI(S)","MRI"),], nn_nihss_tmax)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_mtt.R"))
nn_nihss_mtt = get_plot_data(nn_nihss, "MTT", "nn")
nn_nihss_mtt = rbind(nn_bl_mtt[nn_bl_mtt$model %in% c("MRI(S)","MRI"),], nn_nihss_mtt)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_tmip.R"))
nn_nihss_tmip = get_plot_data(nn_nihss, "tMIP", "nn")
nn_nihss_tmip = rbind(nn_bl_tmip[nn_bl_tmip$model %in% c("MRI(S)","MRI"),], nn_nihss_tmip)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_ttp.R"))
nn_nihss_ttp = get_plot_data(nn_nihss, "TTP", "nn")
nn_nihss_ttp = rbind(nn_bl_ttp[nn_bl_ttp$model %in% c("MRI(S)","MRI"),], nn_nihss_ttp)
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_all.R"))
nn_nihss_all = get_plot_data(nn_nihss, "All", "nn")
nn_nihss_all = rbind(nn_bl_all[nn_bl_all$model %in% c("MRI(S)","MRI"),], nn_nihss_all)
nn_nihss = rbind(nn_nihss_rbf, nn_nihss_rbv, nn_nihss_tmax, nn_nihss_mtt, nn_nihss_tmip, nn_nihss_ttp, nn_nihss_all)

# Create a dataframe
dat_nihss = rbind(rf_nihss, lasso_nihss, nn_nihss)
dat_nihss$model = factor(dat_nihss$model, levels=c("MRI(S)","MRI","PtC","PtC+MRI(S)","PtC+MRI","Expert"))
levels(dat_nihss$model)

# for the baseline model we consider only model rCBV
k = which(dat_nihss$model=="PtC" & dat_nihss$measure %in% c("rCBF","TTP","TMAX","MTT","tMIP","All"))
dat_nihss = dat_nihss[-k,]
dat_nihss$measure = as.character(dat_nihss$measure)
dat_nihss[dat_nihss$model=="PtC","measure"] = "None"
dat_nihss$measure = as.factor(dat_nihss$measure)

dat_bl$measure = factor(dat_bl$measure, levels = c("rCBV","rCBF","TTP","TMAX","MTT","tMIP","All","None"))
dat_nihss$measure = factor(dat_nihss$measure, levels = c("rCBV","rCBF","TTP","TMAX","MTT","tMIP","All","None"))


# Plot
pd = position_dodge(0.3) # move them .05 to the left and right

dat0 = dat_bl[dat_bl$stat_model=="rf",]
p_rf_bl = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  # geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd, size = 3) +
  ylab("Brier Score") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.15, 0.3) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))

dat0 = dat_bl[dat_bl$stat_model=="lasso",]
p_lasso_bl = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  # geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd, size = 3) +
  ylab("Brier Score") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.15, 0.3) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))

dat0 = dat_bl[dat_bl$stat_model=="nn",]
p_nn_bl = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  # geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd, size = 3) +
  ylab("Brier Score") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.15, 0.3) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))



dat0 = dat_nihss[dat_nihss$stat_model=="rf",]
p_rf_nihss = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  # geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd, size = 3) +
  ylab("Brier Score") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.15,0.3) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))


dat0 = dat_nihss[dat_nihss$stat_model=="lasso",]
p_lasso_nihss = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  # geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd, size = 3) +
  ylab("Brier Score") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.15,0.3) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))


dat0 = dat_nihss[dat_nihss$stat_model=="nn",]
p_nn_nihss = ggplot(dat0, aes(x=model, y=acc, colour=measure)) + 
  # geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pd) +
  geom_point(aes(shape=measure, colour=measure), position=pd, size = 3) +
  ylab("Brier Score") +
  xlab("Predictor set") +
  # ggtitle("Random Forest") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(), #) +
        axis.text.x = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle=0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 16),
        legend.title = element_text(color = "black", size = 16)) +
  ylim(0.15,0.3) +
  scale_colour_manual(values=c(rCBF="darkseagreen", rCBV="dodgerblue4", 
                               TTP="darkred",TMAX="darkorchid4",
                               MTT="darksalmon",tMIP="mediumorchid2",
                               All="deepskyblue3", None="black"),
                      name="Perfusion Parameter") +
  scale_shape_manual(values=c(1,2,8,4,5,6,7,9),
                     name="Perfusion Parameter") +
  guides(colour = guide_legend(nrow = 1))


library(ggpubr)
legend = g_legend(p_rf_bl + theme(legend.position='bottom'))
pdf(paste0(dir,"for publication/Measure_comparison_Brier_BL_TICI_NIHSS_Revision.pdf"), width=20, height=15)
# postscript(paste0(dir,"for publication/Measure_comparison_AUC.eps"))
grid.arrange(arrangeGrob(p_rf_bl+theme(legend.position = "hidden", axis.title = element_text(size=15)), 
                         top=text_grob("      Baseline Prediction", face="bold", size=22), 
                         left=text_grob("Random Forest", face="bold", size=22, rot=90)),
             arrangeGrob(p_rf_nihss+theme(legend.position = "hidden", axis.title = element_text(size=15)),
                         top=text_grob("      + TICI + NIHSS 24h", face="bold", size=22), 
                         left=text_grob("", size=5)),
             arrangeGrob(p_lasso_bl+theme(legend.position = "hidden", axis.title = element_text(size=15)), 
                         top=text_grob("", size=5),
                         left=text_grob("Logistic Regression", face="bold", size=22, rot=90)),
             arrangeGrob(p_lasso_nihss+theme(legend.position = "hidden", axis.title = element_text(size=15)),
                         top=text_grob("", size=5),
                         left=text_grob("", size=5)),
             arrangeGrob(p_nn_bl+theme(legend.position = "hidden", axis.title = element_text(size=15)),
                         top=text_grob("", size=5),
                         left=text_grob("Neural Network", face="bold", size=22, rot=90)),
             arrangeGrob(p_nn_nihss+theme(legend.position = "hidden", axis.title = element_text(size=15)),
                         top=text_grob("", size=5),
                         left=text_grob("", size=5)),
             bottom=legend$grobs[[1]],
             nrow=3)
dev.off()





# ROC curves -------------------------------------------------------------------

# Show only the results for all

# Load THRIVE results
dat0 = read.xlsx(paste0(dir,"data/Data_THRIVE_20190312.xlsx"), sheet=1, na.strings="NA")
head(dat0)
load(paste0(dir,"/data/data_rbv_wide_nihss.R"))
thrive = merge(dat0, dat, by.x="PID", by.y="p_id")

# for the baseline model
load(paste0(dir,"data/CV/test_pred_rf_BL_rbv.R"))
rf_bl_rbv = rf_bl
load(paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_rbv.R"))
rf_nihss_rbv = rf_nihss
load(paste0(dir,"data/CV/test_pred_lasso_BL_rbv.R"))
lasso_bl_rbv = lasso_bl
load(paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_rbv.R"))
lasso_nihss_rbv = lasso_nihss
load(paste0(dir,"data/CV/test_pred_nn_BL_rbv.R"))
nn_bl_rbv = nn_bl
load(paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_rbv.R"))
nn_nihss_rbv = nn_nihss

load(file=paste0(dir,"data/CV/test_pred_rf_BL_all.R"))
load(file=paste0(dir,"data/CV/test_pred_rf_BL_TICI_NIHSS_all.R"))
load(file=paste0(dir,"data/CV/test_pred_lasso_BL_all.R"))
load(file=paste0(dir,"data/CV/test_pred_lasso_BL_TICI_NIHSS_all.R"))
load(file=paste0(dir,"data/CV/test_pred_nn_BL_all.R"))
load(file=paste0(dir,"data/CV/test_pred_nn_BL_TICI_NIHSS_all.R"))

# mrs 1 = good outcome
thrive_mri = roc_data(thrive$mrs_3months_binary, thrive$p_good_outcome)


# Stroke MRI only
rf_bl_mri_sv = roc_data(rf_bl$mrs_3months_binary, rf_bl$mri_sv)
lasso_bl_mri_sv = roc_data(lasso_bl$mrs_3months_binary, lasso_bl$mri_sv)
nn_bl_mri_sv = roc_data(nn_bl$mrs_3months_binary, nn_bl$mri_sv)

# MRI only
rf_bl_mri = roc_data(rf_bl$mrs_3months_binary, rf_bl$mri)
lasso_bl_mri = roc_data(lasso_bl$mrs_3months_binary, lasso_bl$mri)
nn_bl_mri = roc_data(nn_bl$mrs_3months_binary, nn_bl$mri)

# Patient data: consider always the results from rbv
rf_bl_bl = roc_data(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$bl)
rf_nihss_bl = roc_data(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$bl)

lasso_bl_bl = roc_data(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$bl)
lasso_nihss_bl = roc_data(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$bl)

nn_bl_bl = roc_data(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$bl)
nn_nihss_bl = roc_data(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$bl)


# Patient + stroke MRI
rf_bl_bl_mri_sv = roc_data(rf_bl$mrs_3months_binary, rf_bl$bl_mri_sv)
rf_nihss_bl_mri_sv = roc_data(rf_nihss$mrs_3months_binary, rf_nihss$bl_mri_sv)

lasso_bl_bl_mri_sv = roc_data(lasso_bl$mrs_3months_binary, lasso_bl$bl_mri_sv)
lasso_nihss_bl_mri_sv = roc_data(lasso_nihss$mrs_3months_binary, lasso_nihss$bl_mri_sv)

nn_bl_bl_mri_sv = roc_data(nn_bl$mrs_3months_binary, nn_bl$bl_mri_sv)
nn_nihss_bl_mri_sv = roc_data(nn_nihss$mrs_3months_binary, nn_nihss$bl_mri_sv)


# Patient + MRI
rf_bl_bl_mri = roc_data(rf_bl$mrs_3months_binary, rf_bl$bl_mri)
rf_nihss_bl_mri = roc_data(rf_nihss$mrs_3months_binary, rf_nihss$bl_mri)

lasso_bl_bl_mri = roc_data(lasso_bl$mrs_3months_binary, lasso_bl$bl_mri)
lasso_nihss_bl_mri = roc_data(lasso_nihss$mrs_3months_binary, lasso_nihss$bl_mri)

nn_bl_bl_mri = roc_data(nn_bl$mrs_3months_binary, nn_bl$bl_mri)
nn_nihss_bl_mri = roc_data(nn_nihss$mrs_3months_binary, nn_nihss$bl_mri)


# Expert
rf_bl_expert = roc_data(rf_bl$mrs_3months_binary, rf_bl$expert)
rf_nihss_expert = roc_data(rf_nihss$mrs_3months_binary, rf_nihss$expert)

lasso_bl_expert = roc_data(lasso_bl$mrs_3months_binary, lasso_bl$expert)
lasso_nihss_expert = roc_data(lasso_nihss$mrs_3months_binary, lasso_nihss$expert)

nn_bl_expert = roc_data(nn_bl$mrs_3months_binary, nn_bl$expert)
nn_nihss_expert = roc_data(nn_nihss$mrs_3months_binary, nn_nihss$expert)



# Plot the ROC curves
pdf(paste0(dir,"for publication/ROC_RF_all_BL_Revision.pdf"))
#postscript(paste0(dir,"for publication/ROC_rbv_BL.eps"))
#par(mfrow=c(1,1), pty="s", mar=c(0,4.15,4.15,0))
par(mfrow = c(1,1))
plot(1-thrive_mri$roc_curve$spec, thrive_mri$roc_curve$sens, type="l", col="black",
     xlab="1-Specificity", ylab="Sensitivity", main="Baseline Prediction", lwd=1.8,
     cex.lab=1.5, cex.axis=1.3, cex.main=1.5, cex.sub=1.3, las=1)
abline(a=0,b=1, col="black", lty=2)

lines(1-rf_bl_mri_sv$roc_curve$spec, rf_bl_mri_sv$roc_curve$sens, col="darkseagreen", lwd=1.8)
lines(1-rf_bl_mri$roc_curve$spec, rf_bl_mri$roc_curve$sens, col="dodgerblue4", lwd=1.8)
lines(1-rf_bl_bl$roc_curve$spec, rf_bl_bl$roc_curve$sens, col="darkred", lwd=1.8)
lines(1-rf_bl_bl_mri_sv$roc_curve$spec, rf_bl_bl_mri_sv$roc_curve$sens, col="darkorchid4", lwd=1.8)
lines(1-rf_bl_bl_mri$roc_curve$spec, rf_bl_bl_mri$roc_curve$sens, col="darksalmon", lwd=1.8)
lines(1-rf_bl_expert$roc_curve$spec, rf_bl_expert$roc_curve$sens, col="mediumorchid2", lwd=1.8)

legend(x=0.4, y=0.35, legend = c(paste0("THRIVE: ",
                                        paste0(round(thrive_mri$auc["auc"],3)," [",
                                               round(thrive_mri$auc["lower_ci"],3), ",",
                                               round(thrive_mri$auc["upper_ci"],3), "]")),
                                 paste0("MRI(S): ",
                                        paste0(round(rf_bl_mri_sv$auc["auc"],3)," [",
                                               round(rf_bl_mri_sv$auc["lower_ci"],3), ",",
                                               round(rf_bl_mri_sv$auc["upper_ci"],3), "]")),
                                 paste0("MRI: ",
                                        paste0(round(rf_bl_mri$auc["auc"],3)," [",
                                               round(rf_bl_mri$auc["lower_ci"],3), ",",
                                               round(rf_bl_mri$auc["upper_ci"],3), "]")),
                                 paste0("PtC: ",
                                        paste0(round(rf_bl_bl$auc["auc"],3)," [",
                                               round(rf_bl_bl$auc["lower_ci"],3), ",",
                                               round(rf_bl_bl$auc["upper_ci"],3), "]")),
                                 paste0("PtC+MRI(S): ",
                                        paste0(round(rf_bl_bl_mri_sv$auc["auc"],3)," [",
                                               round(rf_bl_bl_mri_sv$auc["lower_ci"],3), ",",
                                               round(rf_bl_bl_mri_sv$auc["upper_ci"],3), "]")),
                                 paste0("PtC+MRI: ",
                                        paste0(round(rf_bl_bl_mri$auc["auc"],3)," [",
                                               round(rf_bl_bl_mri$auc["lower_ci"],3), ",",
                                               round(rf_bl_bl_mri$auc["upper_ci"],3), "]")),
                                 paste0("Expert: ",
                                        paste0(round(rf_bl_expert$auc["auc"],3)," [",
                                               round(rf_bl_expert$auc["lower_ci"],3), ",",
                                               round(rf_bl_expert$auc["upper_ci"],3), "]"))),
       col=c("black","darkseagreen","dodgerblue4","darkred","darkorchid4","darksalmon","mediumorchid2"),
       lwd=1.5,
       bty = "n",
       cex=1.15,
       seg.len=1)

dev.off()


# Plot the ROC curves
pdf(paste0(dir,"for publication/ROC_RF_all_NIHSS_Revision.pdf"))
#postscript(paste0(dir,"for publication/ROC_rbv_BL.eps"))
#par(mfrow=c(1,1), pty="s", mar=c(0,4.15,4.15,0))
par(mfrow = c(1,1))
plot(1-rf_bl_mri_sv$roc_curve$spec, rf_bl_mri_sv$roc_curve$sens, type="l", col="darkseagreen",
     xlab="1-Specificity", ylab="", main="+ TICI + NIHSS 24h", lwd=1.8,
     cex.lab=1.5, cex.axis=1.3, cex.main=1.5, cex.sub=1.3, las=1, yaxt="n")
axis(side=2, labels=F)
abline(a=0,b=1, col="black", lty=2)

lines(1-rf_bl_mri$roc_curve$spec, rf_bl_mri$roc_curve$sens, col="dodgerblue4", lwd=1.8)
lines(1-rf_nihss_bl$roc_curve$spec, rf_nihss_bl$roc_curve$sens, col="darkred", lwd=1.8)
lines(1-rf_nihss_bl_mri_sv$roc_curve$spec, rf_nihss_bl_mri_sv$roc_curve$sens, col="darkorchid4", lwd=1.8)
lines(1-rf_nihss_bl_mri$roc_curve$spec, rf_nihss_bl_mri$roc_curve$sens, col="darksalmon", lwd=1.8)
lines(1-rf_nihss_expert$roc_curve$spec, rf_nihss_expert$roc_curve$sens, col="mediumorchid2", lwd=1.8)

legend(x=0.4, y=0.295, legend = c(paste0("MRI(S): ",
                                         paste0(round(rf_bl_mri_sv$auc["auc"],3)," [",
                                                round(rf_bl_mri_sv$auc["lower_ci"],3), ",",
                                                round(rf_bl_mri_sv$auc["upper_ci"],3), "]")),
                                  paste0("MRI: ",
                                         paste0(round(rf_bl_mri$auc["auc"],3)," [",
                                                round(rf_bl_mri$auc["lower_ci"],3), ",",
                                                round(rf_bl_mri$auc["upper_ci"],3), "]")),
                                  paste0("PtC: ",
                                         paste0(round(rf_nihss_bl$auc["auc"],3)," [",
                                                round(rf_nihss_bl$auc["lower_ci"],3), ",",
                                                round(rf_nihss_bl$auc["upper_ci"],3), "]")),
                                  paste0("PtC+MRI(S): ",
                                         paste0(round(rf_nihss_bl_mri_sv$auc["auc"],3)," [",
                                                round(rf_nihss_bl_mri_sv$auc["lower_ci"],3), ",",
                                                round(rf_nihss_bl_mri_sv$auc["upper_ci"],3), "]")),
                                  paste0("PtC+MRI: ",
                                         paste0(round(rf_nihss_bl_mri$auc["auc"],3)," [",
                                                round(rf_nihss_bl_mri$auc["lower_ci"],3), ",",
                                                round(rf_nihss_bl_mri$auc["upper_ci"],3), "]")),
                                  paste0("Expert: ",
                                         paste0(round(rf_nihss_expert$auc["auc"],3)," [",
                                                round(rf_nihss_expert$auc["lower_ci"],3), ",",
                                                round(rf_nihss_expert$auc["upper_ci"],3), "]"))),
       col=c("darkseagreen","dodgerblue4","darkred","darkorchid4","darksalmon","mediumorchid2"),
       lwd=1.5,
       bty = "n",
       cex=1.15,
       seg.len=1)

dev.off()




pdf(paste0(dir,"for publication/ROC_all_BL_and_BL_TICI_NIHSS_Revision.pdf"), width = 13.5, height = 7)
par(mfrow=c(1,2))
plot(1-thrive_mri$roc_curve$spec, thrive_mri$roc_curve$sens, type="l", col="black",
     xlab="1-Specificity", ylab="Sensitivity", main="Baseline Prediction", lwd=1.5,
     cex.lab=1.5, cex.axis=1.3, cex.main=1.5, cex.sub=1.3, las=1)
abline(a=0,b=1, col="black", lty=2)

lines(1-rf_bl_mri_sv$roc_curve$spec, rf_bl_mri_sv$roc_curve$sens, col="darkseagreen", lwd=1.5)
lines(1-rf_bl_mri$roc_curve$spec, rf_bl_mri$roc_curve$sens, col="dodgerblue4", lwd=1.5)
lines(1-rf_bl_bl$roc_curve$spec, rf_bl_bl$roc_curve$sens, col="darkred", lwd=1.5)
lines(1-rf_bl_bl_mri_sv$roc_curve$spec, rf_bl_bl_mri_sv$roc_curve$sens, col="darkorchid4", lwd=1.5)
lines(1-rf_bl_bl_mri$roc_curve$spec, rf_bl_bl_mri$roc_curve$sens, col="darksalmon", lwd=1.5)
lines(1-rf_bl_expert$roc_curve$spec, rf_bl_expert$roc_curve$sens, col="mediumorchid2", lwd=1.5)

legend(x=0.35, y=0.35, legend = c(paste0("THRIVE: ",
                                        paste0(round(thrive_mri$auc["auc"],3)," [",
                                               round(thrive_mri$auc["lower_ci"],3), ",",
                                               round(thrive_mri$auc["upper_ci"],3), "]")),
                                 paste0("MRI(S): ",
                                        paste0(round(rf_bl_mri_sv$auc["auc"],3)," [",
                                               round(rf_bl_mri_sv$auc["lower_ci"],3), ",",
                                               round(rf_bl_mri_sv$auc["upper_ci"],3), "]")),
                                 paste0("MRI: ",
                                        paste0(round(rf_bl_mri$auc["auc"],3)," [",
                                               round(rf_bl_mri$auc["lower_ci"],3), ",",
                                               round(rf_bl_mri$auc["upper_ci"],3), "]")),
                                 paste0("PtC: ",
                                        paste0(round(rf_bl_bl$auc["auc"],3)," [",
                                               round(rf_bl_bl$auc["lower_ci"],3), ",",
                                               round(rf_bl_bl$auc["upper_ci"],3), "]")),
                                 paste0("PtC+MRI(S): ",
                                        paste0(round(rf_bl_bl_mri_sv$auc["auc"],3)," [",
                                               round(rf_bl_bl_mri_sv$auc["lower_ci"],3), ",",
                                               round(rf_bl_bl_mri_sv$auc["upper_ci"],3), "]")),
                                 paste0("PtC+MRI: ",
                                        paste0(round(rf_bl_bl_mri$auc["auc"],3)," [",
                                               round(rf_bl_bl_mri$auc["lower_ci"],3), ",",
                                               round(rf_bl_bl_mri$auc["upper_ci"],3), "]")),
                                 paste0("Expert: ",
                                        paste0(round(rf_bl_expert$auc["auc"],3)," [",
                                               round(rf_bl_expert$auc["lower_ci"],3), ",",
                                               round(rf_bl_expert$auc["upper_ci"],3), "]"))),
       col=c("black","darkseagreen","dodgerblue4","darkred","darkorchid4","darksalmon","mediumorchid2"),
       lwd=1.5,
       bty = "n",
       cex=1.15,
       seg.len=1)


plot(1-rf_bl_mri_sv$roc_curve$spec, rf_bl_mri_sv$roc_curve$sens, type="l", col="darkseagreen",
     xlab="1-Specificity", ylab="", main="+ TICI + NIHSS 24h", lwd=1.5,
     cex.lab=1.5, cex.axis=1.3, cex.main=1.5, cex.sub=1.3, las=1, yaxt="n")
axis(side=2, labels=F)
abline(a=0,b=1, col="black", lty=2)

lines(1-rf_bl_mri$roc_curve$spec, rf_bl_mri$roc_curve$sens, col="dodgerblue4", lwd=1.5)
lines(1-rf_nihss_bl$roc_curve$spec, rf_nihss_bl$roc_curve$sens, col="darkred", lwd=1.5)
lines(1-rf_nihss_bl_mri_sv$roc_curve$spec, rf_nihss_bl_mri_sv$roc_curve$sens, col="darkorchid4", lwd=1.5)
lines(1-rf_nihss_bl_mri$roc_curve$spec, rf_nihss_bl_mri$roc_curve$sens, col="darksalmon", lwd=1.5)
lines(1-rf_nihss_expert$roc_curve$spec, rf_nihss_expert$roc_curve$sens, col="mediumorchid2", lwd=1.5)

legend(x=0.35, y=0.295, legend = c(paste0("MRI(S): ",
                                         paste0(round(rf_bl_mri_sv$auc["auc"],3)," [",
                                                round(rf_bl_mri_sv$auc["lower_ci"],3), ",",
                                                round(rf_bl_mri_sv$auc["upper_ci"],3), "]")),
                                  paste0("MRI: ",
                                         paste0(round(rf_bl_mri$auc["auc"],3)," [",
                                                round(rf_bl_mri$auc["lower_ci"],3), ",",
                                                round(rf_bl_mri$auc["upper_ci"],3), "]")),
                                  paste0("PtC: ",
                                         paste0(round(rf_nihss_bl$auc["auc"],3)," [",
                                                round(rf_nihss_bl$auc["lower_ci"],3), ",",
                                                round(rf_nihss_bl$auc["upper_ci"],3), "]")),
                                  paste0("PtC+MRI(S): ",
                                         paste0(round(rf_nihss_bl_mri_sv$auc["auc"],3)," [",
                                                round(rf_nihss_bl_mri_sv$auc["lower_ci"],3), ",",
                                                round(rf_nihss_bl_mri_sv$auc["upper_ci"],3), "]")),
                                  paste0("PtC+MRI: ",
                                         paste0(round(rf_nihss_bl_mri$auc["auc"],3)," [",
                                                round(rf_nihss_bl_mri$auc["lower_ci"],3), ",",
                                                round(rf_nihss_bl_mri$auc["upper_ci"],3), "]")),
                                  paste0("Expert: ",
                                         paste0(round(rf_nihss_expert$auc["auc"],3)," [",
                                                round(rf_nihss_expert$auc["lower_ci"],3), ",",
                                                round(rf_nihss_expert$auc["upper_ci"],3), "]"))),
       col=c("darkseagreen","dodgerblue4","darkred","darkorchid4","darksalmon","mediumorchid2"),
       lwd=1.5,
       bty = "n",
       cex=1.15,
       seg.len=1)

dev.off()





# Brier score ------------------------------------------------------------------

brier_score = function(true, pred){
  true =  as.numeric(as.character(true))
  pred =  as.numeric(pred)
  bs = (true - pred)^2
  avg_bs = mean(bs)
  return(list(bs = bs, avg_bs = avg_bs))
}

# THRIVE
k = which(is.na(thrive$p_good_outcome))
thrive_brier = brier_score(thrive$mrs_3months_binary[-k], thrive$p_good_outcome[-k])

# Stroke MRI only
rf_bl_mri_sv_brier = brier_score(rf_bl$mrs_3months_binary, rf_bl$mri_sv)
lasso_bl_mri_sv_brier = brier_score(lasso_bl$mrs_3months_binary, lasso_bl$mri_sv)
nn_bl_mri_sv_brier = brier_score(nn_bl$mrs_3months_binary, nn_bl$mri_sv)

# MRI only
rf_bl_mri_brier = brier_score(rf_bl$mrs_3months_binary, rf_bl$mri)
lasso_bl_mri_brier = brier_score(lasso_bl$mrs_3months_binary, lasso_bl$mri)
nn_bl_mri_brier = brier_score(nn_bl$mrs_3months_binary, nn_bl$mri)

# Baseline
rf_bl_bl_brier = brier_score(rf_bl_rbv$mrs_3months_binary, rf_bl_rbv$bl)
rf_nihss_bl_brier = brier_score(rf_nihss_rbv$mrs_3months_binary, rf_nihss_rbv$bl)

lasso_bl_bl_brier = brier_score(lasso_bl_rbv$mrs_3months_binary, lasso_bl_rbv$bl)
lasso_nihss_bl_brier = brier_score(lasso_nihss_rbv$mrs_3months_binary, lasso_nihss_rbv$bl)

nn_bl_bl_brier = brier_score(nn_bl_rbv$mrs_3months_binary, nn_bl_rbv$bl)
nn_nihss_bl_brier = brier_score(nn_nihss_rbv$mrs_3months_binary, nn_nihss_rbv$bl)


# Baseline + stroke MRI
rf_bl_bl_mri_sv_brier = brier_score(rf_bl$mrs_3months_binary, rf_bl$bl_mri_sv)
rf_nihss_bl_mri_sv_brier = brier_score(rf_nihss$mrs_3months_binary, rf_nihss$bl_mri_sv)

lasso_bl_bl_mri_sv_brier = brier_score(lasso_bl$mrs_3months_binary, lasso_bl$bl_mri_sv)
lasso_nihss_bl_mri_sv_brier = brier_score(lasso_nihss$mrs_3months_binary, lasso_nihss$bl_mri_sv)

nn_bl_bl_mri_sv_brier = brier_score(nn_bl$mrs_3months_binary, nn_bl$bl_mri_sv)
nn_nihss_bl_mri_sv_brier = brier_score(nn_nihss$mrs_3months_binary, nn_nihss$bl_mri_sv)

# Baseline + MRI
rf_bl_bl_mri_brier = brier_score(rf_bl$mrs_3months_binary, rf_bl$bl_mri)
rf_nihss_bl_mri_brier = brier_score(rf_nihss$mrs_3months_binary, rf_nihss$bl_mri)

lasso_bl_bl_mri_brier = brier_score(lasso_bl$mrs_3months_binary, lasso_bl$bl_mri)
lasso_nihss_bl_mri_brier = brier_score(lasso_nihss$mrs_3months_binary, lasso_nihss$bl_mri)

nn_bl_bl_mri_brier = brier_score(nn_bl$mrs_3months_binary, nn_bl$bl_mri)
nn_nihss_bl_mri_brier = brier_score(nn_nihss$mrs_3months_binary, nn_nihss$bl_mri)


# Expert
rf_bl_expert_brier = brier_score(rf_bl$mrs_3months_binary, rf_bl$expert)
rf_nihss_expert_brier = brier_score(rf_nihss$mrs_3months_binary, rf_nihss$expert)

lasso_bl_expert_brier = brier_score(lasso_bl$mrs_3months_binary, lasso_bl$expert)
lasso_nihss_expert_brier = brier_score(lasso_nihss$mrs_3months_binary, lasso_nihss$expert)

nn_bl_expert_brier = brier_score(nn_bl$mrs_3months_binary, nn_bl$expert)
nn_nihss_expert_brier = brier_score(nn_nihss$mrs_3months_binary, nn_nihss$expert)

n = length(rf_bl_mri_sv_brier$bs)
rf_plot_brier = data.frame(model = c("THRIVE",
                                     "MRI(S)",
                                     "MRI",
                                     "PtC",
                                     "PtC+MRI(S)",
                                     "PtC+MRI",
                                     "Expert"),
                           bs = c(thrive_brier$avg_bs,
                                  rf_bl_mri_sv_brier$avg_bs,
                                  rf_bl_mri_brier$avg_bs,
                                  rf_bl_bl_brier$avg_bs,
                                  rf_bl_bl_mri_sv_brier$avg_bs,
                                  rf_bl_bl_mri_brier$avg_bs,
                                  rf_bl_expert_brier$avg_bs))
rf_plot_brier$model = factor(rf_plot_brier$model, 
                             levels = c("THRIVE","MRI(S)","MRI","PtC","PtC+MRI(S)","PtC+MRI","Expert"))

rf_plot_brier_nihss = data.frame(model = c("THRIVE",
                                     "MRI(S)",
                                     "MRI",
                                     "PtC",
                                     "PtC+MRI(S)",
                                     "PtC+MRI",
                                     "Expert"),
                           bs = c(thrive_brier$avg_bs,
                                  rf_bl_mri_sv_brier$avg_bs,
                                  rf_bl_mri_brier$avg_bs,
                                  rf_nihss_bl_brier$avg_bs,
                                  rf_nihss_bl_mri_sv_brier$avg_bs,
                                  rf_nihss_bl_mri_brier$avg_bs,
                                  rf_nihss_expert_brier$avg_bs))
rf_plot_brier_nihss$model = factor(rf_plot_brier_nihss$model, 
                             levels = c("THRIVE","MRI(S)","MRI","PtC","PtC+MRI(S)","PtC+MRI","Expert"))


pdf(paste0(dir,"for publication/BRIER_all_BL_and_BL_TICI_NIHSS_Revision.pdf"), width = 13.5, height = 7)
par(mfrow=c(1,2))
colors = c("black", "darkseagreen", "dodgerblue4", "darkred", "darkorchid4", "darksalmon", "mediumorchid2")
colors = colors[as.numeric(rf_plot_brier$model)]

plot(1:7, rf_plot_brier$bs, col = colors, type = "p",
     ylim = c(0.15, 0.3), xlab=" ", ylab="Brier Score", main="Baseline Prediction",
     cex.lab=1.5, cex.axis=1.3, cex.main=1.5, cex.sub=1.3, las=1, pch = 16, xaxt = "n")
axis(side = 1, at = c(1:7), labels = FALSE)

text(x = 1:7,
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.01,
     ## Use names from the data list.
     labels = rf_plot_brier$model,
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 40,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.8,
     ## Increase label size.
     cex = 1.3)


plot(1:7, rf_plot_brier_nihss$bs, col = colors,
     ylim = c(0.15, 0.3), xlab=" ", ylab=" ", main="+ TICI + NIHSS 24h",
     cex.lab=1.5, cex.axis=1.3, cex.main=1.5, cex.sub=1.3, las=1, pch = 16, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(1:7), labels = FALSE)
axis(side=2, labels=F, cex.lab=1.5, cex.axis=1.3, cex.main=1.5, cex.sub=1.3)

text(x = 1:7,
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.01,
     ## Use names from the data list.
     labels = rf_plot_brier$model,
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 40,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.8,
     ## Increase label size.
     cex = 1.3)

dev.off()

