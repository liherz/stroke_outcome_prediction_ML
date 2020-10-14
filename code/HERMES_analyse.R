rm(list=ls())
# We exclude mTICI and contralaters because they are measured AFTER therapy

library(MASS)
library(reshape2)
library(lattice)
library(latticeExtra)

# set the path to the files
dir = "C:/Users/hezo/Dropbox/PhD/Stroke/PowerAnalyse_GewichteteMittelwerte_MixedModelSD_und_PredictionModel/"
# dir = "/Users/janne.hamann/Dropbox/StatistikOtherSideProjekt/"
setwd(dir)

# set the name for the variable of interest
var_name = "tmax"

# source the files with the functions we need
source(paste0(dir, "analyses/functions/roc_data.R"))
# source(paste0(dir, "analyses/functions/get_partial_plots.R"))
source(paste0(dir, "analyses/functions/change_var_names.R"))




###################
#### Load data ####
###################

load(paste0(dir, "/data/data.R"))
dat = dat_final

dat0 = dat

# change the names to identify each measure
dat0$region2[dat0$measure=="rBF"] = paste0(dat0$region2[dat0$measure=="rBF"], "_rbf")
dat0$region2[dat0$measure=="rBV"] = paste0(dat0$region2[dat0$measure=="rBV"], "_rbv")
dat0$region2[dat0$measure=="TTP"] = paste0(dat0$region2[dat0$measure=="TTP"], "_ttp")
dat0$region2[dat0$measure=="TMAX"] = paste0(dat0$region2[dat0$measure=="TMAX"], "_tmax")
dat0$region2[dat0$measure=="MTT"] = paste0(dat0$region2[dat0$measure=="MTT"], "_mtt")
dat0$region2[dat0$measure=="tMIP"] = paste0(dat0$region2[dat0$measure=="tMIP"], "_tmip")

# transform data
# mrs, risk_factors, ..., region1_mean_volume, region2_mean_volume, ...
dat = dcast(dat0, p_id+age+sex+independent_pre_stroke+nihss_bl+nihss_24h+nihss_diff_bl_24h
            +intracranial_bleeding+tah_pre_stroke+antikoagulation_pre_stroke+statin_pre_stroke+antihypertensiva_pre_stroke
            +sys_bloodpressure_bl+dias_bloodpressure_bl+glucose_bl+hba1c+ldl+hdl+triglyceride+crp+inr+anaesthesia
            +atrial_fibrillation+rf_diabetes+rf_hypertonia+rf_hypercholesterinaemie+rf_smoker+rf_chd+rf_pavk+rf_tia_stroke+infarct_side+additional_occlusion
            +lyse+time_to_imaging+time_to_groin_puncture+vessel_open_before_therapy+tici+tici_binary
            +volume_adc+volume_tmax+volume_rbf_adc+volume_rbf_tmax+volume_rbv_adc+volume_rbv_tmax+volume_tar
            +mrs_3months_binary+collateralization+mrs_3months
            ~ region2, value.var="pooled_mean")

# save(dat, file=paste0(dir,"/data/data_wide_all_nihss.R"))

dat$mrs_3months = as.ordered(dat$mrs_3months)
dat$time_to_groin_puncture = as.numeric(dat$time_to_groin_puncture)


# change response values
dat$sex = revalue(dat$sex, replace = c("0" = "male", "1" = "female"))
dat$infarct_side = revalue(dat$infarct_side, replace = c("1" = "left", "2" = "right"))
dat$lyse = revalue(dat$lyse, replace = c("0" = "no", "1" = "yes"))

dat$sex = relevel(dat$sex, ref="female")
dat$infarct_side = relevel(dat$infarct_side, ref="right")
dat$lyse = relevel(dat$lyse, ref = "no")




#######################################
#### Functional outcome prediction ####
#######################################


# volume_adc = ischemic core
# volume_tmax = hypoperfused tissue
# volume_tar = tissue at risk (penumbra/mismatch tmax and core)


# HERMES:
# - Primary outcome: functional outcome (mRS after 90 days)
# - Regression analysis adjusted for 7 baseline prognostic variables:
#   - age: age
#   - sex: sex
#   - baseline clinical severity (NIHSS score): nihss_bl
#   - time from stroke onset to randomisation: time_to_groin_puncture
#   - administration of intravenous alteplase: lyse
#   - core lab-adjudicated non-contrast CT ASPECTS: not available
#   - site of vessel occlusion: infarct_side


# 7 patients were excluded due to missing data
# --> no imputation, 7 out of 222 is not that much and shouldn't
# bias our analysis

# Functional independence prediction: mRS 0-2 vs. 3-6 (Binary logistic regression)
mod_bin = glm(mrs_3months_binary ~ volume_adc + age + sex + nihss_bl + time_to_groin_puncture + lyse + infarct_side, 
          data = dat, family = "binomial")
# # Functional independence prediction: mRS 0-2 vs. 3-6 (Binary logistic regression)
# mod_bin = glm(mrs_3months_binary ~ 1,  data = dat, family = "binomial")

summary(mod_bin)
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             5.0644888  1.0700266   4.733 2.21e-06 ***
# volume_adc             -0.0192742  0.0068882  -2.798  0.00514 ** 
# age                    -0.0580487  0.0127167  -4.565 5.00e-06 ***
# sexmale                -0.0777957  0.3050762  -0.255  0.79872    
# nihss_bl               -0.0291713  0.0264435  -1.103  0.26996    
# time_to_groin_puncture -0.0006446  0.0006701  -0.962  0.33604    
# lyseyes                 0.2544476  0.3379649   0.753  0.45152    
# infarct_sideleft        0.1074286  0.3016963   0.356  0.72178  

# Odds ratios:
or_bin = exp(coef(mod_bin))
ci_or_bin = exp(confint(mod_bin))

tab_bin = cbind(OR = or_bin, ci_or_bin, p_value = summary(mod_bin)$coef[,ncol(summary(mod_bin)$coef)])
tab_bin
#                             OR      2.5 %       97.5 %      p_value
# (Intercept)            158.2994936 21.4784973 1445.3333613 2.211716e-06
# volume_adc               0.9809104  0.9668966    0.9934768 5.139863e-03
# age                      0.9436040  0.9192493    0.9663967 5.000562e-06
# sexmale                  0.9251534  0.5074853    1.6835343 7.987199e-01
# nihss_bl                 0.9712500  0.9215553    1.0229849 2.699588e-01
# time_to_groin_puncture   0.9993556  0.9979980    1.0006524 3.360370e-01
# lyseyes                  1.2897489  0.6656616    2.5133847 4.515211e-01
# infarct_sideleft         1.1134113  0.6167885    2.0186724 7.217793e-01

# If ischemic core volume increases by one unit, the odds ratio is 0.98
# For every unit increase in ischemic core volume, the odds for having a 
# favorable functionaloutcome decreases by 1.9% (1-0.981=0.019).
# With increasing age, the odds for a good functional outcome decreases.
# Women are more likely to have a good functional outcome than men
# With every unit increase in NIHSS, the odds for a good functional outcome 
# decreases by about 3%.
# etc.

#b Change unit
tab_bin["volume_adc",]^10
tab_bin["age",]^5
tab_bin["nihss_bl",]^5

# Functional outcome: mrs 0-6 (Ordinal logistic regression)
mod = polr(mrs_3months ~ volume_adc + age + sex + nihss_bl + time_to_groin_puncture + lyse + infarct_side, 
           data = dat)
# summary(mod)

# for p-value: t-value against the standard normal distribution
ctable = coef(summary(mod))
ctale = 1/ctable
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

# combined table
(ctable = cbind(ctable, "p value" = p))
#                             Value     Std. Error    t value    p value
# volume_adc              0.0120704530 0.0047811111  2.5246125 1.158259e-02
# age                     0.0506901242 0.0095972260  5.2817475 1.279575e-07
# sexmale                 0.2005424504 0.2498595081  0.8026208 4.221939e-01
# nihss_bl                0.0500103025 0.0224614181  2.2264980 2.598084e-02
# time_to_groin_puncture  0.0005314731 0.0005444286  0.9762034 3.289637e-01
# lyseyes                -0.3265971830 0.2761177957 -1.1828183 2.368812e-01
# infarct_sideleft       -0.1649818452 0.2456910185 -0.6715013 5.019012e-01
# 0|1                     2.4774514558 0.7907533535  3.1330268 1.730136e-03
# 1|2                     3.7916839273 0.8018409712  4.7287231 2.259363e-06
# 2|3                     4.6004938401 0.8200226077  5.6102037 2.020886e-08
# 3|4                     5.3391864537 0.8416827971  6.3434663 2.246521e-10
# 4|5                     6.2044541769 0.8669077822  7.1569944 8.246504e-13
# 5|6                     6.3897856131 0.8725432613  7.3231734 2.421745e-13

# odds ratios and confidence intervals:
or = 1/exp(coef(mod))
ci_or = 1/exp(confint(mod))

tab = cbind(OR = or, ci_or, p_value = p[1:7])
tab
#                           OR       97.5 %    2.5 %    p_value
# volume_adc             0.9880021 0.9973549 0.9786478 1.158259e-02
# age                    0.9505732 0.9683427 0.9325198 1.279575e-07
# sexmale                0.8182868 1.3348402 0.5007070 4.221939e-01
# nihss_bl               0.9512196 0.9935520 0.9096118 2.598084e-02
# time_to_groin_puncture 0.9994687 1.0004858 0.9984335 3.289637e-01
# lyseyes                1.3862430 2.3804899 0.8095839 2.368812e-01
# infarct_sideleft       1.1793717 1.9115436 0.7289215 5.019012e-01

# for every unit increase in ischemic core volume, the odds/chance for 
# having a higher mRS (2 or higher vs. 1, 3 or higher vs. 2/1 etc.) increases by 1.2%
# for every more year, the odds/chance for having a higher mRS increases by 5.2%
# --> higher age is associated with worse outcome
# The odds for having a higher mRS score is 18% (1-0.82=0.18) lower for females (1) than for males
# For females the odds for having a higher mRS score is 0.82 times that of males
# --> women are more likely to have a better outcome
# Patients with lyse are less likely to have a higher mRS than patients who hadn't


tab["volume_adc",]^10
tab["age",]^5
tab["nihss_bl",]^5




#################################
#### Partial dependency plot ####
#################################

library(pdp)
info = pdp::partial(mod_bin, pred.var = "volume_adc", ice = T, which.class = 1, 
               rug = T, prob = T, train = dat)
info_avg = partial(mod_bin, pred.var = "volume_adc", ice = F, which.class = 1, 
                   rug = T, prob = T, train = dat)
dat$no = seq(1, dim(dat)[1], 1)
dat_plot = merge(info, dat, by.x="yhat.id", by.y="no")

# find yhat which is closest to the predicted variables
i="volume_adc"
for(j in unique(dat_plot$yhat.id)){
  pat = dat_plot[dat_plot$yhat.id == j,]
  if(class(dat_plot[,paste0(i,".x")])=="factor"){
    k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
  } else{
    k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
  }
  dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
}

pdf(paste0(dir,"images/partial_dependency_plot_functional_outcome_HERMES.pdf"))
p = plotPartial(info, plot.pdp = T, pdp.col = "darksalmon", rug=F, levelplot=T, contour=F, train=dat,
                ylab=list("Probability for favorable outcome", cex=1.5), xlab = list("Ischemic core volume", cex=1.5),
                alpha=0.2, ylim=c(0,1), scales=list(tck=c(1.0,0.0), y=list(at=seq(0,1,0.2),labels=c("0·0","0·2","0·4","0·6","0·8","1·0")), cex=1.3))
print(p + layer(panel.points(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="darkred", cex=0.5))
      + layer(panel.lines(info_avg$volume_adc, info_avg$yhat, col="darksalmon", lwd=2)))
dev.off()




###########################################
#### Mismatch instead of ischemic core ####
###########################################


# Functional independence prediction: mRS 0-2 vs. 3-6 (Binary logistic regression)
mod_bin = glm(mrs_3months_binary ~ volume_tar + age + sex + nihss_bl + time_to_groin_puncture + lyse + infarct_side, 
              data = dat, family = "binomial")

summary(mod_bin)
# Coefficients:
#                         Estimate  Std. Error z value Pr(>|z|)    
# (Intercept)             4.1521572  1.0170499   4.083 4.45e-05 ***
# volume_tar             -0.0002862  0.0024638  -0.116 0.907513    
# age                    -0.0442600  0.0114191  -3.876 0.000106 ***
# sex1                    0.1166462  0.3126850   0.373 0.709113    
# nihss_bl               -0.0555101  0.0260107  -2.134 0.032832 *  
# time_to_groin_puncture -0.0007080  0.0006615  -1.070 0.284498    
# lyse1                   0.1804860  0.3299494   0.547 0.584371    
# infarct_side2          -0.1325113  0.2985381  -0.444 0.657139 

# Odds ratios:
or_bin = exp(coef(mod_bin))
ci_or_bin = exp(confint(mod_bin))

tab_bin = cbind(OR = or_bin, ci_or_bin, p_value = summary(mod_bin)$coef[,ncol(summary(mod_bin)$coef)])
tab_bin
#                               OR      2.5 %       97.5 %      p_value
# (Intercept)            62.5703891 9.4613972 495.4162443 3.915287e-05
# volume_tar              0.9997138 0.9948093   1.0045652 9.075126e-01
# age                     0.9567052 0.9345079   0.9774339 1.062008e-04
# sexmale                 0.8898999 0.4804941   1.6425683 7.091134e-01
# nihss_bl                0.9460024 0.8979565   0.9947493 3.283224e-02
# time_to_groin_puncture  0.9992923 0.9979528   1.0005789 2.844982e-01
# lyseyes                 1.1977993 0.6273583   2.2945866 5.843711e-01
# infarct_sideleft        1.1416919 0.6365831   2.0574479 6.571385e-01

# We found no association between Tissue at risk and functional outcome

tab_bin["volume_tar",]^10
#   OR     2.5 %    97.5 % 
# 0.9971418 0.9492893 1.0466012
tab_bin["age",]^5
#   OR     2.5 %    97.5 % 
# 8.014763e-01 7.127133e-01 8.921483e-01
tab_bin["nihss_bl",]^5
#   OR     2.5 %    97.5 % 
# 7.576372e-01 5.838168e-01 9.740207e-01

# Functional outcome: mrs 0-6 (Ordinal logistic regression)
mod = polr(mrs_3months ~ volume_tar + age + sex + nihss_bl + time_to_groin_puncture + lyse + infarct_side, 
           data = dat)
# summary(mod)

# for p-value: t-value against the standard normal distribution
ctable = coef(summary(mod))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

# combined table
(ctable = cbind(ctable, "p value" = p))
#                             Value     Std. Error    t value    p value
# volume_tar              0.0004466371 0.002105371  0.2121418 8.319964e-01
# age                     0.0423019100 0.009069946  4.6639650 3.101741e-06
# sex1                   -0.1747774360 0.263057170 -0.6644086 5.064289e-01
# nihss_bl                0.0675265427 0.022441265  3.0090345 2.620793e-03
# time_to_groin_puncture  0.0005684293 0.000552425  1.0289711 3.034933e-01
# lyse1                  -0.2916600199 0.277295550 -1.0518020 2.928904e-01
# infarct_side2           0.1488956115 0.247946409  0.6005153 5.481629e-01
# 0|1                     1.9532077301 0.805871481  2.4237211 1.536240e-02
# 1|2                     3.2544249323 0.812739685  4.0042648 6.221064e-05
# 2|3                     4.0438593297 0.827504391  4.8868131 1.024813e-06
# 3|4                     4.7529112171 0.845255950  5.6230438 1.876218e-08
# 4|5                     5.5959745668 0.868250273  6.4451170 1.155113e-10
# 5|6                     5.7803175603 0.873967648  6.6138805 3.743741e-11

# odds ratios and confidence intervals:
or = exp(coef(mod))
ci_or = exp(confint(mod))

or = 1/or
ci_or = 1/ci_or

tab = cbind(OR = or, ci_or, p_value = p[1:7])
tab
#                           OR      97.5 %      2.5 %   p_value
# volume_tar             0.9995535 1.0037556 0.9954853 8.319919e-01
# age                    0.9585803 0.9754410 0.9413228 3.101649e-06
# sexmale                0.8396441 1.4057046 0.5006569 5.064295e-01
# nihss_bl               0.9347029 0.9762909 0.8939734 2.620809e-03
# time_to_groin_puncture 0.9994317 1.0004652 0.9983865 3.034847e-01
# lyseyes                1.3386457 2.3037577 0.7800596 2.928930e-01
# infarct_sideleft       1.1605498 1.8893152 0.7140848 5.481675e-01


tab["volume_tar",]^10
#   OR     2.5 %    97.5 % 
# 0.9955435 1.0381975 0.9557592 
tab["age",]^5
#   OR     2.5 %    97.5 % 
# 8.093613e-01 8.830901e-01 7.390823e-01
tab["nihss_bl",]^5
#   OR     2.5 %    97.5 % 
# 7.134574e-01 8.869438e-01 5.709826e-01