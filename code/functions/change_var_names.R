# Change names for plotting
library(plyr)

change_var_names_RF = function(model){
  # if CV=TrUE, expect a list of models
  model_plot = model
  
  model_plot$forest$xlevels$sex = revalue(model_plot$forest$xlevels$sex, c("0"="male", "1"="female"))
  model_plot$forest$xlevels$independent_pre_stroke = revalue(model_plot$forest$xlevels$independent_pre_stroke, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$tah_pre_stroke = revalue(model_plot$forest$xlevels$tah_pre_stroke, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$antikoagulation_pre_stroke = revalue(model_plot$forest$xlevels$antikoagulation_pre_stroke, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$statin_pre_stroke = revalue(model_plot$forest$xlevels$statin_pre_stroke, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$antihypertensiva_pre_stroke = revalue(model_plot$forest$xlevels$antihypertensiva_pre_stroke, c("0"="no", "1"="yes"))
  # model_plot$forest$xlevels$toast = revalue(model_plot$forest$xlevels$toast, c("1"="Makroangiopathie", "2"="Kardioembolisch","3"="Andere"))
  model_plot$forest$xlevels$atrial_fibrillation = revalue(model_plot$forest$xlevels$atrial_fibrillation, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$rf_diabetes = revalue(model_plot$forest$xlevels$rf_diabetes, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$rf_hypertonia = revalue(model_plot$forest$xlevels$rf_hypertonia, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$rf_hypercholesterinaemie = revalue(model_plot$forest$xlevels$rf_hypercholesterinaemie, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$rf_smoker = revalue(model_plot$forest$xlevels$rf_smoker, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$rf_chd = revalue(model_plot$forest$xlevels$rf_chd, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$rf_pavk = revalue(model_plot$forest$xlevels$rf_pavk, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$rf_tia_stroke = revalue(model_plot$forest$xlevels$rf_tia_stroke, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$additional_occlusion = revalue(model_plot$forest$xlevels$additional_occlusion, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$lyse = revalue(model_plot$forest$xlevels$lyse, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$anaesthesia = revalue(model_plot$forest$xlevels$anaesthesia, c("0"="no", "1"="yes"))
  # model_plot$forest$xlevels$thrombectomy = revalue(model_plot$forest$xlevels$thrombectomy, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$vessel_open_before_therapy = revalue(model_plot$forest$xlevels$vessel_open_before_therapy, c("0"="no", "1"="yes"))
  model_plot$forest$xlevels$tici = revalue(model_plot$forest$xlevels$tici, c("0"="0", "1"="1","2"="2a","3"="2b","4"="3"))
  # model_plot$forest$xlevels$tici_binary = revalue(model_plot$forest$xlevels$tici_binary, c("0"="bad", "1"="good"))
  model_plot$forest$xlevels$infarct_side = revalue(model_plot$forest$xlevels$infarct_side, c("1"="left", "2"="right"))
  model_plot$forest$xlevels$collateralization = revalue(model_plot$forest$xlevels$collateralization, c("0"="poor", "1"="moderate", "2"="good"))
  
  return(model_plot)
  
}




change_var_names_DF = function(dat, CV=F, nfolds=1){
  # if CV=TrUE, expect a list of models
  dat_plot = dat
  
  if(CV){
    
    for(i in 1:nfolds){
      dat_plot[[i]]$sex = revalue(dat_plot[[i]]$sex, c("0"="male", "1"="female"))
      dat_plot[[i]]$independent_pre_stroke = revalue(dat_plot[[i]]$independent_pre_stroke, c("0"="no", "1"="yes"))
      dat_plot[[i]]$tah_pre_stroke = revalue(dat_plot[[i]]$tah_pre_stroke, c("0"="no", "1"="yes"))
      dat_plot[[i]]$antikoagulation_pre_stroke = revalue(dat_plot[[i]]$antikoagulation_pre_stroke, c("0"="no", "1"="yes"))
      dat_plot[[i]]$statin_pre_stroke = revalue(dat_plot[[i]]$statin_pre_stroke, c("0"="no", "1"="yes"))
      dat_plot[[i]]$antihypertensiva_pre_stroke = revalue(dat_plot[[i]]$antihypertensiva_pre_stroke, c("0"="no", "1"="yes"))
      # dat_plot[[i]]$toast = revalue(dat_plot[[i]]$toast, c("1"="Makroangiopathie", "2"="Kardioembolisch","3"="Andere"))
      dat_plot[[i]]$atrial_fibrillation = revalue(dat_plot[[i]]$atrial_fibrillation, c("0"="no", "1"="yes"))
      dat_plot[[i]]$rf_diabetes = revalue(dat_plot[[i]]$rf_diabetes, c("0"="no", "1"="yes"))
      dat_plot[[i]]$rf_hypertonia = revalue(dat_plot[[i]]$rf_hypertonia, c("0"="no", "1"="yes"))
      dat_plot[[i]]$rf_hypercholesterinaemie = revalue(dat_plot[[i]]$rf_hypercholesterinaemie, c("0"="no", "1"="yes"))
      dat_plot[[i]]$rf_smoker = revalue(dat_plot[[i]]$rf_smoker, c("0"="no", "1"="yes"))
      dat_plot[[i]]$rf_chd = revalue(dat_plot[[i]]$rf_chd, c("0"="no", "1"="yes"))
      dat_plot[[i]]$rf_pavk = revalue(dat_plot[[i]]$rf_pavk, c("0"="no", "1"="yes"))
      dat_plot[[i]]$rf_tia_stroke = revalue(dat_plot[[i]]$rf_tia_stroke, c("0"="no", "1"="yes"))
      dat_plot[[i]]$additional_occlusion = revalue(dat_plot[[i]]$additional_occlusion, c("0"="no", "1"="yes"))
      dat_plot[[i]]$lyse = revalue(dat_plot[[i]]$lyse, c("0"="no", "1"="yes"))
      dat_plot[[i]]$anaesthesia = revalue(dat_plot[[i]]$anaesthesia, c("0"="no", "1"="yes"))
      # dat_plot[[i]]$thrombectomy = revalue(dat_plot[[i]]$thrombectomy, c("0"="no", "1"="yes"))
      dat_plot[[i]]$vessel_open_before_therapy = revalue(dat_plot[[i]]$vessel_open_before_therapy, c("0"="no", "1"="yes"))
      dat_plot[[i]]$tici = revalue(dat_plot[[i]]$tici, c("0"="0", "1"="1","2"="2a","3"="2b","4"="3"))
      # dat_plot[[i]]$tici_binary = revalue(dat_plot[[i]]$tici_binary, c("0"="bad", "1"="good"))
      dat_plot[[i]]$infarct_side = revalue(dat_plot[[i]]$infarct_side, c("1"="left", "2"="right"))
      dat_plot[[i]]$collateralization = revalue(dat_plot[[i]]$collateralization, c("0"="poor", "1"="moderate", "2"="good"))
    }
  } else{
    
    dat_plot$sex = revalue(dat_plot$sex, c("0"="male", "1"="female"))
    dat_plot$independent_pre_stroke = revalue(dat_plot$independent_pre_stroke, c("0"="no", "1"="yes"))
    dat_plot$tah_pre_stroke = revalue(dat_plot$tah_pre_stroke, c("0"="no", "1"="yes"))
    dat_plot$antikoagulation_pre_stroke = revalue(dat_plot$antikoagulation_pre_stroke, c("0"="no", "1"="yes"))
    dat_plot$statin_pre_stroke = revalue(dat_plot$statin_pre_stroke, c("0"="no", "1"="yes"))
    dat_plot$antihypertensiva_pre_stroke = revalue(dat_plot$antihypertensiva_pre_stroke, c("0"="no", "1"="yes"))
    # dat_plot$toast = revalue(dat_plot$toast, c("1"="Makroangiopathie", "2"="Kardioembolisch","3"="Andere"))
    dat_plot$atrial_fibrillation = revalue(dat_plot$atrial_fibrillation, c("0"="no", "1"="yes"))
    dat_plot$rf_diabetes = revalue(dat_plot$rf_diabetes, c("0"="no", "1"="yes"))
    dat_plot$rf_hypertonia = revalue(dat_plot$rf_hypertonia, c("0"="no", "1"="yes"))
    dat_plot$rf_hypercholesterinaemie = revalue(dat_plot$rf_hypercholesterinaemie, c("0"="no", "1"="yes"))
    dat_plot$rf_smoker = revalue(dat_plot$rf_smoker, c("0"="no", "1"="yes"))
    dat_plot$rf_chd = revalue(dat_plot$rf_chd, c("0"="no", "1"="yes"))
    dat_plot$rf_pavk = revalue(dat_plot$rf_pavk, c("0"="no", "1"="yes"))
    dat_plot$rf_tia_stroke = revalue(dat_plot$rf_tia_stroke, c("0"="no", "1"="yes"))
    dat_plot$additional_occlusion = revalue(dat_plot$additional_occlusion, c("0"="no", "1"="yes"))
    dat_plot$lyse = revalue(dat_plot$lyse, c("0"="no", "1"="yes"))
    dat_plot$anaesthesia = revalue(dat_plot$anaesthesia, c("0"="no", "1"="yes"))
    # dat_plot$thrombectomy = revalue(dat_plot$thrombectomy, c("0"="no", "1"="yes"))
    dat_plot$vessel_open_before_therapy = revalue(dat_plot$vessel_open_before_therapy, c("0"="no", "1"="yes"))
    dat_plot$tici = revalue(dat_plot$tici, c("0"="0", "1"="1","2"="2a","3"="2b","4"="3"))
    # dat_plot$tici_binary = revalue(dat_plot$tici_binary, c("0"="bad", "1"="good"))
    dat_plot$infarct_side = revalue(dat_plot$infarct_side, c("1"="left", "2"="right"))
    dat_plot$collateralization = revalue(dat_plot$collateralization, c("0"="poor", "1"="moderate", "2"="good"))
  }
  return(dat_plot)
}



change_var_names_logreg_CV = function(model, n_folds){
  
  model_plot = model

  for(i in 1:n_folds){
    
    model_plot[[i]]$xlevels$sex = revalue(model_plot[[i]]$xlevels$sex, c("0"="male", "1"="female"))
    model_plot[[i]]$xlevels$independent_pre_stroke = revalue(model_plot[[i]]$xlevels$independent_pre_stroke, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$tah_pre_stroke = revalue(model_plot[[i]]$xlevels$tah_pre_stroke, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$antikoagulation_pre_stroke = revalue(model_plot[[i]]$xlevels$antikoagulation_pre_stroke, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$statin_pre_stroke = revalue(model_plot[[i]]$xlevels$statin_pre_stroke, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$antihypertensiva_pre_stroke = revalue(model_plot[[i]]$xlevels$antihypertensiva_pre_stroke, c("0"="no", "1"="yes"))
    # model_plot[[i]]$xlevels$toast = revalue(model_plot[[i]]$xlevels$toast, c("1"="Makroangiopathie", "2"="Kardioembolisch","3"="Andere"))
    model_plot[[i]]$xlevels$atrial_fibrillation = revalue(model_plot[[i]]$xlevels$atrial_fibrillation, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$rf_diabetes = revalue(model_plot[[i]]$xlevels$rf_diabetes, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$rf_hypertonia = revalue(model_plot[[i]]$xlevels$rf_hypertonia, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$rf_hypercholesterinaemie = revalue(model_plot[[i]]$xlevels$rf_hypercholesterinaemie, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$rf_smoker = revalue(model_plot[[i]]$xlevels$rf_smoker, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$rf_chd = revalue(model_plot[[i]]$xlevels$rf_chd, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$rf_pavk = revalue(model_plot[[i]]$xlevels$rf_pavk, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$rf_tia_stroke = revalue(model_plot[[i]]$xlevels$rf_tia_stroke, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$additional_occlusion = revalue(model_plot[[i]]$xlevels$additional_occlusion, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$lyse = revalue(model_plot[[i]]$xlevels$lyse, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$anaesthesia = revalue(model_plot[[i]]$xlevels$anaesthesia, c("0"="no", "1"="yes"))
    # model_plot[[i]]$xlevels$thrombectomy = revalue(model_plot[[i]]$xlevels$thrombectomy, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$vessel_open_before_therapy = revalue(model_plot[[i]]$xlevels$vessel_open_before_therapy, c("0"="no", "1"="yes"))
    model_plot[[i]]$xlevels$tici = revalue(model_plot[[i]]$xlevels$tici, c("0"="0", "1"="1","2"="2a","3"="2b","4"="3"))
    # model_plot[[i]]$xlevels$tici_binary = revalue(model_plot[[i]]$xlevels$tici_binary, c("0"="bad", "1"="good"))
    model_plot[[i]]$xlevels$infarct_side = revalue(model_plot[[i]]$xlevels$infarct_side, c("1"="left", "2"="right"))
    model_plot[[i]]$xlevels$collateralization = revalue(model_plot[[i]]$xlevels$collateralization, c("0"="poor", "1"="moderate", "2"="good"))
    
  }
  return(model_plot)
}
  
