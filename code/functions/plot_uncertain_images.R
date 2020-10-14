# make a plot where uncertain images lie close together
# --> find out if there are some patterns in the data

rm(list=ls())

#######################
#### Set directory ####
#######################

dir = 'C:/Users/hezo/Dropbox/PhD/Stroke_paper_1/Analysen/'
setwd(dir)



###########################
#### Get relevant data ####
###########################


features = read.csv(paste0(dir,"scratch/cnn_mc_dropout_parallel_concatenate_16_07_2018/features_dropout_model3_valid1.csv"))
dat = read.csv(paste0(dir,"scratch/cnn_mc_dropout_parallel_concatenate_16_07_2018/predictions_dropout_valid1_new.csv"))
info = merge(features, dat, by='img')

# extract the features
representations = info[,grepl("X", colnames(info))] 
# label of the image
label = info$img_true.x 
pe = info$pe
mi = info$mi
# Path to the images
path = c()
for(i in 1:length(dat$path)){
  # extract the character
  path_new_tmp = strsplit(as.character(dat$path[i]),"\'")[[1]][2]
  first = strsplit(path_new_tmp, 'Dropbox/')[[1]][1]
  sec = "Dropbox/Phd/"
  third = strsplit(path_new_tmp, 'Dropbox/')[[1]][2]
  path_new = paste0(first,sec,third)
  path = c(path, path_new)
}
path[1]




##########################
#### Plot Uncertainty ####
##########################

source("functions/image_points.R")
# Plot the images into the t-SNE plot
png(file = "images/uncertainty_strokes.png", width = 16000, height = 12000)
par(mfrow=c(1,1))

plot(pe, mi, col=label, pch=15, axes = T, type = "n")
title(main = "Uncertainty Stroke", cex.main=30)

for(i in 1:length(path)){
  image = readJPEG(path[i])
  image_points(image, pe[i], mi[i], 0.2)
  print(i)
}
dev.off()
