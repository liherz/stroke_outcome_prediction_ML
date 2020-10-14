library(pdp)
library(gridExtra)
library(lattice)
library(latticeExtra)
library(ellipse)

# Create the partial plots for the 
get_partial_plots_RF = function(rf_model, data, dir, name){
  
  pdf(paste0(dir,name))
  
  for(i in attributes(rf_model$terms)$term.labels){
    print(i)
    info = partial(rf_model, pred.var = i, ice = T, which.class = 2, rug=T, prob=T, train=data)
    data$no = seq(1,dim(data)[1],1)
    dat_plot = merge(info, data, by.x="yhat.id", by.y="no")
    
    # find yhat which is closest to the predicted variables
    for(j in unique(dat_plot$yhat.id)){
      pat = dat_plot[dat_plot$yhat.id == j,]
      if(class(dat_plot[,paste0(i,".x")])=="factor"){
        k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
      } else{
        k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
      }
      dat_plot$true[dat_plot$yhat.id == j] = pat$yhat[k]
    }
    
    p = plotPartial(info, plot.pdp = T, pdp.col = "cyan", rug=T, levelplot=T, contour=F, train=data,
                ylab="Prob for bad outcome") 
    print(p + layer(lpoints(dat_plot[,paste0(i,".y")], dat_plot$true, pch=16, col="red", cex=0.8)))

  }
  dev.off()
  
}



get_partial_plots_logreg = function(logreg_model, data, dir, name){
  
  pdf(paste0(dir,name))

  for(i in attributes(logreg_model[[1]]$terms)$term.labels[1:3]){
    print(i)
    
    p = list()
    
    info1 = partial(logreg_model[[1]], pred.var = i, ice = T, which.class = 1, prob=T, train=data[[1]])
    info2 = partial(logreg_model[[2]], pred.var = i, ice = T, which.class = 1, prob=T, train=data[[2]])
    info3 = partial(logreg_model[[3]], pred.var = i, ice = T, which.class = 1, prob=T, train=data[[3]])
    info4 = partial(logreg_model[[4]], pred.var = i, ice = T, which.class = 1, prob=T, train=data[[4]])
    info5 = partial(logreg_model[[5]], pred.var = i, ice = T, which.class = 1, prob=T, train=data[[5]])
    
    data[[1]]$no = seq(1,dim(data[[1]])[1],1)
    data[[2]]$no = seq(1,dim(data[[2]])[1],1)
    data[[3]]$no = seq(1,dim(data[[3]])[1],1)
    data[[4]]$no = seq(1,dim(data[[4]])[1],1)
    data[[5]]$no = seq(1,dim(data[[5]])[1],1)
    
    dat_plot1 = merge(info1, data[[1]], by.x="yhat.id", by.y="no")
    dat_plot2 = merge(info2, data[[2]], by.x="yhat.id", by.y="no")
    dat_plot3 = merge(info3, data[[3]], by.x="yhat.id", by.y="no")
    dat_plot4 = merge(info4, data[[4]], by.x="yhat.id", by.y="no")
    dat_plot5 = merge(info5, data[[5]], by.x="yhat.id", by.y="no")
    
    
    # find yhat which is closest to the predicted variables
    for(j in unique(dat_plot1$yhat.id)){
      pat = dat_plot1[dat_plot1$yhat.id == j,]
      if(class(dat_plot1[,paste0(i,".x")])=="factor"){
        k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
      } else{
        k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
      }
      dat_plot1$true[dat_plot1$yhat.id == j] = pat$yhat[k]
    }
    
    # find yhat which is closest to the predicted variables
    for(j in unique(dat_plot2$yhat.id)){
      pat = dat_plot2[dat_plot2$yhat.id == j,]
      if(class(dat_plot2[,paste0(i,".x")])=="factor"){
        k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
      } else{
        k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
      }
      dat_plot2$true[dat_plot2$yhat.id == j] = pat$yhat[k]
    }
    
    # find yhat which is closest to the predicted variables
    for(j in unique(dat_plot3$yhat.id)){
      pat = dat_plot3[dat_plot3$yhat.id == j,]
      if(class(dat_plot3[,paste0(i,".x")])=="factor"){
        k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
      } else{
        k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
      }
      dat_plot3$true[dat_plot3$yhat.id == j] = pat$yhat[k]
    }
    
    # find yhat which is closest to the predicted variables
    for(j in unique(dat_plot4$yhat.id)){
      pat = dat_plot4[dat_plot4$yhat.id == j,]
      if(class(dat_plot4[,paste0(i,".x")])=="factor"){
        k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
      } else{
        k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
      }
      dat_plot4$true[dat_plot4$yhat.id == j] = pat$yhat[k]
    }
    
    # find yhat which is closest to the predicted variables
    for(j in unique(dat_plot5$yhat.id)){
      pat = dat_plot5[dat_plot5$yhat.id == j,]
      if(class(dat_plot5[,paste0(i,".x")])=="factor"){
        k = which(pat[,paste0(i,".x")] == pat[,paste0(i,".y")][1])
      } else{
        k = which(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")]) == min(abs(pat[paste0(i,".x")]-pat[paste0(i,".y")])))[1]
      }
      dat_plot5$true[dat_plot5$yhat.id == j] = pat$yhat[k]
    }
    
    p[[1]] = plotPartial(info1, plot.pdp = T, pdp.col = "cyan", rug=T, levelplot=T, contour=F, train=data[[1]],
                         ylab="Prob for bad outcome", ylim=c(0,1))  + layer(lpoints(dat_plot1[,paste0(i,".y")], dat_plot1$true, pch=16, col="red", cex=0.8))

    p[[2]] = plotPartial(info2, plot.pdp = T, pdp.col = "cyan", rug=T, levelplot=T, contour=F, train=data[[2]],
                     ylab="Prob for bad outcome", ylim=c(0,1)) + layer(lpoints(dat_plot2[,paste0(i,".y")], dat_plot2$true, pch=16, col="red", cex=0.8))

    p[[3]] = plotPartial(info3, plot.pdp = T, pdp.col = "cyan", rug=T, levelplot=T, contour=F, train=data[[3]],
                     ylab="Prob for bad outcome", ylim=c(0,1)) + layer(lpoints(dat_plot3[,paste0(i,".y")], dat_plot3$true, pch=16, col="red", cex=0.8))

    p[[4]] = plotPartial(info4, plot.pdp = T, pdp.col = "cyan", rug=T, levelplot=T, contour=F, train=data[[4]],
                     ylab="Prob for bad outcome", ylim=c(0,1)) + layer(lpoints(dat_plot4[,paste0(i,".y")], dat_plot4$true, pch=16, col="red", cex=0.8))

    p[[5]] = plotPartial(info5, plot.pdp = T, pdp.col = "cyan", rug=T, levelplot=T, contour=F, train=data[[5]],
                     ylab="Prob for bad outcome", ylim=c(0,1)) + layer(lpoints(dat_plot5[,paste0(i,".y")], dat_plot5$true, pch=16, col="red", cex=0.8))
    
    # p1 = plotPartial(info1, plot.pdp = T, pdp.col = "cyan", rug=T, levelplot=T, contour=F, train=data[[1]],
    #             ylab="Prob for bad outcome", ylim=c(0,1))
    # p1 = p1 + layer(lpoints(dat_plot1[,paste0(i,".y")], dat_plot1$true, pch=16, col="red", cex=0.8))
    # 
    # p2 = plotPartial(info2, plot.pdp = T, pdp.col = "cyan", rug=T, levelplot=T, contour=F, train=data[[2]],
    #                  ylab="Prob for bad outcome", ylim=c(0,1))
    # p2 = p2 + layer(lpoints(dat_plot2[,paste0(i,".y")], dat_plot2$true, pch=16, col="red", cex=0.8))
    # 
    # p3 = plotPartial(info3, plot.pdp = T, pdp.col = "cyan", rug=T, levelplot=T, contour=F, train=data[[3]],
    #                  ylab="Prob for bad outcome", ylim=c(0,1))
    # p3 = p3 + layer(lpoints(dat_plot3[,paste0(i,".y")], dat_plot3$true, pch=16, col="red", cex=0.8))
    # 
    # p4 = plotPartial(info4, plot.pdp = T, pdp.col = "cyan", rug=T, levelplot=T, contour=F, train=data[[4]],
    #                  ylab="Prob for bad outcome", ylim=c(0,1))
    # p4 = p4 + layer(lpoints(dat_plot4[,paste0(i,".y")], dat_plot4$true, pch=16, col="red", cex=0.8))
    # 
    # p5 = plotPartial(info5, plot.pdp = T, pdp.col = "cyan", rug=T, levelplot=T, contour=F, train=data[[5]],
    #                  ylab="Prob for bad outcome", ylim=c(0,1))
    # p5 = p5 + layer(lpoints(dat_plot5[,paste0(i,".y")], dat_plot5$true, pch=16, col="red", cex=0.8))

    grid.arrange(grobs=p,nrow=3)
  }
  dev.off()
}


