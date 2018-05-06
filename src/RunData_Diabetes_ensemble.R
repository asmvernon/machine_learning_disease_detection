##### ENSEMBLE
RunData_Diabetes_ensemble= function(Title="",titles,object,targetValues,modelTypes,nModels,fourth=FALSE){
# Title="DWT_2D_All_noPCA_ensemble"

#   setwd("~/Dropbox/AaDry mini project/Diabetes data set/")
#   date = Sys.Date()  
#   folders = paste(date,titles)
#   dir.create(folders)
#   directory=(paste("~/Dropbox/AaDry mini project/Diabetes data set/",folders,"/",sep=""))
#   setwd(directory)
nDataItems=nrow(object$predictions$Run1)

predictions=matrix(0,nDataItems,5)
for (i in 1:5) {
if (fourth==FALSE) { 
original_predictions=cbind(object$predictions$Run1[,i],object$predictions$Run2[,i],
                           object$predictions$Run3[,i])
}
if (fourth==TRUE) { 
  original_predictions=cbind(object$predictions$Run1[,i],object$predictions$Run2[,i],
                             object$predictions$Run3[,i],object$predictions$Run4[,i])
}
predictions[,i]=apply(original_predictions,1,mean)
}

auc_matrix=numeric(5)
CI_low=numeric(5)
CI_high=numeric(5)
colnames(predictions)=modelTypes

colors <- c("green3", "orange", "blue", "magenta","red","black")
pdf(file=paste('ROC_',Title,'.pdf',sep=""))
for (i in 1:nModels) {
  output<- PlotALLRocCurve(i,colors,predictions[, i], targetValues, modelTypes[i])
  auc_matrix[i]    <- output[1]
  CI_low[i]        <- output[2]
  CI_high[i]       <- output[3]
}
legend(0.5,0.45,col=colors, modelTypes, bg=NA, lwd=2,cex=0.8,box.col=NA)
dev.off()

dim(auc_matrix)=c(1,5)
dim(CI_low)=c(1,5)
dim(CI_high)=c(1,5)

colnames(auc_matrix)=modelTypes
colnames(CI_low)=modelTypes
colnames(CI_high)=modelTypes

AUCsPlotMultiple_Figure(auc_matrix,CI_low,CI_high,Title,titles, nModels=5)

input=list("Extra"="EnsembleMean")

OUTPUT=list("AUC"=auc_matrix,"CI_low"=CI_low,"CI_high"=CI_high,"predictions"=as.data.frame(predictions),"input"=input)
return(OUTPUT)

}



