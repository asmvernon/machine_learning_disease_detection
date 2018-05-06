##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##                                        Diabetes                                           ##
##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##Function to run cross-validation for classification models
##
CrossValidation <- function(m,PCA,runModels,nKeep,data.train, targetValues, nFolds = 10, PRINT = F, LOG_file = "LOG.txt",featMode = "wilcox",nfeats=1000,
                            Direction = "both"){
  ##----------------------------------------------------------------------
  ## LOAD LIBRARIES ------------------------------------------------------
  ##----------------------------------------------------------------------
#   source("src/ClassifierModels.R")
#   source("src/FeatureSelection.R")
#   source("src/PCPlot.R")
  ##----------------------------------------------------------------------
  ## ASSERTIONS ABOUT THE INPUT ------------------------------------------
  ##----------------------------------------------------------------------
  stopifnot(is.logical(targetValues))
  stopifnot(is.data.frame(data.train))
  ##----------------------------------------------------------------------
  ## FIND USEFUL VALUES --------------------------------------------------
  ##----------------------------------------------------------------------
  nDataItems  = length(targetValues)
  ##----------------------------------------------------------------------
  ## SET SPLIT WINDOW FOR PC PLOT ----------------------------------------
  ##----------------------------------------------------------------------
#   par(mar=c(4,4,2,2)) # set margins clockwise starting from bottom (total window area= 35 by 35)
#   par(mfrow=c(2,3)); # divide plot window
  
  ##----------------------------------------------------------------------
  ## WRITE TO LOG FILE ------------------------------------------
  ##----------------------------------------------------------------------
  write(x = "CROSS VALIDATION" , file = LOG_file, append = T)
  
  ##----------------------------------------------------------------------
  ## RUN THE CROSS-VALIDATION --------------------------------------------
  ##----------------------------------------------------------------------
  features <- character(nFolds)
  for (i in 1:nFolds){
    # LOG START TIME
    startCV <- Sys.time()
    #----------------------------------------------------------------------
    # WRITE TO LOG FILE ------------------------------------------
    #----------------------------------------------------------------------
    write(x = paste("CV",i," ...",sep = ""), file = LOG_file, append = T)
    # if(PRINT)
      cat('CV', i, "...\t", fill=F)
    #----------------------------------------------------------------------
    # SEPARATE DATA INTO TRAINING, TEST SETS
    #----------------------------------------------------------------------
    index           <- seq(i, nDataItems, nFolds)
    testData        <- data.train[index,] #indicates rows (samples) to be used as testData 
    trainingData    <- data.train[-index,] #excludes "test samples" --> training data
    trainingTargets <- targetValues[-index]
    # dim(trainingData); length(trainingTargets);
    if(PCA){
      nKeep       = nrow(trainingData) 
    } else {
      nKeep       = nKeep
    }
    
    #----------------------------------------------------------------------
    # FEATURE SELECTION  -------------------------------------------------
    #----------------------------------------------------------------------
    keep         <- FeatureSelection(dataMatrix = trainingData, targetValues = trainingTargets, nKeep = nKeep,
                                     featMode = featMode,nfeats = nfeats,PRINT = PRINT,
                                     LOG_file = LOG_file,Direction = Direction) # Direction missing
    # keep         <- FeatureSelection(dataMatrix = trainingData, targetValues = trainingTargets, ...)
    # keep         <- FeatureSelection(dataMatrix = trainingData, targetValues = trainingTargets)
    trainingData              <- trainingData[, keep]
    testData                  <- testData[, keep]
    Feature_matrix            <- as.matrix(trainingData)
    colnames(Feature_matrix)  <- names(trainingData)
    # row.names(Feature_matrix) <- as.numeric(trainingTargets)
  
    if(PRINT)cat("(",ncol(trainingData),")...\t",sep = "")
    
    ##----------------------------------------------------------------------
    ## PLOT FEATURES -------------------------------------------------------
    ##----------------------------------------------------------------------
#     if (i==1){
#       dir.create("TrainingData")
#       colourTable      <- heat.colors(100)
#       }
#     pdf(file=paste("TrainingData/Fold_",i,".pdf",sep=""))
#     heatmap(Feature_matrix,xlab = "Features",ylab = "Samples",col=colourTable,cexRow = 0.4,cexCol = 0.7)
#     while(dev.cur()>1) dev.off()
    
    ##----------------------------------------------------------------------
    ## WRITE FEATURES TO LOG FILE ------------------------------------------
    ##----------------------------------------------------------------------
    if(dim(Feature_matrix)[2]<21){
      write(x = paste("\t Features used (",dim(Feature_matrix)[2],"): ",paste(colnames(Feature_matrix),collapse = ", "), sep="") , file = LOG_file, append = T)
      # write(x = "", file = LOG_file, append = T)
    } else {
      write(x = paste("\t Number of Features used:",dim(Feature_matrix)[2]) , file = LOG_file, append = T)
    }
    ##----------------------------------------------------------------------
    ## INPUT DIMENSIONALITY REDUCTION --------------------------------------
    ##----------------------------------------------------------------------
    if (PCA==TRUE){
      if (i==1) dir.create("pcaPlots")
      model.pca <- princomp(trainingData)
      cum.var<-0
      k<-1            
      while (cum.var<0.95) {  ## Choose the principal components that explain 95% of the variance
        k       <- k+1        # ALWAYS KEEP AT LEAST TWO PCs
        cum.var <- sum(model.pca$sdev[1:k]^2)/sum(model.pca$sdev^2)
      }
      dataPCA   <- model.pca$scores[,1:k]
      dataPCA   <- as.data.frame(dataPCA)
      
      if (PRINT==TRUE) cat('No. Principal Components= ', k, fill=TRUE)
      ## TRANSFORM TEST DATA USING PCA MODEL
      object.testData <- predict(model.pca,testData)
      testDataPCA     <- object.testData[,1:k]
      testDataPCA     <- as.data.frame(testDataPCA)
      
      ##----------------------------------------------------------------------
      ## PLOT PC1 & PC2 ------------------------------------------------------
      ##----------------------------------------------------------------------
      pc1_2_var <- sum(model.pca$sdev[1:2]^2)/sum(model.pca$sdev^2)*100
      colour    <-ifelse(trainingTargets,"blue","green3")
      x.label   <-paste('PC1 ','(nFold=',i,',','CumVar=',format(pc1_2_var, digits=4),'%)')
      par(mar=c(4.5,4.5,2,3))
      
      pdf(file=paste('pcaPlots/Run_',m,'_PC_',i,'.pdf',sep=""))
        plot(model.pca$scores[,1],model.pca$scores[,2],pch=21,bg=colour,main='PC1 and PC2', xlab=x.label,ylab='PC2')
        legend(min(model.pca$scores[,1]),0,c("Diabetes","Control"),pch=16,col=c("blue","green3"),bg=c(), cex=1,box.col=NA)
      dev.off()
      
      ##----------------------------------------------------------------------
      ## TRAIN MODELS; MAKE PREDICTIONS --------------------------------------
      ##----------------------------------------------------------------------
       working <- ClassifierModels(data.train = dataPCA, targetValues = trainingTargets, data.test = testDataPCA,runModels = runModels,LOG_file)
    } else {
      
      working <- ClassifierModels(data.train = trainingData, targetValues = trainingTargets, data.test = testData,runModels = runModels,LOG_file)
      
      ##----------------------------------------------------------------------
      ## WRITE TO LOG FILE          ------------------------------------------
      ##----------------------------------------------------------------------
      write(x = paste("\t Running time: ",format(difftime(Sys.time(), startCV,units = "min"))), file = LOG_file, append = T)
      write(x = "", file = LOG_file, append = T)
      
    }
    ##(1st iteration) CONSTRUCT AN OUTPUT DATA FRAME
    ##only at this point will we know how many classification models have been run
    ##might be a smarter way to do this....
      if (i==1){
        nModels                <- ncol(working)
        predictions            <- matrix(0, nDataItems, nModels)
        predictions            <- as.data.frame(predictions)
        names(predictions)     <- names(working)
        row.names(predictions) <- row.names(data.train)
      }
    ##STORE PREDICTIONS IN OUTPUT FRAME
    predictions[index, ] <- working   
    
    features[i] <- paste(colnames(Feature_matrix),collapse = ",")
  }
  ## Restore defaul plot window values
#   par(mfrow=c(1,1)); 
#   par(mar=c(5.1, 4.1, 4.1, 2.1))
  output <- list(predictions=predictions,features=features)
  return(output)
}
##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------