##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##                                        Diabetes                                           ##
##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##Function to perform a feature selection using a Wilcoxon rank-sum test
##
FeatureSelection <- function(dataMatrix, targetValues, nKeep=2,featMode = "wilcox",nfeats = 1000, Direction = "both",PRINT=F, LOG_file = "LOG.txt"){
  ##----------------------------------------------------------------------
  ## FIND USEFUL VALUES --------------------------------------------------
  ##----------------------------------------------------------------------
  nDataItems <- nrow(dataMatrix)
  nFeatures  <- ncol(dataMatrix)
  nMinKeep   <- 2
  startTime  <- Sys.time()
  
  
  keep <- 1:nFeatures
  
 if (featMode == "none") {
    return(keep) 
 } 
 # else if(featMode == "wilcox"){ 
    
  ##----------------------------------------------------------------------
  ## COMPUTE P-VALUES FOR WILCOXON RANK-SUM TEST -------------------------
  ##----------------------------------------------------------------------
  pValues       <- numeric(nFeatures)
  pValues[]     <- NA
  index.disease <- which(targetValues==TRUE)
  index.control <- which(targetValues==FALSE)
  data.disease  <- as.matrix(dataMatrix[index.disease,])
  data.control  <- as.matrix(dataMatrix[index.control,])
  
  for (i in keep){
    current.disease <- data.disease[, i]
    current.control <- data.control[, i]
    outputObject    <- wilcox.test(current.disease, current.control, exact=FALSE)
    pValues[i]      <- outputObject$p.value
  }
  pValues <- pValues * nFeatures ##Bonferroni correction
  
  ##----------------------------------------------------------------------
  ## DISCARD ANY FEATURES SCORING "NA" -----------------------------------
  ##----------------------------------------------------------------------
  ##assume that this is a zero-variance feature, or rejected  using MultiWilcoxonTest
  pValues[is.na(pValues)] <- max(pValues, na.rm=TRUE)
  if(featMode == "wilcox"){
    # cat("wilcox")
  	  ##----------------------------------------------------------------------
      ## DECIDE WHICH FEATURES TO KEEP ----------------------------------------
      ##----------------------------------------------------------------------
      Threshold <- quantile(pValues, nKeep/nFeatures) 
      keep      <- which(pValues < Threshold)
      if (PRINT==TRUE) cat('Threshold= ', Threshold, fill=TRUE)
      
      ##----------------------------------------------------------------------
      ## ALWAYS RETURN AT LEAST A COUPLE OF FEATURES -------------------------
      ##----------------------------------------------------------------------
      if (length(keep)<nMinKeep){
        if (PRINT==TRUE){cat("Always return at least ", nMinKeep," features....", fill=TRUE)}
        index <- order(pValues)
        keep  <- index[1:nMinKeep]
        # print(pValues[keep])    
      }
      if (PRINT==TRUE) {
        cat("nFeatures=", length(keep), fill=TRUE)
        print(Sys.time() - startTime)
      }
      # keep            <- unique(c(1:4,keep)) # FORCE PASS THE FIRST 4 FEATURES (WHICH ARE THE DEMO FEATURES)
      ## SUBSELECTION OF THE DEMO FEATURES
      # keep            <- 1:4   
      # keep            <- keep[-1] # minus Gender 
      # keep            <- keep[-2] # minus Alcohol
      # keep            <- keep[-3] # minus BMI
      # keep            <- keep[-4] # minus AGE
      return(keep)
    } else if (featMode == "step") {
      # cat("step")
      # cat(data.class(pValues),file = T)
      # save(pValues,file = "pValues.R")
      
      if(is.character(nfeats)) nfeats <- ncol(dataMatrix)
      
      # Select the top n features with the lowest p values
      all_feats         <- order(pValues)
      first_n_Features  <- 1:nfeats
      keep_feats        <- all_feats[first_n_Features]
      keep_feats        <- sort(keep_feats)       # Order the feature indexes to be used
      # cat("Features=", keep_feats[1:20], fill=TRUE)
      # cat(names(dataMatrix)[1:10],fill = T)
      # cat("keep_feats ",length(keep_feats),fill = T)
      usedataMatrix 		    <- dataMatrix[1:nrow(dataMatrix),keep_feats]
      colnames(usedataMatrix) 	<- paste("V", keep_feats,sep="")
      # cat("names",colnames(usedataMatrix)[1:10],fill = T)
      			# names(dataMatrix)[keep_feats]

      #### START WITH RANDOM FEATURES
      firstFeat         <- sample(x = names(usedataMatrix),	size = 5)
      
      ##----------------------------------------------------------------------
      ## GENERAL LINEAR MODEL TO SELECT FEATURES -----------------------------
      ##----------------------------------------------------------------------
      model.step      <- glm(formula = as.formula(paste("targetValues ~ ", paste(firstFeat, collapse="+"))), 
                             family=binomial, data=usedataMatrix)
      
      model_range     <- list(lower = as.formula(paste("targetValues ~ .")),
                          upper = as.formula(paste("targetValues ~ ", paste(names(usedataMatrix), collapse="+"))))
      
    	model.step.aic  <- stepAIC(model.step, scope = model_range,trace=F, direction=Direction)
      	
      	 	# model.step.aic  <- stepAIC(model.step, scope = model_range,trace=F, direction="forward")
      
      keepFeatures    <- paste(sort(strsplit(as.character(model.step.aic$formula)[3],split = " + ",fixed = T)[[1]]),collapse = " ")
      keepFeatures    <- strsplit(keepFeatures,split = "V")[[1]]
      keep            <- sort(as.numeric(keepFeatures[2:length(keepFeatures)])) 
      
      if(length(keep)>10){
        usedataMatrix2 		 	<- as.data.frame(dataMatrix[,keep])
        # names(usedataMatrix2) 	<- paste("V", keep,sep="")

        #### START WITH RANDOM FEATURES
        firstFeat2         <- sample(x = names(usedataMatrix2),4)
        
        ##----------------------------------------------------------------------
        ## GENERAL LINEAR MODEL TO SELECT FEATURES -----------------------------
        ##----------------------------------------------------------------------
        model.step      <- glm(formula = as.formula(paste("targetValues ~ ", paste(firstFeat2, collapse="+"))), 
                               family=binomial, data=usedataMatrix2)
        
        model_range     <- list(lower = as.formula(paste("targetValues ~ .")),
                                upper = as.formula(paste("targetValues ~ ", paste(names(usedataMatrix2), collapse="+"))))
        
        model.step.aic  <- stepAIC(model.step, scope = model_range,trace=F, direction=Direction)
        
        # model.step.aic  <- stepAIC(model.step, scope = model_range,trace=F, direction="forward")
        
        keepFeatures    <- paste(sort(strsplit(as.character(model.step.aic$formula)[3],split = " + ",fixed = T)[[1]]),collapse = " ")
        keepFeatures    <- strsplit(keepFeatures,split = "V")[[1]]
        keep            <- sort(as.numeric(keepFeatures[2:length(keepFeatures)])) 
      }
      if(length(keep)>10){
        warning("More than 10 features on second attempt at feature selection. Neural net not run")
        write("More than 10 features on second attempt at feature selection",file = LOG_file,append = T)
      }
      return(keep)
  } else {
    error("No valid feature selection specified")
  }
  
  
  # return(keep)
}
##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------
