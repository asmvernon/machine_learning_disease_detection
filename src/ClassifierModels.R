##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##                                        Diabetes                                           ##
##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##Function to run a set of classification models on input training, test data sets
##
##This function assumes as input:
##  data.train:   a (nItems * nFeatures) data frame --> "trainingData"
##  data.test:    a (nItems * nFeatures) data frame --> "testData"
##  targetValues: a logical vector
##
##It returns a data frame containing prediction probabilities for each classification algorithm.
##These are the predicted probabililty of outcome==TRUE
##
ClassifierModels <- function(data.train, targetValues, data.test,runModels,LOG_file = "LOG.txt"){
  ##----------------------------------------------------------------------
  ## LOAD LIBRARIES ------------------------------------------------------
  ##----------------------------------------------------------------------
  if(is.element("glmnet",runModels)) library(glmnet)
  if(is.element("rf",runModels)) library(randomForest)
  if(is.element("gp",runModels)) library(gbm)
  if(is.element("svm",runModels)) library(kernlab)
  if(is.element("nnet",runModels)) library(neuralnet)
  ##----------------------------------------------------------------------
  ## PRE-ALLOCATE OUTPUT SPACE -------------------------------------------
  ##----------------------------------------------------------------------
  predictions_CV <- NULL
  ##----------------------------------------------------------------------
  ## ASSERTIONS ABOUT THE INPUT ------------------------------------------
  ##----------------------------------------------------------------------
  stopifnot(is.logical(targetValues))
  stopifnot(is.data.frame(data.train))
  stopifnot(is.data.frame(data.test))
  ##convert target values to a more conveneient form
  targetValues <- as.numeric(targetValues)
  
  ##----------------------------------------------------------------------
  ##  SPARSE LOGISTIC REGRESSION -----------------------------------------
  ##----------------------------------------------------------------------
  if(is.element("glmnet",runModels)){
    cv.fit          = cv.glmnet(as.matrix(data.train), targetValues, family="binomial", maxit=1e5, alpha=1)
    predictions.glm = predict(cv.fit, as.matrix(data.test), type="response", s="lambda.min")
    predictions.glm = as.numeric(predictions.glm)
      # NO AIC outputed 
    predictions_CV<- cbind(predictions_CV,predictions.glm)
    colnames(predictions_CV)[ncol(predictions_CV)] <- "glmnet"
  }
  ##----------------------------------------------------------------------
  ##  RANDOM FOREST ------------------------------------------------------
  ##----------------------------------------------------------------------
  if(is.element("rf",runModels)){
    rf.model       = randomForest(data.train, as.factor(targetValues), importance=TRUE, ntree=500)
    predictions.rf = predict(rf.model, data.test, type="prob")[,2]
      # NO AIC outputed 
    predictions_CV<- cbind(predictions_CV,predictions.rf)
    colnames(predictions_CV)[ncol(predictions_CV)] <- "rf"
  }
  ##----------------------------------------------------------------------
  ## GAUSSIAN PROCESS ----------------------------------------------------
  ##----------------------------------------------------------------------
  if(is.element("gp",runModels)){
    model.gp       = gausspr(data.train, as.factor(targetValues), kernel="rbfdot", scaled=FALSE, tol=0.01)
    predictions.gp = predict(model.gp, data.test, type="probabilities")
    predictions.gp = as.numeric(predictions.gp[, 2])
      # NO AIC outputed 
    predictions_CV<- cbind(predictions_CV,predictions.gp)
    colnames(predictions_CV)[ncol(predictions_CV)] <- "gp"
  }
  ##----------------------------------------------------------------------
  ##  SUPPORT VECTOR MACHINE ---------------------------------------------
  ##----------------------------------------------------------------------
  if(is.element("svm",runModels)){
    filter          = ksvm(as.factor(targetValues)~., data=data.train, kernel="rbfdot", prob.model=TRUE, kpar=list(sigma=0.05),C=5,cross=3)
    predictions.svm = predict(filter, data.test, type="probabilities")[, 2]
    # alpha(filter) # outputs the resulting support vectors 
    # error(filter) # outputs the error
      # NO AIC outputed 
    predictions_CV <- cbind(predictions_CV,predictions.svm)
    colnames(predictions_CV)[ncol(predictions_CV)] <- "svm"
  }
  ##----------------------------------------------------------------------
  ## NEURAL NETWORK ------------------------------------------------------
  ##----------------------------------------------------------------------
  if(is.element("nnet",runModels)){
      
        try({inputFormula = as.formula(paste("targetValues ~ ", paste(names(data.train), collapse="+")))
        model.nnet       = neuralnet(inputFormula, data=data.train, hidden=1, threshold=0.01, linear.output=FALSE,likelihood = TRUE)
        output.nnet      = compute(model.nnet, data.test) 
        predictions.nnet = as.numeric(output.nnet$net.result)
            })
        ##  AIC:
        # model.nnet$result.matrix[4]
        ##  BIC:
        # model.nnet$result.matrix[5]
        
    if(exists("predictions.nnet")){
      predictions_CV <- cbind(predictions_CV,predictions.nnet)
      colnames(predictions_CV)[ncol(predictions_CV)] <- "nnet"
    } else {
      predictions_CV <- cbind(predictions_CV,rep(NA,nrow(predictions_CV)))
      colnames(predictions_CV)[ncol(predictions_CV)] <- "nnet"
      write("Neural net not run",file = LOG_file,append = T)
    }
    
  }
  ##----------------------------------------------------------------------
  ## LINEAR DISCRIMINANT ANALYSIS ----------------------------------------
  ##----------------------------------------------------------------------
  try(if(is.element("LDA",runModels)){
    lda.model        = lda(data.train,grouping = targetValues)
    output.lda       = predict(lda.model,data.test)
    predictions.lda  = output.lda$posterior[,2] ## Retrieve probabilities for TRUE case of each sample
    predictions_CV <- cbind(predictions_CV,predictions.lda)
    colnames(predictions_CV)[ncol(predictions_CV)] <- "lda"
    }
  )
  ##----------------------------------------------------------------------
  ## CONSTRUCT A DATA FRAME TO HOLD THE OUTPUT PREDICTIONS ---------------
  ##----------------------------------------------------------------------
  predictions_CV             <- as.data.frame(predictions_CV)
  row.names(predictions_CV)  <- row.names(data.test)
  
  return(predictions_CV)
}
##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------



