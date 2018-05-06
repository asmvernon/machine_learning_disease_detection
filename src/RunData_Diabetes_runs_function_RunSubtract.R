##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##                                        RunData_Diabetes_runs_FUNCTION                     ##
##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
# Rprof(line.profiling=TRUE)##run the R profileSr
##Script to analyse the TYPE 2 DIABETES (t2d) data set
## INCLUDING AUC PLOTS WITH ERROR BARS
##Small data set => just do cross-validation
######### MODIFIED TO OUTPUT ROC OBJECT, AS IT CONTAINS ALL THE INFO #####################
RunData_Diabetes_runs_function_RunSubtract <- function(PATHS, Title="",DWT,Samples="",Run=3,minusRun=1,
                                          titles="",PCA= FALSE, exclude=0,subset=FALSE,
                                          runModels = c("glmnet","rf","gp","svm","nnet","LDA"),
                                          nKeep=2,   featMode = "wilcox",Direction = "both",nfeats=1000,#multiple=TRUE,
                                          dimensions=c(512,102), cropped=FALSE,PRINT=FALSE,
                                          demoData = NULL, only_demoData = T){
  ####  INPUT DESCRIPTION #### 
  # PATHS           - list of PATHS to be used
  # Title           - to be used for files saved
  # DWT             - if 0, no Discrete Wavelet Transform (DWT); 1, 1D DWT; 2, 2D DWT 
  # Samples         - strings indicating the samples being used (e.g. "All", "31", "42", "42 random")
  # fromRun, toRun  - indicate which Runs of the samples will be analysed (e.g. 1:4 for All; 3:3 for #3) 
  # titles          - titles for the AUC curves
  # PCA             - Indicate whether PCA dimensionality reduction should be carried out
  # exclude         - pre-processing step: remove Features that have st.dev.smaller than given by 'exclude'
  # subset          - numeric VECTOR with the index of the samples to be used
  # runModels       - Indicate which classifier models to use 
  # nKeep           - 2 by default; number indicates the number of Features to be kept after 'FeatureSelection';
  #                   "large" keeps n=(0.9*number of samples) Features (90%)
  # featMode        - feature selection mode. "wilcox" by default. "none" to use all features or "step" for stepAIC
  # Direction       - direction of feature selection. "both" by default ("both", "backward" or "forward")
  # nfeats          - number of features to use for feature selection. 1000 by default. 
  # dimensions      - for 2D DWT: specifies dimensions in which data.input is wavelet-transformed
  # cropped         - for 2D DWT: cropps the data to conserve only signal region
  # PRINT           - FALSE by default
  # demoData        - provide dataframe with demo data to use to train the models with. NULL by default. 
  # only_demoData   - if demoData is given, should the classification be done only with the demoData or with both the demoData and FAIMS data
  
  #### PREVIOUS 
  ### multiple        - indicates whether the AUC plot is carried out as a split (2x2) or single window
  # setwd(PATHS$dirPath)
  ##----------------------------------------------------------------------
  ## LOAD LIBRARIES ------------------------------------------------------
  ##----------------------------------------------------------------------
  ### INSTALL PACKAGES: glmnet, randomForest, kernlab, gbm, neuralnet, pROC, plotrix, wavethresh, OOMPA, RColorBrewer
  # NOW LOADED WITHIN analysis.R
  # source("src/ClassifierModels.R")
  # source("src/CrossValidation.R")
  # source("src/FeatureSelection.R")
  # source("src/ReadInFaimsData.R")
  # source("src/WaveletTransform.R")
  # source("src/WaveletTransform2D.R")
  # source("src/PlotFaimsData.R")
  # source("src/AUCsPlot.R")
  # source("src/PlotALLRocCurve.R")
  # # source('src/titles_fun.R')
  
  start_RunData_Diabetes_runs_function <- Sys.time()
  
  ##----------------------------------------------------------------------
  ## DEFINE USEFUL VALUES  -----------------------------------------------
  ##----------------------------------------------------------------------
  input<- list(Title,DWT,Samples,Run,minusRun,PCA,runModels,exclude,subset,nKeep,featMode,Direction,nfeats,dimensions,cropped)
  names(input)<- c("Title","DWT","Samples","Run","minusRun","PCA","runModels","exclude",
                   "subset","nKeep","featMode","Direction","nfeats","dimensions","cropped")
  
  cat("\n****","Running",Title,"****",fill=TRUE)
  if(PRINT) cat(paste(names(input)," - ",input, ";",sep=""),fill=TRUE)
  
  startData <- Sys.time()
  
  if (DWT==1){
    DWT_which<-"1D DWT"
  } 
  if (DWT==2){
    DWT_which <-"2D DWT"
  }
  if (DWT==0){
    DWT_which<-"noDWT"
  }
  
  predictions <- list()
  features    <- list()
  # runs<- c("Run1","Run2","Run3","Run4")
  runs <- paste("Run",Run,"-","Run",minusRun)
  ##----------------------------------------------------------------------
  ## CREATE A FOLDER FOR ALL OUTPUT AND SET IT AS WORKING DIRECTORY ------
  ##----------------------------------------------------------------------
  DATE <- Sys.Date()  
  folders <- paste(PATHS$outPath,DATE,"_",Title,"/",sep="")
  if(!dir.exists(folders)) dir.create(folders)
  # directory<-paste(PATHS$dataPath,folders,"/",sep="")
  setwd(folders)
  
  ##----------------------------------------------------------------------
  ## DEFINE USEFUL VALUES  -----------------------------------------------
  ##----------------------------------------------------------------------
  LOG_file <- paste("LOG.txt",sep="")
  write(x = paste("Script run on",DATE), file = LOG_file, append = F)
  write(x = "INPUT:", file = LOG_file, append = T)
  write(x = paste(names(input),"\t - \t",input, ";",sep=""), file = LOG_file, append = T)
  write(x = "", file = LOG_file, append = T)
  
  ##----------------------------------------------------------------------
  ## DETERMINE NUMBER OF MODELS AND RUNS ---------------------------------
  ##----------------------------------------------------------------------
  nModels <- length(runModels)
  # nRuns   <- toRun - fromRun + 1
  nRuns   <- 1
  ##----------------------------------------------------------------------
  ## CREATE OUTPUT OBJECTS -----------------------------------------------
  ##----------------------------------------------------------------------
  auc_matrix            <- matrix(0,nRuns,nModels) # storage for the auc of each model
  row.names(auc_matrix) <- runs[1:nRuns]
  CI_low                <- matrix(0,nRuns,nModels)
  row.names(CI_low)     <- runs[1:nRuns]
  CI_high               <- matrix(0,nRuns,nModels)
  row.names(CI_high)    <- runs[1:nRuns]
  
  ### LOOP THROUGH THE DESIRED RUNS WITHIN EACH SAMPLE
  # for (m in fromRun:toRun){ 
    # m = 2 
    # OPTION TO RUN FAIMS DATA, DEMO DATA OR BOTH
    if(is.null(demoData)||(!is.null(demoData)&&!only_demoData)){
      dataPattern1 <- paste("export_matrix_000",Run,sep="")
      dataPattern2 <- paste("export_matrix_000",minusRun,sep="")
      
      # cat("****","Running",dataPattern,"****",fill=TRUE)
      
      # dataFiles   <- list.files(PATHS$dataPath, recursive=TRUE, full.names=TRUE, pattern=dataPattern)
      
      dataFiles1   <- list.files(PATHS$dataPath, recursive=TRUE, full.names=TRUE, pattern=dataPattern1)
      dataFiles2   <- list.files(PATHS$dataPath, recursive=TRUE, full.names=TRUE, pattern=dataPattern2)
      stopifnot(length(dataFiles1)==length(dataFiles2))
      nDataItems  <- length(dataFiles1)
      
      ##----------------------------------------------------------------------
      ## HANDLE nKeep INPUT --------------------------------------------------
      ##----------------------------------------------------------------------
      if (is.numeric(nKeep)){ 
        nKeep <- nKeep 
      } else if (nkeep=="large") { 
        nkeep<-floor(nDataItems*0.9)
      } else {
        stop("nKeep not properly defined")
      }
      
      ##----------------------------------------------------------------------
      ## CONSTRUCT SAMPLE NAMES ----------------------------------------------
      ##----------------------------------------------------------------------
      sampleIDs <- numeric(nDataItems)
      for (i in 1:nDataItems){
        currentString <- strsplit(dataFiles1[i], PATHS$dataPath)[[1]][2]
        currentString <- strsplit(currentString, "/")[[1]][2]
        currentString <- strsplit(currentString, " ")[[1]]
        sampleIDs[i]  <- paste(currentString[2], currentString[1], sep="_")
      }
      ##----------------------------------------------------------------------
      ## FIND THE TARGET VALUES ----------------------------------------------
      ##----------------------------------------------------------------------
      # Diabetes vs control
      targetValues                <- logical(nDataItems)
      targetValues[]              <- NA  ##do this so we can spot undefined target values
      index.disease               <- grep("DM", sampleIDs)
      index.control               <- grep("V",  sampleIDs)
      targetValues[index.disease] <- TRUE
      targetValues[index.control] <- FALSE
      names(targetValues)         <- sampleIDs
      
      ##----------------------------------------------------------------------
      ## REMOVE ANY ITEMS FOR WHICH WE DON'T HAVE TARGET VALUES --------------
      ##----------------------------------------------------------------------
      keep_dataItems  <- which(is.na(targetValues)==FALSE)
      targetValues    <- targetValues[keep_dataItems]
      dataFiles1       <- dataFiles1[keep_dataItems]
      sampleIDs       <- sampleIDs[keep_dataItems]
      nDataItems      <- length(targetValues)
      ##----------------------------------------------------------------------
      ## READ IN THE DATA ----------------------------------------------------
      ##----------------------------------------------------------------------
      data.input.1          <- ReadInFaimsData(dataFiles1) 
      data.input.2          <- ReadInFaimsData(dataFiles2) 
      data.input            <- data.input.1 - data.input.2
      nDataItems            <- nrow(data.input)
      nFeatures             <- ncol(data.input)
      row.names(data.input) <- sampleIDs
      ##----------------------------------------------------------------------
      ## WAVELET TRANSFORM ---------------------------------------------------
      ##----------------------------------------------------------------------
      if (DWT==0) { 
        data.wd <- data.input
      }
      if (DWT==1) { 
        data.wd <- WaveletTransform(data.input) 
      } 
      if (DWT==2) { 
        data.wd <- WaveletTransform2D(m,data.input,targetValues,dimensions,cropped) 
      }
      # dim(data.input):  c(125, 52224)
      # dim(data.wd):     c(125, 52395) (1D DWT)
    } #
    
    # OPTION TO RUN FAIMS DATA, DEMO DATA OR BOTH
    if(!is.null(demoData)){
      if(only_demoData){
        data.wd <- demo_data[,2:ncol(demoData)]
        
        ## FIND THE TARGET VALUES ----------------------------------------------
        #     Diabetes vs control
        sampleIDs <- demo_data$STUDY.NUMBER
        targetValues                <- logical(nrow(data.wd))
        targetValues[]              <- NA  ##do this so we can spot undefined target values
        index.disease               <- grep("DM", sampleIDs)
        index.control               <- grep("V",  sampleIDs)
        targetValues[index.disease] <- TRUE
        targetValues[index.control] <- FALSE
        names(targetValues)         <- sampleIDs
        ## REMOVE ANY ITEMS FOR WHICH WE DON'T HAVE TARGET VALUES --------------
        keep_dataItems  <- which(is.na(targetValues)==FALSE)
        targetValues    <- targetValues[keep_dataItems]
        data.wd         <- data.wd[keep_dataItems,]
        sampleIDs       <- sampleIDs[keep_dataItems]
        nDataItems      <- length(targetValues)
      }else{
        demo_labels <- demo_data[,1]
        sample_label_raw <- gsubfn::strapplyc(rownames(data.wd),pattern = "(.*)[_]",simplify = T)
        
        ## CONTROL LABELS CLEAN UP
        # DATA FOLDER LABELS HAVE O instead of 0
        sample_label <- gsub("VO0","V0",sample_label_raw)
        sample_label <- gsub("O","0",sample_label)
        extra_index <- grep("V0\\d{3}",sample_label)
        sample_label[extra_index] <- gsub("V0","V",sample_label[extra_index])
        
        ## DISEASE LABELS CLEAN UP
        # ONLY ONE MISSED LABELLED
        diabetes_index <- grep("DM",sample_label)
        sample_label[diabetes_index[which(nchar(sample_label[diabetes_index])!=5)]] <- "DM003"
        
        # MATCH SAMPLE DATA TO DEMO DATA
        demo_index  <- match(demo_labels,sample_label)
        
        nNewFeats <- length(2:ncol(demo_data))
        TXT <- paste("Adding",nNewFeats,"demographic features:",paste(names(demo_data)[2:ncol(demo_data)],collapse = ", "),"(now features",paste("V",1:nNewFeats,sep="",collapse = ", "),")")
        write(x = TXT, file = LOG_file, append = T)
        write(x = "",  file = LOG_file, append = T)
        
        data.wd           <- cbind(demo_data[,2:ncol(demo_data)],data.wd[demo_index,])
        colnames(data.wd) <- paste("V",1:ncol(data.wd),sep="")
        targetValues <- targetValues[demo_index]
      }
    } # deal with demo + faims data 
    cat("nrow(data.wd)",nrow(data.wd),fill=T)
    
    # MAKE LIFE EASIER
    if(length(fromRun:toRun)==1) m <- 1
    ##----------------------------------------------------------------------
    ## THROW OUT LOW-VARIANCE FEATURES -------------------------------------
    ##----------------------------------------------------------------------
    ##the assumption here is that these are noise features
    cat("ncol(data.wd)",ncol(data.wd),fill=T)
    sigmaValues <- apply(data.wd, 2, sd)
    keep_feats  <- which(sigmaValues>exclude)
    data.wd     <- data.wd[, keep_feats]
    # working     <- hist(sigmaValues, breaks=2e5)
    SigmaValues <- sigmaValues[which(sigmaValues>exclude)]
    # working     <- hist(SigmaValues, breaks=2e5)
    # pdf(file=paste('Histogram_',m,'.pdf',sep=""))
    # plot(working, xlim=c(0, 0.01),col="black")
    # dev.off()
    
    data.train  <- as.data.frame(data.wd) # set here to exclude certain features
    # names(data.train) <- paste("F",1:ncol(data.train),sep="") # To identify features
    #   NOT NECESARRY AS VARIABLES ARE NAMES "V#" BY DEFAULT
    
    ##----------------------------------------------------------------------
    ## SELECT RANDOM SAMPLES -----------------------------------------------
    ##----------------------------------------------------------------------
    if (is.numeric(subset)==TRUE) {
      if (m<4){
        data.train   <- data.train[subset,]
        targetValues <- targetValues[subset]
        sampleIDs    <- sampleIDs[subset]
      }
    }
    
    ##----------------------------------------------------------------------
    ## WRITE TO LOG FILE          ------------------------------------------
    ##----------------------------------------------------------------------
    write(x = paste("\t Data input and processing run time: ",format(difftime(Sys.time(), startData,units = "min"))), file = LOG_file, append = T)
    write(x = "", file = LOG_file, append = T)
    
    ###----------------------------------------------------------------------
    ## CROSS-VALIDATION ----------------------------------------------------
    ##----------------------------------------------------------------------
    out <- CrossValidation(m = m,PCA = PCA,runModels = runModels,nKeep = nKeep,data.train = data.train, 
                           targetValues = targetValues,nFolds = 10, PRINT = PRINT,LOG_file = LOG_file,
                           featMode = featMode, nfeats = nfeats,Direction = Direction) 
    
    predictions.cv <- out$predictions
    modelTypes     <- names(predictions.cv)
    nModels        <- length(modelTypes)
    
    # colors <- c("green3", "orange", "blue", "magenta","red","black")
    # colors <- c("grey79", "grey65", "grey49", "grey29", "grey9" )
    # if(m==1) if(!dir.exists("ROCplots")) dir.create("ROCplots")
    
    # pdf(file=paste('ROCplots/ROC_',Title,"_",m,'.pdf',sep=""))
    for (i in 1:nModels) {
      output <- PlotALLRocCurve(i,colors,predictions.cv[, i], targetValues, modelTypes[i])
      save(output,file = "output.rda")
      auc_matrix[m,i]    <- output[1]
      CI_low[m,i]        <- output[2]
      CI_high[m,i]       <- output[3]
    }
    # legend(0.5,0.45,col=colors, modelTypes, bg=NA, lwd=2,cex=0.8,box.col=NA)
    # while(dev.cur()>1) dev.off()
    
    predictions[[m]]	<- predictions.cv
    features[[m]]     <- out$features
  # } #
  
  if(nModels>1){
    colnames(auc_matrix)<- modelTypes
    colnames(CI_low)    <- modelTypes
    colnames(CI_high)   <- modelTypes
    names(predictions)  <- runs[1:nRuns]
  }
  
  # ### PLOT AUC VALUES FROM ALL RUNS IN ONE PLOT
  # AUCsPlotMultiple_Figure(auc_matrix,CI_low,CI_high,Title,titles, nModels)
  # 
  
  cat("****",Title,"completed successfully in",format(difftime(Sys.time(), start_RunData_Diabetes_runs_function,units = "min")),"****\n",fill=TRUE)
  
  
  write(x = "Algorithm Run Time:", file = LOG_file, append = T)
  write(x = format(difftime(Sys.time(), start_RunData_Diabetes_runs_function,units = "min")), file = LOG_file, append = T)
  
  setwd(PATHS$dirPath)
  ### COLLECT OUTPUT OBJECTS
  OUTPUT = list(auc_matrix=auc_matrix, CI_low=CI_low,CI_high=CI_high, predictions=predictions, input=input, features=features)
  # names(OUTPUT) =c("AUC","CI_low","CI_high", "predictions","input")
  return(OUTPUT)
}

