################################################################################################ 
# Andrea Martinez-Vernon, University of Warwick
# 
# Last modified: 2018/05/04
# 
# LICENCING: VERSION 3 OF THE GNU PUBLIC LICENCE
# 
# MEMORY INTENSIC SCRIPT! 
# 
# RECOMMENDED: start R through Terminal using command:
#         > cd <DIRECTORY>
#         > R --max-ppsize 500000 
#         
#         WITHIN R ENVIROMENT:
#           > source("analysis_features.R")
# 
################################################################################################
# analysis_features.R
# 
# THIS SCRIPT WAS USED TO GENERATE THE SUPPORTING INFORMATION S10 TABLE! 
#   DATA FOR THE MAIN TABLES 6 & 7 ARE A SUBSET OF THESE DATA

####-----------------------------------------------------------------------------------------####
##        INSTALL PACKAGES
####-----------------------------------------------------------------------------------------####
## Required: glmnet, randomForest, kernlab, gbm, neuralnet, pROC, plotrix, wavethresh, RColorBrewer

# packages <- c("glmnet", "randomForest", "kernlab", "gbm", "neuralnet", "pROC", "plotrix", "wavethresh", "RColorBrewer")
# for (i in 1:length(packages)) install.packages(packages[i])
# rm(packages)
# 

####-----------------------------------------------------------------------------------------####
##        Define sources
####-----------------------------------------------------------------------------------------####
Paths <- list(dirPath  = "/Users/smrnab/code_distribution/") # ABSOLUTE PATH REQUIRED!
Paths$dataPath <- paste(Paths$dirPath,"data/DiabetesLonestar/",sep="") 
Paths$figPath  <- paste(Paths$dirPath,"fig/",sep="")
Paths$outPath  <- paste(Paths$dirPath,"out/",sep="")
Paths$srcPath  <- paste(Paths$dirPath,"src/",sep="")
################################################################################################
################################################################################################
################################################################################################
####-----------------------------------------------------------------------------------------####
##        Set working directory and load all scripts and files
####-----------------------------------------------------------------------------------------####
setwd(Paths$dirPath)

# Load files in the main source code folder
R_Files <- dir(Paths$srcPath)
for (i in 1:length(R_Files)){
  source(paste(Paths$srcPath,R_Files[i],sep=""))
}
rm(R_Files,i)

# Load libraries ----
library(glmnet)
library(randomForest)
library(kernlab)
library(gbm)
library(neuralnet)
library(pROC)
library(plotrix)
library(wavethresh)
library(RColorBrewer)
library(MASS)

####-----------------------------------------------------------------------------------------####
##        RUN RANDOM FOREST AND GLMNET WITH ALL FEATURES (featMode == "none")
####-----------------------------------------------------------------------------------------####
## RANDOM FOREST
setwd(Paths$dirPath)
Running       <- "Table_10_RF_allFeatures_2D"
sigma_exclude <- 1E-6
useModels     <- c("rf")
featureMode   <- "none"
RF_allFeatures_2D = RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,
                                                   runModels = useModels,featMode = featureMode,
                                                   DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                   exclude = sigma_exclude)
## GLMNET
setwd(Paths$dirPath)
Running       <- "Table_10_GLMNET_allFeatures_2D"
sigma_exclude <- 1E-6
useModels     <- c("glmnet")
featureMode   <- "none"
GLMNET_allFeatures_2D = RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,
                                                       runModels = useModels,featMode = featureMode,
                                                       DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                       exclude = sigma_exclude)

####-----------------------------------------------------------------------------------------####
##        RUN ALL MODELS IMPLEMENTING STEP-WISE FEATURE SELECTION
####-----------------------------------------------------------------------------------------####
## 2D both directions
setwd(Paths$dirPath)
Running       <- "Table_10_Step-wise_2D_100_feats_bothDirections"
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "step"
Direction     <- "both"

AllModels_stepwise_Features_2D_100 = RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,
                                                                    runModels = useModels,featMode = featureMode,nfeats=100,
                                                                    DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                                    exclude = sigma_exclude)

setwd(Paths$dirPath)
Running       <- "Table_10_Step-wise_2D_250_feats_bothDirections"
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "step"
Direction     <- "both"

AllModels_stepwise_Features_2D_250 = RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,
                                                                    runModels = useModels,featMode = featureMode,nfeats=250,
                                                                    DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                                    exclude = sigma_exclude)

setwd(Paths$dirPath)
Running       <- "Table_10_Step-wise_2D_500_feats_bothDirections"
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "step"
Direction     <- "both"

AllModels_stepwise_Features_2D_500 = RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,
                                                                    runModels = useModels,featMode = featureMode,nfeats=500,
                                                                    DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                                    exclude = sigma_exclude)


setwd(Paths$dirPath)
Running       <- "Table_10_Step-wise_2D_1000_feats_bothDirections"
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "step"
Direction     <- "both"

AllModels_stepwise_Features_2D_1000 = RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,
                                                                runModels = useModels,featMode = featureMode,nfeats=1000,
                                                                DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                                exclude = sigma_exclude)


setwd(Paths$dirPath)
Running       <- "Table_10_Step-wise_2D_2000_feats_bothDirections"
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "step"
Direction     <- "both"

AllModels_stepwise_Features_2D_2000 = RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,
                                                                runModels = useModels,featMode = featureMode,nfeats=2000,
                                                                DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                                exclude = sigma_exclude)


setwd(Paths$dirPath)
Running       <- "Table_10_Step-wise_2D_3000_feats_bothDirections"
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "step"
Direction     <- "both"

AllModels_stepwise_Features_2D_3000 = RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,
                                                                runModels = useModels,featMode = featureMode,nfeats=3000,
                                                                DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                                exclude = sigma_exclude)

