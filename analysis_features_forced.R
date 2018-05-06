################################################################################################ 
# Andrea Martinez-Vernon, University of Warwick
# 
# Last modified: 2018/05/04
# 
# LICENCING: VERSION 3 OF THE GNU PUBLIC LICENCE
# 
# UNCOMMENT LINE 68 of FeatureSelection.R and run this file ----
#    # LINE 68: keep <- unique(c(1:4,keep))
# 
################################################################################################
# analysis_features_forced.R
# 
# THIS SCRIPT WAS USED TO GENERATE THE SUPPORTING INFORMATION S16 TABLE! 
#   DATA FOR THE MAIN TABLE 10 ARE A SUBSET OF THESE DATA

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
Paths <- list(dirPath  = ".")
Paths$dataPath <- paste(Paths$dirPath,"data/",sep="") # NOT PROVIDED
Paths$figPath  <- paste(Paths$dirPath,"fig/",sep="")
Paths$outPath  <- paste(Paths$dirPath,"out/",sep="")
Paths$srcPath  <- paste(Paths$dirPath,"src/",sep="")

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

# LOAD DEMO DATA ----
# load('data/demo_data.rda') 
#### DATA.FRAME 
##      -> ROWS: INDIVIDUALS DEMO DATA
##      -> COLS: STUDY.NUMBER, GENDER, ALCOHOL, BMI, AGE (5)

####-----------------------------------------------------------------------------------------####
####----- DEMO AS FEATS AND OR DEMO-SUBSET FAIMS   ------------------------------------------####
####-----------------------------------------------------------------------------------------####

####        Table 15 - DEMO AS FEATS ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_15_demo_feats_all"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 4, DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                cropped=F,exclude = sigma_exclude,
                                                demoData = demo_data,only_demoData = T)

####        Table 16 - DEMO-SUBSET FAIMS + DEMO AS FEATS ----
Running       <- "Table_16_faims_and_demo_feats"
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
RF_allFeatures_2D = RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,nKeep = 2,
                                                   runModels = useModels,featMode = featureMode,
                                                   DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                   exclude = sigma_exclude,only_demoData = F,demoData = demo_data)

