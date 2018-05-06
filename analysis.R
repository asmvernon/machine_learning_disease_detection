################################################################################################ 
# Andrea Martinez-Vernon, University of Warwick
# 
# Last modified: 2018/05/04
# 
# LICENCING: VERSION 3 OF THE GNU PUBLIC LICENCE
# 
################################################################################################
# analysis.R
# 
# THIS SCRIPT WAS USED TO GENERATE THE SUPPORTING INFORMATION TABLES! 
#   DATA FOR THE MAIN TABLES ARE A SUBSET OF THESE DATA

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

# LOAD DEMO DATA ----
# load('data/demo_data.rda') 
  #### DATA.FRAME 
  ##      -> ROWS: INDIVIDUALS DEMO DATA
  ##      -> COLS: STUDY.NUMBER, GENDER, ALCOHOL, BMI, AGE (5)

# TABLE 1 - RUN 1 ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_1"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 2, DWT = 2,Samples="All", fromRun=1,toRun=1,
                                                cropped=F,
                                                exclude = sigma_exclude,
                                                demoData = NULL) 
# TABLE 2 - RUN 2 ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_2"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 2, DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                cropped=F,exclude = sigma_exclude,demoData = NULL)
# TABLE 3 - RUN 3 ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_3"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 2, DWT = 2,Samples="All", fromRun=3,toRun=3,
                                                cropped=F,
                                                exclude = sigma_exclude,
                                                demoData = NULL)
# TABLE 4 - No DWT ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_4_noDWT_Run2"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 2, DWT = 0,Samples="All", fromRun=2,toRun=2,
                                                cropped=F,
                                                exclude = sigma_exclude,
                                                demoData = NULL)
# TABLE 5 - 1D Discrete Wavelet Transform ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_5_1D_DWT_Run2"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 2, DWT = 1,Samples="All", fromRun=2,toRun=2,
                                                cropped=F,
                                                exclude = sigma_exclude,
                                                demoData = NULL)
# TABLE 6 -  2D Discrete Wavelet Transform (512 x 512 matrix) ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_6_2D_DWT_512_Run2"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 2, DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                cropped=F,
                                                exclude = sigma_exclude,
                                                demoData = NULL)
# TABLE 7 -  Cropped 2D Discrete Wavelet Transform (256 x 256 matrix) ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_7_2D_DWT_256_Run2"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 2, DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                cropped=F,dimensions=c(256,256), 
                                                exclude = sigma_exclude,
                                                demoData = NULL)
# TABLE 8 -  Cropped 2D Discrete Wavelet Transform (128 x 128 matrix) ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_8_2D_DWT_128_Run2"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 2, DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                cropped=F,dimensions=c(128,128), 
                                                exclude = sigma_exclude,
                                                demoData = NULL)
# TABLE 9 -  PCA ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_9_2D_DWT_Run2_PCA"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 2, DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                cropped=F,exclude = sigma_exclude,
                                                PCA = T,demoData = NULL)
#####################################################################################
# TABLE 10 run through terminal file: analysis_features.R ----
## TOO MEMORY-INTENSIVE TO RUN THROUGH R SOFTWARE
##########################################################################################
# TABLE 11 -  Sample Run Ensemble: Run 3 – Run 1 ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_11_Run3_minusRun1"
output        <- RunData_Diabetes_runs_function_RunSubtract(PATHS= Paths, Title=Running,DWT=2,Run=3,minusRun=1,
                                                      exclude=sigma_exclude,subset=FALSE,
                                                      runModels = c("glmnet","rf","gp","svm","nnet"),
                                                      nKeep=2,   featMode = "wilcox",
                                                      cropped=FALSE,PRINT=FALSE,
                                                      demoData = NULL)
# TABLE 12 - Sample Run Ensemble: Run 1 – Run 3  ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_12_Run1_minusRun3"
output        <- RunData_Diabetes_runs_function_RunSubtract(PATHS= Paths, Title=Running,DWT=2,Run=1,minusRun=3,
                                                      exclude=sigma_exclude,subset=FALSE,
                                                      runModels = c("glmnet","rf","gp","svm","nnet"),
                                                      nKeep=2,   featMode = featureMode,
                                                      cropped=FALSE,PRINT=FALSE,
                                                      demoData = NULL)
# TABLE 13 - Sample Run Ensemble: Run Mean  ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_13_Run_mean"
PATHS= Paths; Title=Running;DWT=2;fromRun=1;toRun=3;exclude=sigma_exclude;subset=FALSE;runModels = c("glmnet","rf","gp","svm","nnet");nKeep=2
featMode = featureMode;cropped=FALSE;PRINT=FALSE;demoData = NULL
setwd(Paths$dirPath)

# TABLE 14 -  Probability Ensemble: Ensemble Mean ----
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_14_Ensemble_mean"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 2, DWT = 2,Samples="All", fromRun=1,toRun=3,
                                                cropped=F,exclude = sigma_exclude,
                                                PCA = FALSE,demoData = NULL)
# TABLE 15 - DEMO DATA ----  
sigma_exclude <- 1E-6
useModels     <- c("glmnet","rf","gp","svm","nnet")
featureMode   <- "wilcox"
Running       <- "Table_15_demo_feats_all"
output        <- RunData_Diabetes_runs_function(PATHS = Paths,Title=Running,Direction = "unused",
                                                runModels = useModels,featMode = featureMode,nfeats = "unused",
                                                nKeep = 4, DWT = 2,Samples="All", fromRun=2,toRun=2,
                                                cropped=F,exclude = sigma_exclude,
                                                demoData = demo_data,only_demoData = T)
#####################################################################################
# TABLE 16 - FAIMS + DEMO DATA ----  
# UNCOMMENT LINE 68 of FeatureSelection.R and run file: analysis_features_forced.R ----
          # LINE 68: keep <- unique(c(1:4,keep))
#####################################################################################

# TABLE 17 - p-values ----  
# DEMO DATA USED ONLY
load("/Users/smrnab/code_distribution/out/<DATE>_Table_15_demo_feats_all/predictions.rda")
predicitons_demo_only <- predictions[[1]]
# DEMO + FAIMS DATA USED
load("/Users/smrnab/code_distribution/out/<DATE>_Table_16_faims_and_demo_feats/predictions.rda")
predicitons_demo_faims <- predictions[[1]]

p_values <- numeric(5)
names(p_values) <- names(predicitons_demo_faims)
for(i in 1:5){
  TEST <- wilcox.test(predicitons_demo_faims[,i],predicitons_demo_only[,i])
  p_values[i] <- TEST$p.value
}
## REQUIRES PACKAGE xtable
# TABLE <- xtable::xtable(t(as.data.frame(p_values)),digits = 4)
# xtable::print.xtable(TABLE,caption.placement = "top")
