##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##                                        Diabetes                                           ##
##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##Function to read in the data from a set of FAIMS text files
##
ReadInFaimsData <- function(fileList){
  ##----------------------------------------------------------------------
  ## FIND ALL THE DATA FILES TO READ IN ----------------------------------
  ##----------------------------------------------------------------------
  fileList
  nFiles     = length(fileList)
  dataMatrix = matrix(0, nFiles, 52224)
  ##----------------------------------------------------------------------
  ## READ IN ALL THE DATA FILES ------------------------------------------
  ##----------------------------------------------------------------------
  for (i in 1:nFiles){
    data.positiveIon = scan(fileList[i], skip=116, nlines=51, quiet=TRUE)
    data.negativeIon = scan(fileList[i], skip=168, nlines=51, quiet=TRUE)
    dataVector       = c(data.positiveIon, data.negativeIon)
    dataMatrix[i,]   = dataVector
  }
  ##----------------------------------------------------------------------
  ## ITEM, FEATURE NAMES -------------------------------------------------
  ##----------------------------------------------------------------------


  
  return(dataMatrix)
}
##*****************************************************************************
##*****************************************************************************
##Function to read in FAIMS files from a set of directories
##
ReadInFaimsDirectories <- function(dataPath, filePattern="export_matrix_0002"){ 
  ##----------------------------------------------------------------------
  ## FIND DATA FILES, READ IN THE DATA -----------------------------------
  ##----------------------------------------------------------------------
  dirList    = list.dirs(dataPath, recursive=FALSE)
  dataFiles  = list.files(dirList, pattern=filePattern, full.names=TRUE)
  dataMatrix = ReadInFaimsData(dataFiles)
  ##----------------------------------------------------------------------
  ## CONSTRUCT ITEM NAMES FROM THE DIRECTORY NAMES -----------------------
  ##----------------------------------------------------------------------
  nDataItems = nrow(dataMatrix)
  itemNames  = character(nDataItems)
  working    = strsplit(dirList, "/")
  for (i in 1:nDataItems)
    itemNames[i] = working[[i]][length(working[[i]])]
  row.names(dataMatrix) = itemNames

  return(dataMatrix)
}
##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------
