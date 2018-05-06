##Function to perform a 1D wavelet transform on an input FAIMS data matrix
##
WaveletTransform <- function(dataMatrix){
  library(wavethresh)
  ##----------------------------------------------------------------------
  ## FIND USEFUL VALUES --------------------------------------------------
  ##----------------------------------------------------------------------
  nDataItems         = nrow(dataMatrix)
  nFeatures          = ncol(dataMatrix)
  nWavelets          = 2^ceiling(log2(nFeatures)) - 1
  data.wd            = matrix(0, nDataItems, nWavelets)
  row.names(data.wd) = row.names(dataMatrix)
  ##----------------------------------------------------------------------
  ## GENERATE THE WAVELET TRANSFORMED DATA -------------------------------
  ##----------------------------------------------------------------------
  for (i in 1:nDataItems){
    currentData          = dataMatrix[i,]
    working              = numeric(nWavelets + 1)
    working[1:nFeatures] = currentData
    current.wd           = wd(working, filter.number=10)
    data.wd[i, ]         = current.wd$D
  }
  ##----------------------------------------------------------------------
  ## REMOVE ANY ZERO-VARIANCE FEATURES -----------------------------------
  ##----------------------------------------------------------------------
  sigmaValues = apply(data.wd, 2, sd)
  keep        = which(sigmaValues>0)
  data.wd     = data.wd[, keep]
  
  return(data.wd)
}
##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------
