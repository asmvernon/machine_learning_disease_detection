##Function to perform a 1D wavelet transform on an input FAIMS data matrix
## input = all data 
WaveletTransform2D <- function(m,dataMatrix,targetValues,dimensions=c(512, 102),cropped=FALSE){
  library(wavethresh)
  library(RColorBrewer)
  ##----------------------------------------------------------------------
  ## FIND USEFUL VALUES --------------------------------------------------
  ##----------------------------------------------------------------------
  dataMatrix         = as.matrix(dataMatrix)
  nDataItems         = nrow(dataMatrix)
  nFeatures          = ncol(dataMatrix)
  if (cropped==TRUE){
    DIMS = dimensions
    if(DIMS[1]>512) cropped = FALSE
  }
  dimensions=c(512, 102) ## override input for cropping to work properly (one less consideration)
  ##----------------------------------------------------------------------
  ## GENERATE THE WAVELET TRANSFORMED DATA -------------------------------
  ##----------------------------------------------------------------------
  for (i in 1:nDataItems){
    currentData           = dataMatrix[i,]
    dim(currentData)      = dimensions
    if (!cropped){
      add_zeros           = matrix(0,dimensions[1],(dimensions[1]-dimensions[2]))
    } else if (cropped){
      currentData         = currentData[(256-DIMS[1]/2+1):(256+DIMS[1]/2),]
      add_zeros           = matrix(0,DIMS[1],DIMS[1]-ncol(currentData)) 
      
      # currentData         = currentData[193:320,]
      # add_zeros             = matrix(0,128,26) 
    }
    working               = cbind(currentData,add_zeros)
    # dim(working)
    
    ### PLOT DATA 
#     if (m==1) { 
#       if (i==1) {
#       colourTable      = heat.colors(100)
#       
#       png(file="Cropped_Data_disease.png",pointsize =16)
#       image(working,main=names(targetValues)[i],col=colourTable)
#       dev.off()
#     } 
#     if (i==57) {
#       colourTable      = heat.colors(100)
#       png(file="Cropped_Data_control.png",pointsize =16)
#       image(working,main=names(targetValues)[i],col=colourTable)
#       dev.off()
#     }
#     }
    #### WAVELET TRANSFORM 
    current.wd            = imwd(working, filter.number=10, family="DaubLeAsymm", type="wavelet")
    nLevels               = current.wd$nlevels-1
    working.data          = current.wd$w0Lconstant
    for (j in 0:nLevels) {
      for (k in 1:4){
        working.data      = c(working.data,current.wd[[ lt.to.name(j, k) ]])
      }
    }
    if (i==1){
      data.wd            = matrix(0, nDataItems, length(working.data))
      row.names(data.wd) = row.names(dataMatrix)
    }
    data.wd[i, ]         = working.data
  }
    
# length(current.wd2$w4L4)   ## 8 levels, level 7 has length 16384 (4 components)
                                    ## level 6 has length 4096
                                    ## level 5 has length 1024 ....
# Number of columns for wavelet transform according to matrix size:
#   512x512--> 349525
#   256x256--> 87381
#   128x128--> 21845 ## additional benefit of dimensionality reduction

  return(data.wd)
}
##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------
