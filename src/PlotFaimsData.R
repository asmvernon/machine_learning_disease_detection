##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##                                        Diabetes                                           ##
##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##Function to generate a plot of a single FAIMS data set
##
PlotFaimsData <- function(currentData, titleString="", dimensions=c(512, 102), outputFile=NULL){
  library(RColorBrewer)
  ##----------------------------------------------------------------------
  ## FIND USEFUL VALUES --------------------------------------------------
  ##----------------------------------------------------------------------
  dim(currentData) = dimensions
  colourTable      = heat.colors(100)
#   colourTable      = colorful
  ##----------------------------------------------------------------------
  ## PLOT IMAGE TO SCREEN ------------------------------------------------
  ##----------------------------------------------------------------------
  image(currentData, col=colourTable, axes=FALSE, main=titleString)
}
##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------