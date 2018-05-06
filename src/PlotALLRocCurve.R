##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##                                        Diabetes                                           ##
##ºººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººººº#
##Function to plot a ROC curve and AUC statistic, given input class probabilities
PlotALLRocCurve <- function(i,colors, predictions, targetValues, modelTypes,PRINT=F, LOG_file = "LOG.txt"){
  library(pROC)
  targetValues=as.numeric(targetValues)
  
  ##----------------------------------------------------------------------
  ## GENERATE A ROC CURVE ------------------------------------------------
  ##----------------------------------------------------------------------
  try({
  rocCurve          = roc(targetValues, predictions, ci=TRUE)
  auc               = as.numeric(rocCurve$auc)
  confidence        = as.numeric(rocCurve$ci)
  sensitivityValues = rocCurve$sensitivities
  specificityValues = rocCurve$specificities
  ##----------------------------------------------------------------------
  ## PLOT THE ROC CURVE --------------------------------------------------
  ##----------------------------------------------------------------------
#   lthickness = c(4.54,3.5,3,2.5,2)
#   par(mar=c(5, 5, 3, 5)+.1)
#   if (i==1){
#     plot(rocCurve, lwd=lthickness[i], cex=4,xlab="Specificity",ylab="Sensitivity", col=colors[i],bty="n")
# #     title("ROC curves")
#     
#   } else {
#   lines(rocCurve, lwd=lthickness[i], cex=4,col=colors[i])
#   }

#   title(paste(titleString, " (auc=", format(auc, digits=2), ") (95% CI: ", format(confidence[1], digits=2), ", ", format(confidence[3], digits=2), ")", sep=""))
  ##----------------------------------------------------------------------
  ## FIND A THRESHOLD TO BALANCE SENSITIVITY, SPECIFICITY ----------------
  ##----------------------------------------------------------------------
  ##use distance between null line and actual curve
  ##Obviously, this simplifies... :-)
  metricScore = (sensitivityValues - 1 + specificityValues)^2 +
                (specificityValues - 1 + sensitivityValues)^2
  index       = which.max(metricScore)
  sensitivity = sensitivityValues[index]
  specificity = specificityValues[index]
  threshold   = rocCurve$thresholds[index]
  ##----------------------------------------------------------------------
  ## COMPUTE CONFIDENCE INTERVALS FOR SENSITIVITY, SPECIFICITY -----------
  ##----------------------------------------------------------------------
  predictedClass = predictions<threshold
  truePositive   = sum((predictedClass==TRUE)  & (targetValues==TRUE))
  trueNegative   = sum((predictedClass==FALSE) & (targetValues==FALSE))
  falsePositive  = sum((predictedClass==TRUE)  & (targetValues==FALSE))
  falseNegative  = sum((predictedClass==FALSE) & (targetValues==TRUE))
  sensitivity.ci = as.numeric(binom.test(truePositive, truePositive + falseNegative)$conf.int)
  specificity.ci = as.numeric(binom.test(trueNegative, trueNegative + falsePositive)$conf.int)
  ##----------------------------------------------------------------------
  ## GENERATE ADDITIONAL PERFORMANCE METRICS -----------------------------
  ##----------------------------------------------------------------------
#  predictedClass = predictions>threshold
#  truePositive   = sum((predictedClass==TRUE)  & (targetValues==TRUE))
#  trueNegative   = sum((predictedClass==FALSE) & (targetValues==FALSE))
#  falsePositive  = sum((predictedClass==TRUE)  & (targetValues==FALSE))
#  falseNegative  = sum((predictedClass==FALSE) & (targetValues==TRUE))
#  sensitivity    = truePositive / (truePositive + falseNegative)
#  specificity    = trueNegative / (trueNegative + falsePositive)
#  precision      = truePositive / (truePositive + falsePositive)
#  recall         = sensitivity
#  f.measure      = 2 * precision*recall / (precision + recall)
  ##----------------------------------------------------------------------
  ## PRINT USEFUL INFO TO SCREEN -----------------------------------------
  ##----------------------------------------------------------------------
  titleString = modelTypes
  if (PRINT==TRUE) {
    cat(paste(titleString, "(ROC)"), fill=TRUE)
    cat("------------------", fill=TRUE)
    cat(paste("AUC         = ", format(auc,         digits=2)), "   (", format(confidence[1], digits=2), " - ",
                                                                        format(confidence[3], digits=2), ")", fill=TRUE, sep="")
    cat(paste("sensitivity = ", format(sensitivity, digits=2)), "   (", format(sensitivity.ci[1], digits=2), " - ",
                                                                        format(sensitivity.ci[2], digits=2), ")", fill=TRUE, sep="")
    cat(paste("specificity = ", format(specificity, digits=2)), "   (", format(specificity.ci[1], digits=2), " - ",
                                                                        format(specificity.ci[2], digits=2), ")", fill=TRUE, sep="")
  #  cat(paste("F-measure   = ", format(f.measure,   digits=2)), fill=TRUE)
    cat("\n")
  }
  ##----------------------------------------------------------------------
  ## WRITE INFO TO LOG FILE
  ##----------------------------------------------------------------------
  write(x = paste(titleString, "(ROC)"),file = LOG_file, append = T)
  write(x = "------------------",file = LOG_file, append = T)
  write(x = paste("AUC         = ", format(auc,digits=3), "   (", format(confidence[1], digits=3), " - ", format(confidence[3], digits=2), ")", sep=""),file = LOG_file, append = T)
  write(x = paste("sensitivity = ", format(sensitivity, digits=3), "   (", format(sensitivity.ci[1], digits=3), " - ",format(sensitivity.ci[2], digits=3), ")", sep=""),file = LOG_file, append = T)
  write(x = paste("specificity = ", format(specificity, digits=3), "   (", format(specificity.ci[1], digits=3), " - ",format(specificity.ci[2], digits=3), ")", sep=""),file = LOG_file, append = T)
  write(x = "",file = LOG_file, append = T)
  
  ##----------------------------------------------------------------------
  ## RETURN AUC for further plotting and value storing
  ##----------------------------------------------------------------------
    output=c(auc,confidence[1],confidence[3])
    return(output) 
  }) 
  
  # CHECK IF ROC FAILED
  if(!exists('output')) {
    write(x = paste(modelTypes, "- NO ROC CALCULATED"),file = LOG_file, append = T)
    write(x = "------------------",file = LOG_file, append = T)
    write(x = "",file = LOG_file, append = T)
    output=c(NA,NA,NA)
    return(output)  
  }
}

##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------

