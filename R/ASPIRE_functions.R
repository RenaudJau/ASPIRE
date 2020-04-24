################################################
#----------ASPIRE-functions-script-------------#
################################################

#' Transformation of variable values into variable scores
#'
#' @description Calculates score value after transformation relative to the reference value and according to the chosen utility function.
#'
#' @param VAL variable value, it should only contains one value
#' @param UTIL utility function: "continuous" linear relationship, score can be above 1 ;"threshold" linear relationship,
#' score does not exceed 1 ;"thresholdalpha" linear relationship until ALPHA, then 1 ; "perso", linear relationships between points
#' of which x (values) and y (scores) are given in XVAL et YSCO parameters ; "step", 0 if below reference, 1 when above ; "stepalpha", 0 if below ALPHA, 1 when above ;
#' "stepperso", step relationship between points of which x (values) and y (scores) are given in XVAL et YSCO parameters ; "hump", similar to "continuous" relationship
#' the reference, then decreasing with the same negative slope
#' @param VAL_MEAN_REF mean value for the reference
#' @param ALPHA threshold value (cf UTIL)
#' @param XVAL x value for "perso" and "stepperso" utility fonctions (cf UTIL)
#' @param YSCO y value for "perso" and "stepperso" utility fonctions (cf UTIL)
#' @param NEG if FALSE (by default), score value can't be negative, it is transformed into 0
#'
#' @return a score value
#' 
#' @export
#' 
#' @examples VAL_tests <- seq(from = -4, to = 16, by = 1)
#' VAL_MEAN_REF_tests <- 6
#' XVAL_tests <- c(1, 6, 10)
#' YSCO_tests <- c(0, 1, 0.5)
#' 
#' Transf_ASPIRE(VAL = VAL_tests, UTIL = "continuous",VAL_MEAN_REF = VAL_MEAN_REF_tests)
#' Transf_ASPIRE(VAL = VAL_tests, UTIL = "continuous",VAL_MEAN_REF = VAL_MEAN_REF_tests, NEG = TRUE)
#' Transf_ASPIRE(VAL = VAL_tests, UTIL = "threshold",VAL_MEAN_REF = VAL_MEAN_REF_tests)
#' Transf_ASPIRE(VAL = VAL_tests, UTIL = "thresholdalpha",VAL_MEAN_REF = VAL_MEAN_REF_tests,ALPHA = 4)
#' Transf_ASPIRE(VAL = VAL_tests, UTIL = "hump",VAL_MEAN_REF = VAL_MEAN_REF_tests)
#' Transf_ASPIRE(VAL = VAL_tests, UTIL = "step",VAL_MEAN_REF = VAL_MEAN_REF_tests)
#' Transf_ASPIRE(VAL = VAL_tests, UTIL = "perso",VAL_MEAN_REF = VAL_MEAN_REF_tests,XVAL = XVAL_tests, YSCO = YSCO_tests)
Transf_ASPIRE<-function(VAL, UTIL, VAL_MEAN_REF, ALPHA = NULL, XVAL = NULL, YSCO = NULL, NEG = FALSE)
{
  if(VAL_MEAN_REF == 0) stop("Warning, reference mean is 0, score transformation is currently not possible")
  VAL <- VAL[is.na(VAL) == F]
  
  afine <- function(VAL, XVAL, YSCO){
    s <- NULL
    for(i in 1:length(VAL))
    {
      if(VAL[i] < min(XVAL, na.rm = T)) {
        X1 <- min (XVAL, na.rm = T)
        X2 <- XVAL[rank(XVAL, ties.method = "first") == 2]
      } else {
        if(VAL[i] > max(XVAL, na.rm = T)) {
          X1 <- XVAL[rank(XVAL, ties.method = "last") == (length(XVAL)-1)]
          X2 <- max(XVAL, na.rm = T)
        } else {
          X1 <-max(XVAL[XVAL <= VAL[i]], na.rm = T)
          X2 <-min(XVAL[XVAL >= VAL[i]], na.rm = T)
        }
      }
      
      Y1<-YSCO[XVAL == X1]
      Y2<-YSCO[XVAL == X2]
      coefa <- (Y2-Y1)/(X2-X1)
      coefb <- Y1-coefa*X1
      s[i] <- ifelse(VAL[i] %in% XVAL == TRUE, YSCO[XVAL == VAL[i]], coefa*VAL[i]+coefb)
    }
    return(s)
  }
  
  afinestep <- function(VAL, XVAL, YSCO){
    s <- NULL
    for (i in 1:length(VAL))
    {
      if(VAL[i] < min(XVAL, na.rm = T)) {
        s[i] <- YSCO[XVAL == min(XVAL, na.rm = T)]
      } else {
        if(VAL[i] %in% XVAL == TRUE) {
          s[i] <- YSCO[XVAL == VAL[i]]
        } else {s[i] <- YSCO[XVAL == max(XVAL[XVAL < VAL[i]], na.rm = T)]
        }
      }
      
    }
    return(s)
  }
  
  s<-if(UTIL == "continu" | UTIL == "continuous") VAL/VAL_MEAN_REF else {
    if(UTIL == "seuil" | UTIL == "threshold") ifelse(VAL < VAL_MEAN_REF,VAL/VAL_MEAN_REF,1) else {
      if(UTIL == "seuilalpha" | UTIL == "thresholdalpha") ifelse(VAL < ALPHA, VAL/ALPHA, 1) else {
        if(UTIL == "hump" | UTIL == "bosse") ifelse(VAL <= VAL_MEAN_REF, VAL/VAL_MEAN_REF, 1-((VAL-VAL_MEAN_REF)/VAL_MEAN_REF)) else {
          
          if(UTIL == "perso")
            ifelse(VAL<XVAL[1], YSCO[1], ifelse(VAL > XVAL[length(XVAL)], YSCO[length(XVAL)], afine(VAL, XVAL, YSCO)))
          else {
            
            if(UTIL == "pas" | UTIL == "step") ifelse(VAL<VAL_MEAN_REF, 0, 1) else {
              if(UTIL == "pasalpha" | UTIL == "stepalpha") ifelse(VAL<ALPHA, 0, 1) else {
                if(UTIL == "paspperso" | UTIL == "stepperso") ifelse(VAL < XVAL[1], YSCO[1], ifelse(VAL > XVAL[length(XVAL)], YSCO[length(XVAL)], afinestep(VAL, XVAL, YSCO)))
              } } } } } } }
  
  if(NEG == FALSE) {s <- ifelse(s<0, 0, s)}
  s<-s[is.na(s)==F]
  return(s)
}