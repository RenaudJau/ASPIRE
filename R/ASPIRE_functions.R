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

#' Creation of the ASPIRE Variable object
#'
#' @description Creates an ASPIRE Variable object
#'
#' @param values a value or vector of values for the variable, if NA, they are not taken into account
#' @param ref a value or vector of values for the reference, if NA, they are not taken into account
#' @param utility one category of utility function (cf function \code{\link{Transf_ASPIRE}})
#' @param ALPHA threshold value ALPHA (cf function \code{\link{Transf_ASPIRE}})
#' @param XVAL x values for the "perso" utility function (cf function \code{\link{Transf_ASPIRE}})
#' @param YSCO y scores for the "perso" utility function (cf function \code{\link{Transf_ASPIRE}})
#' @param NEG if FALSE (default), score values cannot be negative, it is then 0 (cf function \code{\link{Transf_ASPIRE}})
#'
#' @seealso \code{\link{Transf_ASPIRE}}, \code{\link{Variable}}, \code{\link{Objective}}, \code{\link{Project}}
#'
#' @return \item{calculated_V}{a list of values or calculated values neede for graphical purposes}
#' @return \item{summary}{Summary of the Variable object attribute}
#' 
#' @export
#'
Variable<-function(values,ref,utility = "continuous",ALPHA=NULL,XVAL=NULL,YSCO=NULL,NEG=FALSE)
{
  #Calculation of standard error
  sem<-function(x)
  {
    sqrt(var(x,na.rm=T))/sqrt(length(x))
  }
  
  #Mean values and errors
  mean_value<-if(length(values[is.na(values)==F])!=1) mean(values,na.rm = T) else values[is.na(values)==F]
  error_value<-if(length(values[is.na(values)==F])!=1) sem(values) else 0
  inf_value<-mean_value-error_value
  sup_value<-mean_value+error_value
  
  #Mean reference and errors
  mean_ref<-if(length(ref[is.na(ref)==F])!=1) mean(ref,na.rm = T) else ref[is.na(ref)==F]
  error_ref<-if(length(ref[is.na(ref)==F])!=1) sem(ref) else 0
  inf_ref<-if(length(ref[is.na(ref)==F])!=1) mean_ref-sem(ref) else ref[is.na(ref)==F]
  sup_ref<-if(length(ref[is.na(ref)==F])!=1) mean_ref+sem(ref) else ref[is.na(ref)==F]
  
  #Variables transformation
  transf_values<-NULL
  for (i in 1:length(values[is.na(values)==F])){transf_values[i]<-Transf_ASPIRE(values[is.na(values)==F][i],utility,mean_ref,ALPHA=ALPHA,XVAL=XVAL,YSCO=YSCO,NEG=NEG)}
  mean_transf_value<-if(length(transf_values)!=1) mean(transf_values,na.rm = T) else transf_values
  mean_transf_value_limited<-if(mean_transf_value>=1) 1 else mean_transf_value
  error_transf_value<-if(length(transf_values)!=1) sem(transf_values) else 0
  inf_transf_value<-mean_transf_value-error_transf_value
  inf_transf_value_limited<-if(inf_transf_value>=1) 1 else inf_transf_value
  sup_transf_value<-mean_transf_value+error_transf_value
  sup_transf_value_limited<-if(sup_transf_value>=1) 1 else sup_transf_value
  
  #References transformation
  transf_ref<-NULL
  for (i in 1:length(ref[is.na(ref)==F])){transf_ref[i]<-Transf_ASPIRE(ref[is.na(ref)==F][i],utility,mean_ref,ALPHA=ALPHA,XVAL=XVAL,YSCO=YSCO,NEG=NEG)}
  mean_transf_ref<-1
  error_transf_ref<-if(length(ref[is.na(ref)==F])!=1) sem(transf_ref) else 0
  sup_transf_ref<-mean_transf_ref+error_transf_ref
  inf_transf_ref<-mean_transf_ref-error_transf_ref
  sup_transf_ref_limited<-if(sup_transf_ref>=1) 1 else sup_transf_ref
  inf_transf_ref_limited<-if(inf_transf_ref>=1) 1 else inf_transf_ref
  
  nb_values<-length(values[is.na(values)==F])
  nb_ref<-length(ref[is.na(ref)==F])
  
  V <- list(values,ref,utility)
  names(V) <- c("values","ref","utility")
  
  #Summary
  Summary_table<-data.frame(utility,
                            nb_values,mean_value,error_value,mean_transf_value,error_transf_value,
                            nb_ref,mean_ref,error_ref,mean_transf_ref,error_transf_ref)
  
  #Calculated_Variable
  Calculated_Variable<-list(ref=ref[is.na(ref)==F],mean_ref,error_ref,inf_ref,sup_ref,nb_ref,
                            values=values[is.na(values)==F],mean_value,error_value,inf_value,sup_value,nb_values,
                            transf_ref,mean_transf_ref,error_transf_ref,sup_transf_ref,inf_transf_ref,sup_transf_ref_limited,inf_transf_ref_limited,
                            transf_values,mean_transf_value,mean_transf_value_limited,error_transf_value,inf_transf_value,inf_transf_value_limited,sup_transf_value,sup_transf_value_limited,V)
  names(Calculated_Variable)<-c("ref","mean_ref","error_ref","inf_ref","sup_ref","nb_ref",
                                "values","mean_value","error_value","inf_value","sup_value","nb_values",
                                "transf_ref","mean_transf_ref","error_transf_ref","sup_transf_ref","inf_transf_ref","sup_transf_ref_limited","inf_transf_ref_limited",
                                "transf_values","mean_transf_value","mean_transf_value_limited","error_transf_value","inf_transf_value","inf_transf_value_limited","sup_transf_value","sup_transf_value_limited","V")
  
  #Output
  Output<-list(Summary_table,Calculated_Variable)
  names(Output)[[1]]<-"summary"
  names(Output)[[2]]<-"calculated_V"
  return(Output)
}

 
#' Variable barplot
#'
#' @description barplot of variable mean value or score
#'
#' @param variab a variable object created with \code{\link{Variable}}
#' @param plot.ref a logical value indicating whether the reference should be drawn or not
#' @param ylim limits for the y axis
#' @param transf a logical value indicating whether the values should be transformated (into scores) or not
#' @param main an overall title for the plot
#' @param ... other arguments applicable to \code{\link{barplot}}
#' 
#' @export
#'
Var.barplot<-function(variab,plot.ref=TRUE,ylim=NULL,transf=FALSE,main = NULL,...)
{
  if(transf==TRUE) {
    yv<-variab$calculated_V$mean_value
    zsup<-variab$calculated_V$sup_value
    zinf<-variab$calculated_V$inf_value
    
    meanref<-variab$calculated_V$mean_ref
    refsup<-variab$calculated_V$sup_ref
    refinf<-variab$calculated_V$inf_ref} else {
      yv<-variab$calculated_V$mean_transf_value
      zsup<-variab$calculated_V$sup_transf_value
      zinf<-variab$calculated_V$inf_transf_value
      
      meanref<-variab$calculated_V$mean_transf_ref
      refsup<-variab$calculated_V$sup_transf_ref
      refinf<-variab$calculated_V$inf_transf_ref
    }
  
  if(is.null(ylim)==TRUE){YL <- c(0,1.1*max(c(zsup,refsup)))}else{YL <- ylim}
  if(is.null(main)==TRUE) main <- deparse(substitute(variab))
  xv<-barplot(yv,ylim=YL, xpd=FALSE, main=main,...)
  g<-(max(xv,na.rm=T)-min(xv,na.rm=T))/50
  g<-ifelse(g==0,0.25,g)
  
  for(i in 1:length(xv))
  {
    lines(c(xv[i],xv[i]),c(zsup[i],zinf[i]))
    lines(c(xv[i]-g,xv[i]+g),c(zsup[i],zsup[i]))
    lines(c(xv[i]-g,xv[i]+g),c(zinf[i],zinf[i]))
    if(plot.ref==TRUE) {
      lines(c(xv[i]-1.8*g,xv[i]+1.9*g),c(refsup[i],refsup[i]),lty=3)
      lines(c(xv[i]-1.8*g,xv[i]+1.9*g),c(meanref[i],meanref[i]),lty=2)
      lines(c(xv[i]-1.8*g,xv[i]+1.9*g),c(refinf[i],refinf[i]),lty=3)
    }
  }
}

#' Creation of the ASPIRE Objective object
#' @description Creates an ASPIRE Objective object
#'
#' @param var_names a vector \code{c()} including variables names defined with \code{\link{Variable}}
#' @param var_weight a vector \code{c()} of weights for each of these variables
#'
#' @seealso \code{\link{Transf_ASPIRE}}, \code{\link{Variable}}, \code{\link{Objective}}, \code{\link{Project}}
#'
#' @return \item{summary}{Summary of the Objective object attribute}
#' @return \item{name_var}{variables names given in the var_names argument}
#' @return \item{DF_Var_Obj}{A dataframe about the objectives useful for graphical purpose}
#' @return \item{Obj_WMean}{Mean objective score}
#' @return \item{Obj_Wsem}{Error for the objective}
#' @return \item{Obj_inf}{Mean objective score minus error (useful for graphical purpose)}
#' @return \item{Obj_sup}{Mean objective score plus error (useful for graphical purpose)}
#' @return \item{V}{a list of variables objects}
#' 
#' @export
#'
Objective<-function(var_names,var_weight)
{
  VarTemp<-get(var_names[1])
  VarTempCalc<-VarTemp[[2]]
  UniqueVal<-c("mean_ref","error_ref","inf_ref","sup_ref","nb_ref","mean_value","error_value","inf_value","sup_value","nb_values",
               "mean_transf_ref","error_transf_ref","sup_transf_ref","inf_transf_ref","sup_transf_ref_limited","inf_transf_ref_limited",
               "mean_transf_value","mean_transf_value_limited","error_transf_value","inf_transf_value","inf_transf_value_limited","sup_transf_value","sup_transf_value_limited")
  VarTempCalcUniqueVal<-VarTempCalc[[UniqueVal[1]]]
  for (j in 2:length(UniqueVal))
  {
    VarTempCalcUniqueVal<-c(VarTempCalcUniqueVal,VarTempCalc[[UniqueVal[j]]])
  }
  Summary_Var_table<-get(var_names[1])$summary
  Var_table<-VarTempCalcUniqueVal
  if(length(var_names)>1) {
    for (i in 2:length(var_names))
    {
      VarTemp<-get(var_names[i])
      VarTempCalc<-VarTemp[[2]]
      VarTempCalcUniqueVal<-VarTempCalc[[UniqueVal[1]]]
      for (j in 2:length(UniqueVal))
      {
        VarTempCalcUniqueVal<-c(VarTempCalcUniqueVal,VarTempCalc[[UniqueVal[j]]])
      }
      Var_table<-rbind(Var_table,VarTempCalcUniqueVal)
      Summary_Var_table<-data.frame(rbind(Summary_Var_table,VarTemp$summary))
    }
  }
  
  #-----------------Variables:
  V<-NULL
  for(i in 1:length(var_names)){
    V[[i]]<-Variable(values = get(var_names[i])$calculated_V$V$values,
                     ref = get(var_names[i])$calculated_V$V$ref,
                     utility = get(var_names[i])$calculated_V$V$utility)
  }
  names(V)<-var_names
  
  Var_table<-data.frame(name_var = var_names, coef_Var = var_weight,Var_table,row.names=NULL)
  names(Var_table)<-c("name_var","coef_var",UniqueVal)
  Summary_Var_table<-data.frame(name_var = var_names, coef_Var = var_weight,Summary_Var_table)
  
  #Weighted mean
  Obj_WMean<-weighted.mean(Var_table$mean_transf_value,var_weight)
  Obj_Wsem<-weighted.mean((Var_table$error_transf_value),var_weight) ##weighted mean of variable error
  
  #For graphical purpose
  Obj_inf<-Obj_WMean-Obj_Wsem
  Obj_sup<-Obj_WMean+Obj_Wsem
  
  Objective_val<-data.frame(Obj_WMean,Obj_Wsem,Obj_inf,Obj_sup)
  
  Output<-list(Objective_val,Summary_Var_table,Var_table,V)
  names(Output)<-c("Objective_val","Summary_Var_table","Var_table","V")
  return(Output)
}

#' objective barplot
#' @description 
#'
#' @param obj an Objective object created with \code{\link{Objective}}
#' @param plot.ref a logical value indicating whether the reference should be drawn or not
#' @param ylim limits for the y axis
#' @param las.x axis label horizontal (1) or perpendicular to the axis (2)
#' @param cex.x magnification to be used for the axis label
#' @param main an overall title for the plot
#' @param ... other arguments from \code{\link{barplot}}
#'
#' @export
#'
Obj.barplot<-function(obj,plot.ref=TRUE,ylim=NULL,las.x = 1,cex.x = 1,main = NULL,...)
{
  nn<-obj$Summary_Var_table$name_var
  yv<-obj$Var_table$mean_transf_value
  zsup<-obj$Var_table$sup_transf_value
  zinf<-obj$Var_table$inf_transf_value
  
  meanref<-rep(1,length(yv))
  refsup<-obj$Var_table$sup_transf_ref
  refinf<-obj$Var_table$inf_transf_ref
  
  if(is.null(ylim)==TRUE){YL <- c(0,1.1*max(c(zsup,refsup)))}else{YL <- ylim}
  if(is.null(main)==TRUE){main <- deparse(substitute(obj))}else{main <- main}
  
  xv<-barplot(yv,ylim=YL,xaxt="n",main = main,...)
  g<-(max(xv,na.rm=T)-min(xv,na.rm=T))/50
  g<-ifelse(g==0,0.25,g)
  
  for(i in 1:length(xv))
  {
    lines(c(xv[i],xv[i]),c(zsup[i],zinf[i]))
    lines(c(xv[i]-g,xv[i]+g),c(zsup[i],zsup[i]))
    lines(c(xv[i]-g,xv[i]+g),c(zinf[i],zinf[i]))
    if(plot.ref==TRUE) {
      lines(c(xv[i]-1.8*g,xv[i]+1.9*g),c(refsup[i],refsup[i]),lty=3)
      lines(c(xv[i]-1.8*g,xv[i]+1.9*g),c(meanref[i],meanref[i]),lty=2)
      lines(c(xv[i]-1.8*g,xv[i]+1.9*g),c(refinf[i],refinf[i]),lty=3)
    }
    
  }
  axis(side = 1, at=xv, labels = nn, las=las.x, cex.axis=cex.x, tick = FALSE)
}

#' Objective radar diagram
#' @description draw a radar diagram with variables scores of one objective 
#' @param obj an objective object created with \code{\link{Objective}}
#' @param limited a logical value indicating whether transformed values above 1 should be plotted (therefore avoiding plot distortion)
#' @param transp a value between 0 and 1 indicating transparancy of the radar polygon
#' @param col radar polygon color, can be any color managed by R, but \code{transp} won't work if other values "c1", "c..", "c8" are used, corresponding to basic R colors from 1 to 8
#' @param main text for main title
#' @param plot.ref a logical value indicating whether the reference should be drawn or not
#' @param ... any other arguments applicable to \code{\link{fmsb::radarchart}}
#'
#' @export
#' @import fmsb
#'
#' @details The function uses the function \code{\link{radarchart}} from the package \code{\link{fmsb}} Minato Nakazawa (2014). fmsb: Functions for medical statistics book with some demographic data. R package version 0.7.0.
#'
Obj.radar.plot<-function(obj,limited=TRUE,transp=0.5,col="c7",main="Variables scores",plot.ref=TRUE,...)
{
  Val_radar<-data.frame(rbind(rep(1,length(obj$Var_table$name_var)),
                              rep(0,length(obj$Var_table$name_var)),
                              obj$Var_table$mean_transf_value,
                              obj$Var_table$inf_transf_value,
                              obj$Var_table$sup_transf_value))
  if(limited==TRUE){Val_radar <- data.frame(apply(Val_radar,c(1,2),function(x) ifelse(x>1,1,x)))}
  names(Val_radar)<-obj$Var_table$name_var
  if(plot.ref==TRUE) {Val_radar<-data.frame(rbind(Val_radar,rep(c(1),ncol(Val_radar))))
  Val_radar<-data.frame(rbind(Val_radar,obj$Var_table$inf_transf_ref))
  Val_radar<-data.frame(rbind(Val_radar,obj$Var_table$sup_transf_ref))}
  #Creation of colors
  colnew<-rgb(c(0/255,255/255,34/255,24/255,152/255,238/255,255/255,112/255)
              ,c(0/255,48/255,139/255,116/255,245/255,48/255,215/255,112/255)
              ,c(0/255,48/255,34/255,205/255,255/255,167/255,0/255,112/255),alpha=rep(transp,8))
  coltab<-data.frame(colnew,colnum=paste0("c",c(1:8)))
  
  coul<-if(col%in%coltab$colnum==TRUE) as.character(coltab$colnew[coltab$colnum==col]) else col
  
  #The plot
  radarchart(Val_radar,plty=c(1,2,2,1,3,3),pty=c(16,32,32,32,32,32),pcol=c(1,1,1,"Gray70","Gray70","Gray70"),pfcol=c(coul,NA,NA,NA,NA,NA),cglcol="Gray90",new=T,title=main,...)
}

#' Creation of the ASPIRE Project object
#' 
#' @description Creates an ASPIRE Project object
#'
#' @param obj_name a vector \code{c()} including objectives names defined with \code{\link{Objective}}
#' @param stakeholders a dataframe \code{data.frame()} of objectives weights with a column for each staekholder (in the same order as  \code{stakeholders}) and the objectives as row  (in the same order than \code{obj_name})
#' @param obj_weight a vector \code{c()} of weights for all the objectives
#'
#' @seealso \code{\link{Transf_ASPIRE}}, \code{\link{Variable}}, \code{\link{Objective}}, \code{\link{Project}}
#'
#' @return \item{name_objective}{objectives names given in the obj_name argument}
#' @return \item{DF_Obj_Proj}{a dataframe with a row by objective and including the values calculated with \code{\link{Objective}}}
#' @return \item{Proj_WMean}{weighted mean of objectives scores}
#' @return \item{Proj_Wsem}{weighted mean of objectives scores errors}
#' @return \item{Proj_inf}{weighted mean of objectives scores minus error (useful for graphical purpose)}
#' @return \item{Proj_sup}{weighted mean of objectives scores plus error (useful for graphical purpose)}
#' @return \item{summary}{A summary dataframe of Project object}
#' @return \item{Variables}{A list of objects obtained from \code{\link{Variable}}, one per variable}
#' @return \item{Objectives}{A list of objects obtained from \code{\link{Objective}}, one per objective}
#' 
#' @export
#'
Project<-function(obj_name, stakeholders, obj_weight)
{
  name_objective<-obj_name
  names(obj_weight)<-paste0("Coef_",stakeholders)
  
  #summary aggregation
  Summary_Var_table<-get(as.character(obj_name[1]))$Summary_Var_table
  Var_table<-get(as.character(obj_name[1]))$Var_table
  Obj_table<-get(as.character(obj_name[1]))$Objective_val
  for(i in 2:length(obj_name))
  {
    Summary_Var_table<-rbind(Summary_Var_table,get(as.character(obj_name[i]))$Summary_Var_table)
    Var_table<-rbind(Var_table,get(as.character(obj_name[i]))$Var_table)
    Obj_table<-rbind(Obj_table,get(as.character(obj_name[i]))$Objective_val)
  }
  
  Obj_table<-data.frame(name_objective,obj_weight,Obj_table)
  Summary_Obj_table<-Obj_table[,1:(ncol(Obj_table)-2)]
  
  #Weighted mean and error
  Proj_WMean<-NULL
  Proj_Wsem<-NULL
  Proj_inf<-NULL
  Proj_sup<-NULL
  for(i in 1:length(stakeholders))
  {
    Proj_WMean[i]<-weighted.mean(Summary_Obj_table$Obj_WMean,Summary_Obj_table[,i+1])
    Proj_Wsem[i]<-weighted.mean(Summary_Obj_table$Obj_Wsem,Summary_Obj_table[,i+1])
    Proj_inf[i]<-Proj_WMean[i]-Proj_Wsem[i]
    Proj_sup[i]<-Proj_WMean[i]+Proj_Wsem[i]
  }
  
  #-----------------Variables:
  V<-NULL
  for(i in 1:length(Summary_Var_table$name_var)){
    V[[i]]<-Variable(values = get(as.vector(Summary_Var_table$name_var[i]))$calculated_V$V$values,
                     ref = get(as.vector(Summary_Var_table$name_var[i]))$calculated_V$V$ref,
                     utility = get(as.vector(Summary_Var_table$name_var[i]))$calculated_V$V$utility)
  }
  names(V)<-Summary_Var_table$name_var
  
  #-----------------Objectives :
  O<-NULL
  for(i in 1:length(Summary_Obj_table$name_objective)){
    O[[i]]<-Objective(var_names = names(get(as.vector(Summary_Obj_table$name_objective[i]))$V),
                      var_weight = Summary_Var_table$coef_Var[Summary_Var_table$name_var
                                                              %in%names(get(as.vector(Summary_Obj_table$name_objective[i]))$V)])
  }
  names(O)<-Summary_Obj_table$name_objective
  
  Summary_Proj_Score<-data.frame(stakeholders = stakeholders, Score = Proj_WMean, Error = Proj_Wsem)
  Proj_table<-data.frame(Summary_Proj_Score,Proj_inf,Proj_sup)
  Summary_All<-list(Scores = Summary_Proj_Score, Objectives = Summary_Obj_table, Variables = Summary_Var_table)
  
  Output<-list(Summary_Proj_Score,Proj_table,Summary_All,Var_table,V,O)
  names(Output)<-c("Summary_Proj_Score","Proj_table","Summary_All","Var_table","Variables","Objectives")
  return(Output)
}

#' Proj.obj.barplot
#'
#' @description displays a barplot of objectives scores
#'
#' @param proj project name defined with \code{\link{Project}} or \code{\link{ASPIRE_all}}
#' @param ylim limits for the y axis
#' @param las.x axis label horizontal (1) or perpendicular to the axis (2)
#' @param cex.x magnification to be used for the axis label 
#' @param ... other arguments applicable to \code{\link{barplot}}
#'
#' @export
#'
Proj.obj.barplot<-function(proj,ylim=NULL,las.x = 1,cex.x = 1,...)
{
  
  nn<-proj$Summary_All$Objectives$name_objective
  yv<-proj$Summary_All$Objectives$Obj_WMean
  zsup<-proj$Summary_All$Objectives$Obj_WMean + proj$Summary_All$Objectives$Obj_Wsem
  zinf<-proj$Summary_All$Objectives$Obj_WMean - proj$Summary_All$Objectives$Obj_Wsem
  
  if(is.null(ylim)==TRUE){YL <- c(0,1.1*max(zsup))} else {YL <- ylim}
  
  xv<-barplot(yv,ylim=YL,xaxt="n",...)
  g<-(max(xv,na.rm=T)-min(xv,na.rm=T))/50
  g<-ifelse(g==0,0.25,g)
  
  for(i in 1:length(xv))
  {
    lines(c(xv[i],xv[i]),c(zsup[i],zinf[i]))
    lines(c(xv[i]-g,xv[i]+g),c(zsup[i],zsup[i]))
    lines(c(xv[i]-g,xv[i]+g),c(zinf[i],zinf[i]))
  }
  
  axis(side = 1, at=xv, labels = nn, las=las.x, cex.axis=cex.x, tick = FALSE)
}

#' Proj.stak.barplot
#'
#' @description displays a barplot of stakeholders project scores
#'
#' @param proj project name defined with \code{\link{Project}} or \code{\link{ASPIRE_all}}
#' @param ylim limits for the y axis
#' @param las.x axis label horizontal (1) or perpendicular to the axis (2)
#' @param cex.x magnification to be used for the axis label 
#' @param ... other arguments applicable to \code{\link{barplot}}
#'
#' @export
#'
Proj.stak.barplot<-function(proj,ylim=NULL,las.x = 1,cex.x = 1,...)
{
  
  nn<-proj$Summary_All$Scores$stakeholders
  yv<-proj$Summary_All$Scores$Score
  zsup<-proj$Summary_All$Scores$Score + proj$Summary_All$Scores$Error
  zinf<-proj$Summary_All$Scores$Score - proj$Summary_All$Scores$Error
  
  if(is.null(ylim)==TRUE){YL <- c(0,1.1*max(zsup))} else {YL <- ylim}
  
  xv<-barplot(yv,ylim=YL,xaxt="n",...)
  g<-(max(xv,na.rm=T)-min(xv,na.rm=T))/50
  g<-ifelse(g==0,0.25,g)
  
  for(i in 1:length(xv))
  {
    lines(c(xv[i],xv[i]),c(zsup[i],zinf[i]))
    lines(c(xv[i]-g,xv[i]+g),c(zsup[i],zsup[i]))
    lines(c(xv[i]-g,xv[i]+g),c(zinf[i],zinf[i]))
  }
  
  axis(side = 1, at=xv, labels = nn, las=las.x, cex.axis=cex.x, tick = FALSE)
}

#' Proj.complete.barplot
#'
#' @description displays a barplot of stakeholders project scores
#'
#' @param proj project name defined with \code{\link{Project}} or \code{\link{ASPIRE_all}}
#' @param col.stak color of stakeholders plot
#' @param col.obj color of objectives plot
#' @param col.var color of variable scores plot (repeated if only one color is provided)
#' @param plot.ref a logical value indicating whether the reference should be drawn or not
#' @param las.x axis label horizontal (1) or perpendicular to the axis (2)
#' @param cex.x magnification to be used for the axis label
#'
#' @export
#'
Proj.complete.barplot<-function(proj,
                                col.stak=NULL, col.obj=NULL, col.var=NULL,
                                plot.ref=TRUE,
                                las.x = 1,cex.x = 1)
{
  nplot<-length(proj$Summary_All$Objectives$name_objective)+2
  par(mfrow=n2mfrow(nplot))
  
  if(is.null(col.stak)==TRUE){col.stak <- 8}else{col.stak <- col.stak}
  if(is.null(col.obj)==TRUE){col.obj <- 8}else{col.obj <- col.obj}
  
  if(is.null(col.var)==FALSE & length(col.var)!=1 & length(col.var)!=length(proj$Summary_All$Objectives$name_objective)){
    stop("numbers of elements in col.var does not correspond to objective number")} 
  if(is.null(col.var)==TRUE){col.var <- rep(8,length(proj$Summary_All$Objectives$name_objective))}else{
    if(length(col.var)==1) {
      col.var<- rep(col.var,length(proj$Summary_All$Objectives$name_objective))
    }else{col.var<- col.var}}
  
  Proj.stak.barplot(proj = proj, las.x = las.x, cex.x = cex.x, 
                    col = col.stak, main = "Stakeholders scores")
  
  Proj.obj.barplot(proj = proj, las.x = las.x, cex.x = cex.x, 
                   col = col.obj, main = "Objectives scores")
  
  
  for (i in 1:(nplot-2))
  {
    Obj.barplot(obj = proj$Objectives[i][[1]], 
                col = col.var[i], main=paste0("Obj: ",proj$Summary_All$Objectives$name_objective[i]),
                las.x = las.x, cex.x = cex.x, font.main = 1,plot.ref = plot.ref)
  }
  par(mfrow=c(1,1))
}

#' Objective scores radar diagram
#'
#' @description displays radar diagram of objectives scores
#'
#' @param proj project name defined with \code{\link{Project}} or \code{\link{ASPIRE_all}}
#' @param limited a logical value indicating whether transformed values above 1 should be plotted (therefore avoiding plot distortion)
#' @param transp a value between 0 and 1 indicating transparancy of the radar polygon
#' @param col radar polygon color, can be any color managed by R, but \code{transp} won't work if other values "c1", "c..", "c8" are used, corresponding to basic R colors from 1 to 8
#' @param main text for main title
#' @param ... any other arguments applicable to \code{\link{fmsb::radarchart}}
#'
#' @export
#' @import fmsb
#'
#' @details The function uses the function \code{\link{radarchart}} from the package \code{\link{fmsb}} Minato Nakazawa (2014). fmsb: Functions for medical statistics book with some demographic data. R package version 0.7.0.
#'
Proj.radar.plot<-function(proj,limited=TRUE,transp=0.5,col="c7",main="Objectives scores",...)
{
  Val_radar<-data.frame(rbind(rep(1,length(proj$Summary_All$Objectives$name_objective)),
                              rep(0,length(proj$Summary_All$Objectives$name_objective)),
                              proj$Summary_All$Objectives$Obj_WMean,
                              proj$Summary_All$Objectives$Obj_WMean - proj$Summary_All$Objectives$Obj_Wsem,
                              proj$Summary_All$Objectives$Obj_WMean + proj$Summary_All$Objectives$Obj_Wsem))
  if(limited==TRUE){Val_radar <- data.frame(apply(Val_radar,c(1,2),function(x) ifelse(x>1,1,x)))}
  names(Val_radar)<-proj$Summary_All$Objectives$name_objective
  
  #Creation of colors
  colnew<-rgb(c(0/255,255/255,34/255,24/255,152/255,238/255,255/255,112/255)
              ,c(0/255,48/255,139/255,116/255,245/255,48/255,215/255,112/255)
              ,c(0/255,48/255,34/255,205/255,255/255,167/255,0/255,112/255),alpha=rep(transp,8))
  coltab<-data.frame(colnew,colnum=paste0("c",c(1:8)))
  
  coul<-if(col%in%coltab$colnum==TRUE) as.character(coltab$colnew[coltab$colnum==col]) else col
  
  #The plot
  radarchart(Val_radar,plty=c(1,2,2,1,3,3),pty=c(16,32,32,32,32,32),pcol=c(1,1,1,"Gray70","Gray70","Gray70"),
             pfcol=c(coul,NA,NA,NA,NA,NA),cglcol="Gray90",new=T,title=main,...)
}

#' All scores radar diagram
#'
#' @description displays either one or several radar diagrams of all variables scores
#'
#' @param proj project name defined with \code{\link{Project}} or \code{\link{ASPIRE_all}}
#' @param transp a value between 0 and 1 indicating transparancy of the radar polygon
#' @param col  polygon color, can be any color managed by R, but \code{transp} won't work if other values than "c1", "c..", "c8" are used, corresponding to basic R colors from 1 to 8. If only one color is provided and \code{AllInOne=FALSE}, color is repeated  for all objectives
#' @param AllInOne a logical value indicating whether all the variables scores should be displeyd on one radardiagram or assembled by objectives
#' @param limited a logical value indicating whether transformed values above 1 should be plotted (therefore avoiding plot distortion)
#' @param main text for main title, only applicable if \code{AllInOne=TRUE}
#' @param plot.ref a logical value indicating whether the reference should be drawn or not
#' @param cex.lab magnification to be used for the labels 
#'
#' @export
#' @import fmsb
#'
#' @details The function uses the function \code{\link{radarchart}} from the package \code{\link{fmsb}} Minato Nakazawa (2014). fmsb: Functions for medical statistics book with some demographic data. R package version 0.7.0.
#'
Proj.all.radar.plot<-function(proj,transp=0.5,col=NULL,AllInOne=TRUE,limited=TRUE,main="",plot.ref=TRUE,cex.lab=1)
{
  # colors
  transp<-transp
  colnew<-rgb(c(0/255,255/255,34/255,24/255,152/255,238/255,255/255,112/255)
              ,c(0/255,48/255,139/255,116/255,245/255,48/255,215/255,112/255)
              ,c(0/255,48/255,34/255,205/255,255/255,167/255,0/255,112/255),alpha=rep(transp,8))
  coltab<-data.frame(colnew,colnum=paste0("c",c(1:8)))
  
  DF <- proj$Summary_All$Variables
  
  if(AllInOne==TRUE){
    Val_radar<-data.frame(rbind(rep(1,length(DF$name_var)),
                                rep(0,length(DF$name_var)),
                                DF$mean_transf_value,
                                DF$mean_transf_value - DF$error_transf_value,
                                DF$mean_transf_value + DF$error_transf_value))
    if(plot.ref==TRUE) {Val_radar<-data.frame(rbind(Val_radar,rep(c(1),ncol(Val_radar))))
    Val_radar<-data.frame(rbind(Val_radar,(1-DF$error_transf_ref)))}
    if(limited==TRUE){Val_radar <- data.frame(apply(Val_radar,c(1,2),function(x) ifelse(x>1,1,x)))}
    names(Val_radar)<-DF$name_var
    
    if(is.null(col)==TRUE){col <- "c7"}else{if(length(col)>1){col <- col[1]}}
    
    coul<-if(col%in%coltab$colnum=="TRUE") as.character(coltab$colnew[coltab$colnum==col]) else col
    
    radarchart(Val_radar,plty=c(1,2,2,1,3,3),pty=c(16,32,32,32,32,32),
               pcol=c(1,1,1,"Gray70","Gray70","Gray70"),pfcol=c(coul,NA,NA,NA,NA,NA),
               cglcol="Gray90",new=T,title=main,vlcex = cex.lab)
    
  } else{
    nplot<-length(proj$Summary_All$Objectives$name_objective)
    par(mfrow=n2mfrow(nplot),mar=c(1,1,2.5,1))
    
    if(is.null(col)==TRUE){col <- "c7"}
    if(length(col)==1) {col <- rep(col,length(proj$Summary_All$Objectives$name_objective))}
    if(length(col)!=length(proj$Summary_All$Objectives$name_objective)) stop("Number of colors provided does not correspond to the number of objectives")
    
    for (i in 1:nplot)
    {
      
      Obj.radar.plot(obj = proj$Objectives[i][[1]],
                     main=paste("Obj:",proj$Summary_All$Objectives$name_objective[i]),
                     col=col[i],limited=limited,plot.ref = plot.ref,transp = transp,vlcex = cex.lab)
    }
    par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))
  }
}

#' Score wheel
#'
#' @description Draw a score wheel inspired from the recovery wheel of SER Standards (McDonald et al., 2016)
#'
#' @param lowercat A vector of scores labels
#' @param supercat A vector of categories labels
#' @param scores A numeric vector of scores
#' @param col.score The color of achieved scores
#' @param col.null The color of unachievde scores
#' @param col.lines The color of lines and circles
#' @param col.border The color of the border containing categories labels
#' @param cex.sup An expansion factor for categories labels
#' @param cex.low An expansion factor for scores labels
#' @param val.max theoretical score maximum value (5 by defaut)
#' @param col.sup text color for categories labels
#' @param col.low text color for scores labels
#' 
#' @details uses function from plotrix package Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12.
#' 
#' @export
#' @import plotrix
#' 
#' @examples # data to draw the same scores as in McDonald et al., 2016
#' sousTcat<-c("Spatial mosaic","All trophic levels","All strata present",
#' "No undesirable species","Desirable animals","Desirable plants",
#' "Water chemo-physical","Substrate chemical","Substrate physical",
#' "Contamination","Invasive species","Over-utilization",
#' "Habitat links","Gene flows","Landscape flows",
#' "Resilience/recruitment","Habitat & interactions","Productivity/cycling")
#' superTcat<-c(rep(c("STRUCTURAL DIVERSITY","SPECIES COMPOSITION", "PHYSICAL CONDITIONS", 
#' "ABSENCE OF THREATS", "EXTERNAL EXCHANGES", "ECOSYSTEM FUNCTION"),each=3))
#' scoTres<-c(2,2,3,4,2,4,4,4,4,5,3,5,3,2,2,0,2,2)
#' 
#' Wheelscores(sousTcat,superTcat,scoTres,col.lines = "cadetblue4",col.score = "darkolivegreen2",
#' col.border = "gray97",cex.sup = 1.2,cex.low = 0.6)
Wheelscores<-function(lowercat,supercat,scores,col.score='#a1d99b',col.null='white',
                      col.lines="black",col.border='grey',cex.sup=1,cex.low=0.7,val.max=5,
                      col.low = 1, col.sup = 1)
{
  pie(1, radius=1, init.angle=90, col=col.border, border = NA, labels='')
  
  col.score<-col.score
  col.null<-col.null
  
  scores <- sapply(scores,function(x) ifelse(x>val.max,val.max,x))
  scores <- scores*5/val.max
  
  soscores<-sort(unique(scores),decreasing = TRUE)
  for(i in 1:length(soscores))
  {
    col_cir<-ifelse(scores>=soscores[i],col.score,col.null)
    floating.pie(0,0,rep(c(1),length(scores)),radius=soscores[i]/6, startpos=0, col=as.character(col_cir),border=NA)
  }
  
  for(i in 1:6)
  {
    lwdcir<-ifelse(i>=5, 2, 1)
    draw.circle(0,0,i/6,border = col.lines, lwd = lwdcir)
  }
  
  nb_cat<-length(lowercat)
  for(i in 1:nb_cat){
    segments(x0 = 0,y0=0,x1 = 5/6*cos(i*pi/(nb_cat/2)), y1 = 5/6*sin(i*pi/(nb_cat/2)),col = col.lines)
    rot<-ifelse(i>=(nb_cat/4) & i<3*(nb_cat/4), 180, 0)
    text(x= 0.5*cos((i-0.5)*pi/(nb_cat/2)), y= 0.5*sin((i-0.5)*pi/(nb_cat/2)),
         labels = lowercat[i], srt=rot+180/pi*(i-0.5)*pi/(nb_cat/2),cex = cex.low, col = col.low)
  }
  
  largsupercat<-as.vector(tapply(supercat,factor(supercat,levels=unique(supercat)),length))
  angline<-0
  for(i in 1:length(largsupercat)){
    angline<-angline+largsupercat[i]
    segments(x0 = 0,y0=0,x1 = cos(angline*pi/(nb_cat/2)), y1 = sin(angline*pi/(nb_cat/2)),
             lwd = 2,col=col.lines)
    angtext<-angline-(largsupercat[i]/2)
    arctext(x = as.character(unique(supercat)[i]), center = c(0, 0), radius = 5.5/6,col = col.sup, 
            middle = angtext*pi/(nb_cat/2),cex=cex.sup,clockwise = ifelse(angtext>(nb_cat/2),FALSE,TRUE))
  }
}

#' ASPIRE project creation with 3 dataframes
#'
#' @description Creates an ASPIRE project object based on three formated dataframe
#'
#' @param variable_df the dataframe containing variables data, each variable has two columns: one named by the variable name and containing data from the project, one named by the variable + '_Ref' and containing data from the reference. If there is missing values, 'NA' should be used.
#' @param objective_df the dataframe containing objective data, the first column, named 'Variables', contains variables names, the second, named 'Utility' contains variable utilities (cf function \code{\link{Transf_ASPIRE}}), then there is one column by objective, named by objectives names and containing weight of variables per objective
#' @param project_df the dataframe conatining project data, the first column, named 'Objectives' contains objectives names, the other columns, named by stakeholders names contains objectives weights by stakeholders
#'
#' @return \item{Summary_Proj_Score}{A dataframe of scores for each stakeholder}
#' @return \item{Proj_table}{A dataframe of scores for each stakeholder with errors (useful for graphical purpose)}
#' @return \item{Summary_All}{A list of dataframe with all calculated values, scores and errors for objectives ad variables}
#' @return \item{Var_table}{A dataframe with variables values, scores and errors}
#' @return \item{RecovW_data}{A dataframe formated to be used with \code{\link{Wheelscores.aspire}}}
#' @return \item{Variables}{A list of objects obtained from \code{\link{Variable}}, one per variable}
#' @return \item{Objectives}{A list of objects obtained from \code{\link{Objective}}, one per objective}
#' 
#' @export
#'
ASPIRE_all<-function(variable_df,objective_df,project_df)
{
  #--------------Check up:
  if(names(project_df)[1]=="Objectifs") names(project_df)[1] <- "Objectives"
  if(names(project_df)[1]!="Objectives"){
    stop("Warning, project dataframe does not contain a first column named 'Objectives' ")}
  
  if(setequal(as.character(project_df$Objectives),names(objective_df)[-c(1,2)])==FALSE){
    stop("Warning, project dataframe objectives don't correspond to objective dataframe objectives names. 
         ASPIRE analyses can't be continued")}
  
  Ref_Var<-NULL
  nam_Ref<-NULL
  for (i in 1:ncol(variable_df)){
    Ref_Var[i]<-if(substr(names(variable_df)[i],
                          start=(nchar(names(variable_df)[i])-3),
                          stop=nchar(names(variable_df)[i]))=="_Ref") "Ref" else "Var"
    nam_Ref[i]<-if(Ref_Var[i]=="Ref") substr(names(variable_df)[i],
                                             start=1,stop=nchar(names(variable_df)[i])-4) else names(variable_df)[i]
  }
  
  if(setequal(nam_Ref[Ref_Var=="Var"],nam_Ref[Ref_Var=="Ref"])==FALSE) {
    stop("Warning, there is not one reference column by variable,
         or reference are not in the same order as the variables values. 
         ASPIRE analyses can't be continued")}
  
  if(setequal(nam_Ref[Ref_Var=="Var"],as.character(objective_df$Variables))==FALSE) {
    stop("Warning, variables names in Objective dataframe don't corespond to variables names in Variable dataframe.
         ASPIRE analyses can't be continued")}
  
  
  if(names(objective_df)[2]=="Utilite") names(objective_df)[2] <- "Utility"
  if(names(objective_df)[2]!="Utility"){
    stop("Warning, objectives dataframe does not contain a second column named 'Utility' ")}
  
  Utility<-NULL
  for(i in 1:nrow(objective_df))
  {
    Utility[i]<-if(objective_df$Utility[i]%in%c("continu","continuous","seuil","threshold",
                                                "seuilalpha","thresholdalpha","hump","bosse","perso",
                                                "pas","pasalpha","stepalpha","paspperso","stepperso")==FALSE) 1 else 0
  }
  if(sum(Utility)>0) stop("Warning, at least one utility is not recognized. ASPIRE analyses can't be continued")
  
  #-----------------Variables:
  V<-NULL
  for(i in 1:nrow(objective_df)){
    V[[i]]<-Variable(values = variable_df[is.na(variable_df[,i])==FALSE,i],
                     ref = variable_df[,Ref_Var=="Ref"][is.na(variable_df[,Ref_Var=="Ref"][,i])==FALSE,i],
                     utility = objective_df$Utility[i])
  }
  names(V)<-objective_df$Variables
  
  
  #-----------------Objectives:
  O<-NULL
  for(j in 1:nrow(project_df)){
    var_names <- as.character(objective_df$Variables[objective_df[,j+2]>0])
    Var_weight <- objective_df[,j+2][objective_df[,j+2]>0]
    name_var<-var_names
    n<-c(1:length(V))[objective_df[,j+2]>0]
    
    VarTemp<-V[[n[1]]]
    VarTempCalc<-VarTemp[[2]]
    UniqueVal<-c("mean_ref","error_ref","inf_ref","sup_ref","nb_ref","mean_value","error_value","inf_value","sup_value","nb_values",
                 "mean_transf_ref","error_transf_ref","sup_transf_ref","inf_transf_ref","sup_transf_ref_limited","inf_transf_ref_limited",
                 "mean_transf_value","mean_transf_value_limited","error_transf_value","inf_transf_value","inf_transf_value_limited","sup_transf_value","sup_transf_value_limited")
    VarTempCalcUniqueVal<-VarTempCalc[[UniqueVal[1]]]
    for (jj in 2:length(UniqueVal))
    {
      VarTempCalcUniqueVal<-c(VarTempCalcUniqueVal,VarTempCalc[[UniqueVal[jj]]])
    }
    Summary_Var_table<-V[[n[1]]]$summary
    Var_table<-VarTempCalcUniqueVal
    if(length(var_names)>1) {
      for (i in 2:length(var_names))
      {
        k<-n[i]
        VarTemp<-V[[k]]
        VarTempCalc<-VarTemp[[2]]
        VarTempCalcUniqueVal<-VarTempCalc[[UniqueVal[1]]]
        for (jj in 2:length(UniqueVal))
        {
          VarTempCalcUniqueVal<-c(VarTempCalcUniqueVal,VarTempCalc[[UniqueVal[jj]]])
        }
        Var_table<-rbind(Var_table,VarTempCalcUniqueVal)
        Summary_Var_table<-data.frame(rbind(Summary_Var_table,VarTemp$summary))
      }
    }
    
    Var_table<-data.frame(name_var = var_names, coef_Var = Var_weight,Var_table,row.names=NULL)
    names(Var_table)<-c("name_var","coef_var",UniqueVal)
    Summary_Var_table<-data.frame(name_var = var_names, coef_Var = Var_weight,Summary_Var_table)
    
    #Weighted mean
    Obj_WMean<-weighted.mean(Var_table$mean_transf_value,Var_weight)
    Obj_Wsem<-weighted.mean((Var_table$error_transf_value),Var_weight) ##weighted mean of variable error
    
    #For graphical purpose
    Obj_inf<-Obj_WMean-Obj_Wsem
    Obj_sup<-Obj_WMean+Obj_Wsem
    
    Objective_val<-data.frame(Obj_WMean,Obj_Wsem,Obj_inf,Obj_sup)
    
    O[[j]]<-list(Objective_val,Summary_Var_table,Var_table)
    names(O[[j]])<-c("Objective_val","Summary_Var_table","Var_table")
    assign(as.character(project_df$Objectives)[j],O[[j]])
  }
  names(O)<-project_df$Objectives
  
  #Project
  obj_name <- as.character(project_df$Objectives)
  stakeholders <- names(project_df)[2:ncol(project_df)]
  obj_weight <- data.frame(project_df[,2:ncol(project_df)])
  
  name_objective<-obj_name
  names(obj_weight)<-paste0("Coef_",stakeholders)
  
  #summary aggregation
  Summary_Var_table<-get(as.character(obj_name[1]))$Summary_Var_table
  Var_table<-get(as.character(obj_name[1]))$Var_table
  Obj_table<-get(as.character(obj_name[1]))$Objective_val
  for(i in 2:length(obj_name))
  {
    Summary_Var_table<-rbind(Summary_Var_table,get(as.character(obj_name[i]))$Summary_Var_table)
    Var_table<-rbind(Var_table,get(as.character(obj_name[i]))$Var_table)
    Obj_table<-rbind(Obj_table,get(as.character(obj_name[i]))$Objective_val)
  }
  
  Obj_table<-data.frame(name_objective,obj_weight,Obj_table)
  Summary_Obj_table<-Obj_table[,1:(ncol(Obj_table)-2)]
  
  #Weighted mean and error
  Proj_WMean<-NULL
  Proj_Wsem<-NULL
  Proj_inf<-NULL
  Proj_sup<-NULL
  for(i in 1:length(stakeholders))
  {
    Proj_WMean[i]<-weighted.mean(Summary_Obj_table$Obj_WMean,Summary_Obj_table[,i+1])
    Proj_Wsem[i]<-weighted.mean(Summary_Obj_table$Obj_Wsem,Summary_Obj_table[,i+1])
    Proj_inf[i]<-Proj_WMean[i]-Proj_Wsem[i]
    Proj_sup[i]<-Proj_WMean[i]+Proj_Wsem[i]
  }
  
  Summary_Proj_Score<-data.frame(stakeholders = stakeholders, Score = Proj_WMean, Error = Proj_Wsem)
  Proj_table<-data.frame(Summary_Proj_Score,Proj_inf,Proj_sup)
  Summary_All<-list(Scores = Summary_Proj_Score, Objectives = Summary_Obj_table, Variables = Summary_Var_table)
  
  #Recov data :
  wh_obj <- NULL
  for(i in 3:ncol(objective_df))
  {
    rep_o <- rep(names(objective_df)[i],length(objective_df[objective_df[,i]!=0,i]))
    wh_obj <- c(wh_obj,rep_o)
  }
  wh_va <- objective_df$Variables
  wh_sco <- Summary_All$Variables$mean_transf_value
  RecovW_data <- data.frame(Objectives = wh_obj, Variables = wh_va, Scores = wh_sco)
  
  #Output
  Output<-list(Summary_Proj_Score,Proj_table,Summary_All,Var_table,RecovW_data,V,O)
  names(Output)<-c("Summary_Proj_Score","Proj_table","Summary_All","Var_table","RecovW_data","Variables","Objectives")
  
  return(Output)
  }

#' Variable barplot
#'
#' @description barplot of variable mean value or score, after \code{\link{ASPIRE_all}}
#'
#' @param variable_name a variable name
#' @param project_all a project object from \code{\link{ASPIRE_all}}
#' @param plot.ref a logical value indicating whether the reference should be drawn or not
#' @param ylim limits for the y axis
#' @param transf a logical value indicating whether the values should be transformated (into scores) or not
#' @param main an overall title for the plot
#' @param ... other arguments applicable to \code{\link{barplot}}
#' 
#' @export
#'
Var.barplot.all<-function(variable_name,project_all,plot.ref=TRUE,ylim=NULL,transf=FALSE,main=NULL,...)
{
  
  variable_all <- project_all$Variables[names(project_all$Variables)==variable_name][[1]]
  if(is.null(main)==TRUE){main <- variable_name}else{main <- main}
  Var.barplot(variab = variable_all, plot.ref=plot.ref, ylim=ylim, transf=transf, main = main, ...)
}

#' objective barplot
#' @description after \code{\link{ASPIRE_all}}
#'
#' @param objective_name an objective name
#' @param project_all a project object from \code{\link{ASPIRE_all}}
#' @param plot.ref a logical value indicating whether the reference should be drawn or not
#' @param ylim limits for the y axis
#' @param las.x axis label horizontal (1) or perpendicular to the axis (2)
#' @param cex.x magnification to be used for the axis label
#' @param main an overall title for the plot
#' @param ... other arguments from \code{\link{barplot}}
#'
#' @export
#'
Obj.barplot.all<-function(objective_name,project_all,plot.ref=TRUE,ylim=NULL,
                          las.x = 1,cex.x = 1,main = NULL,...)
{
  objective_all <- project_all$Objectives[names(project_all$Objectives)==objective_name][[1]]
  if(is.null(main)==TRUE){main <- objective_name}else{main <- main}
  Obj.barplot(obj = objective_all,
              plot.ref=plot.ref,ylim=ylim,las.x = las.x,cex.x = cex.x, main = main,...)
}

#' Objective radar diagram
#' @description draw a radar diagram with variables scores of one objective after \code{\link{ASPIRE_all}}
#'
#' @param objective_name an objective name
#' @param project_all a project object from \code{\link{ASPIRE_all}}
#' @param limited a logical value indicating whether transformed values above 1 should be plotted (therefore avoiding plot distortion)
#' @param transp a value between 0 and 1 indicating transparancy of the radar polygon
#' @param col radar polygon color, can be any color managed by R, but \code{transp} won't work if other values "c1", "c..", "c8" are used, corresponding to basic R colors from 1 to 8
#' @param main text for main title
#' @param plot.ref a logical value indicating whether the reference should be drawn or not
#' @param ... any other arguments applicable to \code{\link{fmsb::radarchart}}
#'
#' @export
#' @import fmsb
#'
#' @details The function uses the function \code{\link{radarchart}} from the package \code{\link{fmsb}} Minato Nakazawa (2014). fmsb: Functions for medical statistics book with some demographic data. R package version 0.7.0.
#'
Obj.radar.plot.all<-function(objective_name,project_all,limited=TRUE,transp=0.5,
                             col="c7",main="Variables scores",plot.ref=TRUE,...)
{
  objective_all <- project_all$Objectives[names(project_all$Objectives)==objective_name][[1]]
  Obj.radar.plot(obj = objective_all, limited = limited,transp = transp,
                 col = col, main = main, plot.ref = plot.ref, ...)
}

#' Score wheel
#'
#' @description Draw a score wheel inspired from the recovery wheel of SER Standards (McDonald et al., 2016), after \code{\link{ASPIRE_all}}
#'
#' @param project_all a project object from \code{\link{ASPIRE_all}}
#' @param col.score The color of achieved scores
#' @param col.null The color of unachieved scores
#' @param col.lines The color of lines and circles
#' @param col.border The color of the border containing categories labels
#' @param cex.sup An expansion factor for categories labels
#' @param cex.low An expansion factor for scores labels
#' @param col.sup text color for categories labels
#' @param col.low text color for scores labels
#' 
#' @details uses function from plotrix package Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12.
#' 
#' @export
#' @import plotrix
#' 
Wheelscores.all<-function(project_all,col.score='#FDC919',col.null='white',
                          col.lines="grey90",col.border='#FFEEC3',cex.sup=1,cex.low=0.7,
                          col.low = 1, col.sup = 1)
{
  Wheelscores(lowercat = project_all$RecovW_data$Variables,
              supercat = project_all$RecovW_data$Objectives,
              scores = project_all$RecovW_data$Scores,
              val.max = 1,col.null = col.null,
              col.lines = col.lines,col.score = col.score,
              col.border = col.border,cex.sup = cex.sup,cex.low = cex.low,
              col.low = col.low, col.sup = col.sup)
}