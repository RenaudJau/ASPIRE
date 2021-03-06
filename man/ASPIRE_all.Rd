% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ASPIRE_functions.R
\name{ASPIRE_all}
\alias{ASPIRE_all}
\title{ASPIRE project creation with 3 dataframes}
\usage{
ASPIRE_all(variable_df, objective_df, project_df)
}
\arguments{
\item{variable_df}{the dataframe containing variables data, each variable has two columns: one named by the variable name and containing data from the project, one named by the variable + '_Ref' and containing data from the reference. If there is missing values, 'NA' should be used.}

\item{objective_df}{the dataframe containing objective data, the first column, named 'Variables', contains variables names, the second, named 'Utility' contains variable utilities (cf function \code{\link{Transf_ASPIRE}}), then there is one column by objective, named by objectives names and containing weight of variables per objective}

\item{project_df}{the dataframe conatining project data, the first column, named 'Objectives' contains objectives names, the other columns, named by stakeholders names contains objectives weights by stakeholders}
}
\value{
\item{Summary_Proj_Score}{A dataframe of scores for each stakeholder}

\item{Proj_table}{A dataframe of scores for each stakeholder with errors (useful for graphical purpose)}

\item{Summary_All}{A list of dataframe with all calculated values, scores and errors for objectives ad variables}

\item{Var_table}{A dataframe with variables values, scores and errors}

\item{RecovW_data}{A dataframe formated to be used with \code{\link{Wheelscores.aspire}}}

\item{Variables}{A list of objects obtained from \code{\link{Variable}}, one per variable}

\item{Objectives}{A list of objects obtained from \code{\link{Objective}}, one per objective}
}
\description{
Creates an ASPIRE project object based on three formated dataframe
}
\examples{
# ASPIRE.var, ASPIRE.obj and ASPIRE.proj are data.frame included in the ASPIRE package
# creation of an ASPIRE_all object:
Proj_all <- ASPIRE_all(variable_df = ASPIRE.var,objective_df = ASPIRE.obj,project_df = ASPIRE.proj)
}
