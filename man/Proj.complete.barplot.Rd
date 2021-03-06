% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ASPIRE_functions.R
\name{Proj.complete.barplot}
\alias{Proj.complete.barplot}
\title{Complete project barplots}
\usage{
Proj.complete.barplot(
  proj,
  col.stak = NULL,
  col.obj = NULL,
  col.var = NULL,
  plot.ref = TRUE,
  las.x = 1,
  cex.x = 1
)
}
\arguments{
\item{proj}{project name defined with \code{\link{Project}} or \code{\link{ASPIRE_all}}}

\item{col.stak}{color of stakeholders plot}

\item{col.obj}{color of objectives plot}

\item{col.var}{color of variable scores plot (repeated if only one color is provided)}

\item{plot.ref}{a logical value indicating whether the reference should be drawn or not}

\item{las.x}{axis label horizontal (1) or perpendicular to the axis (2)}

\item{cex.x}{magnification to be used for the axis label}
}
\description{
displays a barplot of stakeholders project scores
}
\examples{
# ASPIRE.var, ASPIRE.obj and ASPIRE.proj are data.frame included in the ASPIRE package
# creation of an ASPIRE_all object:
Proj_all <- ASPIRE_all(variable_df = ASPIRE.var,objective_df = ASPIRE.obj,project_df = ASPIRE.proj)
Proj.complete.barplot(proj = Proj_all)
Proj.complete.barplot(proj = Proj_all, plot.ref = FALSE, col.stak = "cadetblue",
                      col.obj = "firebrick", col="orange", las.x = 2, cex.x = 0.7)
}
