% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dichotomous.h.R
\name{dichotomous}
\alias{dichotomous}
\title{Dichotomous Model}
\usage{
dichotomous(
  data,
  vars,
  prop = TRUE,
  imeasure = FALSE,
  ise = FALSE,
  infit = FALSE,
  outfit = FALSE,
  reliability = TRUE,
  modelfit = FALSE,
  modelfitp = FALSE,
  mat = FALSE,
  wrightmap = TRUE,
  inplot = FALSE,
  outplot = FALSE,
  angle = 0,
  to = FALSE,
  plot2 = FALSE,
  st = FALSE,
  plot3 = FALSE,
  width = 500,
  height = 500,
  num = 1,
  plot4 = FALSE,
  width4 = 500,
  height4 = 500,
  width1 = 500,
  height1 = 500,
  width3 = 500,
  height3 = 500,
  width2 = 500,
  height2 = 500
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{vars}{.}

\item{prop}{.}

\item{imeasure}{.}

\item{ise}{.}

\item{infit}{.}

\item{outfit}{.}

\item{reliability}{.}

\item{modelfit}{.}

\item{modelfitp}{.}

\item{mat}{.}

\item{wrightmap}{.}

\item{inplot}{.}

\item{outplot}{.}

\item{angle}{a number from 0 to 90 defining the angle of the x-axis labels,
where 0 degrees represents completely horizontal labels.}

\item{to}{.}

\item{plot2}{.}

\item{st}{.}

\item{plot3}{.}

\item{width}{.}

\item{height}{.}

\item{num}{.}

\item{plot4}{.}

\item{width4}{.}

\item{height4}{.}

\item{width1}{.}

\item{height1}{.}

\item{width3}{.}

\item{height3}{.}

\item{width2}{.}

\item{height2}{.}
}
\value{
A results object containing:
\tabular{llllll}{
  \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
  \code{results$items} \tab \tab \tab \tab \tab a table \cr
  \code{results$stand$st} \tab \tab \tab \tab \tab a table \cr
  \code{results$stand$to} \tab \tab \tab \tab \tab a table \cr
  \code{results$mf$scale} \tab \tab \tab \tab \tab a table \cr
  \code{results$mf$mat} \tab \tab \tab \tab \tab a table \cr
  \code{results$plot} \tab \tab \tab \tab \tab an image \cr
  \code{results$plot4} \tab \tab \tab \tab \tab an image \cr
  \code{results$inplot} \tab \tab \tab \tab \tab an image \cr
  \code{results$outplot} \tab \tab \tab \tab \tab an image \cr
  \code{results$total} \tab \tab \tab \tab \tab an output \cr
  \code{results$pmeasure} \tab \tab \tab \tab \tab an output \cr
  \code{results$pse} \tab \tab \tab \tab \tab an output \cr
  \code{results$pinfit} \tab \tab \tab \tab \tab an output \cr
  \code{results$poutfit} \tab \tab \tab \tab \tab an output \cr
  \code{results$resid} \tab \tab \tab \tab \tab an output \cr
  \code{results$plot3} \tab \tab \tab \tab \tab an image \cr
  \code{results$plot2} \tab \tab \tab \tab \tab an image \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$items$asDF}

\code{as.data.frame(results$items)}
}
\description{
Dichotomous Model
}
