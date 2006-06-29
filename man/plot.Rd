\name{plot}
\alias{plot}
\alias{plot-methods}
\alias{plot,ANY,ANY-method}
\alias{plot,odeModel,missing-method}
\alias{plot,gridModel,missing-method}
\alias{plot,rwalkModel,missing-method}
\title{Methods for Function plot in Package `simecol'}
\description{
 Methods for function \code{plot} in Package `simecol'. 
}

\usage{
  plot(x, y, ...)
  # plot(x, y, index=1:length(x@out), delay=0, ...)
}

\arguments{
  \item{x}{an object of class \code{gridModel}.}
  \item{y}{omitted.}
  \item{index}{index of time steps to be plotted.}
  \item{delay}{delay  (in ms) between consecutive images (for \code{gridModel}s)
    or xy-plots (for \code{rwalkModel}s).}
  \item{...}{optional plotting parameters.}
}

\section{Methods}{
\describe{

\item{x = "ANY", y = "ANY"}{Calls the standard generic \code{\link{plot}} function.}

\item{x = "odeModel", y = "missing"}{
  plots time series of the state variables. Optional plotting parameters
  are possible.}

\item{x = "gridModel", y = "missing"}{displays a series of images for
  the simulated grid.}


\item{x = "rwalkModel", y = "missing"}{displays a series of x-y plots
  of the simulated individuals.}


}}
\keyword{methods}
\keyword{hplot}
