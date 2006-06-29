\name{print-methods}
\docType{methods}
\alias{print}
\alias{print-methods}
\alias{print,ANY-method}
\alias{print,simObj-method}
\title{Methods for Function `print' in Package `simecol'}
\description{
 Methods for function \code{print} in Package `simecol'.
}
\usage{
  print(x, ...)
}

\arguments{
  \item{x}{an object of class \code{simObj} or one of its subclasses.}
   \item{...}{optional parameters passed to print.}
}


\section{Methods}{
\describe{

\item{x = "ANY"}{calls the default method, see \code{\link{print}}.}

\item{x = "simObj"}{prints the contents (slots) of the \code{simObj} object.}
}}
\keyword{methods}
\keyword{print}
