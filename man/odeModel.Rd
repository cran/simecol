\name{odeModel}
\alias{odeModel}
\alias{gridModel}
\alias{rwalkModel}

\title{The `generating-functions' (constructors) to create objects of
  classes odeModel, rwalkModel and gridModel.}

\description{
  This function can be used to create \code{simObj} instances from scratch or
  from already existing objects of the same type.
}
\usage{
odeModel(obj = NULL, main = NULL,
        equations = NULL, times = c(from = 0, to = 10, by = 1),
        init = numeric(0), parms = numeric(0),
        inputs = NULL, solver = "rk4", initialize = NULL)

gridModel(obj=NULL, main=NULL,
        equations=NULL, times=c(from=0, to=10, by=1),
        init=matrix(0), parms=list(),
        inputs=NULL, solver="iteration", initialize=NULL)

rwalkModel(obj=NULL, main=NULL, 
        equations=NULL, times=c(from=0, to=10, by=1),
        init=NULL, parms=list(),
        inputs=NULL, solver="iteration", initialize=NULL)
}

\arguments{
  \item{obj}{an already existing object of the respective class from
    which a modified version is to be created. If \code{obj} is omitted,
    the new object is created from scratch.}
  \item{main}{the main equations of the model.}
  \item{equations}{the sub-models (sub-equations and utility functions
   of the model).}
  \item{times}{a vector of time steps or a vector with three
      named values \code{from}, \code{to}, \code{by} specifying the
      simulation time steps. The from-to-by form can be edited with
     \code{fixParms}.}
  \item{init}{initial values (start values) of the
      state variable given as named vector.}
  \item{parms}{a vector or list (depending on the respective class) of constant parameters.}
  \item{inputs}{optional time-dependend input
    variables (matrix od data frame).}
  \item{solver}{the solver used to integrate the model.}
  \item{initialize}{the \code{initialize} function is called at creation
    time and allows direct access and manipulation of all slots of the
    object in creation}
}

\details{This function provides an alternative way to create
  \code{simObj} instances in addition to the standard S4 \code{new}
  mechanism. One of its advantages is the possibility to use a dedicated
  \code{initialize} function that allows computations using internal
  functions of the \code{equations} slot at creation time.

  See \code{\link{simecol-package}} and the examples for details about the
    slots. 
}

\value{
 The function returns an S4 object of type \code{odeModel},
 \code{rwalkModel}, \code{gridModel}
}

\seealso{\code{\link{simecol-package}}}



\keyword{programming}

