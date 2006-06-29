\name{parms}

\alias{main}
\alias{main-methods}
\alias{main,simObj-method}

\alias{main<-}
\alias{main<--methods}
\alias{main<-,simObj-method}

\alias{equations}
\alias{equations-methods}
\alias{equations,simObj-method}

\alias{equations<-}
\alias{equations<--methods}
\alias{equations<-,simObj-method}


\alias{parms}
\alias{parms-methods}
\alias{parms,simObj-method}

\alias{parms<-}
\alias{parms<--methods}
\alias{parms<-,simObj-method}

\alias{init}
\alias{init-methods}
\alias{init,simObj-method}

\alias{init<-}
\alias{init<--methods}
\alias{init<-,simObj,ANY-method}
\alias{init<-,gridModel,matrix-method}
\alias{init<-,gridModel,ANY-method}


\alias{times}
\alias{times-methods}
\alias{times,simObj-method}

\alias{times<-}
\alias{times<--methods}
\alias{times<-,simObj-method}

\alias{inputs}
\alias{inputs-methods}
\alias{inputs,simObj-method}

\alias{inputs<-}
\alias{inputs<--methods}
\alias{inputs<-,simObj-method}

\alias{solver}
\alias{solver-methods}
\alias{solver,simObj-method}

\alias{solver<-}
\alias{solver<--methods}
\alias{solver<-,simObj-method}

\alias{out}
\alias{out-methods}
\alias{out,simObj-method}

\title{Accessor functions for simObj objects}
\description{
  Get or set simulation model parameters, main or sub-equations, initial
  values, time steps or solvers and extract simulation results.
}
\usage{
   parms(obj) 
   parms(obj) <- value

   main(obj) 
   main(obj) <- value

   equations(obj) 
   equations(obj) <- value

   init(obj) 
   init(obj) <- value

   inputs(obj) 
   inputs(obj) <- value

   times(obj) 
   times(obj) <- value

   solver(obj) 
   solver(obj) <- value

   out(obj) 

}
\arguments{
  \item{obj}{a valid \code{simObj} instance.}
  \item{value}{named list, vector, function or other data structure (depending
    on the slot and model class) with the same structure as the value returned by 
    \code{parms}. Either all or a subset of values (e.g. single
    elements of vectors or lists can be changed at once.}
}

\details{The accessor functions for \code{parms}, \code{times} and
  The \code{init}-method for class \code{odeModel} allows to change
  single named values (in contrast to the
  whole) of the target slot. Special care has to be taken to use valid
  names.  In \code{init} the
  applied names must correspond to the names and number (!) of state
  variables but the restrictions of \code{parms} are less strict
  (additional parameter values for ``future use'' are allowed). When
  \code{NA} is assigned to a named element in \code{parms} or
  \code{init} the respective element (name and value) is deleted.
  \code{times} allows either to assign or to modify a special vector
  with three elements named \code{from}, \code{to} and \code{by} or to
  overwrite \code{times} with an un-named sequence (e.g. \code{seq(1,
  100, 0.1)}.}

\value{A list, named vector, matrix or function (for
\code{main} slot) or list of functions (\code{equation} slot) or other
appropriate data structure depending on the class of the model object.}

\seealso{
  \code{\link{simecol-package}}.
}
\examples{
  data(lv)
  parms(lv)
  parms(lv) <- list(k2=0.5)
  parms(lv) <- list(k2=0.5, k1=0.3)
  parms(lv)

  data(conway)
  parms(conway)
  parms(conway) <- list(srv=c(2, 2))
  parms(conway)

  # add a new named parameter value
  parms(lv) <- c(dummy = 1)
  # remove this value from the parameter vector
  parms(lv) <- c(dummy = NA)

}
\keyword{programming}
