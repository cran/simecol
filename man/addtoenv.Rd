\name{addtoenv}
\alias{addtoenv}

\title{Add Functions from a Non-nested List of Named Functions to a
  Common Environment}

\description{
  Create and set an environment where functions within a non-nested named
  list of functions see each other. This function is normally used
  within other functions, in particular solvers of the \pkg{simecol} package.
}
\usage{
addtoenv(L, p=parent.frame())
}

\arguments{
  \item{L}{A non-nested list of named functions.}
  \item{p}{The environment where the functions are assigned to. Defaults
  to the parent frame.}
}
\details{
  This function is normally called indirectly by `solver functions' and
  from \code{initialize} functions (constructors) of \code{simObj}ects.
}
\value{
  The list of equations within a common environment.
}

\note{
  This is a very special function that uses environment manipulations.
  Its purpose is to `open' the access to interdependend functions within
  a common list structure (function list). This enables the user to
  avoid the explicit specification of the `function list' and the
  necessity to pass this list explicitely to the other functions.
}
\seealso{
  \code{\link{rk4}},
  \code{\link{iteration}},
  \code{\link{odeModel}}}

\examples{
    eq <- list(f1 = function(x, y)    x + y,
               f2 = function(a, x, y) a * f1(x, y)
              )

    fx <- function(eq) {
      eq <- addtoenv(eq)
      print(ls())
      print(environment(eq$f1))
      f1(3,4) + f2(1,2,3)
    }

    fx(eq)
    # eq$f2(2,3,4)       # should give an error outside fx
    environment(eq$f2)   # should return R_GlobalEnv again

}
\keyword{programming}
\keyword{environment}

