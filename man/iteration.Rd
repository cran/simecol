\name{iteration}
\alias{iteration}

\title{Discrete Simulation}
\description{
  Solver function to simulate discrete ecological (or other) dynamic
  models. It is normally called indirectly from \code{sim}.
}
\usage{
  iteration(obj, animate = FALSE, ...) 
}
\arguments{
  \item{obj}{the \code{simObj} object to be iterated}
  \item{animate}{animation during the simulation}
  \item{\dots}{optional arguments passed to the \code{plot} function if
    \code{animate=TRUE}}
}

\value{a list of the model outputs (states \dots) for each timestep.}

\details{
  The solver method \code{iteration} is used to simulate discrete event
  models. It creates a local environment in a way that parameters,
  inputs and equations are visible within the main function of the
  \code{simObj}.
  Normally, this function is run indirectly from \code{\link{sim}}.
}
\seealso{
  \code{\link{sim}}, \code{\link{parms}},
  \code{lsoda}, \code{\link{rk4}}.
}

\examples{
  data(conway)
  ## plot after simulation:
  plot(sim(conway), delay=100)

  ## plot during simulation
  sim(conway, animate=TRUE, delay=100)

}
\keyword{misc}
