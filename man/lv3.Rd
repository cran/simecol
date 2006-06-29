\name{lv3}
\docType{data}
\alias{lv3}
\title{Lotka-Volterra-Type model with resource, prey and predator}
\description{
  \code{simecol} example: predator prey-model with three equations:
  predator, prey and resource (e.g. nutriens, grassland).  }


\usage{data(lv3)}

\format{A valid S4 object according to the \code{\link{odeModel}}
specification.  The object contains the following slots:

\describe{
  \item{\code{main}}{the Lotka-Volterra equations for predator
    prey and resource}.
       
  \item{\code{parms}}{vector with named parameters of the model:
        \describe{
          \item{\code{c}}{growth rate of the prey population.}
          \item{\code{d}}{encounter rate of predator and prey.}
      \item{\code{e}}{encounter rate of predator and prey (allows
        conversion with respect to d).}
          \item{\code{f}}{death rate of the predator population.}
      \item{\code{g}}{recycling parameter.}
      }}
  \item{\code{inputs}}{time series specifying external delivery of the resource.}
  \item{\code{times}}{simulation time and integration interval.}
  \item{\code{init}}{vector with start values for s, p and k.
  \describe{
          \item{\code{s}}{resource (e.g. grassland or phosphorus).}
          \item{\code{p}}{producer (prey).}
          \item{\code{k}}{consumer (predator).}
       }
  }
  \item{\code{solver}}{character string with the integration method.}
}}

\seealso{
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}


\examples{
  data(lv3)
  plot(sim(lv3))
  solver(lv3) <- "lsoda"
  plot(sim(lv3))         # wrong! integration overlooks internal inputs
  plot(sim(lv3, hmax=1)) # integration with correct maximum time step
}

\keyword{datasets}
