\name{upca}
\docType{data}
\alias{upca}
\title{The Uniform Period Chaotic Amplitude Model}
\description{
  \code{simecol} example: resource-predator-prey model, which is able to show chaotic behaviour.
}
\usage{data(upca)}

\format{S4 object according to the \code{\link{odeModel}} specification. 
The object contains the following slots:


\describe{
  \item{\code{main}}{the Lotka-Volterra equations for predator
    prey and resource}.
  \describe{
          \item{\code{u}}{resource (e.g. grassland or phosphorus).}
          \item{\code{v}}{producer (prey).}
          \item{\code{w}}{consumer (predator).}
       }}
  \item{\code{equations}}{two alternative (and switcheable) equations
    for the functional response.}
  \item{\code{parms}}{vector with the named parameters of the model,
    see reference for details.}
  \item{\code{times}}{simulation time and integration interval.}
  \item{\code{init}}{vector with start values for s, p and k.}
  \item{\code{solver}}{character string with the integration method.}
} 

}

\details{
  To see all details, please have a look into the implementation.
}


\references{
  
Blasius, B., Huppert, A., and Stone, L. (1999) Complex dynamics and
phase synchronization in spatially extended ecological systems.
\emph{Nature}, \bold{399} 354--359.

Blasius, B. and Stone, L. (2000) Chaos and phase synchronization in
ecological systems.  \emph{International Journal of Bifurcation and
Chaos}, \bold{10} 2361--2380.  }

\seealso{
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}


\examples{
  data(upca)
  plot(sim(upca))

  # omit stabilizing parameter wstar
  parms(upca) <- list(wstar=0)
  plot(sim(upca))

  # change functional response from
  # Holling II (default) to Lotka-Volterra
  equations(upca)$f <- function(x, y, k) x * y
  plot(sim(upca))

}

\keyword{datasets}
