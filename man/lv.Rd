\name{lv}
\docType{data}
\alias{lv}
\title{Lotka-Volterra Predator-Prey Model}
\description{
  \code{simecol} example: basic Lotka-Volterra predator prey-model.
}
\usage{data(lv)}

\format{An S4 object according to the \code{\link{odeModel}} specification. 
The object contains the following slots:

\describe{
  \item{\code{main}}{the Lotka-Volterra equations for predator and prey.}
  \item{\code{parms}}{vector with the named parameters of the model:
       \describe{
          \item{\code{k1}}{growth rate of the prey population.}
          \item{\code{k2}}{encounter rate of predator and prey.}
          \item{\code{k3}}{death rate of the predator population.}
       }}
  \item{\code{times}}{simulation time and integration interval.}
  \item{\code{init}}{vector with start values for predator and prey.}
} 
}

\details{To see all details, please have a look into the implementation.
}


\references{
Lotka, A. J. 1925. \emph{Elements of physical biology.} Williams and Wilkins, Baltimore.

Volterra, V. (1926). Variazionie fluttuazioni del numero d'individui in specie animali conviventi.
\emph{Mem. Acad.Lincei}, \bold{2}, 31-113.
}

\seealso{
  \code{\link{simecol}},
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}


\examples{
  data(lv)
  print(lv)
  plot(sim(lv))

  parms(lv) <- list(k1=0.5, k2=0.5, k3=0.5) 
  plot(sim(lv))
}

\keyword{datasets}
