\name{simecol-package}
\alias{simecol-package}
\alias{simecol}
\alias{simObj}
%\alias{odeModel}
%\alias{gridModel}
%\alias{rwalkModel}

\alias{simObj-class}
\alias{odeModel-class}
\alias{gridModel-class}
\alias{rwalkModel-class}

\docType{package}

\title{General Information about the simecol Package}

\non_function{}

\description{
  The \code{simecol} package is intended to give users
  (scientists and students) and interactive environment to implement,
  distribute, simulate and document ecological and other dynamic models without the
  need to write long simulation programs. For this purpose, an object
  oriented approach on the S4 class system
  is proposed, which should provide a consistent but still
  flexible and extensible way to implement simulation models of different
  types, namely
  \itemize{
    \item ordinary differential equation (ode) models (class \code{odeModel}),
    \item non-spatial individual-based models, 
    \item grid-oriented individual-based models (class \code{gridModel}), and
    \item particle diffusion-type models (class \code{rwalkModel}).
  }
  Each simulation model is implemented as S4 object (superclass \code{simObj})
  with the following slots:
  \itemize{
    \item \code{main = function(time, init, parms, ...)}: a function holding the
      main equations of the model,
    \item \code{equations}: an optional non-nested list holding arbitrary
      sub-equations (submodels, utility functions) of the
      model. Sub-equations can be interdependent and can be called
      directly from within \code{main} or \code{initialize}.
    \item \code{parms}: currently a list (for \code{gridModel}s) or vector (all
      other pre-definded classes) with constant model parameters,
    \item  \code{times}: vector of time steps or a vector with three
      named values \code{from}, \code{to}, \code{by} specifying the
      simulation time steps. The from-to-by form can be edited with
     \code{fixParms}.
    \item \code{init}: initial values (start values) of the
      simulation. This is typically a named vector (state variables in
      \code{odeModel}s) or matrix (e.g. initial grid of
      \code{gridModel}s).
    \item \code{inputs}: time dependend or spatially resolved external
      inputs can be specified as data frame or matrix (more efficient). It
      is up to the user to evaluate the corresponding values within the
      \code{main} function. A performance optimized version of
      \code{approx}, \code{\link{approxTime}} is available.
     \item \code{solver}: charachter string specifying the solver
       used. Currently \code{"lsoda"} (from package \code{odesolve}),
       \code{"rk4"} and \code{"iterator"} are supported. It is possible
       to write own solvers. Please have a look at the environment
       manipulatiion code at the beginning of \code{"rk4"} or
       \code{"iterator"}.
     \item \code{out}: this slot holds the simulation results after a
       simulation run as data frame or list (of matrices).
  }  

  \code{simObj} model objects can be defined and created using the
  common S4 mechanisms (\code{\link{new}}) or respective generating
  functions (e.g. \code{\link{odeModel}}).
  
  
  In this way, a \code{simObj} object may contain all data, which are
  needed to run simulations simply by entering the model object
  via \code{source()} or \code{data()} and then to run and plot the
  model for example via \code{plot(sim(obj))}.

  Accessor functions (with names identical to the slot names) are
  provided to get or set model parameters,
  time steps, initial values, inputs, the solver or the main and
  sub-equations or to extract the model outputs. It is also possible to
  modify the components of the simecol objects directly, e.g. the model
  equations of a model \code{\link{lv}} with \code{lv@main}. 

  Models of different type are provided as data
  and in source code (directory examples). The examples are intended as
  a starting point to write your own
  \code{simObj} objects and to distribute them to whomever you wish. 

  The package is supplemented with several utility functions
  (e.g. \code{\link{seedfill}} or \code{\link{neighbours}}), which can
  be used independently from
  \code{simObj} objects.

The following are sources of information on \code{simecol}:
\tabular{ll}{
Home page\tab u <- "http://hhbio.wasser.tu-dresden.de/projects/simecol/"; browseURL(u)\cr
News\tab  file.show(system.file("NEWS", package = "simecol"))\cr
Wish List\tab  file.show(system.file("WISHLIST", package = "simecol"))\cr
Acknowledgements\tab  file.show(system.file("THANKS", package = "simecol"))\cr
This File\tab  package?simecol\cr
Help files as PDF\tab  u <- "http://hhbio.wasser.tu-dresden.de/projects/simecol/simecol.pdf"; browseURL(u)\cr
}

}

%\references{} 

\author{
  Thomas Petzoldt \email{petzoldt@rcs.urz.tu-dresden.de}
}

\examples{
    data(lv)        # load basic Lotka-Volterra model

    \dontrun{fixParms(lv)}
    parms(lv)
    main(lv)
    lv <- sim(lv)
    plot(lv)
    results <- out(lv)

    data(conway)    # Conway's game of life
    init(conway) <- matrix(0, 10, 10)
    \dontrun{fixInit(conway)} # enter some "1"
    sim(conway, animate=TRUE, delay=10)
}



\keyword{misc}
