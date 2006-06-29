## constructors ('generating functions')

odeModel  <- function(obj=NULL, main=NULL, equations=NULL,
                      times=c(from=0, to=10, by=1),
                      init=numeric(0), parms=numeric(0), inputs=NULL,
                      solver="rk4", initialize=NULL) {
    ## ToDo: restructure and cleanup (of if's)
    if (is.null(obj) | data.class(obj) != "odeModel") {
      obj <- new("odeModel", main=main, times=times,
                 init=init, parms=parms, inputs=inputs, solver=solver)
    } else {
      equations <- obj@equations
      main      <- obj@main
    }
    if (!is.null(equations)) {
      obj@equations     <- equations
      equations         <- addtoenv(equations)
      environment(main) <- environment()
    }
    if (is.function(initialize)) {
      environment(initialize) <- environment()
      obj <- initialize(obj)
    }
    invisible(obj)
  }

gridModel  <- function(obj=NULL, main=NULL, equations=NULL,
                      times=c(from=0, to=10, by=1),
                      init=matrix(0), parms=list(), inputs=NULL,
                      solver="iteration", initialize=NULL) {
    ## ToDo: restructure and cleanup (of if's)
    if (is.null(obj) | data.class(obj) != "gridModel") {
      obj <- new("gridModel", main=main, times=times,
                 init=init, parms=parms, inputs=inputs, solver=solver)
    } else {
      equations <- obj@equations
      main      <- obj@main
    }
    if (!is.null(equations)) {
      obj@equations     <- equations
      equations         <- selfrefer(equations, environment())
      environment(main) <- environment()
    }
    if (is.function(initialize)) {
      environment(initialize) <- environment()
      obj <- initialize(obj)
    }
    invisible(obj)
  }

rwalkModel  <- function(obj=NULL, main=NULL, equations=NULL,
                      times=c(from=0, to=10, by=1),
                      init=NULL, parms=list(), inputs=NULL,
                      solver="iteration", initialize=NULL) {
    ## ToDo: restructure and cleanup (of if's)
    if (is.null(obj) | data.class(obj) != "gridModel") {
      obj <- new("rwalkModel", main=main, times=times,
                 init=init, parms=parms, inputs=inputs, solver=solver)
    } else {
      equations <- obj@equations
      main      <- obj@main
    }
    if (!is.null(equations)) {
      obj@equations     <- equations
      equations         <- selfrefer(equations, environment())
      environment(main) <- environment()
    }
    if (is.function(initialize)) {
      environment(initialize) <- environment()
      obj <- initialize(obj)
    }
    invisible(obj)
  }

