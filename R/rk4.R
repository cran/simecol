## Classical Runge-Kutta-fixed-step-integration
## using imported function from package odesolve

setGeneric("rk4")

setMethod("rk4", "odeModel", 
  function(y, times=NULL, func=NULL, parms=NULL) {
    times             <- fromtoby(y@times)
    func              <- y@main
    inputs            <- y@inputs
    equations         <- y@equations
    equations         <- addtoenv(equations)
    environment(func) <- environment()
    rk4(y@init, times, func, y@parms)
  }
)