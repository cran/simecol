
wraplsoda <- function(obj, ...) {
  #if (!require(odesolve, pos = "package:base")) {
  #  stop("'lsoda' requires the installation of the 'odesolve' package")
  #}
  inputs <- obj@inputs
  if (is(obj, "odeModel")) {
      init       <- obj@init
      times      <- fromtoby(obj@times)
      func       <- obj@main
      equations  <- obj@equations
      parms      <- obj@parms
      inputs     <- obj@inputs
  } else {     
      print("wraplsoda requires an odeModel as first argument")
  } 
  equations <- addtoenv(equations)
  environment(func) <- environment()
  lsoda(init, times, func, parms, ...)
}


