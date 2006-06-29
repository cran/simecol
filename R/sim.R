setGeneric("sim", function(obj, ...) standardGeneric("sim"))

setMethod("sim", "odeModel",
  function(obj, ...) {
    if (sum(names(obj@times) %in% c("from","to","by"))==3) {
      times <- seq(obj@times["from"], obj@times["to"], obj@times["by"])
    } else {
      times <- obj@times
    }

    if (obj@solver==FALSE) {
        obj@out <- obj@main(obj@times, obj@init, obj@parms, obj@inputs)
    } else {
        if (obj@solver == "lsoda") {
          out <- wraplsoda(obj, ...)  
        } else {
          out <- do.call(obj@solver, list(obj, ...))
        }
    }
    obj@out <- as.data.frame(out)
    invisible(obj)
  }
)

setMethod("sim", "gridModel",
  function(obj, ...) {
    if (sum(names(obj@times) %in% c("from","to","by"))==3) {
      times <- seq(obj@times["from"], obj@times["to"], obj@times["by"])
    } else {
      times <- obj@times
    }
    out <- do.call(obj@solver, list(obj, ...))
    obj@out <- out
    invisible(obj)

  }
)

setMethod("sim", "rwalkModel",
  function(obj, ...) {
    out <- do.call(obj@solver, list(obj, ...))
    obj@out <- out
    invisible(obj)
  }
)
