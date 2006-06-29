
## check for from-to-by structure of vector
isfromtoby <- function(x) {
  all(sort(names(x)) == c("by", "from","to"))
}

hasfromtoby <- function(x) {
  any(names(x) %in% c("from","to","by"))
}


fromtoby <- function(times) {
  if (isfromtoby(times)) {
    times <- seq(times["from"], times["to"], times["by"])
  } 
  times
}
