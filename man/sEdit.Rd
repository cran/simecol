\name{sEdit}
\alias{sEdit}
\title{Simple editing}
\description{
  Simple editing of vectors, lists of vectors and other
  objects.
}
\usage{
 sEdit(x, title="Please enter values:")
}
\arguments{
  \item{x}{a named object that you want to edit.}
  \item{title}{a title for the dialog box.}
}
\value{
  An object with the same type like \code{x}.
}
\details{
  If called with a vector or list of vectors and if \code{tcltk} is
  installed, a dialog box is shown in which data can be entered. If the
  \code{x} is not of type vector or list of vectors, a default
  editing method is called.
}
\seealso{
  \code{\link{edit}}
  \code{\link{fixParms}}
}
\examples{
  \dontrun{ 
    ## named vector
    vec  <- c(a=1, b=20, c=0.03)
    new  <- sEdit(vec)
    ## unnamed vector
    sEdit(numeric(10))
    ## list of vectors
    lst <- list(vec=vec, test=1:10)
    sEdit(lst)
    ## list with numeric and character vectors mixed
    lst <- list(vec=vec, test=c("a","b","c"))
    sEdit(lst)
  }
}
\keyword{utilities}
