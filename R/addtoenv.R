addtoenv <- function(L, p = parent.frame()) {
  for(nm in names(L)) {
	assign(nm, L[[nm]], p)
	environment(p[[nm]]) <- p
      }
  L
}
