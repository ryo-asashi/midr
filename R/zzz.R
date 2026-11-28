# R/zzz.R

.onLoad <- function(libname, pkgname) {
  set_default_midr_options()
  invisible(NULL)
}
