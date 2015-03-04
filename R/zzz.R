# Creates new environment for the package
if (!exists(".yarn")) {
  .yarn <- new.env()
  .yarn$domain <- 'http://russianword.net/'
}

.onLoad <- function(libname, pkgname) {
  ver <- read.dcf(file.path(libname, pkgname, "DESCRIPTION"), "Version")
  ver <- as.character(ver)
  cat("yarn", ver, "loaded\n")
}