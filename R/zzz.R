# Creates new environment for the package
if (!exists(".yarn")) {
  .yarn <- new.env()
}

.onLoad <- function(libname, pkgname) {
  ver <- read.dcf(file.path(libname, pkgname, "DESCRIPTION"), "Version")
  ver <- as.character(ver)
  cat("yarn", ver, "loaded\n")
  
  domain <<- 'http://russianword.net/'
}