# Creates new environment for the package
if (!exists(".yarn")) {
  .yarn <- new.env()
}

.onLoad <- function(libname, pkgname) {
  ver <- read.dcf(file.path(libname, pkgname, "DESCRIPTION"), "Version")
  ver <- as.character(ver)
  cat("yarn", ver, "loaded\n")
}

#' Update dictionary (current trouble: <yarn ...>)
#' 
#' @param url URL
#' @param set Set dictionary (by default True)
# updateDict <- function(url='http://russianword.net/yarn.xml', set=TRUE) {
#   download.file(url, destfile = 'yarn.xml')
#   if (set)
#     setYarn('yarn.xml')
# }
# 
# 
# getDict <- function() {
#   if (!exists('yarn.root') || is.null(yarn.root))
#     stop('could not find YARN dictionary. For more details see ?setYarn')
#   else
#     yarn.root
# }