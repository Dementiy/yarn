#' Set the path to the dictionary
#'
#' @param path Path to the YARN dictionary
#' @examples
#' setDict("yarn.xml")
#' @export
setDict <- function(path) {
  if (!file.exists(path)) {
    stop("File doesn't exists")
  }
  
  yarn <- xmlParse(path, useInternalNodes = TRUE, options = HUGE)
  .yarn$root <- xmlRoot(yarn)
  if (xmlName(.yarn$root) != 'yarn') {
    .yarn$root <- NULL
    stop("Wrong dictionary")
  }
}


#' Download dictionary
#' 
#' @param url URL
#' @param set Set dictionary (by default True)
#' @export
# downloadDict <- function(url='http://russianword.net/yarn.xml', set=TRUE) {
#   download.file(url, destfile = 'yarn.xml')
#   if (set)
#     setDict('yarn.xml')
# }


getDict <- function() {
  if (!is.null(.yarn$root)) {
    .yarn$root
  } else {
    stop("Could not find YARN dictionary. For more details see ?setDict")
  }
}