#' Get word by specified id
#' 
#' @param id Id
#' @return The word
#' @export
#' @import XML
getWordById <- function(id) {
  if (!is.character(id)) id <- paste("w", id, sep="")
  # if (is.null(.yarn$root)) stop("You must specify the yarn dictionary")
  root <- getDict()
  
  # Can reproduce NA's!!!
  as.vector(xmlValue(getNodeSet(root, sprintf('//words/wordEntry[@id="%s"]/word', id))[[1]]))
}