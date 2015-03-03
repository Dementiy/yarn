#' Get word by specified id
#' 
#' @param word_id Id
#' @return The word
#' @export
#' @import XML
getWordById <- function(word_id) {
  if (!is.character(word_id)) word_id <- paste("w", word_id, sep="")
  # if (is.null(.yarn$root)) stop("You must specify the yarn dictionary")
  root <- getDict()
  
  # Can reproduce NA's!!!
  as.vector(xmlValue(getNodeSet(root, sprintf('//words/wordEntry[@id="%s"]/word', word_id))[[1]]))
}