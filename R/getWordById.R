#' Get the word for a given id
#' 
#' @param word_id The input word id
#' @return The word
#' @seealso \code{\link{getIdByWord}}
#' @examples
#' getWordById(131)
#' @export
getWordById <- function(word_id) {
  if (!is.character(word_id)) word_id <- paste("w", word_id, sep="")
  root <- getDict()
  
  # Can reproduce NA's!!!
  as.vector(xmlValue(getNodeSet(root, sprintf('//words/wordEntry[@id="%s"]/word', word_id))[[1]]))
}