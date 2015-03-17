#' Get the ID for a given word
#' 
#' @param word The input word
#' @return ID
#' @seealso \code{\link{getIdByWord}}
#' @references
#' Dmitriy Ustalov. YARN API. \url{http://nlpub.ru/YARN/API}
#' @examples \dontrun{
#' getIdByWordAPI('house')
#' }
#' @export
getIdByWordAPI <- function(word='') {
  if (!is.character(word)) stop('word must be a character')
  
  words <- getWordsAPI(q=word)
  word <- Filter(function(word_object) xmlValue(xmlChildren(word_object)$word) == word, words)
  xmlValue(xmlChildren(word[[1]])$id)
}