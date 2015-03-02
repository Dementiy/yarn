#' Get id for a given word
#' 
#' @param word The input word
#' @examples
#' word_id <- getIdByWordAPI('машина')
#' @export
getIdByWordAPI <- function(word='') {
  words <- getWordsAPI(q=word)
  word <- Filter(function(word_object) xmlValue(xmlChildren(word_object)$word) == word, words)
  xmlValue(xmlChildren(word[[1]])$id)
}