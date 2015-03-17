#' Get the words in a synset
#' 
#' @param synset The synset whose words are returned
#' @return A character vector holding the words
#' @examples \dontrun{
#' synsets <- getSynsets('house')
#' getWords(synsets[[1]])
#' }
#' @export
getWords <- function(synset) {
  if (class(synset)[1] != 'XMLInternalElementNode') stop('synset must be a xml node')
  
  words <- synset['word']
  references <- sapply(words, function(word) xmlGetAttr(word, name = 'ref'))
  words <- as.vector(sapply(references, getWordById))
  words[!is.na(words)]
}