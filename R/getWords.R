#' Get the words in a synset
#' 
#' @param synset The synset whose words are returned
#' @import XML
#' @export
getWords <- function(synset) {
  # if (is.null(.yarn$root)) stop('you must specify the yarn dictionary')
  if (class(synset)[1] != 'XMLInternalElementNode') stop('synset must be a xml node')
  
  words <- synset['word']
  references <- sapply(words, function(word) xmlGetAttr(word, name = 'ref'))
  #as.vector(getNodeSet(.yarn$root, sprintf('//words/wordEntry[@id="%s"]/word', references)))
  words <- as.vector(sapply(references, getWordById))
  words[!is.na(words)]
}