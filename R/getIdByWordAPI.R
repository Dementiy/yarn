#' Get id for a given word
#' 
#' @param word The input word
#' @export
getIdByWordAPI <- function(word='') {
  words <- getWordsAPI(start=word)
  word  <- getNodeSet(words, sprintf('//words/word/word[text()="%s"]', word))
  xmlValue(xmlChildren(xmlParent(word[[1]]))$id)
}

# getIdByWordAPI('машина')