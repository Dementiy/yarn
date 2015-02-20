#' Get id for a given word
#' 
#' @param word The input word
#' @examples
#' word_id <- getIdByWordAPI('машина')
#' @export
getIdByWordAPI <- function(word='') {
  words <- getWordsAPI(start=word)
  word <- Filter(function(word_object) xmlValue(xmlChildren(word_object)$word) == word, words)
  #word  <- getNodeSet(words, sprintf('//words/word/word[text()="%s"]', word))
  xmlValue(xmlChildren(word[[1]])$id)
  #xmlValue(xmlChildren(xmlParent(word[[1]]))$id)
}

# getIdByWordAPI('кот')
