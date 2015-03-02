#' A list of all words
#' 
#' The full list of known words with page breakdown. 
#' @param page Page number
#' @param q The word starts with the substring `word`
#' @return The output is an array with objects of class Word
#' @import httr
#' @examples
#' word_objs <- getWordsAPI(q = 'машина')
#' words <- words2df(word_objs)
#' @export
getWordsAPI <- function(page=1, q='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'words.xml', sep='')
  if (q != '')
    query <- paste(query, '?q=', q, '&page=', page, sep='')
  response <- GET(URLencode(query))
  words_xml <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(words_xml, "//words/word")
}


#' Convert words to data.frame object
#' 
#' @param words The input words
#' @export
words2df <- function(word_objects) {
  words <- as.data.frame(
    do.call(rbind,
            lapply(word_objects,
                   function(word_object) c(id=xmlValue(xmlChildren(word_object)$id), 
                                    word=xmlValue(xmlChildren(word_object)$word), 
                                    grammar=xmlValue(xmlChildren(word_object)$grammar)))
    )
  )
  rownames(words) <- NULL
  words
}


#' Filter words by specified grammar
#' 
#' @param words The input words
#' @param grammar The input grammar
#' @examples
#' word_objs <- getWordsAPI(q = 'машина')
#' filtered_word_objs <- filterByGrammar(word_objs, "a")
#' filtered_words <- words2df(filtered_word_objs)
#' @export
filterByGrammar <- function(word_objects, grammar) {
  Filter(function(word_object) xmlValue(xmlChildren(word_object)$grammar) == grammar, word_objects)
}