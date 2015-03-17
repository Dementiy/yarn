#' Get a list of all words from a YARN dictionary
#' 
#' @param page Page number
#' @param q The word starts with the substring `word`
#' @return A list with objects of class Word
#' @references
#' Dmitriy Ustalov. YARN API. \url{http://nlpub.ru/YARN/API}
#' @examples \dontrun{
#' word_objs <- getWordsAPI(q = 'house')
#' words2df(word_objs)
#' 
#' word_objs <- getWordsAPI(page = 2)
#' words2df(word_objs)
#' }
#' @export
getWordsAPI <- function(page=1, q='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  if (!is.numeric(page)) stop('page must be an integer value')
  if (q != '' && !is.character(q)) stop('q must be a character')
  
  query <- paste(.yarn$domain, 'words.xml', sep='')
  if (q != '')
    query <- paste(query, '?q=', q, '&page=', page, sep='')
  response <- GET(URLencode(query))
  words_xml <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(words_xml, "//words/word")
}


#' Convert words to a Data Frame
#' 
#' @param word_objects A list with objects of class Word
#' @return Returns a data frame
#' @seealso \code{\link{getWordsAPI}}
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


#' Filter a list of words by specified grammar
#' 
#' @param word_objects A list with objects of class Word
#' @param grammar The input grammar. Must be either "a" (adjective), "v" (verb) or "n" (noun)
#' @return A list of words for a given grammar
#' @examples
#' word_objs <- getWordsAPI(q = 'house')
#' filtered_word_objs <- filterByGrammar(word_objs, "a")
#' words2df(filtered_word_objs)
#' @export
filterByGrammar <- function(word_objects, grammar) {
  Filter(function(word_object) xmlValue(xmlChildren(word_object)$grammar) == grammar, word_objects)
}