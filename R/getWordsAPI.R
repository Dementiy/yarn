#' Получить список всех слов
#' 
#' Поиск по известным словам с постраничной разбивкой. Слово начинается с подстроки word.
#' @param page Номер страницы
#' @param start Подстрока
#' @return На выходе получается массив с объектами класса Word
#' @import httr
#' @examples
#' word_objs <- getWordsAPI(start='машина')
#' words <- words2df(words_obj)
#' @export
getWordsAPI <- function(page=1, start='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'words.xml', sep='')
  if (start != '')
    query <- paste(query, '?q=', start, '&page=', page, sep='')
  response <- GET(URLencode(query))
  words_xml <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(words_xml, "//words/word")
}


# words2df <- function(words) {
#   words <- as.data.frame(
#     do.call(rbind,
#             lapply(xmlChildren(xmlRoot(words)),
#                    function(node) c(id=xmlValue(xmlChildren(node)$id), 
#                                     word=xmlValue(xmlChildren(node)$word), 
#                                     grammar=xmlValue(xmlChildren(node)$grammar)))
#     )
#   )
#   rownames(words) <- NULL
#   words
# }

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
#' words_objects <- getWordsAPI(start = 'машина')
#' filterByGrammar(words_objects, "a")
#' @export
filterByGrammar <- function(word_objects, grammar) {
  Filter(function(word_object) xmlValue(xmlChildren(word_object)$grammar) == grammar, word_objects)
}

# word_objects <- getWordsAPI(start = 'машина')
# filterByGrammar(word_objects, "n")


# as.data.frame(
#  do.call(
#   rbind, 
#   lapply(
#      getNodeSet(words_list, "//words/word"), function(node) c(Word = xmlValue(xmlChildren(node)$word), Id = xmlValue(xmlChildren(node)$id))
#   )
#  )
# )