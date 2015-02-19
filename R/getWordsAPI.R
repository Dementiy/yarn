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
  content(response, type='text/xml', encoding = 'UTF-8')
}


#' Convert words to data.frame object
#' 
#' @param words The input words
#' @export
words2df <- function(words) {
  words <- as.data.frame(
    do.call(rbind,
            lapply(xmlChildren(xmlRoot(words)),
                   function(node) c(id=xmlValue(xmlChildren(node)$id), 
                                    word=xmlValue(xmlChildren(node)$word), 
                                    grammar=xmlValue(xmlChildren(node)$grammar)))
    )
  )
  rownames(words) <- NULL
  words
}


filterByGrammar <- function(words, grammar) {
  
}

# as.data.frame(
#  do.call(
#   rbind, 
#   lapply(
#      getNodeSet(words_list, "//words/word"), function(node) c(Word = xmlValue(xmlChildren(node)$word), Id = xmlValue(xmlChildren(node)$id))
#   )
#  )
# )