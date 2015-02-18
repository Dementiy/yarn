#' Get words by API
#' 
#' Short description of this function
#' @param page Page
#' @param start Start
#' @import httr
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

# head(words2df(getWordsAPI(page=1, start='помо')))

# as.data.frame(
#  do.call(
#   rbind, 
#   lapply(
#      getNodeSet(words_list, "//words/word"), function(node) c(Word = xmlValue(xmlChildren(node)$word), Id = xmlValue(xmlChildren(node)$id))
#   )
#  )
# )